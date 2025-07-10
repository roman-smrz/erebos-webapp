module Main (main, setup) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Bifunctor
import Data.ByteString.Char8 qualified as BC
import Data.Foldable
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.LocalTime

import GHC.Wasm.Prim

import Erebos.Chatroom
import Erebos.Conversation
import Erebos.DirectMessage
import Erebos.Discovery
import Erebos.Error
import Erebos.Identity
import Erebos.Network
import Erebos.Object
import Erebos.PubKey
import Erebos.Service
import Erebos.State
import Erebos.Storable
import Erebos.Storage
import Erebos.Sync

import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Html.Renderer.String

import JavaScript qualified as JS
import WebSocket (startClient, receiveMessage)

main :: IO ()
main = error "unused"

data GlobalState = GlobalState
    { globalStorage :: Storage
    , globalHead :: Head LocalState
    , currentConversationVar :: MVar (Maybe Conversation)
    }

initGlobalState :: IO GlobalState
initGlobalState = do
    globalStorage <- memoryStorage
    identity <- createIdentity globalStorage Nothing Nothing
    globalHead <- storeHead globalStorage $ LocalState
        { lsPrev = Nothing, lsIdentity = idExtData identity, lsShared = [], lsOther = [] }
    currentConversationVar <- newMVar Nothing
    return GlobalState {..}

foreign export javascript setup :: IO ()
setup :: IO ()
setup = do
    body <- js_document_getElementById (toJSString "body")
    js_set_innerHTML body $ toJSString $ renderHtml $ do
        H.div $ do
            "Name: "
            H.span ! A.id "name_text" $ return ()
        H.form ! A.id "name_set_form" ! A.action "javascript:void(0);" $ do
            H.input ! A.id "name_set_input" ! A.type_ "text"
            H.input ! A.type_ "submit" ! A.value "set name"
        H.hr
        H.div $ do
            H.h2 ! A.id "msg_header" $ do
                return ()
            H.div ! A.id "msg_list" $ do
                H.ul $ return ()
        H.form ! A.id "msg_form" ! A.action "javascript:void(0);" $ do
            H.input ! A.id "msg_text" ! A.type_ "text"
            H.input ! A.type_ "submit" ! A.value "send"
        H.hr
        H.div $ do
            H.h2 $ do
                "Peers"
            H.ul ! A.id "peer_list" $ return ()

    gs@GlobalState {..} <- initGlobalState

    nameElem <- js_document_getElementById (toJSString "name_text")
    _ <- watchHead globalHead $ \ls -> do
        js_set_textContent nameElem $ toJSString $ maybe "(Anonymous)" T.unpack $ idName $ finalOwner $ headLocalIdentity ls

    let devName = T.pack "WebApp"
    let st = globalStorage
    owner <- createIdentity st Nothing Nothing
    identity <- createIdentity st (Just devName) (Just owner)

    shared <- wrappedStore st $ SharedState
        { ssPrev = []
        , ssType = Just $ sharedTypeID @(Maybe ComposedIdentity) Proxy
        , ssValue = [ storedRef $ idExtData owner ]
        }
    flip runReaderT globalHead $ do
        updateLocalState_ $ \_ -> do
            mstore $ LocalState
                { lsPrev = Nothing
                , lsIdentity = idExtData identity
                , lsShared = [ shared ]
                , lsOther = []
                }

    setNameInput <- JS.getElementById "name_set_input"
    setNameForm <- JS.getElementById "name_set_form"
    JS.addEventListener setNameForm "submit" $ \_ -> do
        name <- T.pack . fromJSString <$> js_get_value setNameInput
        js_set_value setNameInput $ toJSString ""
        Just h <- reloadHead globalHead
        res <- runExceptT $ flip runReaderT h $ updateSharedIdentity name
        case res of
            Right _ -> return ()
            Left err -> JS.consoleLog $ "Failed to set name: " <> showErebosError err

    messagesList <- JS.getElementById "msg_list"
    tzone <- getCurrentTimeZone
    void $ watchReceivedMessages globalHead $ \msg -> do
        li <- js_document_createElement (toJSString "li")
        js_set_textContent li $ toJSString $ formatDirectMessage tzone $ fromStored msg
        ul <- js_get_firstChild messagesList
        js_appendChild ul li

    sendText <- JS.getElementById "msg_text"
    sendForm <- JS.getElementById "msg_form"

    server <- startServer defaultServerOptions globalHead JS.consoleLog
        [ someService @ChatroomService Proxy
        , someService @DiscoveryService Proxy
        , someService @DirectMessage Proxy
        , someService @SyncService Proxy
        ]

    peerList <- JS.getElementById "peer_list"
    watchPeers gs server peerList

    startClient "localhost" 9160 "" $ \conn -> do
        void $ forkIO $ forever $ do
            msg <- receiveMessage conn
            receivedFromCustomAddress server conn msg

        void $ serverPeerCustom server conn

    JS.addEventListener sendForm "submit" $ \_ -> do
        readMVar currentConversationVar >>= \case
            Nothing -> JS.consoleLog "no selected conversation"
            Just conv -> do
                msg <- T.pack . fromJSString <$> js_get_value sendText
                js_set_value sendText $ toJSString ""
                res <- runExceptT $ flip runReaderT globalHead $ sendMessage conv msg
                case res of
                    Right _ -> return ()
                    Left err -> JS.consoleLog $ "Failed to send message: " <> showErebosError err


updateSharedIdentity :: (MonadHead LocalState m, MonadError e m, FromErebosError e) => Text -> m ()
updateSharedIdentity name = updateLocalState_ $ updateSharedState_ $ \case
    Just identity -> do
        Just . toComposedIdentity <$> interactiveIdentityUpdate name identity
    Nothing -> throwOtherError "no existing shared identity"

interactiveIdentityUpdate :: (Foldable f, MonadStorage m, MonadIO m, MonadError e m, FromErebosError e) => Text -> Identity f -> m UnifiedIdentity
interactiveIdentityUpdate name fidentity = do
    identity <- mergeIdentity fidentity
    if  | T.null name -> return identity
        | otherwise -> do
            secret <- loadKey $ idKeyIdentity identity
            maybe (throwOtherError "created invalid identity") return . validateExtendedIdentity =<<
                mstore =<< sign secret =<< mstore . ExtendedIdentityData =<< return (emptyIdentityExtension $ idData identity)
                { idePrev = toList $ idExtDataF identity
                , ideName = Just name
                }


selectConversation :: GlobalState -> Conversation -> IO ()
selectConversation GlobalState {..} conv = do
    void $ swapMVar currentConversationVar (Just conv)
    header <- JS.getElementById "msg_header"
    messagesList <- JS.getElementById "msg_list"

    tzone <- getCurrentTimeZone
    ul <- js_document_createElement (toJSString "ul")
    forM_ (reverse $ conversationHistory conv) $ \msg -> do
        li <- js_document_createElement (toJSString "li")
        js_set_textContent li $ toJSString $ formatMessage tzone msg
        js_appendChild ul li

    js_set_textContent header $ toJSString $ T.unpack $ conversationName conv
    js_replaceChildren messagesList ul


watchPeers :: GlobalState -> Server -> JSVal -> IO ()
watchPeers gs@GlobalState {..} server htmlList = do
    peers <- liftIO $ newMVar []
    void $ forkIO $ void $ forever $ do
        peer <- getNextPeerChange server
        peerIdentity peer >>= \case
            pid@(PeerIdentityFull pid') -> do
                dropped <- isPeerDropped peer
                let shown = showPeer pid $ peerAddress peer
                let update [] = ( [ ( peer, shown ) ], ( Nothing, "NEW" ) )
                    update (( p, s ) : ps)
                        | p == peer && dropped = ( ps, ( Nothing, "DEL" ) )
                        | p == peer = ( ( peer, shown ) : ps, ( Just s, "UPD" ) )
                        | otherwise = first ( ( p, s ) :) $ update ps
                (op, updateType) <- modifyMVar peers (return . update)
                let updateType' = if dropped then "DEL" else updateType
                when (Just shown /= op) $ do
                    a <- js_document_createElement (toJSString "a")
                    js_setAttribute a (toJSString "href") (toJSString "javascript:void(0)")
                    JS.addEventListener a "click" $ \_ -> do
                        Just h <- reloadHead globalHead
                        selectConversation gs =<< runReaderT (directMessageConversation $ finalOwner pid') h

                    li <- js_document_createElement (toJSString "li")
                    content <- js_document_createTextNode $ toJSString $ updateType' <> " " <> shown
                    js_appendChild a content
                    js_appendChild li a
                    js_appendChild htmlList li
            _ -> return ()

showPeer :: PeerIdentity -> PeerAddress -> String
showPeer pidentity paddr =
    let name = case pidentity of
                    PeerIdentityUnknown _  -> "<noid>"
                    PeerIdentityRef wref _ -> "<" ++ BC.unpack (showRefDigest $ wrDigest wref) ++ ">"
                    PeerIdentityFull pid   -> T.unpack $ displayIdentity pid
     in name ++ " [" ++ show paddr ++ "]"


foreign import javascript unsafe "document.getElementById($1)"
    js_document_getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "$1.innerHTML = $2"
    js_set_innerHTML :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.textContent = $2"
    js_set_textContent :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.firstChild"
    js_get_firstChild :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.appendChild($2)"
    js_appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.replaceChildren($2)"
    js_replaceChildren :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "document.createElement($1)"
    js_document_createElement :: JSString -> IO JSVal

foreign import javascript unsafe "document.createTextNode($1)"
    js_document_createTextNode :: JSString -> IO JSVal

foreign import javascript unsafe "$1.setAttribute($2, $3)"
    js_setAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$1.value"
    js_get_value :: JSVal -> IO JSString

foreign import javascript unsafe "$1.value = $2"
    js_set_value :: JSVal -> JSString -> IO ()
