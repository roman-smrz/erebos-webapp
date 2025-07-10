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
import Erebos.DirectMessage
import Erebos.Discovery
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
import WebSocket

main :: IO ()
main = error "unused"

data GlobalState = GlobalState
    { globalStorage :: Storage
    , globalHead :: Head LocalState
    }

initGlobalState :: IO GlobalState
initGlobalState = do
    globalStorage <- memoryStorage
    identity <- createIdentity globalStorage Nothing Nothing
    globalHead <- storeHead globalStorage $ LocalState
        { lsPrev = Nothing, lsIdentity = idExtData identity, lsShared = [], lsOther = [] }
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
            H.ul ! A.id "msg_list" $ return ()
        H.form ! A.id "msg_form" ! A.action "javascript:void(0);" $ do
            H.input ! A.id "msg_text" ! A.type_ "text"
            H.input ! A.type_ "submit" ! A.value "send"
        H.hr
        H.div $ do
            H.h2 $ do
                "Peers"
            H.ul ! A.id "peer_list" $ return ()

    GlobalState {..} <- initGlobalState

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
        content <- js_document_createTextNode $ toJSString $ formatDirectMessage tzone $ fromStored msg
        js_appendChild li content
        js_appendChild messagesList li

    sendText <- JS.getElementById "msg_text"
    sendForm <- JS.getElementById "msg_form"

    server <- startServer defaultServerOptions globalHead JS.consoleLog
        [ someService @ChatroomService Proxy
        , someService @DiscoveryService Proxy
        , someService @DirectMessage Proxy
        , someService @SyncService Proxy
        ]

    peerList <- JS.getElementById "peer_list"
    watchPeers server peerList

    startClient "localhost" 9160 "" $ \conn -> do
        void $ forkIO $ forever $ do
            msg <- receiveMessage conn
            receivedFromCustomAddress server conn msg

        peer <- serverPeerCustom server conn
        JS.addEventListener sendForm "submit" $ \_ -> do
            peerIdentity peer >>= \case
                PeerIdentityUnknown {} -> JS.consoleLog "unknown peer identity"
                PeerIdentityRef {} -> JS.consoleLog "unresolved peer identity"
                PeerIdentityFull pid -> do
                    msg <- T.pack . fromJSString <$> js_get_value sendText
                    js_set_value sendText $ toJSString ""
                    res <- runExceptT $ flip runReaderT globalHead $ sendDirectMessage pid msg
                    case res of
                        Right _ -> return ()
                        Left err -> JS.consoleLog $ "Failed to send message: " <> err


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


watchPeers :: Server -> JSVal -> IO ()
watchPeers server htmlList = do
    peers <- liftIO $ newMVar []
    void $ forkIO $ void $ forever $ do
        peer <- getNextPeerChange server
        peerIdentity peer >>= \case
            pid@(PeerIdentityFull _) -> do
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
                    li <- js_document_createElement (toJSString "li")
                    content <- js_document_createTextNode $ toJSString $ updateType' <> " " <> shown
                    js_appendChild li content
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

foreign import javascript unsafe "$1.appendChild($2)"
    js_appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "document.createElement($1)"
    js_document_createElement :: JSString -> IO JSVal

foreign import javascript unsafe "document.createTextNode($1)"
    js_document_createTextNode :: JSString -> IO JSVal

foreign import javascript unsafe "$1.value"
    js_get_value :: JSVal -> IO JSString

foreign import javascript unsafe "$1.value = $2"
    js_set_value :: JSVal -> JSString -> IO ()
