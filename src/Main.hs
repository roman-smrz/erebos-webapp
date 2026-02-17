module Main (main, setup) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.ByteString.Char8 qualified as BC
import Data.Foldable
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Time.Format
import Data.Time.LocalTime

import GHC.Wasm.Prim

import Erebos.Chatroom
import Erebos.Conversation
import Erebos.DirectMessage
import Erebos.Discovery
import Erebos.Error
import Erebos.Identity
import Erebos.Invite
import Erebos.Network
import Erebos.Object
import Erebos.PubKey
import Erebos.Service
import Erebos.State
import Erebos.Storable
import Erebos.Storage
import Erebos.Sync

import Network.HTTP.Types.URI

import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Html.Renderer.String

import JavaScript qualified as JS
import Storage.Cache
import Storage.IndexedDB
import WebSocket (startClient, receiveMessage)

main :: IO ()
main = error "unused"

data GlobalState = GlobalState
    { globalStorage :: Storage
    , globalHead :: Head LocalState
    , peerListVar :: MVar [ ( Peer, String, JSVal ) ]
    , currentConversationVar :: MVar CurrentConversation
    , conversationsVar :: MVar [ ( Int, Conversation ) ]
    }

data CurrentConversation
    = NoCurrentConversation
    | SelectedConversation Conversation
    | WaitingForPeerConversation RefDigest InviteToken

initGlobalState :: IO GlobalState
initGlobalState = do
    --globalStorage <- memoryStorage
    globalStorage <- cacheStorage =<< indexedDBStorage "erebos"
    (either (fail . showErebosError) return =<<) $ runExceptT $ flip runReaderT globalStorage $ do
        globalHead <- loadHeads globalStorage >>= \case
            (h : _) -> do
                return h
            [] -> do
                let devName = T.pack "WebApp"
                owner <- createIdentity Nothing Nothing
                identity <- createIdentity (Just devName) (Just owner)

                shared <- mstore SharedState
                    { ssPrev = []
                    , ssType = Just $ sharedTypeID @(Maybe ComposedIdentity) Proxy
                    , ssValue = [ storedRef $ idExtData owner ]
                    }

                storeHead globalStorage $ LocalState
                    { lsPrev = Nothing
                    , lsIdentity = idExtData identity
                    , lsShared = [ shared ]
                    , lsOther = []
                    }

        peerListVar <- liftIO $ newMVar []
        currentConversationVar <- liftIO $ newMVar NoCurrentConversation
        conversationsVar <- liftIO $ newMVar []
        return GlobalState {..}

foreign export javascript setup :: IO ()
setup :: IO ()
setup = do
    body <- js_document_getElementById (toJSString "body")
    js_set_innerHTML body $ toJSString $ renderHtml $ do
        H.div ! A.id "sidebar" $ do
            H.div ! A.id "self" $ do
                H.h2 $ do
                    "Yourself"
                H.div $ do
                    "Name: "
                    H.span ! A.id "name_text" $ return ()
                H.div $ do
                    "("
                    H.span ! A.id "self_ref_value" $ return ()
                    ")"
                H.form ! A.id "name_set_form" ! A.action "javascript:void(0);" $ do
                    H.input ! A.id "name_set_input" ! A.type_ "text"
                    H.button ! A.type_ "submit" $ "set name"

            H.div ! A.id "conversations" $ do
                H.h2 $ do
                    "Conversations"
                H.div ! A.id "conversation_list" $ return ()

            H.div ! A.id "invite" $ do
                H.h2 $ do
                    "Invite contact"
                H.form ! A.id "invite_generate" ! A.action "javascript:void(0);" $ do
                    "Name: "
                    H.input ! A.id "invite_name"
                    H.button ! A.type_ "submit" $ "create invite"
                H.div ! A.id "invite_generated" $ do
                    H.span ! A.id "invite_generated_url" $ return ()
                    H.button ! A.id "invite_clipboard" $ "copy to clipboard"

            H.div ! A.id "peers" $ do
                H.h2 $ do
                    "Peers"
                H.ul ! A.id "peer_list" $ return ()
                H.form ! A.id "peer_add_form" ! A.action "javascript:void(0);" $ do
                    H.input ! A.id "peer_add_input" ! A.type_ "text"
                    H.input ! A.type_ "submit" ! A.value "search"

        H.div ! A.id "conversation" $ do
            H.h2 ! A.id "msg_header" $ do
                return ()
            H.div ! A.id "msg_list" $ do
                H.ul $ return ()
            H.form ! A.id "msg_form" ! A.action "javascript:void(0);" $ do
                H.input ! A.id "msg_text" ! A.type_ "text"
                H.button ! A.type_ "submit" $ "send"


    gs@GlobalState {..} <- initGlobalState
    watchIdentityUpdates gs
    watchConversations gs

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
    void $ watchDirectMessageThreads globalHead $ \prev cur -> do
        withMVar currentConversationVar $ \case
            SelectedConversation conv
              | maybe False (msgPeer cur `sameIdentity`) (conversationPeer conv)
              -> do
                ul <- js_get_firstChild messagesList
                appendMessages gs ul $ map Left $ reverse $ dmThreadToListSince prev cur

            _ -> return ()

    sendText <- JS.getElementById "msg_text"
    sendForm <- JS.getElementById "msg_form"

    server <- startServer defaultServerOptions globalHead JS.consoleLog
        [ someService @ChatroomService Proxy
        , someService @DiscoveryService Proxy
        , someService @DirectMessage Proxy
        , someService @SyncService Proxy
        , someService @InviteService Proxy
        ]

    peerList <- JS.getElementById "peer_list"
    watchPeers gs server peerList

    startClient "a.discovery.erebosprotocol.net" 443 "" $ \conn -> do
        void $ forkIO $ forever $ do
            msg <- receiveMessage conn
            receivedFromCustomAddress server conn msg
        void $ serverPeerCustom server conn

    inviteGenerateInput <- JS.getElementById "invite_name"
    inviteGenerateForm <- JS.getElementById "invite_generate"
    inviteGeneratedUrl <- JS.getElementById "invite_generated_url"
    inviteClipboard <- JS.getElementById "invite_clipboard"
    JS.addEventListener inviteGenerateForm "submit" $ \_ -> do
        name <- T.pack . fromJSString <$> js_get_value inviteGenerateInput
        js_set_value inviteGenerateInput $ toJSString ""
        href <- fromJSString <$> js_get_location_href
        res <- runExceptT $ flip runReaderT globalHead $ do
            (lookupSharedValue . lsShared . fromStored <$> getLocalHead) >>= \case
                Just (self :: ComposedIdentity) -> do
                    invite <- createSingleContactInvite name
                    dgst : _ <- return $ refDigest . storedRef <$> idDataF self
                    return $ href <> "#inv" <> (maybe "" (("=" <>) . showInviteToken) (inviteToken invite)) <> "&from=blake2%23" <> drop 7 (show dgst)
                Nothing -> do
                    throwOtherError "no shared identity"
        case res of
            Right inviteText -> js_set_textContent inviteGeneratedUrl $ toJSString inviteText
            Left err -> JS.consoleLog $ "Failed to send message: " <> showErebosError err
    JS.addEventListener inviteClipboard "click" $ \_ -> do
        js_navigator_clipboard_writeText =<< js_get_textContent inviteGeneratedUrl

    peerAddInput <- JS.getElementById "peer_add_input"
    peerAddForm <- JS.getElementById "peer_add_form"
    JS.addEventListener peerAddForm "submit" $ \_ -> do
        value <- T.pack . fromJSString <$> js_get_value peerAddInput
        js_set_value peerAddInput $ toJSString ""
        case readRefDigest $ encodeUtf8 value of
            Just dgst -> runExceptT (discoverySearch server dgst) >>= \case
                Right _ -> return ()
                Left err -> JS.consoleLog $ "Failed to search for " <> show dgst <> ": " <> showErebosError err
            Nothing -> JS.consoleLog "invalid identity reference"

    JS.addEventListener sendForm "submit" $ \_ -> do
        readMVar currentConversationVar >>= \case
            NoCurrentConversation -> JS.consoleLog "no selected conversation"
            SelectedConversation conv -> do
                msg <- T.pack . fromJSString <$> js_get_value sendText
                js_set_value sendText $ toJSString ""
                res <- runExceptT $ flip runReaderT globalHead $ sendMessage conv msg
                case res of
                    Right _ -> return ()
                    Left err -> JS.consoleLog $ "Failed to send message: " <> showErebosError err
            WaitingForPeerConversation _ _ -> JS.consoleLog "waiting for peer to start conversation"

    processUrlParams gs server


processUrlParams :: GlobalState -> Server -> IO ()
processUrlParams GlobalState {..} server = do
    hash <- fromJSString <$> js_get_location_hash
    case hash of
        '#' : str -> do
            let params = parseQuery $ BC.pack str
            if
              | Just token <- parseInviteToken . decodeUtf8 =<< id =<< lookup "inv" params
              , Just from <- readRefDigest =<< id =<< lookup "from" params
              -> do
                void $ swapMVar currentConversationVar $ WaitingForPeerConversation from token
                runExceptT (discoverySearch server from) >>= \case
                    Right () -> return ()
                    Left err -> JS.consoleLog $ "Failed to search for " <> show from <> ": " <> showErebosError err

              | otherwise -> do
                JS.consoleLog $ "Unrecognized URL parameters: " <> show params

            js_history_pushState (toJSString " ")

        _ -> return ()


watchIdentityUpdates :: GlobalState -> IO ()
watchIdentityUpdates GlobalState {..} = do
    nameElem <- js_document_getElementById (toJSString "name_text")
    selfRefElem <- js_document_getElementById (toJSString "self_ref_value")
    void $ watchHeadWith globalHead headLocalIdentity $ \lid -> do
        let fowner = finalOwner lid
        js_set_textContent nameElem $ toJSString $ maybe "(Anonymous)" T.unpack $ idName fowner
        js_set_textContent selfRefElem $ toJSString $ maybe "" (show . refDigest . storedRef) $ listToMaybe $ idDataF fowner

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


watchConversations :: GlobalState -> IO ()
watchConversations gs@GlobalState {..} = do
    void $ watchHead globalHead $ \ls -> do
        modifyMVar_ conversationsVar $ \_ -> do
            conversations <- zip [1 ..] . fst <$>
                runLocalHeadT lookupConversations globalStorage (headStoredObject ls)

            modifyMVar_ currentConversationVar $ \case
                x@(SelectedConversation selected) -> do
                    let updateCurrent [] = return x
                        updateCurrent (( _, c ) : cs)
                            | c `isSameConversation` selected = do
                                when (conversationName c /= conversationName selected) $ do
                                    header <- JS.getElementById "msg_header"
                                    js_set_textContent header $ toJSString $ T.unpack $ conversationName c
                                return $ SelectedConversation c
                            | otherwise = do
                                updateCurrent cs
                    updateCurrent conversations
                x -> return x

            convList <- JS.getElementById "conversation_list"
            ul <- js_document_createElement (toJSString "ul")
            forM_ conversations $ \( _, conv ) -> do
                li <- js_document_createElement (toJSString "li")
                a <- js_document_createElement (toJSString "a")
                js_setAttribute a (toJSString "href") (toJSString "javascript:void(0)")
                JS.addEventListener a "click" $ \_ -> do
                    js_removeClassFromAllChildren ul (toJSString "selected")
                    js_classList_add li (toJSString "selected")
                    body <- JS.getElementById "body"
                    js_classList_add body (toJSString "conversation-selected")
                    selectConversation gs conv

                js_set_textContent a $ toJSString $ T.unpack $ conversationName conv
                js_appendChild li a
                js_appendChild ul li
            js_replaceChildren convList ul

            return conversations

appendMessages :: GlobalState -> JSVal -> [ Either DirectMessage Message ] -> IO ()
appendMessages GlobalState {..} ul msgs = do
    tzone <- getCurrentTimeZone
    mbSelf <- join . fmap (lookupSharedValue @(Maybe ComposedIdentity) . lsShared . headObject) <$> reloadHead globalHead
    forM_ msgs $ \msg -> do
        let parts =
                [ ( "msg-time", formatTime defaultTimeLocale "%H:%M" $ utcToLocalTime tzone $ zonedTimeToUTC $ either msgTime messageTime msg )
                , ( "msg-from", maybe "<unnamed>" T.unpack $ idName $ either msgFrom messageFrom msg )
                , ( "msg-text", maybe "" T.unpack $ either (Just . msgText) messageText msg )
                ]
        li <- js_document_createElement (toJSString "li")
        forM_ parts $ \( cls, content ) -> do
            element <- js_document_createElement (toJSString "span")
            js_classList_add element $ toJSString cls
            js_set_textContent element $ toJSString content
            js_appendChild li element

        case mbSelf of
            Just self -> js_classList_add li $
                if either msgFrom messageFrom msg `sameIdentity` self then toJSString "sent" else toJSString "received"
            Nothing -> return ()
        js_appendChild ul li

selectConversation :: GlobalState -> Conversation -> IO ()
selectConversation gs@GlobalState {..} conv = do
    void $ swapMVar currentConversationVar (SelectedConversation conv)
    header <- JS.getElementById "msg_header"
    messagesList <- JS.getElementById "msg_list"

    ul <- js_document_createElement (toJSString "ul")
    appendMessages gs ul $ map Right $ reverse $ conversationHistory conv

    js_set_textContent header $ toJSString $ T.unpack $ conversationName conv
    js_replaceChildren messagesList ul


watchPeers :: GlobalState -> Server -> JSVal -> IO ()
watchPeers gs@GlobalState {..} server htmlList = do
    void $ forkIO $ void $ forever $ do
        peer <- getNextPeerChange server
        getPeerIdentity peer >>= \case
            pid@(PeerIdentityFull pidf) -> do
                dropped <- isPeerDropped peer
                shown <- showPeer pid <$> getPeerAddress peer
                let update [] = do
                        a <- js_document_createElement (toJSString "a")
                        js_setAttribute a (toJSString "href") (toJSString "javascript:void(0)")
                        JS.addEventListener a "click" $ \_ -> do
                            pidf' <- getPeerIdentity peer >>= \case
                                PeerIdentityFull pidf' -> return pidf'
                                _                      -> return pidf
                            selectConversation gs =<< runReaderT (directMessageConversation $ finalOwner pidf') globalHead

                        li <- js_document_createElement (toJSString "li")
                        js_set_textContent a $ toJSString shown
                        js_appendChild li a
                        js_appendChild htmlList li
                        return [ ( peer, shown, li ) ]

                    update (( p, s, li ) : ps)
                        | p == peer && dropped
                        = do
                            js_element_remove li
                            return ps

                        | p == peer
                        = do
                            when (shown /= s) $ do
                                a <- js_get_firstChild li
                                js_set_textContent a $ toJSString shown
                            return (( peer, shown, li ) : ps)

                        | otherwise
                        = do
                            (( p, s, li ) :) <$> update ps
                modifyMVar_ peerListVar update

                readMVar currentConversationVar >>= \case
                    WaitingForPeerConversation dgst token
                      | dgst `elem` identityDigests pidf -> do
                          Just h <- reloadHead globalHead
                          (either (fail . showErebosError) return =<<) $ runExceptT $ flip runReaderT h $ do
                              acceptInvite dgst token
                          selectConversation gs =<< runReaderT (directMessageConversation $ finalOwner pidf) globalHead
                    _ -> return ()

            _ -> return ()

showPeer :: PeerIdentity -> PeerAddress -> String
showPeer pidentity paddr =
    let name = case pidentity of
                    PeerIdentityUnknown _  -> "<noid>"
                    PeerIdentityRef wref _ -> "<" ++ BC.unpack (showRefDigest $ wrDigest wref) ++ ">"
                    PeerIdentityFull pid   -> T.unpack $ displayIdentity pid
     in name ++ " [" ++ show paddr ++ "]"

identityDigests :: Foldable f => Identity f -> [ RefDigest ]
identityDigests pid = map (refDigest . storedRef) $ idDataF =<< unfoldOwners pid


foreign import javascript unsafe "document.getElementById($1)"
    js_document_getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "$1.innerHTML = $2"
    js_set_innerHTML :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.textContent = $2"
    js_set_textContent :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.textContent"
    js_get_textContent :: JSVal -> IO JSString

foreign import javascript unsafe "$1.firstChild"
    js_get_firstChild :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.appendChild($2)"
    js_appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.replaceChildren($2)"
    js_replaceChildren :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "document.createElement($1)"
    js_document_createElement :: JSString -> IO JSVal

foreign import javascript unsafe "$1.remove()"
    js_element_remove :: JSVal -> IO ()

foreign import javascript unsafe "$1.setAttribute($2, $3)"
    js_setAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$1.classList.add($2)"
    js_classList_add :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.querySelectorAll('*').forEach(child => child.classList.remove($2))"
    js_removeClassFromAllChildren :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.value"
    js_get_value :: JSVal -> IO JSString

foreign import javascript unsafe "$1.value = $2"
    js_set_value :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "window.location.hash"
    js_get_location_hash :: IO JSString

foreign import javascript unsafe "window.location.href"
    js_get_location_href :: IO JSString

foreign import javascript unsafe "history.pushState(null, '', $1)"
    js_history_pushState :: JSString -> IO ()

foreign import javascript unsafe "navigator.clipboard.writeText($1)"
    js_navigator_clipboard_writeText :: JSString -> IO ()
