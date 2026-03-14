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
    , currentContextVar :: MVar SelectedContext
    , conversationsVar :: MVar [ ( Int, Conversation ) ]
    }

data SelectedContext
    = NoContext
    | SelectedConversation Conversation
    | WaitingForPeerConversation RefDigest InviteToken
    | SelectedPeer (Either RefDigest Peer)

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
        currentContextVar <- liftIO $ newMVar NoContext
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

        H.div ! A.id "conversation" ! A.class_ "selected-content" $ do
            H.h2 ! A.id "msg_header" $ do
                return ()
            H.div ! A.id "msg_list" $ do
                H.ul $ return ()
            H.form ! A.id "msg_form" ! A.action "javascript:void(0);" $ do
                H.input ! A.id "msg_text" ! A.type_ "text"
                H.button ! A.type_ "submit" $ "send"

        H.div ! A.id "peer_details" ! A.class_ "selected-content" $ do
            H.h2 ! A.id "peer_name" $ do
                "Peer: "
                H.span ! A.id "peer_name_value" $ do
                    return ()
            H.div ! A.class_ "content" $ do
                H.div ! A.id "peer_address" $ do
                    H.span ! A.class_ "label" $ "Address:"
                    " "
                    H.span ! A.id "peer_address_value" $ return ()
                H.div ! A.id "peer_ref" $ do
                    H.span ! A.class_ "label" $ "Ref:"
                    " "
                    H.span ! A.id "peer_ref_value" $ return ()
                H.div ! A.id "peer_dm" $ do
                    H.a ! A.id "peer_dm_link" $ do
                        "Direct message"


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
        withMVar currentContextVar $ \case
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

    startClient server "a.discovery.erebosprotocol.net" 443 "" $ \conn -> do
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
        origin <- fromJSString <$> js_get_location_origin
        pathname <- fromJSString <$> js_get_location_pathname
        res <- runExceptT $ flip runReaderT globalHead $ do
            (lookupSharedValue . lsShared . fromStored <$> getLocalHead) >>= \case
                Just (self :: ComposedIdentity) -> do
                    invite <- createSingleContactInvite name
                    dgst : _ <- return $ refDigest . storedRef <$> idDataF self
                    return $ origin <> pathname <> "#inv" <> (maybe "" (("=" <>) . showInviteToken) (inviteToken invite)) <> "&from=blake2%23" <> drop 7 (show dgst)
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
        readMVar currentContextVar >>= \case
            NoContext -> JS.consoleLog "no selected conversation"
            SelectedConversation conv -> do
                msg <- T.pack . fromJSString <$> js_get_value sendText
                js_set_value sendText $ toJSString ""
                res <- runExceptT $ flip runReaderT globalHead $ sendMessage conv msg
                case res of
                    Right _ -> return ()
                    Left err -> JS.consoleLog $ "Failed to send message: " <> showErebosError err
            WaitingForPeerConversation _ _ -> JS.consoleLog "waiting for peer to start conversation"
            SelectedPeer {} -> JS.consoleLog "selected peer, not conversation"

    JS.addEventListener js_window "hashchange" $ \_ -> do
        processUrlParams gs server
    processUrlParams gs server


processUrlParams :: GlobalState -> Server -> IO ()
processUrlParams gs@GlobalState {..} server = do
    hash <- fromJSString <$> js_get_location_hash
    case hash of
        '#' : str -> do
            let params = parseQuery $ BC.pack str
            if
              | Just dgst <- readRefDigest . ("blake2#" <>) =<< id =<< lookup "conv" params
              -> do
                reloadHead globalHead >>= \case
                    Just ls -> do
                        runLocalHeadT (lookupConversationByRef dgst) globalStorage (headStoredObject ls) >>= \case
                            ( Just conv, _ ) -> do
                                readMVar currentContextVar >>= \case
                                    SelectedConversation selected
                                        | selected `isSameConversation` conv -> do
                                            return ()
                                    _ -> do
                                        selectConversation gs conv
                            ( Nothing, _ ) -> do
                                let match p = do
                                        getPeerIdentity p >>= \case
                                            PeerIdentityFull pid -> return $ dgst `elem` (refDigest . storedRef <$> idDataF (finalOwner pid))
                                            _ -> return False
                                findPeer server match >>= \case
                                    Just peer -> do
                                        getPeerIdentity peer >>= \case
                                            PeerIdentityFull pid -> do
                                                selectConversation gs =<< runReaderT (directMessageConversation $ finalOwner pid) globalHead
                                            _ -> return ()
                                    Nothing -> refFromDigest globalStorage dgst >>= \case
                                        Just ref
                                            | Just pid <- validateIdentity (wrappedLoad ref)
                                            , Nothing <- idOwner pid
                                            -> do
                                                selectConversation gs =<< runReaderT (directMessageConversation $ toComposedIdentity pid) globalHead

                                            | otherwise -> do
                                                JS.consoleLog $ "Failed to validate conversation peer"

                                        Nothing -> do
                                            JS.consoleLog $ "Failed to load conversation ref"

                    Nothing -> JS.consoleLog "Failed to reload local state head"

              | Just dgst <- readRefDigest . ("blake2#" <>) =<< id =<< lookup "peer" params
              -> do
                selectPeer gs server dgst

              | Just token <- parseInviteToken . decodeUtf8 =<< id =<< lookup "inv" params
              , Just from <- readRefDigest =<< id =<< lookup "from" params
              -> do
                void $ swapMVar currentContextVar $ WaitingForPeerConversation from token
                runExceptT (discoverySearch server from) >>= \case
                    Right () -> return ()
                    Left err -> JS.consoleLog $ "Failed to search for " <> show from <> ": " <> showErebosError err
                js_history_pushState (toJSString " ")

              | otherwise -> do
                JS.consoleLog $ "Unrecognized URL parameters: " <> show params

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
watchConversations GlobalState {..} = do
    void $ watchHead globalHead $ \ls -> do
        modifyMVar_ conversationsVar $ \_ -> do
            conversations <- fst <$>
                runLocalHeadT lookupConversations globalStorage (headStoredObject ls)

            conversations' <- modifyMVar currentContextVar $ \case
                x@(SelectedConversation selected) -> do
                    let updateCurrent [] = return ( x, [] )
                        updateCurrent (c : cs)
                            | c `isSameConversation` selected = do
                                when (conversationName c /= conversationName selected) $ do
                                    header <- JS.getElementById "msg_header"
                                    js_set_textContent header $ toJSString $ T.unpack $ conversationName c
                                return ( SelectedConversation c, ( c, True ) : map (, False) cs )
                            | otherwise = do
                                fmap (( c, False ) :) <$> updateCurrent cs
                    updateCurrent conversations
                x -> return ( x, map (, False ) conversations )

            convList <- JS.getElementById "conversation_list"
            ul <- js_document_createElement (toJSString "ul")
            forM_ conversations' $ \( conv, selected ) -> do
                li <- js_document_createElement (toJSString "li")
                when selected $ do
                    js_classList_add li (toJSString "selected")
                js_setAttribute li (toJSString "data-conv") $ toJSString $ show $ conversationReference conv
                a <- js_document_createElement (toJSString "a")
                js_setAttribute a (toJSString "href") $ toJSString $ "#conv=" <> drop 7 (show $ conversationReference conv)

                js_set_textContent a $ toJSString $ T.unpack $ conversationName conv
                js_appendChild li a
                js_appendChild ul li
            js_replaceChildren convList ul

            return $ zip [ 1 .. ] conversations

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
    modifyMVar_ currentContextVar $ \cur -> do
        if
          | SelectedConversation conv' <- cur
          , conv' `isSameConversation` conv -> do
              return ()

          | otherwise -> do
            header <- JS.getElementById "msg_header"
            messagesList <- JS.getElementById "msg_list"

            ul <- js_document_createElement (toJSString "ul")
            appendMessages gs ul $ map Right $ reverse $ conversationHistory conv

            js_set_textContent header $ toJSString $ T.unpack $ conversationName conv
            js_replaceChildren messagesList ul

            JS.documentQuerySelector "ul#peer_list" >>= \case
                Just ulPeers -> do
                    js_removeClassFromAllChildren ulPeers (toJSString "selected")
                Nothing -> return ()
            JS.documentQuerySelector "#conversation_list ul" >>= \case
                Just ulConv -> do
                    js_removeClassFromAllChildren ulConv (toJSString "selected")
                    maybe (return ()) (`js_classList_add` toJSString "selected") =<<
                        JS.querySelector ("li[data-conv='" <> show (conversationReference conv) <> "']") ulConv
                Nothing -> return ()

            body <- JS.getElementById "body"
            js_classList_remove body (toJSString "peer-selected")
            js_classList_add body (toJSString "conversation-selected")

        return $ SelectedConversation conv

selectPeer :: GlobalState -> Server -> RefDigest -> IO ()
selectPeer GlobalState {..} server dgst = do
    let match p = do
            getPeerIdentity p >>= \case
                PeerIdentityFull pid -> return $ dgst == (refDigest $ storedRef $ idData pid)
                _ -> return False
    modifyMVar_ currentContextVar $ \_ -> do
        selected <- findPeer server match >>= \case
            Just peer -> do
                updatePeerDetails peer
                return $ SelectedPeer $ Right peer
            Nothing -> do
                return $ SelectedPeer $ Left dgst

        JS.documentQuerySelector "#conversation_list ul" >>= \case
            Just ul -> do
                js_removeClassFromAllChildren ul (toJSString "selected")
            Nothing -> return ()
        JS.documentQuerySelector "ul#peer_list" >>= \case
            Just ul -> do
                js_removeClassFromAllChildren ul (toJSString "selected")
                maybe (return ()) (`js_classList_add` toJSString "selected") =<<
                    JS.querySelector ("li[data-peer='" <> show dgst <> "']") ul
            Nothing -> return ()

        body <- JS.getElementById "body"
        js_classList_remove body (toJSString "conversation-selected")
        js_classList_add body (toJSString "peer-selected")
        return selected

updatePeerDetails :: Peer -> IO ()
updatePeerDetails peer = do
    nameElem <- JS.getElementById "peer_name_value"
    refElem <- JS.getElementById "peer_ref_value"
    addrElem <- JS.getElementById "peer_address_value"
    dmLinkElem <- JS.getElementById "peer_dm_link"
    paddr <- getPeerAddress peer
    pid <- getPeerIdentity peer
    js_set_textContent nameElem $ toJSString $ showPeer pid
    case pid of
        PeerIdentityFull pidf -> do
            js_set_textContent refElem $ toJSString $ show $ refDigest $ storedRef $ idData pidf
            js_setAttribute dmLinkElem (toJSString "href") $ toJSString $ "#conv=" <> drop 7 (show $ refDigest $ storedRef $ head $ idDataF $ finalOwner pidf)
        PeerIdentityRef wref _ -> do
            js_set_textContent refElem $ toJSString $ show $ wrDigest wref
        PeerIdentityUnknown _ -> do
            js_set_textContent refElem $ toJSString "unknown"
    js_set_textContent addrElem $ toJSString $ show paddr


watchPeers :: GlobalState -> Server -> JSVal -> IO ()
watchPeers gs@GlobalState {..} server htmlList = do
    void $ forkIO $ void $ forever $ do
        peer <- getNextPeerChange server
        getPeerIdentity peer >>= \case
            pid@(PeerIdentityFull pidf) -> do
                selected <- modifyMVar currentContextVar $ \case
                    SelectedPeer (Right peer') | peer' == peer -> do
                        return ( SelectedPeer $ Right peer, True )
                    SelectedPeer (Left dgst) | dgst == refDigest (storedRef $ idData pidf) -> do
                        return ( SelectedPeer $ Right peer, True )
                    x -> return ( x, False )
                when selected $ do
                    updatePeerDetails peer
                dropped <- isPeerDropped peer
                paddr <- getPeerAddress peer
                let shown = showPeer pid ++ " [" ++ show paddr ++ "]"
                let update [] = do
                        a <- js_document_createElement (toJSString "a")
                        js_setAttribute a (toJSString "href") (toJSString "javascript:void(0)")
                        js_setAttribute a (toJSString "href") $ toJSString $ "#peer=" <> drop 7 (show $ refDigest $ storedRef $ idData pidf)

                        li <- js_document_createElement (toJSString "li")
                        when selected $ do
                            js_classList_add li (toJSString "selected")
                        js_setAttribute li (toJSString "data-peer") $ toJSString $ show $ refDigest $ storedRef $ idData pidf
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

                readMVar currentContextVar >>= \case
                    WaitingForPeerConversation dgst token
                      | dgst `elem` identityDigests pidf -> do
                          Just h <- reloadHead globalHead
                          (either (fail . showErebosError) return =<<) $ runExceptT $ flip runReaderT h $ do
                              acceptInvite dgst token
                          selectConversation gs =<< runReaderT (directMessageConversation $ finalOwner pidf) globalHead
                    _ -> return ()

            _ -> return ()

showPeer :: PeerIdentity -> String
showPeer = \case
    PeerIdentityUnknown _  -> "<noid>"
    PeerIdentityRef wref _ -> "<" ++ BC.unpack (showRefDigest $ wrDigest wref) ++ ">"
    PeerIdentityFull pid   -> T.unpack $ displayIdentity pid

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

foreign import javascript unsafe "$1.classList.remove($2)"
    js_classList_remove :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.querySelectorAll('*').forEach(child => child.classList.remove($2))"
    js_removeClassFromAllChildren :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.value"
    js_get_value :: JSVal -> IO JSString

foreign import javascript unsafe "$1.value = $2"
    js_set_value :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "window"
    js_window :: JSVal

foreign import javascript unsafe "window.location.hash"
    js_get_location_hash :: IO JSString

foreign import javascript unsafe "window.location.origin"
    js_get_location_origin :: IO JSString

foreign import javascript unsafe "window.location.pathname"
    js_get_location_pathname :: IO JSString

foreign import javascript unsafe "history.pushState(null, '', $1)"
    js_history_pushState :: JSString -> IO ()

foreign import javascript unsafe "navigator.clipboard.writeText($1)"
    js_navigator_clipboard_writeText :: JSString -> IO ()
