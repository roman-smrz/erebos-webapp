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
import Version
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
    | SelectedSelf

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
    experimentalAccepted <- js_storage_getItem (toJSString "experimental-accepted")

    body <- js_document_getElementById (toJSString "body")
    js_set_innerHTML body $ toJSString $ renderHtml $ do
        H.div ! A.id "sidebar" $ do
            H.div ! A.id "self" $ do
                H.h2 $ do
                    "Yourself"
                H.ul $ do
                    H.li $ do
                        H.a ! A.href "#account" $ do
                            H.div $ do
                                H.span ! A.id "name_text" $ return ()

            H.div ! A.id "conversations" $ do
                H.h2 $ do
                    "Conversations"
                H.div ! A.id "conversations_inner" $ do
                    H.div ! A.id "conversation_list" $ return ()
                    H.ul $ do
                        H.li ! A.id "create_invite_item" $ do
                            H.a ! A.href "#create-invite" $ do
                                "Invite contact"

            H.div ! A.id "peers" !  A.class_ "collapsed" $ do
                H.h2 $ do
                    H.span $ "Peers (" >> (H.span ! A.id "peer_count" $ "0") >> ")"
                    H.preEscapedText $ T.concat
                        [ "<svg viewBox=\"0 0 100 100\" xmlns=\"http://www.w3.org/2000/svg\" class=\"collapse-icon\">"
                        , "<polygon points=\"0,25 100,25 50,75\" style=\"fill:currentColor\" />"
                        , "</svg>"
                        ]
                H.ul ! A.id "peer_list" $ return ()

        H.div ! A.id "conversation" ! A.class_ "selected-content" $ do
            H.h2 $ do
                H.span ! A.id "msg_header" $ return ()
                H.a ! A.href "#" ! A.class_ "back-button" $ "back"
            H.div ! A.id "msg_list" $ do
                H.ul $ return ()
            H.form ! A.id "msg_form" ! A.action "javascript:void(0);" $ do
                H.input ! A.id "msg_text" ! A.type_ "text"
                H.button ! A.type_ "submit" $ "send"

        H.div ! A.id "create_invite_details" ! A.class_ "selected-content" $ do
            H.h2 $ do
                "Invite contact"
                H.a ! A.href "#" ! A.class_ "back-button" $ "back"
            H.div ! A.class_ "content" $ do
                H.form ! A.id "invite_generate" ! A.action "javascript:void(0);" $ do
                    "Contact name: "
                    H.input ! A.id "invite_name" ! A.placeholder "(optional)"
                    H.button ! A.type_ "submit" $ "create invite"
                H.div ! A.id "invite_generated" $ do
                    H.h3 "Created invite"
                    "Contact name: "
                    H.span ! A.id "invite_generated_name" $ return ()
                    H.div $ do
                        "Invite URL:"
                        H.div ! A.id "invite_generated_url" $ return ()
                    H.button ! A.id "invite_clipboard" $ "copy to clipboard"
                    H.div ! A.class_ "notice" $ do
                        "To accept the invite, the intended recipient needs to receive the URL above by secure means and either open it in a web browser (to use this WebApp), or accept it using the "
                        H.code "/invite-accept"
                        " command in the CLI client."

        H.div ! A.id "peer_details" ! A.class_ "selected-content" $ do
            H.h2 ! A.id "peer_name" $ do
                "Peer: "
                H.span ! A.id "peer_name_value" $ do
                    return ()
                H.a ! A.href "#" ! A.class_ "back-button" $ "back"
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

        H.div ! A.id "account_details" ! A.class_ "selected-content" $ do
            H.h2 $ do
                "Your account"
                H.a ! A.href "#" ! A.class_ "back-button" $ "back"
            H.div ! A.class_ "content" $ do
                H.div ! A.class_ "notice" $ do
                    "All data are stored locally in browser storage.\
                     \ Deleting the site data will remove all the account information,\
                     \ including contacts, message history or cryptographic keys."

                H.div $ do
                    H.span ! A.class_ "label" $ "Name:"
                    " "
                    H.span ! A.id "name_text_detail" $ return ()

                H.div ! A.id "self_ref" $ do
                    H.span ! A.class_ "label" $ "Ref:"
                    " "
                    H.span ! A.id "self_ref_value" $ return ()

                H.h3 $ "Change name"
                H.form ! A.id "name_set_form" ! A.action "javascript:void(0);" $ do
                    H.input ! A.id "name_set_input" ! A.type_ "text"
                    H.button ! A.type_ "submit" $ "set name"

        H.div ! A.id "version" $ do
            H.toHtml versionLine

        when (js_string_is_null experimentalAccepted) $ do
            H.div ! A.id "experimental_warning" $ do
                H.div ! A.class_ "text" $ do
                    H.b "Experimental software. "
                    "Use at your own risk. "
                    H.br
                    "See "
                    H.a ! A.href "//erebosprotocol.net/webapp" ! A.target "_blank" $ "homepage"
                    " for details."
                H.a ! A.class_ "close-button" ! A.href "javascript:void(0)" $ do
                    H.preEscapedText $ T.concat
                        [ "<svg viewBox=\"0 0 100 100\" xmlns=\"http://www.w3.org/2000/svg\">"
                        , "<line x1=\"10\" y1=\"90\" x2=\"90\" y2=\"10\" stroke=\"currentColor\" stroke-width=\"12\" stroke-linecap=\"round\" />"
                        , "<line x1=\"10\" y1=\"10\" x2=\"90\" y2=\"90\" stroke=\"currentColor\" stroke-width=\"12\" stroke-linecap=\"round\" />"
                        , "</svg>"
                        ]


    gs@GlobalState {..} <- initGlobalState
    watchIdentityUpdates gs
    watchConversations gs

    JS.getElementById "name_set_input" >>= \case
        Just setNameInput -> JS.getElementById "name_set_form" >>= \case
            Just setNameForm -> JS.addEventListener setNameForm "submit" $ \_ -> do
                name <- T.pack . fromJSString <$> js_get_value setNameInput
                js_set_value setNameInput $ toJSString ""
                Just h <- reloadHead globalHead
                res <- runExceptT $ flip runReaderT h $ updateSharedIdentity name
                case res of
                    Right _ -> return ()
                    Left err -> JS.consoleLog $ "Failed to set name: " <> showErebosError err
            Nothing -> return ()
        Nothing -> return ()

    JS.getElementById "msg_list" >>= \case
        Just messagesList -> do
            void $ watchDirectMessageThreads globalHead $ \prev cur -> do
                withMVar currentContextVar $ \case
                    SelectedConversation conv
                      | maybe False (msgPeer cur `sameIdentity`) (conversationPeer conv)
                      -> do
                        scrollTop <- js_get_scrollTop messagesList
                        scrollHeight <- js_get_scrollHeight messagesList
                        clientHeight <- js_get_clientHeight messagesList

                        ul <- js_get_firstChild messagesList
                        appendMessages gs ul $ map Left $ reverse $ dmThreadToListSince prev cur

                        when (scrollTop + clientHeight >= scrollHeight) $ do
                            js_set_scrollTop messagesList =<< js_get_scrollHeight messagesList

                    _ -> return ()
        Nothing -> return ()

    Just sendText <- JS.getElementById "msg_text"
    Just sendForm <- JS.getElementById "msg_form"

    server <- startServer defaultServerOptions globalHead JS.consoleLog
        [ someService @ChatroomService Proxy
        , someService @DiscoveryService Proxy
        , someService @DirectMessage Proxy
        , someService @SyncService Proxy
        , someService @InviteService Proxy
        ]

    maybe (return ()) (watchPeers gs server) =<<
        JS.getElementById "peer_list"

    startClient server "a.discovery.erebosprotocol.net" 443 "" $ \conn -> do
        void $ forkIO $ forever $ do
            msg <- receiveMessage conn
            receivedFromCustomAddress server conn msg
        void $ serverPeerCustom server conn

    Just inviteGenerateInput <- JS.getElementById "invite_name"
    Just inviteGenerateForm <- JS.getElementById "invite_generate"
    Just inviteGeneratedName <- JS.getElementById "invite_generated_name"
    Just inviteGeneratedUrl <- JS.getElementById "invite_generated_url"
    Just inviteClipboard <- JS.getElementById "invite_clipboard"
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
            Right inviteText -> do
                mapM_ (flip js_classList_add $ toJSString "generated") =<< JS.getElementById "invite_generated"
                js_set_textContent inviteGeneratedName $ toJSString $ T.unpack name
                js_set_textContent inviteGeneratedUrl $ toJSString inviteText
            Left err -> do
                JS.consoleLog $ "Failed to send message: " <> showErebosError err
                mapM_ (flip js_classList_remove $ toJSString "generated") =<< JS.getElementById "invite_generated"
    JS.addEventListener inviteClipboard "click" $ \_ -> do
        js_navigator_clipboard_writeText =<< js_get_textContent inviteGeneratedUrl

    JS.getElementById "peers" >>= \case
        Just peersElem -> JS.querySelector "h2" peersElem >>= \case
            Just header -> do
                JS.addEventListener header "click" $ \_ -> do
                    js_classList_toggle peersElem (toJSString "collapsed")
            Nothing -> return ()
        Nothing -> return ()

    JS.addEventListener sendForm "submit" $ \_ -> do
        js_focus sendText
        readMVar currentContextVar >>= \case
            SelectedConversation conv -> do
                msg <- T.pack . fromJSString <$> js_get_value sendText
                js_set_value sendText $ toJSString ""
                res <- runExceptT $ flip runReaderT globalHead $ sendMessage conv msg
                case res of
                    Right _ -> return ()
                    Left err -> JS.consoleLog $ "Failed to send message: " <> showErebosError err
            WaitingForPeerConversation _ _ -> JS.consoleLog "waiting for peer to start conversation"
            _ -> JS.consoleLog "no selected conversation"

    JS.getElementById "experimental_warning" >>= \case
        Just experimentalWarning -> do
            JS.querySelector ".close-button" experimentalWarning >>= \case
                Just experimentalAccept -> do
                    JS.addEventListener experimentalAccept "click" $ \_ -> do
                        js_element_remove experimentalWarning
                        js_storage_setItem (toJSString "experimental-accepted") (toJSString "")
                Nothing -> return ()
        Nothing -> return ()

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

              | Just Nothing <- lookup "account" params
              -> do
                selectSelf gs server

              | Just Nothing <- lookup "create-invite" params
              -> do
                selectCreateInvite gs server

              | otherwise -> do
                JS.consoleLog $ "Unrecognized URL parameters: " <> show params

        _ -> do
            modifyMVar_ currentContextVar $ \_ -> do
                mapM_ (flip js_classList_remove (toJSString "selected")) =<<
                    JS.documentQuerySelector "#sidebar ul li.selected"
                mapM_ (\body -> js_removeAttribute body (toJSString "data-selected")) =<<
                    JS.getElementById "body"
                return NoContext


watchIdentityUpdates :: GlobalState -> IO ()
watchIdentityUpdates GlobalState {..} = do
    nameElem <- js_document_getElementById (toJSString "name_text")
    nameElemDetail <- js_document_getElementById (toJSString "name_text_detail")
    selfRefElem <- js_document_getElementById (toJSString "self_ref_value")
    void $ watchHeadWith globalHead headLocalIdentity $ \lid -> do
        let fowner = finalOwner lid
        js_set_textContent nameElem $ toJSString $ maybe "(Anonymous)" T.unpack $ idName fowner
        js_set_textContent nameElemDetail $ toJSString $ maybe "(Anonymous)" T.unpack $ idName fowner
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
                                    JS.getElementById "msg_header" >>= \case
                                        Just header -> js_set_textContent header $ toJSString $ T.unpack $ conversationName c
                                        Nothing -> return ()
                                return ( SelectedConversation c, ( c, True ) : map (, False) cs )
                            | otherwise = do
                                fmap (( c, False ) :) <$> updateCurrent cs
                    updateCurrent conversations
                x -> return ( x, map (, False ) conversations )

            JS.getElementById "conversation_list" >>= \case
                Just convList -> do
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
                Nothing -> return ()

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

            ul <- js_document_createElement (toJSString "ul")
            appendMessages gs ul $ map Right $ reverse $ conversationHistory conv

            JS.getElementById "msg_header" >>= \case
                Just header -> js_set_textContent header $ toJSString $ T.unpack $ conversationName conv
                Nothing -> return ()
            maybe (return ()) (flip js_replaceChildren ul) =<< JS.getElementById "msg_list"

            mapM_ (flip js_classList_remove (toJSString "selected")) =<<
                JS.documentQuerySelector "#sidebar ul li.selected"
            mapM_ (flip js_classList_add (toJSString "selected")) =<<
                JS.documentQuerySelector ("#conversation_list ul li[data-conv='" <> show (conversationReference conv) <> "']")

            JS.getElementById "body" >>= \case
                Just body -> do
                    js_setAttribute body (toJSString "data-selected") (toJSString "conversation")
                Nothing -> return ()

            mapM_ (\mlist -> js_set_scrollTop mlist =<< js_get_scrollHeight mlist) =<< JS.getElementById "msg_list"
            mapM_ js_focus =<< JS.getElementById "msg_text"

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

        mapM_ (flip js_classList_remove (toJSString "selected")) =<<
            JS.documentQuerySelector "#sidebar ul li.selected"
        mapM_ (`js_classList_add` toJSString "selected") =<<
            JS.documentQuerySelector ("ul#peer_list li[data-peer='" <> show dgst <> "']")

        JS.getElementById "body" >>= \case
            Just body -> do
                js_setAttribute body (toJSString "data-selected") (toJSString "peer")
            Nothing -> return ()
        return selected

updatePeerDetails :: Peer -> IO ()
updatePeerDetails peer = do
    paddr <- getPeerAddress peer
    pid <- getPeerIdentity peer
    maybe (return ()) (flip js_set_textContent $ toJSString $ showPeer pid) =<<
        JS.getElementById "peer_name_value"
    case pid of
        PeerIdentityFull pidf -> do
            maybe (return ()) (flip js_set_textContent $ toJSString $ show $ refDigest $ storedRef $ idData pidf) =<< JS.getElementById "peer_ref_value"
            maybe (return ()) (\dmLinkElem ->
                js_setAttribute dmLinkElem (toJSString "href") $ toJSString $ "#conv=" <> drop 7 (show $ refDigest $ storedRef $ head $ idDataF $ finalOwner pidf)
                ) =<< JS.getElementById "peer_dm_link"
        PeerIdentityRef wref _ -> do
            maybe (return ()) (flip js_set_textContent $ toJSString $ show $ wrDigest wref) =<< JS.getElementById "peer_ref_value"
        PeerIdentityUnknown _ -> do
            maybe (return ()) (flip js_set_textContent $ toJSString "unknown") =<< JS.getElementById "peer_ref_value"
    maybe (return ()) (flip js_set_textContent $ toJSString $ show paddr) =<<
        JS.getElementById "peer_address_value"


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
                count <- modifyMVar peerListVar $ \plist -> do
                    plist' <- update plist
                    return ( plist', length plist' )
                maybe (return ()) (flip js_set_textContent $ toJSString $ show count) =<<
                    JS.getElementById "peer_count"

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


selectSelf :: GlobalState -> Server -> IO ()
selectSelf GlobalState {..} _ = do
    modifyMVar_ currentContextVar $ \_ -> do
        mapM_ (\body -> js_setAttribute body (toJSString "data-selected") (toJSString "self")) =<<
            JS.getElementById "body"
        mapM_ (flip js_classList_remove (toJSString "selected")) =<<
            JS.documentQuerySelector "#sidebar ul li.selected"
        mapM_ (flip js_classList_add (toJSString "selected")) =<<
            JS.documentQuerySelector ("#self ul li")
        return SelectedSelf


selectCreateInvite :: GlobalState -> Server -> IO ()
selectCreateInvite GlobalState {..} _ = do
    modifyMVar_ currentContextVar $ \_ -> do
        mapM_ (\body -> js_setAttribute body (toJSString "data-selected") (toJSString "create-invite")) =<<
            JS.getElementById "body"
        mapM_ (flip js_classList_remove (toJSString "selected")) =<<
            JS.documentQuerySelector "#sidebar ul li.selected"
        mapM_ (flip js_classList_add (toJSString "selected")) =<<
            JS.documentQuerySelector ("#create_invite_item")
        return SelectedSelf


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

foreign import javascript unsafe "$1.removeAttribute($2)"
    js_removeAttribute :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.classList.add($2)"
    js_classList_add :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.classList.remove($2)"
    js_classList_remove :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.classList.toggle($2)"
    js_classList_toggle :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.value"
    js_get_value :: JSVal -> IO JSString

foreign import javascript unsafe "$1.value = $2"
    js_set_value :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.focus()"
    js_focus :: JSVal -> IO ()

foreign import javascript unsafe "$1.scrollTop"
    js_get_scrollTop :: JSVal -> IO Int

foreign import javascript unsafe "$1.scrollTop = $2"
    js_set_scrollTop :: JSVal -> Int -> IO ()

foreign import javascript unsafe "$1.scrollHeight"
    js_get_scrollHeight :: JSVal -> IO Int

foreign import javascript unsafe "$1.clientHeight"
    js_get_clientHeight :: JSVal -> IO Int

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

foreign import javascript unsafe "window.localStorage.setItem($1, $2)"
    js_storage_setItem :: JSString -> JSString -> IO ()

foreign import javascript unsafe "window.localStorage.getItem($1)"
    js_storage_getItem :: JSString -> IO JSString

foreign import javascript unsafe "$1 === null"
    js_string_is_null :: JSString -> Bool
