module Main (main, setup) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Maybe
import Data.Proxy
import Data.Text qualified as T
import Data.Time.LocalTime

import GHC.Wasm.Prim

import Erebos.Chatroom
import Erebos.DirectMessage
import Erebos.Discovery
import Erebos.Identity
import Erebos.Network
import Erebos.Service
import Erebos.State
import Erebos.Storable
import Erebos.Storage
import Erebos.Sync

import System.IO.Unsafe

import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Html.Renderer.String

import JavaScript qualified as JS
import WebSocket

main :: IO ()
main = error "unused"

{-# NOINLINE globalStorage #-}
globalStorage :: Storage
globalStorage = unsafePerformIO $ memoryStorage

{-# NOINLINE globalHead #-}
globalHead :: Head LocalState
globalHead = unsafePerformIO $ do
    identity <- createIdentity globalStorage (Just $ T.pack "<init>") Nothing
    storeHead globalStorage $ LocalState { lsPrev = Nothing, lsIdentity = idExtData identity, lsShared = [], lsOther = [] }

foreign export javascript setup :: IO ()
setup :: IO ()
setup = do
    body <- js_document_getElementById (toJSString "body")
    js_set_innerHTML body $ toJSString $ renderHtml $ do
        H.div $ do
            "Name: "
            H.span ! A.id "name_text" $ return ()
        H.hr
        H.div $ do
            H.ul ! A.id "msg_list" $ return ()
        H.div $ do
            H.input ! A.id "msg_text" ! A.type_ "text"
            H.button ! A.id "msg_send" $ "send"

    nameElem <- js_document_getElementById (toJSString "name_text")
    _ <- watchHead globalHead $ \ls -> do
        js_set_textContent nameElem $ toJSString $ T.unpack $ displayIdentity $ headLocalIdentity ls

    let name = T.pack "My Name"
        devName = T.pack "WebApp"

    let st = globalStorage
    owner <- if
        | T.null name -> return Nothing
        | otherwise -> Just <$> createIdentity st (Just name) Nothing

    identity <- createIdentity st (if T.null devName then Nothing else Just devName) owner

    shared <- wrappedStore st $ SharedState
        { ssPrev = []
        , ssType = Just $ sharedTypeID @(Maybe ComposedIdentity) Proxy
        , ssValue = [ storedRef $ idExtData $ fromMaybe identity owner ]
        }
    flip runReaderT globalHead $ do
        updateLocalState_ $ \_ -> do
            mstore $ LocalState
                { lsPrev = Nothing
                , lsIdentity = idExtData identity
                , lsShared = [ shared ]
                , lsOther = []
                }

    messagesList <- JS.getElementById "msg_list"
    tzone <- getCurrentTimeZone
    void $ watchReceivedMessages globalHead $ \msg -> do
        JS.consoleLog $ formatDirectMessage tzone $ fromStored msg
        li <- js_document_createElement (toJSString "li")
        content <- js_document_createTextNode $ toJSString $ formatDirectMessage tzone $ fromStored msg
        js_appendChild li content
        js_appendChild messagesList li

    sendText <- JS.getElementById "msg_text"
    sendButton <- JS.getElementById "msg_send"

    server <- startServer defaultServerOptions globalHead JS.consoleLog
        [ someService @ChatroomService Proxy
        , someService @DiscoveryService Proxy
        , someService @DirectMessage Proxy
        , someService @SyncService Proxy
        ]

    startClient "localhost" 9160 "" $ \conn -> do
        void $ forkIO $ forever $ do
            msg <- receiveMessage conn
            receivedFromCustomAddress server conn msg

        peer <- serverPeerCustom server conn
        JS.addEventListener sendButton "click" $ \_ -> do
            peerIdentity peer >>= \case
                PeerIdentityUnknown {} -> JS.consoleLog "unknown peer identity"
                PeerIdentityRef {} -> JS.consoleLog "unresolved peer identity"
                PeerIdentityFull pid -> do
                    msg <- T.pack . fromJSString <$> js_get_value sendText
                    js_set_value sendText $ toJSString ""
                    res <- runExceptT $ flip runReaderT globalHead $ sendDirectMessage pid msg
                    case res of
                        Right sent -> JS.consoleLog . ("sent message: " <>) $ formatDirectMessage tzone $ fromStored sent
                        Left err -> JS.consoleLog $ "Failed to send message: " <> err


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
