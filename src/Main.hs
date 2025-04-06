module Main (main, setup) where

import Control.Monad.Reader

import Data.Maybe
import Data.Proxy
import Data.Text qualified as T

import GHC.Wasm.Prim

import Erebos.Identity
import Erebos.State
import Erebos.Storable
import Erebos.Storage

import System.IO.Unsafe

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
    js_set_innerHTML body (toJSString "<div>Name: <span id=\"name_text\"></span></div><hr><input id=\"some_input\" type=\"text\" value=\"xyz\" /><button id=\"some_button\">add</button></div><div><ul id=\"some_list\"></ul></div>")

    nameElem <- js_document_getElementById (toJSString "name_text")
    _ <- watchHead globalHead $ \ls -> do
        js_set_textContent nameElem $ toJSString $ T.unpack $ displayIdentity $ headLocalIdentity ls

    buttonElem <- js_document_getElementById (toJSString "some_button")
    buttonCallback <- asEventListener onButtonClick

    js_addEventListener buttonElem (toJSString "click") buttonCallback

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
    return ()


onButtonClick :: JSVal -> IO ()
onButtonClick _event = do
    inputElem <- js_document_getElementById (toJSString "some_input")
    listElem <- js_document_getElementById (toJSString "some_list")

    li <- js_document_createElement (toJSString "li")
    content <- js_document_createTextNode =<< js_get_value inputElem
    js_appendChild li content
    js_appendChild listElem li

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

foreign import javascript unsafe "$1.addEventListener($2, $3)"
    js_addEventListener :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript "wrapper"
    asEventListener :: (JSVal -> IO ()) -> IO JSVal
