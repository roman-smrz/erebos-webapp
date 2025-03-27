module Main (main, setup) where

import GHC.Wasm.Prim

main :: IO ()
main = error "unused"

foreign export javascript setup :: IO ()
setup :: IO ()
setup = do
    body <- js_document_getElementById (toJSString "body")
    js_set_innerHTML body (toJSString "<input id=\"some_input\" type=\"text\" value=\"xyz\" /><button id=\"some_button\">add</button></div><div><ul id=\"some_list\"></ul></div>")

    buttonElem <- js_document_getElementById (toJSString "some_button")
    buttonCallback <- asEventListener onButtonClick

    js_addEventListener buttonElem (toJSString "click") buttonCallback

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
