module JavaScript (
    getElementById,

    asEventListener,
    addEventListener,

    consoleLog,
    consoleLogVal,
) where

import GHC.Wasm.Prim

getElementById :: String -> IO JSVal
getElementById = js_document_getElementById . toJSString
foreign import javascript unsafe "document.getElementById($1)"
    js_document_getElementById :: JSString -> IO JSVal

foreign import javascript "wrapper"
    asEventListener :: (JSVal -> IO ()) -> IO JSVal

addEventListener :: JSVal -> String -> (JSVal -> IO ()) -> IO ()
addEventListener obj ev cb = do
    js_addEventListener obj (toJSString ev) =<< asEventListener cb
foreign import javascript unsafe "$1.addEventListener($2, $3)"
    js_addEventListener :: JSVal -> JSString -> JSVal -> IO ()

consoleLog :: String -> IO ()
consoleLog = js_consoleLog . toJSString
foreign import javascript unsafe "console.log($1)"
    js_consoleLog :: JSString -> IO ()

foreign import javascript unsafe "console.log($1)"
    consoleLogVal :: JSVal -> IO ()
