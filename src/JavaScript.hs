module JavaScript (
    asEventListener,
    addEventListener,

    consoleLog,
    consoleLogVal,
) where

import GHC.Wasm.Prim

foreign import javascript "wrapper"
    asEventListener :: (JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe "$1.addEventListener($2, $3)"
    addEventListener :: JSVal -> JSString -> JSVal -> IO ()

consoleLog :: String -> IO ()
consoleLog = js_consoleLog . toJSString
foreign import javascript unsafe "console.log($1)"
    js_consoleLog :: JSString -> IO ()

foreign import javascript unsafe "console.log($1)"
    consoleLogVal :: JSVal -> IO ()
