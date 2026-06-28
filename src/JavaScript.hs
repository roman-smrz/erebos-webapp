module JavaScript (
    module JavaScript.Event,
    module JavaScript.Element,
    module JavaScript.Window,

    toJSVal,
    consoleLog,
    consoleLogVal,
) where

import JavaScript.Element
import JavaScript.Event
import JavaScript.Val
import JavaScript.Window


consoleLog :: String -> IO ()
consoleLog = js_consoleLog . toJSString
foreign import javascript unsafe "console.log($1)"
    js_consoleLog :: JSString -> IO ()

foreign import javascript unsafe "console.log($1)"
    consoleLogVal :: JSVal -> IO ()
