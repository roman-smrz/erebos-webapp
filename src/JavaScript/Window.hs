module JavaScript.Window (
    Window, IsWindow(..),
    globalWindow,
) where

import JavaScript.Event
import JavaScript.Val


newtype Window = Window JSVal
    deriving (ToJSVal, FromJSVal)

class IsWindow a where
    toWindow :: a -> Window

instance IsWindow Window where
    toWindow = id

instance IsEventTarget Window where
    toEventTarget = fromJSValUnchecked . toJSVal

globalWindow :: Window
globalWindow = Window js_window
foreign import javascript unsafe "window"
    js_window :: JSVal
