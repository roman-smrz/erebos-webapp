module JavaScript.Event (
    EventTarget, IsEventTarget(..),
    Event, IsEvent(..),
    asEventListener,
    addEventListener,
) where

import JavaScript.Val


newtype EventTarget = EventTarget JSVal
    deriving (ToJSVal, FromJSVal)

class IsEventTarget a where
    toEventTarget :: a -> EventTarget

instance IsEventTarget EventTarget where
    toEventTarget = id


newtype Event = Event JSVal
    deriving (ToJSVal, FromJSVal)

class IsEvent a where
    toEvent :: a -> Event

instance IsEvent Event where
    toEvent = id


foreign import javascript "wrapper"
    asEventListener :: (JSVal -> IO ()) -> IO JSVal

addEventListener :: IsEventTarget a => a -> String -> (Event -> IO ()) -> IO ()
addEventListener target ev cb = do
    js_addEventListener (toJSVal $ toEventTarget target) (toJSString ev) =<< asEventListener (cb . fromJSValUnchecked)
foreign import javascript unsafe "$1.addEventListener($2, $3)"
    js_addEventListener :: JSVal -> JSString -> JSVal -> IO ()
