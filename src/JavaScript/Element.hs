module JavaScript.Element (
    Element, IsElement(..),
    getElementById,
    documentQuerySelector,
    querySelector,
) where

import JavaScript.Event
import JavaScript.Val


newtype Element = Element JSVal
    deriving (ToJSVal, FromJSVal)

class IsElement a where
    toElement :: a -> Element

instance IsElement Element where
    toElement = id

instance IsEventTarget Element where
    toEventTarget = fromJSValUnchecked . toJSVal


getElementById :: String -> IO (Maybe Element)
getElementById = fmap nullToNothing . js_document_getElementById . toJSString
foreign import javascript unsafe "document.getElementById($1)"
    js_document_getElementById :: JSString -> IO JSVal

documentQuerySelector :: String -> IO (Maybe Element)
documentQuerySelector = fmap nullToNothing . js_document_querySelector . toJSString
foreign import javascript unsafe "document.querySelector($1)"
    js_document_querySelector :: JSString -> IO JSVal

querySelector :: IsElement e => String -> e -> IO (Maybe Element)
querySelector sel e = nullToNothing <$> js_querySelector (toJSVal $ toElement e) (toJSString sel)
foreign import javascript unsafe "$1.querySelector($2)"
    js_querySelector :: JSVal -> JSString -> IO JSVal
