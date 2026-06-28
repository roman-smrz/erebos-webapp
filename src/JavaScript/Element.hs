module JavaScript.Element (
    Element, IsElement(..),

    getElementById,
    documentQuerySelector,
    querySelector,

    createElement,
    removeElement,

    firstElementChild,
    lastElementChild,
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

createElement :: String -> IO Element
createElement = fmap Element . js_document_createElement . toJSString
foreign import javascript unsafe "document.createElement($1)"
    js_document_createElement :: JSString -> IO JSVal

removeElement :: IsElement e => e -> IO ()
removeElement = js_element_remove . toJSVal . toElement
foreign import javascript unsafe "$1.remove()"
    js_element_remove :: JSVal -> IO ()

firstElementChild :: IsElement e => e -> IO (Maybe Element)
firstElementChild = fmap nullToNothing . js_element_firstElementChild . toJSVal . toElement
foreign import javascript unsafe "$1.firstElementChild"
    js_element_firstElementChild :: JSVal -> IO JSVal

lastElementChild :: IsElement e => e -> IO (Maybe Element)
lastElementChild = fmap nullToNothing . js_element_lastElementChild . toJSVal . toElement
foreign import javascript unsafe "$1.lastElementChild"
    js_element_lastElementChild :: JSVal -> IO JSVal
