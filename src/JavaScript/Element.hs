module JavaScript.Element (
    Element, IsElement(..),
    module JavaScript.Node,

    getElementById,
    documentQuerySelector,
    querySelector,

    createElement,
    removeElement,

    firstElementChild,
    lastElementChild,

    getAttribute, setAttribute, removeAttribute,
    addClass, removeClass, toggleClass,
) where

import JavaScript.Event
import JavaScript.Node
import JavaScript.Val


newtype Element = Element JSVal
    deriving (ToJSVal, FromJSVal)

class IsElement a where
    toElement :: a -> Element

instance IsElement Element where
    toElement = id

instance IsNode Element where
    toNode = fromJSValUnchecked . toJSVal

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


getAttribute :: IsElement e => String -> e -> IO String
getAttribute name e = fromJSString <$> js_getAttribute (toJSVal $ toElement e) (toJSString name)
foreign import javascript unsafe "$1.getAttribute($2)"
    js_getAttribute :: JSVal -> JSString -> IO JSString

setAttribute :: IsElement e => String -> String -> e -> IO ()
setAttribute name value e = js_setAttribute (toJSVal $ toElement e) (toJSString name) (toJSString value)
foreign import javascript unsafe "$1.setAttribute($2, $3)"
    js_setAttribute :: JSVal -> JSString -> JSString -> IO ()

removeAttribute :: IsElement e => String -> e -> IO ()
removeAttribute name e = js_removeAttribute (toJSVal $ toElement e) (toJSString name)
foreign import javascript unsafe "$1.removeAttribute($2)"
    js_removeAttribute :: JSVal -> JSString -> IO ()

addClass :: IsElement e => String -> e -> IO ()
addClass cls e = js_classList_add (toJSVal $ toElement e) (toJSString cls)
foreign import javascript unsafe "$1.classList.add($2)"
    js_classList_add :: JSVal -> JSString -> IO ()

removeClass :: IsElement e => String -> e -> IO ()
removeClass cls e = js_classList_remove (toJSVal $ toElement e) (toJSString cls)
foreign import javascript unsafe "$1.classList.remove($2)"
    js_classList_remove :: JSVal -> JSString -> IO ()

toggleClass :: IsElement e => String -> e -> IO ()
toggleClass cls e = js_classList_toggle (toJSVal $ toElement e) (toJSString cls)
foreign import javascript unsafe "$1.classList.toggle($2)"
    js_classList_toggle :: JSVal -> JSString -> IO ()
