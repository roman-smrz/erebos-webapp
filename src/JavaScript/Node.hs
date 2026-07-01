module JavaScript.Node (
    Node, IsNode(..),
    getTextContent, setTextContent,
    appendChild,
) where

import JavaScript.Event
import JavaScript.Val


newtype Node = Node JSVal
    deriving (ToJSVal, FromJSVal)

class IsNode a where
    toNode :: a -> Node

instance IsNode Node where
    toNode = id

instance IsEventTarget Node where
    toEventTarget = fromJSValUnchecked . toJSVal


getTextContent :: IsNode n => n -> IO String
getTextContent n = fromJSString <$> js_get_textContent (toJSVal $ toNode n)
foreign import javascript unsafe "$1.textContent"
    js_get_textContent :: JSVal -> IO JSString

setTextContent :: IsNode n => String -> n -> IO ()
setTextContent value n = js_set_textContent (toJSVal $ toNode n) (toJSString value)
foreign import javascript unsafe "$1.textContent = $2"
    js_set_textContent :: JSVal -> JSString -> IO ()


appendChild :: (IsNode c, IsNode p) => c -> p -> IO ()
appendChild child parent = js_appendChild (toJSVal $ toNode parent) (toJSVal $ toNode child)
foreign import javascript unsafe "$1.appendChild($2)"
    js_appendChild :: JSVal -> JSVal -> IO ()
