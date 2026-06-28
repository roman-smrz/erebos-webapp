module JavaScript.Val (
    module GHC.Wasm.Prim,
    ToJSVal(..), FromJSVal(..),
    nullToNothing,
) where

import GHC.Wasm.Prim


class ToJSVal a where
    toJSVal :: a -> JSVal

class FromJSVal a where
    fromJSValUnchecked :: JSVal -> a

instance ToJSVal JSVal where toJSVal = id
instance FromJSVal JSVal where fromJSValUnchecked = id


nullToNothing :: FromJSVal a => JSVal -> Maybe a
nullToNothing val | isNull val = Nothing
                  | otherwise  = Just $ fromJSValUnchecked val
foreign import javascript unsafe "$1 === null"
    isNull :: JSVal -> Bool
