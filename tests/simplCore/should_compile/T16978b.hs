module T16978b (renderNode) where

import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as B

data Value = String !Text | Null

renderNode :: Value -> B.Builder -> ((), B.Builder)
renderNode v b =
  case renderValue v b of
    (t, s') -> ((), s' <> B.fromText t)

renderValue :: Value -> B.Builder -> (Text, B.Builder)
renderValue v b = case v of
    String str -> (str, b)
    Null       -> let x = x in x
{-# INLINE renderValue #-}

