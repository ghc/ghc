{-# LANGUAGE LinearTypes, BangPatterns #-}
module T23025 where

import Data.Void

f :: a %1 -> a
f !x = x

g :: Void %m -> Maybe ()
g a = Just (case a of {})
