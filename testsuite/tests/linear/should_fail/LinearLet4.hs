{-# LANGUAGE LinearTypes #-}
module LinearLet4 where

import GHC.Types

f :: a -> (a, a)
f x = let %1 y = x ; %'Many z = y in (z, z)
