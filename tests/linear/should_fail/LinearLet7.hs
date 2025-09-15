{-# LANGUAGE LinearTypes #-}
module LinearLet7 where

-- Should fail because recursive lets should never be linear
f :: a -> a
f x = let %1 g = \y -> g y in g x
