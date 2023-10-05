{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
module T15139 where

import Data.Type.Equality

can'tHappen :: Int :~: Bool
can'tHappen = undefined

f1, f2, g :: Bool -> Bool
f1 True = case can'tHappen of {}
f2 True = case can'tHappen of
g  True = case () of () -> True
