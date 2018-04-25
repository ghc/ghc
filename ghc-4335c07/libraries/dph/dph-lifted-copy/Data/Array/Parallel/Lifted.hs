module Data.Array.Parallel.Lifted (
  module Data.Array.Parallel.Lifted.PArray,
  module Data.Array.Parallel.PArray.PReprInstances,

  (:->), ($:), ($:^),

  fromPArrayPA, toPArrayPA, fromNestedPArrayPA,
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.PArray.PReprInstances

fromPArrayPA :: PA a => PArray a :-> PArray a
{-# INLINE fromPArrayPA #-}
fromPArrayPA = closure1 (\x -> x) (\xs -> xs)

toPArrayPA :: PA a => PArray a :-> PArray a
{-# INLINE toPArrayPA #-}
toPArrayPA = closure1 (\x -> x) (\xs -> xs)

fromNestedPArrayPA :: PA a => (PArray (PArray a) :-> PArray (PArray a))
{-# INLINE fromNestedPArrayPA #-}
fromNestedPArrayPA = closure1 (\xs -> xs) (\xss -> xss)

