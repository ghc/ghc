module T23074 where

import Data.Semigroup

-- Test that stimes for SumInt is specialized

newtype SumInt = SumInt Int

instance Semigroup SumInt where
  SumInt x <> SumInt y = SumInt (x + y)


foo :: Int -> SumInt -> SumInt
foo = stimes
