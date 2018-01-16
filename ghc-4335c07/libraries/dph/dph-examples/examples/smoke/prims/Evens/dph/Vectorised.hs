
{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module Vectorised (evensPA) where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Int  as I
import Data.Array.Parallel.Prelude.Bool
import qualified Prelude as P

evens :: [:Int:] -> [:Int:]
evens ints = filterP (\x -> x `mod` 2 I.== 0) ints

evensPA :: PArray Int -> PArray Int
{-# NOINLINE evensPA #-}
evensPA arr = toPArrayP (evens (fromPArrayP arr))

