module DotPPrim where

import Data.Array.Parallel.Unlifted as U

dotp :: U.Array Double -> U.Array Double -> Double
{-# NOINLINE dotp #-}
dotp v w = U.sum (U.zipWith (*) v w)

