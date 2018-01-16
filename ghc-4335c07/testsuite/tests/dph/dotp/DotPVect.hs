{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module DotPVect ( dotp ) where

import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double as D

import qualified Prelude

dotp :: PArray Double -> PArray Double -> Double
{-# NOINLINE dotp #-}
dotp v w = dotp' (fromPArrayP v) (fromPArrayP w)

dotp' :: [:Double:] -> [:Double:] -> Double
dotp' v w = D.sumP (zipWithP (D.*) v w)
