{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module Vectorised ( dotPA ) where

import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double as D

import qualified Prelude

dotPA :: PArray Double -> PArray Double -> Double
dotPA v w = dotp' (fromPArrayP v) (fromPArrayP w)
{-# NOINLINE dotPA #-}

dotp' :: [:Double:] -> [:Double:] -> Double
dotp' v w = D.sumP (zipWithP (D.*) v w)
