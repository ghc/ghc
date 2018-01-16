{-# LANGUAGE MagicHash, BangPatterns, ParallelArrays, TypeOperators #-}
{-# OPTIONS_GHC -fvectorise #-}

module HandvecWrp ( quickhullPA )
where

import Points2D.Types

import Data.Array.Parallel as PA
import Data.Array.Parallel.Prelude.Double as D
import qualified Data.Array.Parallel.Prelude.Int as I

import Prelude as P

import Handvec ( hsplit_v )

-- Manually override vectorisation of hsplit
-- hsplit :: [:Point:] -> Line -> [:Point:]
hsplit :: PArray Point -> Line -> PArray Point
hsplit ps _ = ps -- should never be called
{-# VECTORISE hsplit = hsplit_v #-}
-- vectorisation NOT overridden without NOINLINE pragma:
{-# NOINLINE hsplit #-}

quickHull :: [:Point:] -> [:Point:]
quickHull points
  | lengthP points I.== 0 = points
  | otherwise
  = concatP [: fromPArrayP (hsplit (toPArrayP points) ends)
               | ends <- [: (minx, maxx), (maxx, minx) :] :]
  where
    xs   = [: x | (x, y) <- points :]
    minx = points PA.!: minIndexP xs
    maxx = points PA.!: maxIndexP xs

quickhullPA :: PArray Point -> PArray Point
quickhullPA ps = toPArrayP (quickHull (fromPArrayP ps))
{-# NOINLINE quickhullPA #-}
