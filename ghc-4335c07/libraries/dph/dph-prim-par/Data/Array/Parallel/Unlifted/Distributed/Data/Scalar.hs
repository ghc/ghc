{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of values of primitive types.
module Data.Array.Parallel.Unlifted.Distributed.Data.Scalar
        ( DT(..), Dist(..)
        , scalarD
        , sumD)
where
import Data.Array.Parallel.Unlifted.Distributed.Data.Scalar.Base
import Data.Array.Parallel.Unlifted.Distributed.Data.Unit
import Data.Array.Parallel.Unlifted.Distributed.Combinators
import Data.Array.Parallel.Unlifted.Distributed.Primitive
import qualified Data.Array.Parallel.Unlifted.Distributed.What  as W
import Data.Array.Parallel.Pretty
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as V
import Prelude as P


instance PprPhysical (Dist Int) where
 pprp (DInt xs)
  =  text "DInt" <+> text (show $ V.toList xs)
 {-# NOINLINE pprp #-}


-- | Distribute a scalar.
--   Each thread gets its own copy of the same value.
--   Example:  scalarD theGangN4 10 = [10, 10, 10, 10] 
scalarD :: DT a => Gang -> a -> Dist a
scalarD gang x 
        = mapD W.WScalar gang (const x) (unitD gang)
{-# INLINE_DIST scalarD #-}

-- | Sum all instances of a distributed number.
sumD :: (Num a, DT a) => Gang -> Dist a -> a
sumD g  = foldD (W.What "sumD") g (+)
{-# INLINE_DIST sumD #-}
