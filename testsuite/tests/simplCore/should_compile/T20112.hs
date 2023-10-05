module T20112 (
    -- * Data structure
    AdjacencyMap, transpose, overlays1

    ) where

import Prelude hiding (reverse)
import Data.List.NonEmpty(NonEmpty, toList)
import Data.Coerce

import qualified T20112A as AM

newtype AdjacencyMap a = NAM ( AM.AdjacencyMap a )

overlays1 :: Ord a => NonEmpty (AdjacencyMap a) -> AdjacencyMap a
overlays1 = coerce AM.overlays . toList
{-# NOINLINE overlays1 #-}

transpose :: Ord a => AdjacencyMap a -> AdjacencyMap a
transpose = coerce AM.transpose
{-# NOINLINE [1] transpose #-}

{-# RULES
"transpose/overlays1" forall xs. transpose (overlays1 xs) = overlays1 (fmap transpose xs)
 #-}

