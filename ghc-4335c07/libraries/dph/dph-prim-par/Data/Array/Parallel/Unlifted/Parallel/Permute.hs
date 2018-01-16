{-# LANGUAGE CPP, ScopedTypeVariables #-}
#include "fusion-phases.h"

-- | Parallel permutations for unlifted arrays
module Data.Array.Parallel.Unlifted.Parallel.Permute 
        ( bpermuteUP, updateUP)
where
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Distributed

{-
  We can't support this for arbitrary types. The problem is:
  what happens if the second array maps multiple elements to the same position?
  I don't know what the semantics is supposed to be in Nesl, the spec don't
  seem to say anything. Note that it is not sufficient to say, e.g., that it
  is unspecified which value gets written; if we have an array of pairs,
  for instance, we might well get the first and second components from
  different values.

  We could require that the second array maps at most one element to each index.
  However, this is not what is wanted most of the time, at least not in the
  algorithms I've seen.

  So we only do the update in parallel if writing an element into the array is
  atomic. Otherwise, we do a sequential update.
-}

-- | Backwards permutation.
bpermuteUP :: Unbox a => Vector a -> Vector Int -> Vector a
bpermuteUP as is = splitJoinD theGang (bpermuteD theGang as) is
{-# INLINE_UP bpermuteUP #-}


-- | Update elements in an array.
updateUP :: forall a. Unbox a => Vector a -> Vector (Int,a) -> Vector a
updateUP as us
  {- hasAtomicWriteMU (undefined :: a) 
  = atomicUpdateD theGang (splitD theGang unbalanced as)
                          (splitD theGang unbalanced us)
  -}

  | otherwise
  = Seq.update as us
{-# INLINE_UP updateUP #-}

