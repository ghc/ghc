{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Sum-like parallel combinators for unlifted arrays
module Data.Array.Parallel.Unlifted.Parallel.Sums 
        ( andUP, orUP
        , allUP, anyUP
        , sumUP, productUP
        , maximumUP, maximumByUP
        , maximumIndexByUP)
where
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Combinators
import Data.Array.Parallel.Unlifted.Parallel.Basics (indexedUP)


-- | Compute the logical AND of all the elements in a array.
andUP :: Vector Bool -> Bool
andUP = foldUP (&&) True
{-# INLINE_UP andUP #-}


-- | Compute the logical OR of all the elements in a array.
orUP :: Vector Bool -> Bool
orUP = foldUP (||) False
{-# INLINE_UP orUP #-}


-- | Check whether all the elements in a array meet the given predicate.
allUP :: Unbox e => (e -> Bool) -> Vector e -> Bool
allUP p = andUP . mapUP p
{-# INLINE_UP allUP #-}


-- | Check whether any of the elements in a array meet the given predicate.
anyUP :: Unbox e => (e -> Bool) -> Vector e -> Bool
anyUP p =  orUP . mapUP p
{-# INLINE_UP anyUP #-}


-- | Compute the sum all the elements of a array.
sumUP :: (Unbox a, DT a, Num a) => Vector a -> a
sumUP = foldUP (+) 0
{-# INLINE_UP sumUP #-}


-- | Compute the product of all the elements of an array.
productUP :: (DT e, Num e, Unbox e) => Vector e -> e
productUP = foldUP (*) 1
{-# INLINE_UP productUP #-}


-- | Determine the maximum element in an array.
maximumUP :: (DT e, Ord e, Unbox e) => Vector e -> e
maximumUP = fold1UP max
{-# INLINE_UP maximumUP #-}


-- | Determine the maximum element in an array under the given ordering
maximumByUP :: (DT e, Unbox e) => (e -> e -> Ordering) -> Vector e -> e
maximumByUP 
  = fold1UP . maxBy
  where
    maxBy compare' x y 
        = case x `compare'` y of
           LT -> y
           _  -> x
{-# INLINE_UP maximumByUP #-}


-- | Determine the index of the maximum element in an array under the
--   given ordering
maximumIndexByUP 
        :: (DT e, Unbox e) => (e -> e -> Ordering) -> Vector e -> Int
maximumIndexByUP cmp 
  = fst . maximumByUP cmp' . indexedUP
  where
    cmp' (_,x) (_,y) = cmp x y
{-# INLINE_UP maximumIndexByUP #-}
