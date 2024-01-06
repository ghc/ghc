{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples, FlexibleInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Arr (
        Array(..), (!!!), array
    ) where

import GHC.Num
import GHC.ST
import GHC.Base
import Good

data Array e = Array !Int !Int Int (Array# e)

array :: (Int,Int) -> [(Int, e)] -> Array e
array (l,u) ies
    = unsafeArray (l,u) [(index (l,u) i, e) | (i, e) <- ies]

unsafeArray :: (Int,Int) -> [(Int, e)] -> Array e
unsafeArray b ies = unsafeArray' b (rangeSize b) ies

unsafeArray' :: (Int,Int) -> Int -> [(Int, e)] -> Array e
unsafeArray' (l,u) n@(I# n#) ies =
  if n == 0 then error "aa" else runST (ST $ \s1# ->
    case newArray# n# arrEleBottom s1# of
        (# s2#, marr# #) ->
            foldr (fill marr#) (done l u n marr#) ies s2#)

{-# NOINLINE arrEleBottom #-}
arrEleBottom :: a
arrEleBottom = error "(Array.!): undefined array element"

unsafeAt :: Array e -> Int -> e
unsafeAt (Array _ _ _ arr#) (I# i#) =
    case indexArray# arr# i# of (# e #) -> e

fill :: MutableArray# s e -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) next 
 = \s1# -> case writeArray# marr# i# e s1# of 
             s2# -> next s2# 

done :: Int -> Int -> Int -> MutableArray# s e -> STRep s (Array e)
done l u n marr# 
  = \s1# -> case unsafeFreezeArray# marr# s1# of
              (# s2#, arr# #) -> (# s2#, Array l u n arr# #)

instance Eq (Array e) where
    (Array l1 _ _ _) == (Array l2 _ _ _) = l1 == l2

instance Ord (Array e) where
    compare (Array l1 _ _ _) (Array l2 _ _ _) = compare l1 l2

{-# INLINE index #-}
index               :: (Int,Int) -> Int -> Int
index (m,n) i | m <= i && i <= n =  i - m
              | otherwise   =  error "index out of range"

rangeSize           :: (Int,Int) -> Int
rangeSize (l,h) = h - l + 1


{-# INLINE (!!!) #-}
(!!!) :: Array e -> Int -> e
arr@(Array l u _ _) !!! i = unsafeAt arr $ index (l,u) i

instance Good (Array Int) where
    isGood (Array _ _ n _) = 0 < n
