{-# LANGUAGE UnboxedTuples, MagicHash #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}

-- #hide
module Data.Array.Parallel.PArr (
  PArr, emptyPArr, replicatePArr, singletonPArr, indexPArr, lengthPArr
) where

import GHC.ST   ( ST(..), runST )
import GHC.Base ( Int (I#), MutableArray#, newArray#,
                  unsafeFreezeArray#, indexArray#, {- writeArray# -} )
import GHC.PArr -- provides the definition of '[::]' and 'PArr'


emptyPArr :: PArr a
{-# NOINLINE emptyPArr #-}
emptyPArr = replicatePArr 0 undefined

replicatePArr :: Int -> a -> PArr a
{-# NOINLINE replicatePArr #-}
replicatePArr n e  = runST (do
  marr# <- newArray n e
  mkPArr n marr#)

singletonPArr :: a -> PArr a
{-# NOINLINE singletonPArr #-}
singletonPArr e = replicatePArr 1 e

indexPArr :: PArr e -> Int -> e
{-# NOINLINE indexPArr #-}
indexPArr (PArr n arr#) i@(I# i#)
  | i >= 0 && i < n =
    case indexArray# arr# i# of (# e #) -> e
  | otherwise = error $ "indexPArr: out of bounds parallel array index; " ++
                        "idx = " ++ show i ++ ", arr len = "
                        ++ show n

lengthPArr :: PArr a -> Int
{-# NOINLINE lengthPArr #-}
lengthPArr (PArr n _) = n

-- auxiliary functions
-- -------------------

-- internally used mutable boxed arrays
--
data MPArr s e = MPArr !Int (MutableArray# s e)

-- allocate a new mutable array that is pre-initialised with a given value
--
newArray :: Int -> e -> ST s (MPArr s e)
{-# INLINE newArray #-}
newArray n@(I# n#) e  = ST $ \s1# ->
  case newArray# n# e s1# of { (# s2#, marr# #) ->
  (# s2#, MPArr n marr# #)}

-- convert a mutable array into the external parallel array representation
--
mkPArr :: Int -> MPArr s e -> ST s (PArr e)
{-# INLINE mkPArr #-}
mkPArr n (MPArr _ marr#)  = ST $ \s1# ->
  case unsafeFreezeArray# marr# s1#   of { (# s2#, arr# #) ->
  (# s2#, PArr n arr# #) }
