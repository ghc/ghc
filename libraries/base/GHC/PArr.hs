{-# LANGUAGE CPP, ParallelArrays, MagicHash, UnboxedTuples #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.PArr
-- Copyright   :  (c) 2001-2002 Manuel M T Chakravarty & Gabriele Keller
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  Manuel M. T. Chakravarty <chak@cse.unsw.edu.au>
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- !!!THIS FILE IS ABOUT TO GO AWAY!!!

module GHC.PArr (
  -- [::]              -- Built-in syntax
  emptyPArr, replicatePArr, singletonPArr, indexPArr, lengthPArr
) where

#ifndef __HADDOCK__

import Prelude

import GHC.ST   ( ST(..), runST )
import GHC.Base ( Int#, Array#, Int(I#), MutableArray#, newArray#,
                  unsafeFreezeArray#, indexArray#, {- writeArray#, -} (<#), (>=#) )


-- representation of parallel arrays
-- ---------------------------------

-- this rather straight forward implementation maps parallel arrays to the
-- internal representation used for standard Haskell arrays in GHC's Prelude
-- (EXPORTED ABSTRACTLY)
--
-- * This definition *must* be kept in sync with `TysWiredIn.parrTyCon'!
--
data [::] e = PArr Int# (Array# e)

emptyPArr :: [:a:]
{-# NOINLINE emptyPArr #-}
emptyPArr = replicatePArr 0 undefined

replicatePArr :: Int -> a -> [:a:]
{-# NOINLINE replicatePArr #-}
replicatePArr n e  = runST (do
  marr# <- newArray n e
  mkPArr n marr#)

singletonPArr :: a -> [:a:]
{-# NOINLINE singletonPArr #-}
singletonPArr e = replicatePArr 1 e

indexPArr :: [:e:] -> Int -> e
{-# NOINLINE indexPArr #-}
indexPArr (PArr n# arr#) (I# i#) 
  | i# >=# 0# && i# <# n# =
    case indexArray# arr# i# of (# e #) -> e
  | otherwise = error $ "indexPArr: out of bounds parallel array index; " ++
                        "idx = " ++ show (I# i#) ++ ", arr len = "
                        ++ show (I# n#)

lengthPArr :: [:a:] -> Int
{-# NOINLINE lengthPArr #-}
lengthPArr (PArr n# _)  = I# n#

-- auxiliary functions
-- -------------------

-- internally used mutable boxed arrays
--
data MPArr s e = MPArr Int# (MutableArray# s e)

-- allocate a new mutable array that is pre-initialised with a given value
--
newArray             :: Int -> e -> ST s (MPArr s e)
{-# INLINE newArray #-}
newArray (I# n#) e  = ST $ \s1# ->
  case newArray# n# e s1# of { (# s2#, marr# #) ->
  (# s2#, MPArr n# marr# #)}

-- convert a mutable array into the external parallel array representation
--
mkPArr                           :: Int -> MPArr s e -> ST s [:e:]
{-# INLINE mkPArr #-}
mkPArr (I# n#) (MPArr _ marr#)  = ST $ \s1# ->
  case unsafeFreezeArray# marr# s1#   of { (# s2#, arr# #) ->
  (# s2#, PArr n# arr# #) }

#endif
