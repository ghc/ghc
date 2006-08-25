-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parallel Constructs
--
-----------------------------------------------------------------------------

module Control.Parallel (
          par, seq -- re-exported
#if defined(__GRANSIM__)
	, parGlobal, parLocal, parAt, parAtAbs, parAtRel, parAtForNow     
#endif
    ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Conc	( par )
#endif

#if defined(__GRANSIM__)
import PrelBase
import PrelErr   ( parError )
import PrelGHC   ( parGlobal#, parLocal#, parAt#, parAtAbs#, parAtRel#, parAtForNow# )

{-# INLINE parGlobal #-}
{-# INLINE parLocal #-}
{-# INLINE parAt #-}
{-# INLINE parAtAbs #-}
{-# INLINE parAtRel #-}
{-# INLINE parAtForNow #-}
parGlobal   :: Int -> Int -> Int -> Int -> a -> b -> b
parLocal    :: Int -> Int -> Int -> Int -> a -> b -> b
parAt	    :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAtAbs    :: Int -> Int -> Int -> Int -> Int -> a -> b -> b
parAtRel    :: Int -> Int -> Int -> Int -> Int -> a -> b -> b
parAtForNow :: Int -> Int -> Int -> Int -> a -> b -> c -> c

parGlobal (I# w) (I# g) (I# s) (I# p) x y = case (parGlobal# x w g s p y) of { 0# -> parError; _ -> y }
parLocal  (I# w) (I# g) (I# s) (I# p) x y = case (parLocal#  x w g s p y) of { 0# -> parError; _ -> y }

parAt       (I# w) (I# g) (I# s) (I# p) v x y = case (parAt#       x v w g s p y) of { 0# -> parError; _ -> y }
parAtAbs    (I# w) (I# g) (I# s) (I# p) (I# q) x y = case (parAtAbs#  x q w g s p y) of { 0# -> parError; _ -> y }
parAtRel    (I# w) (I# g) (I# s) (I# p) (I# q) x y = case (parAtRel#  x q w g s p y) of { 0# -> parError; _ -> y }
parAtForNow (I# w) (I# g) (I# s) (I# p) v x y = case (parAtForNow# x v w g s p y) of { 0# -> parError; _ -> y }

#endif

-- Maybe parIO and the like could be added here later.

-- | Indicates that it may be beneficial to evaluate the first
-- argument in parallel with the second.  Returns the value of the
-- second argument.
-- 
-- @a `par` b@ is exactly equivalent semantically to @b@.
--
-- @par@ is generally used when the value of @a@ is likely to be
-- required later, but not immediately.  Also it is a good idea to
-- ensure that @a@ is not a trivial computation, otherwise the cost of
-- spawning it in parallel overshadows the benefits obtained by
-- running it in parallel.
--
-- Note that actual parallelism is only supported by certain
-- implementations (GHC with the @-threaded@ option, and GPH, for
-- now).  On other implementations, @par a b = b@.
--
par :: a -> b -> b
#ifdef __GLASGOW_HASKELL__
par = GHC.Conc.par
#else
-- For now, Hugs does not support par properly.
par a b = b
#endif
