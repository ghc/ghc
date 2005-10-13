-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST.Lazy
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires universal quantification for runST)
--
-- This module presents an identical interface to "Control.Monad.ST",
-- except that the monad delays evaluation of state operations until
-- a value depending on them is required.
--
-----------------------------------------------------------------------------

module Control.Monad.ST.Lazy (
	-- * The 'ST' monad
	ST,
	runST,
	fixST,

	-- * Converting between strict and lazy 'ST'
	strictToLazyST, lazyToStrictST,

	-- * Converting 'ST' To 'IO'
	RealWorld,
	stToIO,

	-- * Unsafe operations
	unsafeInterleaveST,
	unsafeIOToST
    ) where

import Prelude

import Control.Monad.Fix

import Control.Monad.ST (RealWorld)
import qualified Control.Monad.ST as ST

#ifdef __GLASGOW_HASKELL__
import qualified GHC.ST
import GHC.Base
import Control.Monad
#endif

#ifdef __HUGS__
import Hugs.LazyST
#endif

#ifdef __GLASGOW_HASKELL__
-- | The lazy state-transformer monad.
-- A computation of type @'ST' s a@ transforms an internal state indexed
-- by @s@, and returns a value of type @a@.
-- The @s@ parameter is either
--
-- * an unstantiated type variable (inside invocations of 'runST'), or
--
-- * 'RealWorld' (inside invocations of 'stToIO').
--
-- It serves to keep the internal states of different invocations of
-- 'runST' separate from each other and from invocations of 'stToIO'.
--
-- The '>>=' and '>>' operations are not strict in the state.  For example,
--
-- @'runST' (writeSTRef _|_ v >>= readSTRef _|_ >> return 2) = 2@
newtype ST s a = ST (State s -> (a, State s))
data State s = S# (State# s)

instance Functor (ST s) where
    fmap f m = ST $ \ s ->
      let 
       ST m_a = m
       (r,new_s) = m_a s
      in
      (f r,new_s)

instance Monad (ST s) where

        return a = ST $ \ s -> (a,s)
        m >> k   =  m >>= \ _ -> k
	fail s   = error s

        (ST m) >>= k
         = ST $ \ s ->
           let
             (r,new_s) = m s
             ST k_a = k r
           in
           k_a new_s

{-# NOINLINE runST #-}
-- | Return the value computed by a state transformer computation.
-- The @forall@ ensures that the internal state used by the 'ST'
-- computation is inaccessible to the rest of the program.
runST :: (forall s. ST s a) -> a
runST st = case st of ST the_st -> let (r,_) = the_st (S# realWorld#) in r

-- | Allow the result of a state transformer computation to be used (lazily)
-- inside the computation.
-- Note that if @f@ is strict, @'fixST' f = _|_@.
fixST :: (a -> ST s a) -> ST s a
fixST m = ST (\ s -> 
		let 
		   ST m_r = m r
		   (r,s') = m_r s
		in
		   (r,s'))
#endif

instance MonadFix (ST s) where
	mfix = fixST

-- ---------------------------------------------------------------------------
-- Strict <--> Lazy

#ifdef __GLASGOW_HASKELL__
{-|
Convert a strict 'ST' computation into a lazy one.  The strict state
thread passed to 'strictToLazyST' is not performed until the result of
the lazy state thread it returns is demanded.
-}
strictToLazyST :: ST.ST s a -> ST s a
strictToLazyST m = ST $ \s ->
        let 
  	   pr = case s of { S# s# -> GHC.ST.liftST m s# }
	   r  = case pr of { GHC.ST.STret _ v -> v }
	   s' = case pr of { GHC.ST.STret s2# _ -> S# s2# }
	in
	(r, s')

{-| 
Convert a lazy 'ST' computation into a strict one.
-}
lazyToStrictST :: ST s a -> ST.ST s a
lazyToStrictST (ST m) = GHC.ST.ST $ \s ->
        case (m (S# s)) of (a, S# s') -> (# s', a #)

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST = strictToLazyST . ST.unsafeInterleaveST . lazyToStrictST
#endif

unsafeIOToST :: IO a -> ST s a
unsafeIOToST = strictToLazyST . ST.unsafeIOToST

-- | A monad transformer embedding lazy state transformers in the 'IO'
-- monad.  The 'RealWorld' parameter indicates that the internal state
-- used by the 'ST' computation is a special one supplied by the 'IO'
-- monad, and thus distinct from those used by invocations of 'runST'.
stToIO :: ST RealWorld a -> IO a
stToIO = ST.stToIO . lazyToStrictST
