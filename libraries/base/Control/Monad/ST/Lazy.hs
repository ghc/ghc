-----------------------------------------------------------------------------
-- 
-- Module      :  Control.Monad.ST.Lazy
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires universal quantification for runST)
--
-- $Id: Lazy.hs,v 1.4 2002/01/02 14:40:09 simonmar Exp $
--
-- This module presents an identical interface to Control.Monad.ST,
-- but the underlying implementation of the state thread is lazy.
--
-----------------------------------------------------------------------------

module Control.Monad.ST.Lazy (
	ST,

	runST,
	unsafeInterleaveST,
	fixST,

	ST.unsafeIOToST, ST.stToIO,

	strictToLazyST, lazyToStrictST
    ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import qualified Control.Monad.ST as ST
import qualified GHC.ST
import GHC.Base
import Control.Monad
#endif

#ifdef __GLASGOW_HASKELL__
newtype ST s a = ST (State s -> (a, State s))
data State s = S# (State# s)
#endif

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


#ifdef __GLASGOW_HASKELL__
{-# NOINLINE runST #-}
runST :: (forall s. ST s a) -> a
runST st = case st of ST the_st -> let (r,_) = the_st (S# realWorld#) in r

fixST :: (a -> ST s a) -> ST s a
fixST m = ST (\ s -> 
		let 
		   ST m_r = m r
		   (r,s)  = m_r s
		in
		   (r,s))
#endif

-- ---------------------------------------------------------------------------
-- Strict <--> Lazy

#ifdef __GLASGOW_HASKELL__
strictToLazyST :: ST.ST s a -> ST s a
strictToLazyST m = ST $ \s ->
        let 
  	   pr = case s of { S# s# -> GHC.ST.liftST m s# }
	   r  = case pr of { GHC.ST.STret _ v -> v }
	   s' = case pr of { GHC.ST.STret s2# _ -> S# s2# }
	in
	(r, s')

lazyToStrictST :: ST s a -> ST.ST s a
lazyToStrictST (ST m) = GHC.ST.ST $ \s ->
        case (m (S# s)) of (a, S# s') -> (# s', a #)
#endif

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST = strictToLazyST . ST.unsafeInterleaveST . lazyToStrictST
