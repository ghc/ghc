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
-- $Id: Lazy.hs,v 1.2 2001/07/03 11:37:49 simonmar Exp $
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

	STRef.STRef,
	newSTRef, readSTRef, writeSTRef,

	STArray.STArray,
	newSTArray, readSTArray, writeSTArray, boundsSTArray, 
	thawSTArray, freezeSTArray, unsafeFreezeSTArray, 
#ifdef __GLASGOW_HASKELL__
-- no 'good' reason, just doesn't support it right now.
        unsafeThawSTArray,
#endif

	ST.unsafeIOToST, ST.stToIO,

	strictToLazyST, lazyToStrictST
    ) where

import Prelude

import qualified Data.STRef as STRef
import Data.Array

#ifdef __GLASGOW_HASKELL__
import qualified Control.Monad.ST as ST
import qualified GHC.Arr as STArray
import qualified GHC.ST
import GHC.Base	( ($), ()(..) )
import Control.Monad
import Data.Ix
import GHC.Prim
#endif

#ifdef __HUGS__
import qualified ST
import Monad
import Ix
import Array
import PrelPrim ( unST 
		 , mkST 
		 , PrimMutableArray
		 , PrimArray
		 , primNewArray
		 , primReadArray
		 , primWriteArray
		 , primUnsafeFreezeArray
		 , primSizeMutableArray
		 , primSizeArray
		 , primIndexArray
		 )
#endif


#ifdef __GLASGOW_HASKELL__
newtype ST s a = ST (State s -> (a, State s))
data State s = S# (State# s)
#endif

#ifdef __HUGS__
newtype ST s a = ST (s -> (a,s))
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
#endif

#ifdef __HUGS__
runST :: (__forall s. ST s a) -> a
runST st = case st of ST the_st -> let (r,_) = the_st realWorld in r
	where realWorld = error "runST: entered the RealWorld"
#endif

fixST :: (a -> ST s a) -> ST s a
fixST m = ST (\ s -> 
		let 
		   ST m_r = m r
		   (r,s)  = m_r s
		in
		   (r,s))

-- ---------------------------------------------------------------------------
-- Variables

newSTRef   :: a -> ST s (STRef.STRef s a)
readSTRef  :: STRef.STRef s a -> ST s a
writeSTRef :: STRef.STRef s a -> a -> ST s ()

newSTRef   = strictToLazyST . STRef.newSTRef
readSTRef  = strictToLazyST . STRef.readSTRef
writeSTRef r a = strictToLazyST (STRef.writeSTRef r a)

-- --------------------------------------------------------------------------
-- Arrays

newSTArray 	    :: Ix ix => (ix,ix) -> elt -> ST s (STArray.STArray s ix elt)
readSTArray   	    :: Ix ix => STArray.STArray s ix elt -> ix -> ST s elt 
writeSTArray	    :: Ix ix => STArray.STArray s ix elt -> ix -> elt -> ST s () 
boundsSTArray       :: Ix ix => STArray.STArray s ix elt -> (ix, ix)  
thawSTArray 	    :: Ix ix => Array ix elt -> ST s (STArray.STArray s ix elt)
freezeSTArray	    :: Ix ix => STArray.STArray s ix elt -> ST s (Array ix elt)
unsafeFreezeSTArray :: Ix ix => STArray.STArray s ix elt -> ST s (Array ix elt)

#ifdef __GLASGOW_HASKELL__

newSTArray ixs init   	= strictToLazyST (STArray.newSTArray ixs init)

readSTArray arr ix      = strictToLazyST (STArray.readSTArray arr ix)
writeSTArray arr ix v   = strictToLazyST (STArray.writeSTArray arr ix v)
boundsSTArray arr       = STArray.boundsSTArray arr
thawSTArray arr	        = strictToLazyST (STArray.thawSTArray arr)
freezeSTArray arr       = strictToLazyST (STArray.freezeSTArray arr)
unsafeFreezeSTArray arr = strictToLazyST (STArray.unsafeFreezeSTArray arr)
unsafeThawSTArray arr   = strictToLazyST (STArray.unsafeThawSTArray arr)
#endif


#ifdef __HUGS__
newSTArray ixs elt = do
  { arr <- strictToLazyST (primNewArray (rangeSize ixs) elt)
  ; return (STArray ixs arr)
  }

boundsSTArray (STArray ixs arr)        = ixs
readSTArray   (STArray ixs arr) ix     
	= strictToLazyST (primReadArray arr (index ixs ix))
writeSTArray  (STArray ixs arr) ix elt 
	= strictToLazyST (primWriteArray arr (index ixs ix) elt)
freezeSTArray (STArray ixs arr)        = do
  { arr' <- strictToLazyST (primFreezeArray arr)
  ; return (Array ixs arr')
  }

unsafeFreezeSTArray (STArray ixs arr)  = do 
  { arr' <- strictToLazyST (primUnsafeFreezeArray arr)
  ; return (Array ixs arr')
  }

thawSTArray (Array ixs arr) = do
  { arr' <- strictToLazyST (primThawArray arr)
  ; return (STArray ixs arr')
  }

primFreezeArray :: PrimMutableArray s a -> ST.ST s (PrimArray a)
primFreezeArray arr = do
  { let n = primSizeMutableArray arr
  ; arr' <- primNewArray n arrEleBottom
  ; mapM_ (copy arr arr') [0..n-1]
  ; primUnsafeFreezeArray arr'
  }
 where
  copy arr arr' i = do { x <- primReadArray arr i; primWriteArray arr' i x }
  arrEleBottom = error "primFreezeArray: panic"

primThawArray :: PrimArray a -> ST.ST s (PrimMutableArray s a)
primThawArray arr = do
  { let n = primSizeArray arr
  ; arr' <- primNewArray n arrEleBottom
  ; mapM_ (copy arr arr') [0..n-1]
  ; return arr'
  }
 where
  copy arr arr' i = primWriteArray arr' i (primIndexArray arr i)
  arrEleBottom = error "primFreezeArray: panic"
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

#ifdef __HUGS__
strictToLazyST :: ST.ST s a -> ST s a
strictToLazyST m = ST $ \s ->
        let 
  	   pr = unST m s
	   r  = fst pr
	   s' = snd pr
	in
	(r, s')


lazyToStrictST :: ST s a -> ST.ST s a
lazyToStrictST (ST m) = mkST $ m
#endif

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST = strictToLazyST . ST.unsafeInterleaveST . lazyToStrictST
