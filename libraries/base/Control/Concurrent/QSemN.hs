-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.QSemN
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Quantity semaphores in which each thread may wait for an arbitrary
-- \"amount\".
--
-----------------------------------------------------------------------------

module Control.Concurrent.QSemN
	(  -- * General Quantity Semaphores
	  QSemN,	-- abstract
	  newQSemN,	-- :: Int   -> IO QSemN
	  waitQSemN,	-- :: QSemN -> Int -> IO ()
	  signalQSemN	-- :: QSemN -> Int -> IO ()
      ) where

import Prelude

import Control.Concurrent.MVar
import Data.Typeable

#include "Typeable.h"

-- |A 'QSemN' is a quantity semaphore, in which the available
-- \"quantity\" may be signalled or waited for in arbitrary amounts.
newtype QSemN = QSemN (MVar (Int,[(Int,MVar ())]))

INSTANCE_TYPEABLE0(QSemN,qSemNTc,"QSemN")

-- |Build a new 'QSemN' with a supplied initial quantity.
newQSemN :: Int -> IO QSemN 
newQSemN init = do
   sem <- newMVar (init,[])
   return (QSemN sem)

-- |Wait for the specified quantity to become available
waitQSemN :: QSemN -> Int -> IO ()
waitQSemN (QSemN sem) sz = do
  (avail,blocked) <- takeMVar sem   -- gain ex. access
  if (avail - sz) >= 0 then
       -- discharging 'sz' still leaves the semaphore
       -- in an 'unblocked' state.
     putMVar sem (avail-sz,blocked)
   else do
     block <- newEmptyMVar
     putMVar sem (avail, blocked++[(sz,block)])
     takeMVar block

-- |Signal that a given quantity is now available from the 'QSemN'.
signalQSemN :: QSemN -> Int  -> IO ()
signalQSemN (QSemN sem) n = do
   (avail,blocked)   <- takeMVar sem
   (avail',blocked') <- free (avail+n) blocked
   putMVar sem (avail',blocked')
 where
   free avail []    = return (avail,[])
   free avail ((req,block):blocked)
     | avail >= req = do
	putMVar block ()
	free (avail-req) blocked
     | otherwise    = do
	(avail',blocked') <- free avail blocked
        return (avail',(req,block):blocked')
