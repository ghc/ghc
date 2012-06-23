{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif

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
        {-# DEPRECATED "Control.Concurrent.QSemN will be removed in GHC 7.8. Please use an alternative, e.g. the SafeSemaphore package, instead." #-}
        (  -- * General Quantity Semaphores
          QSemN,        -- abstract
          newQSemN,     -- :: Int   -> IO QSemN
          waitQSemN,    -- :: QSemN -> Int -> IO ()
          signalQSemN   -- :: QSemN -> Int -> IO ()
      ) where

import Prelude

import Control.Concurrent.MVar
import Control.Exception ( mask_ )
import Data.Typeable

#include "Typeable.h"

-- |A 'QSemN' is a quantity semaphore, in which the available
-- \"quantity\" may be signalled or waited for in arbitrary amounts.
newtype QSemN = QSemN (MVar (Int,[(Int,MVar ())])) deriving Eq

INSTANCE_TYPEABLE0(QSemN,qSemNTc,"QSemN")

-- |Build a new 'QSemN' with a supplied initial quantity.
--  The initial quantity must be at least 0.
newQSemN :: Int -> IO QSemN
newQSemN initial =
    if initial < 0
    then fail "newQSemN: Initial quantity must be non-negative"
    else do sem <- newMVar (initial, [])
            return (QSemN sem)

-- |Wait for the specified quantity to become available
waitQSemN :: QSemN -> Int -> IO ()
waitQSemN (QSemN sem) sz = mask_ $ do
  (avail,blocked) <- takeMVar sem   -- gain ex. access
  let remaining = avail - sz
  if remaining >= 0 then
       -- discharging 'sz' still leaves the semaphore
       -- in an 'unblocked' state.
     putMVar sem (remaining,blocked)
   else do
     b <- newEmptyMVar
     putMVar sem (avail, blocked++[(sz,b)])
     takeMVar b

-- |Signal that a given quantity is now available from the 'QSemN'.
signalQSemN :: QSemN -> Int  -> IO ()
signalQSemN (QSemN sem) n = mask_ $ do
   (avail,blocked)   <- takeMVar sem
   (avail',blocked') <- free (avail+n) blocked
   avail' `seq` putMVar sem (avail',blocked')
 where
   free avail []    = return (avail,[])
   free avail ((req,b):blocked)
     | avail >= req = do
        putMVar b ()
        free (avail-req) blocked
     | otherwise    = do
        (avail',blocked') <- free avail blocked
        return (avail',(req,b):blocked')
