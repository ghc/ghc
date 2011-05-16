{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.QSem
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Simple quantity semaphores.
--
-----------------------------------------------------------------------------

module Control.Concurrent.QSem
        ( -- * Simple Quantity Semaphores
          QSem,         -- abstract
          newQSem,      -- :: Int  -> IO QSem
          waitQSem,     -- :: QSem -> IO ()
          signalQSem    -- :: QSem -> IO ()
        ) where

import Prelude
import Control.Concurrent.MVar
import Control.Exception ( mask_ )
import Data.Typeable

#include "Typeable.h"

-- General semaphores are also implemented readily in terms of shared
-- @MVar@s, only have to catch the case when the semaphore is tried
-- waited on when it is empty (==0). Implement this in the same way as
-- shared variables are implemented - maintaining a list of @MVar@s
-- representing threads currently waiting. The counter is a shared
-- variable, ensuring the mutual exclusion on its access.

-- |A 'QSem' is a simple quantity semaphore, in which the available
-- \"quantity\" is always dealt with in units of one.
newtype QSem = QSem (MVar (Int, [MVar ()])) deriving Eq

INSTANCE_TYPEABLE0(QSem,qSemTc,"QSem")

-- |Build a new 'QSem' with a supplied initial quantity.
--  The initial quantity must be at least 0.
newQSem :: Int -> IO QSem
newQSem initial =
    if initial < 0
    then fail "newQSem: Initial quantity must be non-negative"
    else do sem <- newMVar (initial, [])
            return (QSem sem)

-- |Wait for a unit to become available
waitQSem :: QSem -> IO ()
waitQSem (QSem sem) = mask_ $ do
   (avail,blocked) <- takeMVar sem  -- gain ex. access
   if avail > 0 then
     let avail' = avail-1
     in avail' `seq` putMVar sem (avail',[])
    else do
     b <- newEmptyMVar
      {-
        Stuff the reader at the back of the queue,
        so as to preserve waiting order. A signalling
        process then only have to pick the MVar at the
        front of the blocked list.

        The version of waitQSem given in the paper could
        lead to starvation.
      -}
     putMVar sem (0, blocked++[b])
     takeMVar b

-- |Signal that a unit of the 'QSem' is available
signalQSem :: QSem -> IO ()
signalQSem (QSem sem) = mask_ $ do
   (avail,blocked) <- takeMVar sem
   case blocked of
     [] -> let avail' = avail+1
           in avail' `seq` putMVar sem (avail',blocked)

     (b:blocked') -> do
           putMVar sem (0,blocked')
           putMVar b ()
