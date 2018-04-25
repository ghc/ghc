-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TSem
-- Copyright   :  (c) The University of Glasgow 2012
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- 'TSem': transactional semaphores.
--
-- @since 2.4.2
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
module Control.Concurrent.STM.TSem (
      TSem, newTSem, waitTSem, signalTSem
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.Typeable

-- | 'TSem' is a transactional semaphore.  It holds a certain number
-- of units, and units may be acquired or released by 'waitTSem' and
-- 'signalTSem' respectively.  When the 'TSem' is empty, 'waitTSem'
-- blocks.
--
-- Note that 'TSem' has no concept of fairness, and there is no
-- guarantee that threads blocked in `waitTSem` will be unblocked in
-- the same order; in fact they will all be unblocked at the same time
-- and will fight over the 'TSem'.  Hence 'TSem' is not suitable if
-- you expect there to be a high number of threads contending for the
-- resource.  However, like other STM abstractions, 'TSem' is
-- composable.
--
-- @since 2.4.2
newtype TSem = TSem (TVar Int)
  deriving (Eq, Typeable)

newTSem :: Int -> STM TSem
newTSem i = fmap TSem (newTVar i)

waitTSem :: TSem -> STM ()
waitTSem (TSem t) = do
  i <- readTVar t
  when (i <= 0) retry
  writeTVar t $! (i-1)

signalTSem :: TSem -> STM ()
signalTSem (TSem t) = do
  i <- readTVar t
  writeTVar t $! i+1

