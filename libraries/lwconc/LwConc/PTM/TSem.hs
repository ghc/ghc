-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.PTM.TSem
-- Copyright   :  (c) The University of Glasgow 2012
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires PTM)
--
-- 'TSem': transactional semaphores.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
module LwConc.PTM.TSem (
      TSem, newTSem, waitTSem, signalTSem
  ) where

import LwConc.Substrate
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
-- resource.  However, like other PTM abstractions, 'TSem' is
-- composable.
--
newtype TSem = TSem (PVar Int)
  deriving (Eq, Typeable)

newTSem :: Int -> PTM TSem
newTSem i = fmap TSem (newPVar i)

waitTSem :: TSem -> PTM ()
waitTSem (TSem t) = do
  i <- readPVar t
  when (i <= 0) retry
  writePVar t $! (i-1)

signalTSem :: TSem -> PTM ()
signalTSem (TSem t) = do
  i <- readPVar t
  writePVar t $! i+1

