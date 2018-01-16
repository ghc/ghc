{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Software Transactional Memory: a modular composable concurrency
-- abstraction.  See
--
--  * /Composable memory transactions/, by Tim Harris, Simon Marlow, Simon
--    Peyton Jones, and Maurice Herlihy, in /ACM Conference on Principles
--    and Practice of Parallel Programming/ 2005.
--    <http://research.microsoft.com/Users/simonpj/papers/stm/index.htm>
--
-----------------------------------------------------------------------------

module Control.Concurrent.STM (
        module Control.Monad.STM,
        module Control.Concurrent.STM.TVar,
#ifdef __GLASGOW_HASKELL__
        module Control.Concurrent.STM.TMVar,
        module Control.Concurrent.STM.TChan,
        module Control.Concurrent.STM.TQueue,
        module Control.Concurrent.STM.TBQueue,
#endif
        module Control.Concurrent.STM.TArray
  ) where

import Control.Monad.STM
import Control.Concurrent.STM.TVar
#ifdef __GLASGOW_HASKELL__
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
#endif
import Control.Concurrent.STM.TArray
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TBQueue
