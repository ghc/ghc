{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.PTM
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires PTM)
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

module LwConc.PTM (
	module LwConc.PTM.TVar,
#ifdef __GLASGOW_HASKELL__
	module LwConc.PTM.TMVar,
        module LwConc.PTM.TChan,
        module LwConc.PTM.TQueue,
        module LwConc.PTM.TBQueue,
#endif
	module LwConc.PTM.TArray
  ) where

import LwConc.PTM.TVar
#ifdef __GLASGOW_HASKELL__
import LwConc.PTM.TMVar
import LwConc.PTM.TChan
#endif
import LwConc.PTM.TArray
import LwConc.PTM.TQueue
import LwConc.PTM.TBQueue
