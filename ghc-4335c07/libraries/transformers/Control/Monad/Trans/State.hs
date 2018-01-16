{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.State
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- State monads, passing an updatable state through a computation.
--
-- Some computations may not require the full power of state transformers:
--
-- * For a read-only state, see "Control.Monad.Trans.Reader".
--
-- * To accumulate a value without using it on the way, see
--   "Control.Monad.Trans.Writer".
--
-- This version is lazy; for a strict version, see
-- "Control.Monad.Trans.State.Strict", which has the same interface.
-----------------------------------------------------------------------------

module Control.Monad.Trans.State (
  module Control.Monad.Trans.State.Lazy
  ) where

import Control.Monad.Trans.State.Lazy
