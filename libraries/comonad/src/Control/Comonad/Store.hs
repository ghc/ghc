{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Store
-- Copyright   :  (C) 2008-2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Store (
  -- * ComonadStore class
    ComonadStore(..)
  -- * The Store comonad
  , Store
  , store
  , runStore
  -- * The StoreT comonad transformer
  , StoreT(..)
  , runStoreT
  -- * Re-exported modules
  , module Control.Comonad
  , module Control.Comonad.Trans.Class
  ) where

import Control.Comonad
import Control.Comonad.Store.Class (ComonadStore(..))
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Store (Store, store, runStore, StoreT(..), runStoreT)
