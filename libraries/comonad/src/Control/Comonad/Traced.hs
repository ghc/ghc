{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Traced
-- Copyright   :  (C) 2008-2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Traced (
  -- * ComonadTraced class
    ComonadTraced(..)
  , traces
  -- * The Traced comonad
  , Traced
  , traced
  , runTraced
  -- * The TracedT comonad transformer
  , TracedT(..)
  -- * Re-exported modules
  , module Control.Comonad
  , module Control.Comonad.Trans.Class
  , module Data.Monoid
  ) where

import Control.Comonad
import Control.Comonad.Traced.Class (ComonadTraced(..), traces)
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Traced (Traced, traced, runTraced, TracedT(..), runTracedT)
import Data.Monoid
