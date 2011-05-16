{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Group
-- Copyright   :  (c) Nils Schweinsberg 2011,
--                (c) University Tuebingen 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Monadic grouping (used for monad comprehensions)
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances #-}

module Control.Monad.Group where

import Prelude
#if defined(__GLASGOW_HASKELL__)
import GHC.Exts (groupWith)
#endif

-- | `MonadGroup` type class without restrictions on the type `t`
class Monad m => MonadGroup m t where
    mgroupWith :: (a -> t) -> m a -> m (m a)

#if defined(__GLASGOW_HASKELL__)
-- | Grouping instance for lists using the `groupWith` function from the
-- "GHC.Exts" library
instance Ord t => MonadGroup [] t where
    mgroupWith = groupWith
#endif
