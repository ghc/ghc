-- |
-- Module      :  GHC.Internal.ExecutionStack.Internal
-- Copyright   :  (c) The University of Glasgow 2013-2015
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Internals of the "GHC.ExecutionStack" module.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-- @since 4.9.0.0

module GHC.ExecutionStack.Internal {-# DEPRECATED "This module will be removed from base in the next version (v4.22)" #-} (
  -- * Internal
    Location (..)
  , SrcLoc (..)
  , StackTrace
  , stackFrames
  , stackDepth
  , collectStackTrace
  , showStackFrames
  , invalidateDebugCache
  ) where

import GHC.Internal.ExecutionStack.Internal
