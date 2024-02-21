{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Stack.Types
-- Copyright   :  (c) The University of Glasgow 2015
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Type definitions for implicit call-stacks.
-- Use "GHC.Stack" from the base package instead of importing this
-- module directly.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

module GHC.Stack.Types
    (-- *  Implicit call stacks
     CallStack(..),
     HasCallStack,
     emptyCallStack,
     freezeCallStack,
     fromCallSiteList,
     getCallStack,
     pushCallStack,
     -- *  Source locations
     SrcLoc(..)
     ) where

import GHC.Internal.Stack.Types
