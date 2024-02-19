{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.Stack
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Access to GHC's call-stack simulation
--
-- @since 4.5.0.0

module GHC.Stack
    (errorWithStackTrace,
     -- *  Profiling call stacks
     currentCallStack,
     whoCreated,
     -- *  HasCallStack call stacks
     CallStack,
     HasCallStack,
     callStack,
     emptyCallStack,
     freezeCallStack,
     fromCallSiteList,
     getCallStack,
     popCallStack,
     prettyCallStack,
     pushCallStack,
     withFrozenCallStack,
     -- *  Source locations
     SrcLoc(..),
     prettySrcLoc,
     -- *  Internals
     CostCentreStack,
     CostCentre,
     getCurrentCCS,
     getCCSOf,
     clearCCS,
     ccsCC,
     ccsParent,
     ccLabel,
     ccModule,
     ccSrcSpan,
     ccsToStrings,
     renderStack
     ) where

import GHC.Internal.Stack