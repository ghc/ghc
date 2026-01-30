{-# LANGUAGE Trustworthy #-}

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

import GHC.Internal.Base
import GHC.Internal.Stack
import GHC.Internal.Stack.CCS
import GHC.Internal.IO
import GHC.Internal.Exception


-- | Like the function 'error', but appends a stack trace to the error
-- message if one is available.
--
-- @since base-4.7.0.0
{-# DEPRECATED errorWithStackTrace "'error' appends the call stack now" #-}
  -- DEPRECATED in 8.0.1
errorWithStackTrace :: String -> a
errorWithStackTrace x = unsafeDupablePerformIO $ throwIO (ErrorCall x)
