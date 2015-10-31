{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Stack
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Access to GHC's call-stack simulation
--
-- @since 4.5.0.0
-----------------------------------------------------------------------------

{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
module GHC.Stack (
    -- * Call stacks
    currentCallStack,
    whoCreated,
    errorWithStackTrace,

    -- * Implicit parameter call stacks
    CallStack, getCallStack, pushCallStack, prettyCallStack,

    -- * Source locations
    SrcLoc(..), prettySrcLoc,

    -- * Internals
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

import GHC.Stack.CCS
import GHC.Stack.Types
import GHC.IO
import GHC.Base
import GHC.List
import GHC.Exception

-- | Like the function 'error', but appends a stack trace to the error
-- message if one is available.
--
-- @since 4.7.0.0
{-# DEPRECATED errorWithStackTrace "'error' appends the call stack now" #-}
  -- DEPRECATED in 8.0.1
errorWithStackTrace :: String -> a
errorWithStackTrace x = unsafeDupablePerformIO $ do
   stack <- ccsToStrings =<< getCurrentCCS x
   if null stack
      then throwIO (ErrorCall x)
      else throwIO (ErrorCallWithLocation x (renderStack stack))
