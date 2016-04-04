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

{-# LANGUAGE MagicHash, NoImplicitPrelude, ImplicitParams, RankNTypes #-}
module GHC.Stack (
    errorWithStackTrace,

    -- * Profiling call stacks
    currentCallStack,
    whoCreated,

    -- * HasCallStack call stacks
    CallStack, HasCallStack, callStack, emptyCallStack, freezeCallStack,
    fromCallSiteList, getCallStack, popCallStack, prettyCallStack,
    pushCallStack, withFrozenCallStack,

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


-- | Pop the most recent call-site off the 'CallStack'.
--
-- This function, like 'pushCallStack', has no effect on a frozen 'CallStack'.
--
-- @since 4.9.0.0
popCallStack :: CallStack -> CallStack
popCallStack stk = case stk of
  EmptyCallStack         -> errorWithoutStackTrace "popCallStack: empty stack"
  PushCallStack _ _ stk' -> stk'
  FreezeCallStack _      -> stk
{-# INLINE popCallStack #-}

-- | Return the current 'CallStack'.
--
-- Does *not* include the call-site of 'callStack'.
--
-- @since 4.9.0.0
callStack :: HasCallStack => CallStack
callStack = popCallStack ?callStack
{-# INLINE callStack #-}

-- | Perform some computation without adding new entries to the 'CallStack'.
--
-- @since 4.9.0.0
withFrozenCallStack :: HasCallStack
                    => ( HasCallStack => a )
                    -> a
withFrozenCallStack do_this =
  -- we pop the stack before freezing it to remove
  -- withFrozenCallStack's call-site
  let ?callStack = freezeCallStack (popCallStack callStack)
  in do_this
