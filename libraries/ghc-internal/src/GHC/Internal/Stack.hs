{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Stack
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Access to GHC's call-stack simulation
--
-- @since base-4.5.0.0
-----------------------------------------------------------------------------

module GHC.Internal.Stack (
    errorWithStackTrace,

    -- * Profiling call stacks
    currentCallStack,
    currentCallStackIds,
    whoCreated,

    -- * HasCallStack call stacks
    CallStack, HasCallStack, callStack, emptyCallStack, freezeCallStack,
    fromCallSiteList, getCallStack, popCallStack,
    pushCallStack, withFrozenCallStack,
    prettyCallStackLines, prettyCallStack,

    -- * Source locations
    SrcLoc(..), prettySrcLoc,

    -- * Internals
    CostCentreStack,
    CostCentre,
    CostCentreId,
    getCurrentCCS,
    getCCSOf,
    clearCCS,
    ccsCC,
    ccsParent,
    ccId,
    ccLabel,
    ccModule,
    ccSrcSpan,
    ccsToIds,
    ccsToStrings,
    renderStack
  ) where

import GHC.Internal.Show
import GHC.Internal.Stack.CCS
import GHC.Internal.Stack.Types
import GHC.Internal.IO
import GHC.Internal.Base
import GHC.Internal.List
import GHC.Internal.Data.OldList (intercalate)
import GHC.Internal.Exception

-- | Like the function 'error', but appends a stack trace to the error
-- message if one is available.
--
-- @since base-4.7.0.0
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
-- @since base-4.9.0.0
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
-- @since base-4.9.0.0
callStack :: HasCallStack => CallStack
callStack =
  case ?callStack of
    EmptyCallStack -> EmptyCallStack
    _              -> popCallStack ?callStack
{-# INLINE callStack #-}

-- | Perform some computation without adding new entries to the 'CallStack'.
--
-- @since base-4.9.0.0
withFrozenCallStack :: HasCallStack
                    => ( HasCallStack => a )
                    -> a
withFrozenCallStack do_this =
  -- we pop the stack before freezing it to remove
  -- withFrozenCallStack's call-site
  let ?callStack = freezeCallStack (popCallStack callStack)
  in do_this

-- prettySrcLoc and prettyCallStack are defined here to avoid hs-boot
-- files. See Note [Definition of CallStack]

-- | Pretty print a 'SrcLoc'.
--
-- @since 4.9.0.0
prettySrcLoc :: SrcLoc -> String
prettySrcLoc SrcLoc {..}
  = foldr (++) ""
      [ srcLocFile, ":"
      , show srcLocStartLine, ":"
      , show srcLocStartCol, " in "
      , srcLocPackage, ":", srcLocModule
      ]

-- | Pretty print a 'CallStack'.
--
-- @since 4.9.0.0
prettyCallStack :: CallStack -> String
prettyCallStack = intercalate "\n" . prettyCallStackLines

prettyCallStackLines :: CallStack -> [String]
prettyCallStackLines cs = case getCallStack cs of
  []  -> []
  stk -> "CallStack (from HasCallStack):"
       : map (("  " ++) . prettyCallSite) stk
  where
    prettyCallSite (f, loc) = f ++ ", called at " ++ prettySrcLoc loc
