{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Stack.CCS
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

{-# LANGUAGE UnboxedTuples, MagicHash, NoImplicitPrelude #-}
module GHC.Stack.CCS (
    -- * Call stacks
    currentCallStack,
    whoCreated,

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

import Foreign
import Foreign.C

import GHC.Base
import GHC.Ptr
import GHC.Foreign as GHC
import GHC.IO.Encoding
import GHC.List ( concatMap, reverse )

#define PROFILING
#include "Rts.h"

-- | A cost-centre stack from GHC's cost-center profiler.
data CostCentreStack

-- | A cost-centre from GHC's cost-center profiler.
data CostCentre

-- | Returns the current 'CostCentreStack' (value is @nullPtr@ if the current
-- program was not compiled with profiling support). Takes a dummy argument
-- which can be used to avoid the call to @getCurrentCCS@ being floated out by
-- the simplifier, which would result in an uninformative stack ("CAF").
getCurrentCCS :: dummy -> IO (Ptr CostCentreStack)
getCurrentCCS dummy = IO $ \s ->
   case getCurrentCCS## dummy s of
     (## s', addr ##) -> (## s', Ptr addr ##)

-- | Get the 'CostCentreStack' associated with the given value.
getCCSOf :: a -> IO (Ptr CostCentreStack)
getCCSOf obj = IO $ \s ->
   case getCCSOf## obj s of
     (## s', addr ##) -> (## s', Ptr addr ##)

-- | Run a computation with an empty cost-center stack. For example, this is
-- used by the interpreter to run an interpreted computation without the call
-- stack showing that it was invoked from GHC.
clearCCS :: IO a -> IO a
clearCCS (IO m) = IO $ \s -> clearCCS## m s

-- | Get the 'CostCentre' at the head of a 'CostCentreStack'.
ccsCC :: Ptr CostCentreStack -> IO (Ptr CostCentre)
ccsCC p = (# peek CostCentreStack, cc) p

-- | Get the tail of a 'CostCentreStack'.
ccsParent :: Ptr CostCentreStack -> IO (Ptr CostCentreStack)
ccsParent p = (# peek CostCentreStack, prevStack) p

-- | Get the label of a 'CostCentre'.
ccLabel :: Ptr CostCentre -> IO CString
ccLabel p = (# peek CostCentre, label) p

-- | Get the module of a 'CostCentre'.
ccModule :: Ptr CostCentre -> IO CString
ccModule p = (# peek CostCentre, module) p

-- | Get the source span of a 'CostCentre'.
ccSrcSpan :: Ptr CostCentre -> IO CString
ccSrcSpan p = (# peek CostCentre, srcloc) p

-- | Returns a @[String]@ representing the current call stack.  This
-- can be useful for debugging.
--
-- The implementation uses the call-stack simulation maintained by the
-- profiler, so it only works if the program was compiled with @-prof@
-- and contains suitable SCC annotations (e.g. by using @-fprof-auto@).
-- Otherwise, the list returned is likely to be empty or
-- uninformative.
--
-- @since 4.5.0.0
currentCallStack :: IO [String]
currentCallStack = ccsToStrings =<< getCurrentCCS ()

-- | Format a 'CostCentreStack' as a list of lines.
ccsToStrings :: Ptr CostCentreStack -> IO [String]
ccsToStrings ccs0 = go ccs0 []
  where
    go ccs acc
     | ccs == nullPtr = return acc
     | otherwise = do
        cc  <- ccsCC ccs
        lbl <- GHC.peekCString utf8 =<< ccLabel cc
        mdl <- GHC.peekCString utf8 =<< ccModule cc
        loc <- GHC.peekCString utf8 =<< ccSrcSpan cc
        parent <- ccsParent ccs
        if (mdl == "MAIN" && lbl == "MAIN")
           then return acc
           else go parent ((mdl ++ '.':lbl ++ ' ':'(':loc ++ ")") : acc)

-- | Get the stack trace attached to an object.
--
-- @since 4.5.0.0
whoCreated :: a -> IO [String]
whoCreated obj = do
  ccs <- getCCSOf obj
  ccsToStrings ccs

renderStack :: [String] -> String
renderStack strs =
  "CallStack (from -prof):" ++ concatMap ("\n  "++) (reverse strs)
