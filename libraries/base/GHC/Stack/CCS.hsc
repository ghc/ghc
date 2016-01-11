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

data CostCentreStack
data CostCentre

getCurrentCCS :: dummy -> IO (Ptr CostCentreStack)
getCurrentCCS dummy = IO $ \s ->
   case getCurrentCCS## dummy s of
     (## s', addr ##) -> (## s', Ptr addr ##)

getCCSOf :: a -> IO (Ptr CostCentreStack)
getCCSOf obj = IO $ \s ->
   case getCCSOf## obj s of
     (## s', addr ##) -> (## s', Ptr addr ##)

clearCCS :: IO a -> IO a
clearCCS (IO m) = IO $ \s -> clearCCS## m s

ccsCC :: Ptr CostCentreStack -> IO (Ptr CostCentre)
ccsCC p = (# peek CostCentreStack, cc) p

ccsParent :: Ptr CostCentreStack -> IO (Ptr CostCentreStack)
ccsParent p = (# peek CostCentreStack, prevStack) p

ccLabel :: Ptr CostCentre -> IO CString
ccLabel p = (# peek CostCentre, label) p

ccModule :: Ptr CostCentre -> IO CString
ccModule p = (# peek CostCentre, module) p

ccSrcSpan :: Ptr CostCentre -> IO CString
ccSrcSpan p = (# peek CostCentre, srcloc) p

-- | Returns a @[String]@ representing the current call stack.  This
-- can be useful for debugging.
--
-- The implementation uses the call-stack simulation maintined by the
-- profiler, so it only works if the program was compiled with @-prof@
-- and contains suitable SCC annotations (e.g. by using @-fprof-auto@).
-- Otherwise, the list returned is likely to be empty or
-- uninformative.
--
-- @since 4.5.0.0
currentCallStack :: IO [String]
currentCallStack = ccsToStrings =<< getCurrentCCS ()

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
