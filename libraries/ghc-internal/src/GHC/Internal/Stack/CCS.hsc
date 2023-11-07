{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Stack.CCS
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

{-# LANGUAGE UnboxedTuples, MagicHash, NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module GHC.Internal.Stack.CCS (
    -- * Call stacks
    currentCallStack,
    currentCallStackIds,
    whoCreated,

    -- * Internals
    CostCentreStack,
    CostCentre,
    CostCentreId,
    getCurrentCCS,
    getCCSOf,
    clearCCS,
    ccsCC,
    ccsParent,
    ccLabel,
    ccModule,
    ccId,
    ccSrcSpan,
    ccsToIds,
    ccsToStrings,
    renderStack,
  ) where

import GHC.Internal.Foreign.C.String
import GHC.Internal.Foreign.C.String.Encoding as GHC
import GHC.Internal.Foreign.Storable

import GHC.Internal.Base
import GHC.Internal.Ptr
import GHC.Internal.IO.Encoding
import GHC.Internal.List ( concatMap, reverse )
import GHC.Internal.Word ( Word32 )
import GHC.Internal.Show
import GHC.Internal.Read
import GHC.Internal.Enum
import GHC.Internal.Real
import GHC.Internal.Num

#define PROFILING
#include "Rts.h"

-- | A cost-centre stack from GHC's cost-center profiler.
data CostCentreStack

-- | A cost-centre from GHC's cost-center profiler.
data CostCentre

-- | Cost centre identifier
--
-- @since 4.20.0.0
newtype CostCentreId = CostCentreId Word32
  deriving (Show, Read)
  deriving newtype (Eq, Ord, Bounded, Enum, Integral, Num, Real)

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
#if defined(javascript_HOST_ARCH)
ccsCC :: Ptr CostCentreStack -> IO (Ptr CostCentre)
ccsCC p = peekByteOff p 4

ccsParent :: Ptr CostCentreStack -> IO (Ptr CostCentreStack)
ccsParent p = peekByteOff p 8

-- | Get the 'CostCentreId' of a 'CostCentre'.
--
-- @since 4.20.0.0
ccId :: Ptr CostCentre -> IO CostCentreId
ccId p = fmap CostCentreId $ peekByteOff p 0

ccLabel :: Ptr CostCentre -> IO CString
ccLabel p = peekByteOff p 4

ccModule :: Ptr CostCentre -> IO CString
ccModule p = peekByteOff p 8

ccSrcSpan :: Ptr CostCentre -> IO CString
ccSrcSpan p = peekByteOff p 12
#else
ccsCC :: Ptr CostCentreStack -> IO (Ptr CostCentre)
ccsCC p = (# peek CostCentreStack, cc) p

-- | Get the tail of a 'CostCentreStack'.
ccsParent :: Ptr CostCentreStack -> IO (Ptr CostCentreStack)
ccsParent p = (# peek CostCentreStack, prevStack) p

-- | Get the 'CostCentreId' of a 'CostCentre'.
--
-- @since 4.20.0.0
ccId :: Ptr CostCentre -> IO CostCentreId
ccId p = fmap CostCentreId $ (# peek CostCentre, ccID) p

-- | Get the label of a 'CostCentre'.
ccLabel :: Ptr CostCentre -> IO CString
ccLabel p = (# peek CostCentre, label) p

-- | Get the module of a 'CostCentre'.
ccModule :: Ptr CostCentre -> IO CString
ccModule p = (# peek CostCentre, module) p

-- | Get the source span of a 'CostCentre'.
ccSrcSpan :: Ptr CostCentre -> IO CString
ccSrcSpan p = (# peek CostCentre, srcloc) p
#endif

-- | Returns a @[String]@ representing the current call stack.  This
-- can be useful for debugging.
--
-- The implementation uses the call-stack simulation maintained by the
-- profiler, so it only works if the program was compiled with @-prof@
-- and contains suitable SCC annotations (e.g. by using @-fprof-auto@).
-- Otherwise, the list returned is likely to be empty or
-- uninformative.
--
-- @since base-4.5.0.0
currentCallStack :: IO [String]
currentCallStack = ccsToStrings =<< getCurrentCCS ()

-- | Returns a @[CostCentreId]@ representing the current call stack.  This
-- can be useful for debugging.
--
-- The implementation uses the call-stack simulation maintained by the
-- profiler, so it only works if the program was compiled with @-prof@
-- and contains suitable SCC annotations (e.g. by using @-fprof-late@).
-- Otherwise, the list returned is likely to be empty or
-- uninformative.
--
-- @since 4.20.0.0
currentCallStackIds :: IO [CostCentreId]
currentCallStackIds = ccsToIds =<< getCurrentCCS ()

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

-- | Format a 'CostCentreStack' as a list of cost centre IDs.
--
-- @since 4.20.0.0
ccsToIds :: Ptr CostCentreStack -> IO [CostCentreId]
ccsToIds ccs0 = go ccs0 []
  where
    go ccs acc
     | ccs == nullPtr = return acc
     | otherwise = do
        cc <- ccsCC ccs
        cc_id <- ccId cc
        lbl <- GHC.peekCString utf8 =<< ccLabel cc
        mdl <- GHC.peekCString utf8 =<< ccModule cc
        parent <- ccsParent ccs
        if (mdl == "MAIN" && lbl == "MAIN")
           then return acc
           else go parent (cc_id : acc)

-- | Get the stack trace attached to an object.
--
-- @since base-4.5.0.0
whoCreated :: a -> IO [String]
whoCreated obj = do
  ccs <- getCCSOf obj
  ccsToStrings ccs

renderStack :: [String] -> String
renderStack strs =
  "CallStack (from -prof):" ++ concatMap ("\n  "++) (reverse strs)

