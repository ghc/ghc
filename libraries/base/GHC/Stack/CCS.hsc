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
    whereFrom,

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
    renderStack,
    ipeProv,
    peekInfoProv,
    InfoProv(..),
    InfoProvEnt,
  ) where

import Foreign
import Foreign.C

import GHC.Base
import GHC.Ptr
import GHC.Foreign as GHC
import GHC.IO.Encoding
import GHC.List ( concatMap, reverse )
import GHC.Show (Show)

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

-- Static Closure Information

data InfoProv = InfoProv {
  ipName :: String,
  ipDesc :: String,
  ipTyDesc :: String,
  ipLabel :: String,
  ipMod :: String,
  ipLoc :: String
} deriving (Eq, Show)
data InfoProvEnt

getIPE :: a -> IO (Ptr InfoProvEnt)
getIPE obj = IO $ \s ->
   case whereFrom## obj s of
     (## s', addr ##) -> (## s', Ptr addr ##)

ipeProv :: Ptr InfoProvEnt -> Ptr InfoProv
ipeProv p = (#ptr InfoProvEnt, prov) p

peekIpName, peekIpDesc, peekIpLabel, peekIpModule, peekIpSrcLoc, peekIpTyDesc :: Ptr InfoProv -> IO CString
peekIpName p   =  (# peek InfoProv, table_name) p
peekIpDesc p   =  (# peek InfoProv, closure_desc) p
peekIpLabel p  =  (# peek InfoProv, label) p
peekIpModule p =  (# peek InfoProv, module) p
peekIpSrcLoc p =  (# peek InfoProv, srcloc) p
peekIpTyDesc p =  (# peek InfoProv, ty_desc) p

peekInfoProv :: Ptr InfoProv -> IO InfoProv
peekInfoProv infop = do
  name <- GHC.peekCString utf8 =<< peekIpName infop
  desc <- GHC.peekCString utf8 =<< peekIpDesc infop
  tyDesc <- GHC.peekCString utf8 =<< peekIpTyDesc infop
  label <- GHC.peekCString utf8 =<< peekIpLabel infop
  mod <- GHC.peekCString utf8 =<< peekIpModule infop
  loc <- GHC.peekCString utf8 =<< peekIpSrcLoc infop
  return InfoProv {
      ipName = name,
      ipDesc = desc,
      ipTyDesc = tyDesc,
      ipLabel = label,
      ipMod = mod,
      ipLoc = loc
    }

-- | Get information about where a value originated from.
-- This information is stored statically in a binary when `-finfo-table-map` is
-- enabled.  The source positions will be greatly improved by also enabled debug
-- information with `-g3`. Finally you can enable `-fdistinct-constructor-tables` to
-- get more precise information about data constructor allocations.
--
-- The information is collect by looking at the info table address of a specific closure and
-- then consulting a specially generated map (by `-finfo-table-map`) to find out where we think
-- the best source position to describe that info table arose from.
whereFrom :: a -> IO (Maybe InfoProv)
whereFrom obj = do
  ipe <- getIPE obj
  -- The primop returns the null pointer in two situations at the moment
  -- 1. The lookup fails for whatever reason
  -- 2. -finfo-table-map is not enabled.
  -- It would be good to distinguish between these two cases somehow.
  if ipe == nullPtr
    then return Nothing
    else do
      infoProv <- peekInfoProv (ipeProv ipe)
      return $ Just infoProv
