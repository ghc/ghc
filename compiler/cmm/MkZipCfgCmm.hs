{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- This is the module to import to be able to build C-- programs.
-- It should not be necessary to import MkZipCfg or ZipCfgCmmRep.
-- If you find it necessary to import these other modules, please
-- complain to Norman Ramsey.

module MkZipCfgCmm
  ( mkNop, mkAssign, mkStore, mkCall, mkUnsafeCall, mkFinalCall
         , mkJump, mkCbranch, mkSwitch, mkReturn, mkComment, mkCmmIfThenElse
         , mkCmmWhileDo
  , (<*>), mkLabel, mkBranch
  , emptyAGraph, withFreshLabel, withUnique, outOfLine
  , lgraphOfAGraph, graphOfAGraph, labelAGraph
  , CmmZ, CmmTopZ, CmmGraph, CmmBlock, CmmAGraph, Middle, Last, Convention(..)
  )
where

#include "HsVersions.h"

import CmmExpr
import Cmm ( GenCmm(..), GenCmmTop(..), CmmStatic, CmmInfo
           , CmmCallTarget(..), CmmActuals, CmmFormals
           )
import ZipCfgCmmRep hiding (CmmGraph, CmmAGraph, CmmBlock, CmmZ, CmmTopZ)
  -- ^ to make this module more self-contained, these definitions are duplicated below
import PprCmm()

import ClosureInfo
import FastString
import ForeignCall
import ZipCfg 
import MkZipCfg

type CmmGraph  = LGraph Middle Last
type CmmAGraph = AGraph Middle Last
type CmmBlock  = Block  Middle Last
type CmmZ      = GenCmm    CmmStatic CmmInfo CmmGraph
type CmmTopZ   = GenCmmTop CmmStatic CmmInfo CmmGraph

mkNop        :: CmmAGraph
mkAssign     :: CmmReg  -> CmmExpr -> CmmAGraph
mkStore      :: CmmExpr -> CmmExpr -> CmmAGraph
mkCall       :: CmmExpr -> CCallConv -> CmmFormals -> CmmActuals -> C_SRT -> CmmAGraph
mkUnsafeCall :: CmmCallTarget -> CmmFormals -> CmmActuals -> CmmAGraph
mkFinalCall  :: CmmExpr -> CCallConv -> CmmActuals -> CmmAGraph
		 -- Never returns; like exit() or barf()
mkJump       :: CmmExpr -> CmmActuals -> CmmAGraph
mkCbranch    :: CmmExpr -> BlockId -> BlockId -> CmmAGraph
mkSwitch     :: CmmExpr -> [Maybe BlockId] -> CmmAGraph
mkReturn     :: CmmActuals -> CmmAGraph
mkComment    :: FastString -> CmmAGraph

-- Not to be forgotten, but exported by MkZipCfg:
--mkBranch      :: BlockId -> CmmAGraph
--mkLabel       :: BlockId -> CmmAGraph
mkCmmIfThenElse :: CmmExpr -> CmmAGraph -> CmmAGraph -> CmmAGraph
mkCmmWhileDo    :: CmmExpr -> CmmAGraph -> CmmAGraph 

--------------------------------------------------------------------------

mkCmmIfThenElse e = mkIfThenElse (mkCbranch e)
mkCmmWhileDo    e = mkWhileDo    (mkCbranch e)


-- ================ IMPLEMENTATION ================--

mkNop                     = mkMiddle $ MidNop
mkComment fs              = mkMiddle $ MidComment fs
mkAssign l r              = mkMiddle $ MidAssign l r
mkStore  l r              = mkMiddle $ MidStore  l r

mkCbranch pred ifso ifnot = mkLast   $ LastCondBranch pred ifso ifnot
mkSwitch e tbl            = mkLast   $ LastSwitch e tbl

mkUnsafeCall tgt results actuals = mkMiddle $ MidUnsafeCall tgt results actuals

cmmArgConv, cmmResConv :: Convention
cmmArgConv = ConventionStandard CmmCallConv Arguments
cmmResConv = ConventionStandard CmmCallConv Arguments

mkJump e actuals = mkMiddle (CopyOut cmmArgConv actuals) <*> mkLast (LastJump e)
mkReturn actuals = mkMiddle (CopyOut cmmResConv actuals) <*> mkLast LastReturn

mkFinalCall  f conv actuals =
    mkMiddle (CopyOut (ConventionStandard conv Arguments) actuals) <*>
    mkLast   (LastCall f Nothing)

mkCall f conv results actuals srt = 
    withFreshLabel "call successor" $ \k ->
      mkMiddle (CopyOut (ConventionStandard conv Arguments) actuals) <*>
      mkLast (LastCall f (Just k)) <*>
      mkLabel k <*>
      mkMiddle (CopyIn (ConventionStandard conv Results) results srt)
