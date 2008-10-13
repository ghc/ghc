{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- This is the module to import to be able to build C-- programs.
-- It should not be necessary to import MkZipCfg or ZipCfgCmmRep.
-- If you find it necessary to import these other modules, please
-- complain to Norman Ramsey.

module MkZipCfgCmm
  ( mkNop, mkAssign, mkStore, mkCall, mkCmmCall, mkSafeCall, mkUnsafeCall, mkFinalCall
         , mkJump, mkForeignJump, mkJumpGC, mkCbranch, mkSwitch, mkReturn
         , mkReturnSimple, mkComment, copyIn, copyOut
         , mkEntry, mkCmmIfThenElse, mkCmmIfThen, mkCmmWhileDo
  , (<*>), catAGraphs, mkLabel, mkBranch
  , emptyAGraph, withFreshLabel, withUnique, outOfLine
  , lgraphOfAGraph, graphOfAGraph, labelAGraph
  , CmmZ, CmmTopZ, CmmGraph, CmmBlock, CmmAGraph
  , Middle, Last, Convention(..), ForeignConvention(..), MidCallTarget(..), Transfer(..)
  , emptyStackInfo, stackStubExpr, pprAGraph
  )
where

#include "HsVersions.h"

import BlockId
import CmmExpr
import Cmm ( GenCmm(..), GenCmmTop(..), CmmStatic, CmmInfo
           , CmmActuals, CmmFormals
           )
import CmmCallConv (assignArgumentsPos, ParamLocation(..))
import ZipCfgCmmRep hiding (CmmGraph, CmmAGraph, CmmBlock, CmmZ, CmmTopZ)
  -- to make this module more self-contained, the above definitions are
  -- duplicated below
import PprCmm()

import FastString
import ForeignCall
import MkZipCfg
import Panic 
import StaticFlags 
import ZipCfg 

type CmmGraph  = LGraph Middle Last
type CmmAGraph = AGraph Middle Last
type CmmBlock  = Block  Middle Last
type CmmZ      = GenCmm    CmmStatic CmmInfo CmmGraph
type CmmTopZ   = GenCmmTop CmmStatic CmmInfo CmmGraph

data Transfer = Call | Jump | Ret deriving Eq

---------- No-ops
mkNop        :: CmmAGraph
mkComment    :: FastString -> CmmAGraph

---------- Assignment and store
mkAssign     :: CmmReg  -> CmmExpr -> CmmAGraph
mkStore      :: CmmExpr -> CmmExpr -> CmmAGraph

---------- Calls
mkCall       :: CmmExpr -> Convention -> CmmFormals -> CmmActuals ->
                  UpdFrameOffset -> CmmAGraph
mkCmmCall    :: CmmExpr ->              CmmFormals -> CmmActuals ->
                  UpdFrameOffset -> CmmAGraph
			-- Native C-- calling convention
mkSafeCall    :: MidCallTarget -> CmmFormals -> CmmActuals -> UpdFrameOffset -> CmmAGraph
mkUnsafeCall  :: MidCallTarget -> CmmFormals -> CmmActuals -> CmmAGraph
mkFinalCall   :: CmmExpr -> CCallConv -> CmmActuals -> UpdFrameOffset -> CmmAGraph
		 -- Never returns; like exit() or barf()

---------- Control transfer
mkJump       	::               CmmExpr -> CmmActuals -> UpdFrameOffset -> CmmAGraph
mkJumpGC       	::               CmmExpr -> CmmActuals -> UpdFrameOffset -> CmmAGraph
mkForeignJump   :: Convention -> CmmExpr -> CmmActuals -> UpdFrameOffset -> CmmAGraph
mkCbranch    	:: CmmExpr -> BlockId -> BlockId          -> CmmAGraph
mkSwitch     	:: CmmExpr -> [Maybe BlockId]             -> CmmAGraph
mkReturn     	:: CmmExpr -> CmmActuals -> UpdFrameOffset -> CmmAGraph
mkReturnSimple  :: CmmActuals -> UpdFrameOffset -> CmmAGraph

mkCmmIfThenElse :: CmmExpr -> CmmAGraph -> CmmAGraph -> CmmAGraph
mkCmmIfThen     :: CmmExpr -> CmmAGraph -> CmmAGraph
mkCmmWhileDo    :: CmmExpr -> CmmAGraph -> CmmAGraph

-- Not to be forgotten, but exported by MkZipCfg:
-- mkBranch   	  :: BlockId -> CmmAGraph
-- mkLabel    	  :: BlockId -> Maybe Int -> CmmAGraph
-- outOfLine  	  :: CmmAGraph -> CmmAGraph
-- withUnique 	  :: (Unique -> CmmAGraph) -> CmmAGraph
-- withFreshLabel :: String -> (BlockId -> CmmAGraph) -> CmmAGraph

--------------------------------------------------------------------------

mkCmmWhileDo    e = mkWhileDo (mkCbranch e)
mkCmmIfThenElse e = mkIfThenElse (mkCbranch e)

mkCmmIfThen e tbranch
  = withFreshLabel "end of if"     $ \endif ->
    withFreshLabel "start of then" $ \tid ->
    mkCbranch e tid endif <*>
    mkLabel tid   emptyStackInfo <*> tbranch <*> mkBranch endif <*>
    mkLabel endif emptyStackInfo



-- ================ IMPLEMENTATION ================--

mkNop                     = emptyAGraph
mkComment fs              = mkMiddle $ MidComment fs
mkStore  l r              = mkMiddle $ MidStore  l r

-- NEED A COMPILER-DEBUGGING FLAG HERE
-- Sanity check: any value assigned to a pointer must be non-zero.
-- If it's 0, cause a crash immediately.
mkAssign l r = if opt_StubDeadValues then assign l r <*> check l else assign l r
  where assign l r = mkMiddle (MidAssign l r)
        check (CmmGlobal _) = mkNop
        check l@(CmmLocal reg) = -- if a ptr arg is NULL, cause a crash!
          if isGcPtrType ty then
            mkCmmIfThen (CmmMachOp (MO_Eq w) [r, stackStubExpr w])
                        (assign l (CmmLoad (CmmLit (CmmInt 0 w)) ty))
          else mkNop
            where ty = localRegType reg
                  w  = typeWidth ty
                  r  = CmmReg l


-- Why are we inserting extra blocks that simply branch to the successors?
-- Because in addition to the branch instruction, @mkBranch@ will insert
-- a necessary adjustment to the stack pointer.
mkCbranch pred ifso ifnot = mkLast (LastCondBranch pred ifso ifnot)
mkSwitch e tbl            = mkLast $ LastSwitch e tbl

mkSafeCall   t fs as upd =
  withFreshLabel "safe call" $ \k ->
    mkMiddle $ MidForeignCall (Safe k upd) t fs as
mkUnsafeCall t fs as = mkMiddle $ MidForeignCall Unsafe t fs as

-- For debugging purposes, we can stub out dead stack slots:
stackStubExpr :: Width -> CmmExpr
stackStubExpr w = CmmLit (CmmInt 0 w)

-- Return the number of bytes used for copying arguments, as well as the
-- instructions to copy the arguments.
copyIn :: Convention -> Bool -> Area -> CmmFormals -> (Int, AGraph Middle Last)
copyIn conv isCall area formals =
  foldr ci (init_offset, mkNop) $ assignArgumentsPos conv isCall localRegType formals
  where ci (reg, RegisterParam r) (n, ms) =
          (n, mkAssign (CmmLocal reg) (CmmReg $ CmmGlobal r) <*> ms)
        ci (reg, StackParam off) (n, ms) =
          let ty = localRegType reg
              off' = off + init_offset
          in (max n off',
              mkAssign (CmmLocal reg) (CmmLoad (CmmStackSlot area off') ty) <*> ms)
        init_offset = widthInBytes wordWidth

-- The argument layout function ignores the pointer to the info table, so we slot that
-- in here. When copying-out to a young area, we set the info table for return
-- and adjust the offsets of the other parameters.
-- If this is a call instruction, we adjust the offsets of the other parameters.
copyOut :: Convention -> Transfer -> Area -> CmmActuals -> UpdFrameOffset -> (Int, [Middle])
copyOut conv transfer area@(CallArea a) actuals updfr_off =
  foldr co (init_offset, []) args'
  where args = assignArgumentsPos conv skip_node cmmExprType actuals
        skip_node = transfer /= Ret
        (setRA, init_offset) =
          case a of Young id@(BlockId _) -> -- set RA if making a call
                      if transfer == Call then
                        ([(CmmLit (CmmBlock id), StackParam init_offset)], ra_width)
                      else ([], 0)
                    Old -> ([], updfr_off)
        ra_width = widthInBytes wordWidth
        args' = foldl adjust setRA args
          where adjust rst (v, StackParam off) = (v, StackParam (off + init_offset)) : rst
                adjust rst x@(_, RegisterParam _) = x : rst
        co (v, RegisterParam r) (n, ms) = (n, MidAssign (CmmGlobal r) v : ms)
        co (v, StackParam off)  (n, ms) =
          (max n off, MidStore (CmmStackSlot area off) v : ms)
copyOut _ _ (RegSlot _) _ _ = panic "cannot copy arguments into a register slot"

mkEntry :: BlockId -> Convention -> CmmFormals -> (Int, CmmAGraph)
mkEntry _ conv formals = copyIn conv False (CallArea Old) formals

lastWithArgs :: Transfer -> Area -> Convention -> CmmActuals -> UpdFrameOffset ->
                (Int -> Last) -> CmmAGraph
lastWithArgs transfer area conv actuals updfr_off last =
  let (outArgs, copies) = copyOut conv transfer area actuals updfr_off in
  mkMiddles copies <*> mkLast (last outArgs)

-- The area created for the jump and return arguments is the same area as the
-- procedure entry.
old :: Area
old = CallArea Old
toCall :: CmmExpr -> Maybe BlockId -> UpdFrameOffset -> Int -> Last
toCall e cont updfr_off arg_space = LastCall e cont arg_space (Just updfr_off)
mkJump e actuals updfr_off =
  lastWithArgs Jump old Native actuals updfr_off $ toCall e Nothing updfr_off
mkJumpGC e actuals updfr_off =
  lastWithArgs Jump old GC actuals updfr_off $ toCall e Nothing updfr_off
mkForeignJump conv e actuals updfr_off =
  lastWithArgs Jump old conv actuals updfr_off $ toCall e Nothing updfr_off
mkReturn e actuals updfr_off =
  lastWithArgs Ret  old Native actuals updfr_off $ toCall e Nothing updfr_off
    -- where e = CmmLoad (CmmStackSlot (CallArea Old) updfr_off) gcWord
mkReturnSimple actuals updfr_off =
  lastWithArgs Ret  old Native actuals updfr_off $ toCall e Nothing updfr_off
    where e = CmmLoad (CmmStackSlot (CallArea Old) updfr_off) gcWord

mkFinalCall f _ actuals updfr_off =
  lastWithArgs Call old Native actuals updfr_off $ toCall f Nothing updfr_off

mkCmmCall f results actuals = mkCall f Native results actuals

-- I'm dropping the SRT, but that should be okay: we plan to reconstruct it later.
mkCall f conv results actuals updfr_off =
  withFreshLabel "call successor" $ \k ->
    let area = CallArea $ Young k
        (off, copyin) = copyIn conv False area results
        copyout = lastWithArgs Call area conv actuals updfr_off 
                               (toCall f (Just k) updfr_off)
    in (copyout <*> mkLabel k (StackInfo (Just off) (Just updfr_off))
                <*> copyin)
