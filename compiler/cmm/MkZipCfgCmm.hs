{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- This is the module to import to be able to build C-- programs.
-- It should not be necessary to import MkZipCfg or ZipCfgCmmRep.
-- If you find it necessary to import these other modules, please
-- complain to Norman Ramsey.

module MkZipCfgCmm
  ( mkNop, mkAssign, mkStore, mkCall, mkCmmCall, mkSafeCall, mkUnsafeCall, mkFinalCall
         , mkJump, mkForeignJump, mkJumpGC, mkCbranch, mkSwitch, mkReturn
         , mkReturnSimple, mkComment, copyInOflow, copyInSlot, copyOutOflow, copyOutSlot
         , mkEntry, mkCmmIfThenElse, mkCmmIfThen, mkCmmWhileDo
  , (<*>), catAGraphs, mkLabel, mkBranch
  , emptyAGraph, withFreshLabel, withUnique, outOfLine
  , lgraphOfAGraph, graphOfAGraph, labelAGraph
  , CmmZ, CmmTopZ, CmmGraph, CmmBlock, CmmAGraph, CmmStackInfo
  , Middle, Last, Convention(..), ForeignConvention(..), MidCallTarget(..), Transfer(..)
  , stackStubExpr, pprAGraph
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
import SMRep (ByteOff) 
import StaticFlags 
import ZipCfg 

type CmmGraph  = LGraph Middle Last
type CmmAGraph = AGraph Middle Last
type CmmBlock  = Block  Middle Last
type CmmStackInfo            = (ByteOff, Maybe ByteOff)
  -- probably want a record; (SP offset on entry, update frame space)
type CmmZ                    = GenCmm    CmmStatic CmmInfo (CmmStackInfo, CmmGraph)
type CmmTopZ                 = GenCmmTop CmmStatic CmmInfo (CmmStackInfo, CmmGraph)

data Transfer = Call | Jump | Ret deriving Eq

---------- No-ops
mkNop        :: CmmAGraph
mkComment    :: FastString -> CmmAGraph

---------- Assignment and store
mkAssign     :: CmmReg  -> CmmExpr -> CmmAGraph
mkStore      :: CmmExpr -> CmmExpr -> CmmAGraph

---------- Calls
mkCall       :: CmmExpr -> (Convention, Convention) -> CmmFormals -> CmmActuals ->
                  UpdFrameOffset -> CmmAGraph
mkCmmCall    :: CmmExpr ->              CmmFormals -> CmmActuals ->
                  UpdFrameOffset -> CmmAGraph
  -- Native C-- calling convention
mkSafeCall    :: MidCallTarget -> CmmFormals -> CmmActuals -> UpdFrameOffset -> Bool -> CmmAGraph
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
    mkLabel tid   <*> tbranch <*> mkBranch endif <*>
    mkLabel endif



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

mkSafeCall   t fs as upd interruptible =
  withFreshLabel "safe call" $ \k ->
    mkMiddle $ MidForeignCall (Safe k upd interruptible) t fs as
mkUnsafeCall t fs as = mkMiddle $ MidForeignCall Unsafe t fs as

-- For debugging purposes, we can stub out dead stack slots:
stackStubExpr :: Width -> CmmExpr
stackStubExpr w = CmmLit (CmmInt 0 w)

-- When we copy in parameters, we usually want to put overflow
-- parameters on the stack, but sometimes we want to pass
-- the variables in their spill slots.
-- Therefore, for copying arguments and results, we provide different
-- functions to pass the arguments in an overflow area and to pass them in spill slots.
copyInOflow  :: Convention -> Area -> CmmFormals -> (Int, CmmAGraph)
copyInSlot   :: Convention -> CmmFormals -> CmmAGraph
copyOutOflow :: Convention -> Transfer -> Area -> CmmActuals -> UpdFrameOffset ->
                              (Int, [Middle])
copyOutSlot  :: Convention -> [LocalReg] -> [Middle]
  -- why a list of middles here instead of an AGraph?

copyInOflow      = copyIn oneCopyOflowI
copyInSlot c f = snd $ copyIn oneCopySlotI c (panic "no area for copying to slots") f

type SlotCopier = Area -> (LocalReg, ByteOff) -> (ByteOff, CmmAGraph) ->
                          (ByteOff, CmmAGraph)
type CopyIn  = SlotCopier -> Convention -> Area -> CmmFormals -> (ByteOff, CmmAGraph)

-- Return the number of bytes used for copying arguments, as well as the
-- instructions to copy the arguments.
copyIn :: CopyIn
copyIn oflow conv area formals =
  foldr ci (init_offset, mkNop) args'
  where ci (reg, RegisterParam r) (n, ms) =
          (n, mkAssign (CmmLocal reg) (CmmReg $ CmmGlobal r) <*> ms)
        ci (r, StackParam off) (n, ms) = oflow area (r, off) (n, ms)
        init_offset = widthInBytes wordWidth -- infotable
        args  = assignArgumentsPos conv localRegType formals
        args' = foldl adjust [] args
          where adjust rst (v, StackParam off) = (v, StackParam (off + init_offset)) : rst
                adjust rst x@(_, RegisterParam _) = x : rst

-- Copy-in one arg, using overflow space if needed.
oneCopyOflowI, oneCopySlotI :: SlotCopier
oneCopyOflowI area (reg, off) (n, ms) =
  (max n off, mkAssign (CmmLocal reg) (CmmLoad (CmmStackSlot area off) ty) <*> ms)
  where ty = localRegType reg

-- Copy-in one arg, using spill slots if needed -- used for calling conventions at
-- a procpoint that is not a return point. The offset is irrelevant here...
oneCopySlotI _ (reg, _) (n, ms) =
  (n, mkAssign (CmmLocal reg) (CmmLoad (CmmStackSlot (RegSlot reg) w) ty) <*> ms)
  where ty = localRegType reg
        w  = widthInBytes (typeWidth ty)


-- Factoring out the common parts of the copyout functions yielded something
-- more complicated:

-- The argument layout function ignores the pointer to the info table, so we slot that
-- in here. When copying-out to a young area, we set the info table for return
-- and adjust the offsets of the other parameters.
-- If this is a call instruction, we adjust the offsets of the other parameters.
copyOutOflow conv transfer area@(CallArea a) actuals updfr_off =
  foldr co (init_offset, []) args'
  where co (v, RegisterParam r) (n, ms) = (n, MidAssign (CmmGlobal r) v : ms)
        co (v, StackParam off)  (n, ms) = 
          (max n off, MidStore (CmmStackSlot area off) v : ms)
        (setRA, init_offset) =
          case a of Young id@(BlockId _) -> -- set RA if making a call
                      if transfer == Call then
                        ([(CmmLit (CmmBlock id), StackParam init_offset)],
                         widthInBytes wordWidth)
                      else ([], 0)
                    Old -> ([], updfr_off)
        args = assignArgumentsPos conv cmmExprType actuals
        args' = foldl adjust setRA args
          where adjust rst (v, StackParam off) = (v, StackParam (off + init_offset)) : rst
                adjust rst x@(_, RegisterParam _) = x : rst
copyOutOflow _ _ (RegSlot _) _ _ = panic "cannot copy arguments into a register slot"

-- Args passed only in registers and stack slots; no overflow space.
-- No return address may apply!
copyOutSlot conv actuals = foldr co [] args
  where co (v, RegisterParam r) ms = MidAssign (CmmGlobal r) (toExp v) : ms
        co (v, StackParam off)  ms =
          MidStore (CmmStackSlot (RegSlot v) off) (toExp v) : ms
        toExp r = CmmReg (CmmLocal r)
        args = assignArgumentsPos conv localRegType actuals

-- oneCopySlotO _ (reg, _) (n, ms) =
--   (n, MidStore (CmmStackSlot (RegSlot reg) w) reg : ms)
--   where w = widthInBytes (typeWidth (localRegType reg))

mkEntry :: BlockId -> Convention -> CmmFormals -> (Int, CmmAGraph)
mkEntry _ conv formals = copyInOflow conv (CallArea Old) formals

lastWithArgs :: Transfer -> Area -> Convention -> CmmActuals -> UpdFrameOffset ->
                (ByteOff -> Last) -> CmmAGraph
lastWithArgs transfer area conv actuals updfr_off last =
  let (outArgs, copies) = copyOutOflow conv transfer area actuals updfr_off in
  mkMiddles copies <*> mkLast (last outArgs)

-- The area created for the jump and return arguments is the same area as the
-- procedure entry.
old :: Area
old = CallArea Old
toCall :: CmmExpr -> Maybe BlockId -> UpdFrameOffset -> ByteOff -> ByteOff -> Last
toCall e cont updfr_off res_space arg_space =
  LastCall e cont arg_space res_space (Just updfr_off)
mkJump e actuals updfr_off =
  lastWithArgs Jump old NativeNodeCall actuals updfr_off $ toCall e Nothing updfr_off 0
mkJumpGC e actuals updfr_off =
  lastWithArgs Jump old GC actuals updfr_off $ toCall e Nothing updfr_off 0
mkForeignJump conv e actuals updfr_off =
  lastWithArgs Jump old conv actuals updfr_off $ toCall e Nothing updfr_off 0
mkReturn e actuals updfr_off =
  lastWithArgs Ret  old NativeReturn actuals updfr_off $ toCall e Nothing updfr_off 0
    -- where e = CmmLoad (CmmStackSlot (CallArea Old) updfr_off) gcWord
mkReturnSimple actuals updfr_off =
  lastWithArgs Ret  old NativeReturn actuals updfr_off $ toCall e Nothing updfr_off 0
    where e = CmmLoad (CmmStackSlot (CallArea Old) updfr_off) gcWord

mkFinalCall f _ actuals updfr_off =
  lastWithArgs Call old NativeDirectCall actuals updfr_off $ toCall f Nothing updfr_off 0

mkCmmCall f results actuals = mkCall f (NativeDirectCall, NativeReturn) results actuals

-- I'm dropping the SRT, but that should be okay: we plan to reconstruct it later.
mkCall f (callConv, retConv) results actuals updfr_off =
  withFreshLabel "call successor" $ \k ->
    let area = CallArea $ Young k
        (off, copyin) = copyInOflow retConv area results
        copyout = lastWithArgs Call area callConv actuals updfr_off 
                               (toCall f (Just k) updfr_off off)
    in (copyout <*> mkLabel k <*> copyin)
