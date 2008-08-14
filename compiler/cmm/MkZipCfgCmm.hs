{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- This is the module to import to be able to build C-- programs.
-- It should not be necessary to import MkZipCfg or ZipCfgCmmRep.
-- If you find it necessary to import these other modules, please
-- complain to Norman Ramsey.

module MkZipCfgCmm
  ( mkNop, mkAssign, mkStore, mkCall, mkCmmCall, mkUnsafeCall, mkFinalCall
         , mkJump, mkCbranch, mkSwitch, mkReturn, mkComment, copyIn, copyOut, mkEntry 
	 , mkCmmIfThenElse, mkCmmIfThen, mkCmmWhileDo
	 , mkAddToContext
  , (<*>), catAGraphs, mkLabel, mkBranch
  , emptyAGraph, withFreshLabel, withUnique, outOfLine
  , lgraphOfAGraph, graphOfAGraph, labelAGraph
  , CmmZ, CmmTopZ, CmmGraph, CmmBlock, CmmAGraph
  , Middle, Last, Convention(..), ForeignConvention(..), MidCallTarget(..), Transfer(..)
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

import ClosureInfo
import FastString
import ForeignCall
import MkZipCfg
import Panic 
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
mkCall       :: CmmExpr -> CCallConv -> CmmFormals -> CmmActuals -> C_SRT -> CmmAGraph
mkCmmCall    :: CmmExpr -> CmmFormals -> CmmActuals -> C_SRT -> CmmAGraph
			-- Native C-- calling convention
mkUnsafeCall :: MidCallTarget -> CmmFormals -> CmmActuals -> CmmAGraph
mkFinalCall  :: CmmExpr -> CCallConv -> CmmActuals -> CmmAGraph
		 -- Never returns; like exit() or barf()

---------- Context manipulation ("return via")
mkAddToContext :: CmmExpr -> [CmmExpr] -> CmmAGraph

---------- Control transfer
mkJump       	:: CmmExpr -> CmmActuals -> CmmAGraph
mkCbranch    	:: CmmExpr -> BlockId -> BlockId          -> CmmAGraph
mkSwitch     	:: CmmExpr -> [Maybe BlockId]             -> CmmAGraph
mkReturn     	:: CmmActuals -> CmmAGraph

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
    mkLabel tid Nothing <*> tbranch <*> mkBranch endif <*>
    mkLabel endif Nothing



-- ================ IMPLEMENTATION ================--

mkNop                     = emptyAGraph
mkComment fs              = mkMiddle $ MidComment fs
mkAssign l r              = mkMiddle $ MidAssign l r
mkStore  l r              = mkMiddle $ MidStore  l r


-- Why are we inserting extra blocks that simply branch to the successors?
-- Because in addition to the branch instruction, @mkBranch@ will insert
-- a necessary adjustment to the stack pointer.
mkCbranch pred ifso ifnot = mkLast (LastCondBranch pred ifso ifnot)
mkSwitch e tbl            = mkLast   $ LastSwitch e tbl

mkUnsafeCall tgt results actuals = mkMiddle $ MidUnsafeCall tgt results actuals
mkAddToContext ra actuals        = mkMiddle $ MidAddToContext ra actuals

cmmResConv :: Convention
cmmResConv = Native

-- Return the number of bytes used for copying arguments, as well as the
-- instructions to copy the arguments.
copyIn :: Convention -> Bool -> Area -> CmmFormals -> (Int, [Middle])
copyIn _ isCall area formals =
  foldr ci (init_offset, []) $ assignArgumentsPos isCall localRegType formals
  where ci (reg, RegisterParam r) (n, ms) =
          (n, MidAssign (CmmLocal reg) (CmmReg $ CmmGlobal r) : ms)
        ci (reg, StackParam off) (n, ms) =
          let ty = localRegType reg
              off' = off + init_offset
          in (max n off',
              MidAssign (CmmLocal reg) (CmmLoad (CmmStackSlot area off') ty) : ms)
        init_offset = widthInBytes wordWidth

-- The argument layout function ignores the pointer to the info table, so we slot that
-- in here. When copying-out to a young area, we set the info table for return
-- and adjust the offsets of the other parameters.
-- If this is a call instruction, we adjust the offsets of the other parameters.
copyOut :: Convention -> Transfer -> Area -> CmmActuals -> (Int, [Middle])
copyOut _ transfer area@(CallArea a) actuals =
  foldr co (init_offset, []) args'
  where args = assignArgumentsPos skip_node cmmExprType actuals
        skip_node = transfer /= Ret
        (setRA, init_offset) =
          case a of Young id -> -- set RA if making a call
                      if transfer == Call then
                        ([(CmmLit (CmmLabel (infoTblLbl id)),
                           StackParam init_offset)], ra_width)
                      else ([], 0)
                    Old -> ([], ra_width)
        ra_width = widthInBytes wordWidth
        args' = foldl adjust setRA args
          where adjust rst (v, StackParam off) = (v, StackParam (off + init_offset)) : rst
                adjust rst x@(_, RegisterParam _) = x : rst
        co (v, RegisterParam r) (n, ms) = (n, MidAssign (CmmGlobal r) v : ms)
        co (v, StackParam off)  (n, ms) =
          (max n off, MidStore (CmmStackSlot area off) v : ms)
copyOut _ _ (RegSlot _) _ = panic "cannot copy arguments into a register slot"

mkEntry :: BlockId -> Convention -> CmmFormals -> (Int, CmmAGraph)
mkEntry _ conv formals =
  let (off, copies) = copyIn conv False (CallArea Old) formals in
  (off, mkMiddles copies)

-- I'm not sure how to get the calling conventions right yet,
-- and I suspect this should not be resolved until sometime after
-- Simon's patch is applied.
-- For now, I apply a bogus calling convention: all arguments go on the
-- stack, using the same amount of stack space.

lastWithArgs :: Transfer -> Area -> Convention -> CmmActuals -> (Int -> Last) -> CmmAGraph
lastWithArgs transfer area conv actuals last =
  let (outArgs, copies) = copyOut conv transfer area actuals in
  mkMiddles copies <*> mkLast (last outArgs)

-- The area created for the jump and return arguments is the same area as the
-- procedure entry.
mkJump e actuals = lastWithArgs Jump (CallArea Old) cmmResConv actuals $ LastJump e
mkReturn actuals = lastWithArgs Ret  (CallArea Old) cmmResConv actuals $ LastJump e
  where e = CmmStackSlot (CallArea Old) (widthInBytes wordWidth)

mkFinalCall f _ actuals =
  lastWithArgs Call (CallArea Old) Native actuals $ LastCall f Nothing

mkCmmCall f results actuals srt = mkCall f CmmCallConv results actuals srt

-- I'm dropping the SRT, but that should be okay: we plan to reconstruct it later.
mkCall f _ results actuals _ =
  withFreshLabel "call successor" $ \k ->
  let area = CallArea $ Young k
      (off, copyin) = copyIn Native False area results
      copyout = lastWithArgs Call area Native actuals $ LastCall f (Just k)
  in copyout <*> mkLabel k (Just off) <*> (mkMiddles copyin)
