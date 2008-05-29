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
  , CmmZ, CmmTopZ, CmmGraph, CmmBlock, CmmAGraph, Middle, Last, Convention(..)
  )
where

#include "HsVersions.h"

import CmmExpr
import Cmm ( GenCmm(..), GenCmmTop(..), CmmStatic, CmmInfo
           , CmmCallTarget(..), CmmActuals, CmmFormals, CmmFormalsWithoutKinds
           , CmmKinded (..)
           )
import MachOp (MachHint(..))
import ZipCfgCmmRep hiding (CmmGraph, CmmAGraph, CmmBlock, CmmZ, CmmTopZ)
  -- ^ to make this module more self-contained, these definitions are duplicated below
import PprCmm()
import StackSlot

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
mkUnsafeCall :: CmmCallTarget -> CmmFormals -> CmmActuals -> CmmAGraph
mkFinalCall  :: CmmExpr -> CCallConv -> CmmActuals -> CmmAGraph
		 -- Never returns; like exit() or barf()

---------- Context manipulation ('return via')
mkAddToContext :: CmmExpr -> [CmmExpr] -> CmmAGraph

---------- Control transfer
mkJump       	:: CmmExpr -> CmmActuals -> CmmAGraph
mkCbranch    	:: CmmExpr -> BlockId -> BlockId -> CmmAGraph
mkSwitch     	:: CmmExpr -> [Maybe BlockId] -> CmmAGraph
mkReturn     	:: CmmActuals -> CmmAGraph

mkCmmIfThenElse :: CmmExpr -> CmmAGraph -> CmmAGraph -> CmmAGraph
mkCmmIfThen     :: CmmExpr -> CmmAGraph -> CmmAGraph
mkCmmWhileDo    :: CmmExpr -> CmmAGraph -> CmmAGraph

-- Not to be forgotten, but exported by MkZipCfg:
-- mkBranch   	  :: BlockId -> CmmAGraph
-- mkLabel    	  :: BlockId -> CmmAGraph
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
    mkLabel tid <*> tbranch <*> mkBranch endif <*>
    mkLabel endif



-- ================ IMPLEMENTATION ================--

mkNop                     = emptyAGraph
mkComment fs              = mkMiddle $ MidComment fs
mkAssign l r              = mkMiddle $ MidAssign l r
mkStore  l r              = mkMiddle $ MidStore  l r

mkCbranch pred ifso ifnot = mkLast   $ LastCondBranch pred ifso ifnot
mkSwitch e tbl            = mkLast   $ LastSwitch e tbl

mkUnsafeCall tgt results actuals = mkMiddle $ MidUnsafeCall tgt results actuals
mkAddToContext ra actuals        = mkMiddle $ MidAddToContext ra actuals

--cmmArgConv :: Convention
cmmResConv :: Convention
--cmmArgConv = ConventionStandard CmmCallConv Arguments
cmmResConv = ConventionStandard CmmCallConv Arguments

copyIn :: Convention -> StackArea -> CmmFormals -> [Middle]
copyIn _ area formals = reverse $ snd $ foldl ci (1, []) formals
  where ci (n, ms) v = (n+1, MidAssign (CmmLocal $ kindlessCmm v)
                                       (CmmReg $ CmmStack $ StackSlot area n) : ms)

copyOut :: Convention -> StackArea -> CmmActuals -> [Middle]
copyOut _ area actuals = moveSP : reverse (snd $ foldl co (1, []) actuals)
  where moveSP = MidAssign spReg $ CmmReg $ CmmStack $ outgoingSlot area
        co (n, ms) v = (n+1, MidAssign (CmmStack $ StackSlot area n) 
                                       (kindlessCmm v) : ms)
mkEntry :: BlockId -> Convention -> CmmFormalsWithoutKinds -> [Middle]
mkEntry entryId conv formals = copyIn conv (mkStackArea entryId [] $ Just fs) fs
  where fs = map (\f -> CmmKinded f NoHint) formals

-- I'm not sure how to get the calling conventions right yet,
-- and I suspect this should not be resolved until sometime after
-- Simon's patch is applied.
-- For now, I apply a bogus calling convention: all arguments go on the
-- stack, using the same amount of stack space.
lastWithArgs :: Convention -> CmmActuals -> Maybe CmmFormals -> (BlockId -> Last) ->
                CmmAGraph
lastWithArgs conv actuals formals toLast =
  withFreshLabel "call successor" $ \k ->
    let area = mkStackArea k actuals formals
    in (mkMiddles $ copyOut conv area actuals) <*>
       -- adjust the sp
       mkLast (toLast k) <*>
       case formals of
         Just formals -> mkLabel k <*> (mkMiddles $ copyIn conv area formals)
         Nothing      -> emptyAGraph
always :: a -> b -> a
always x _ = x

mkJump e actuals = lastWithArgs cmmResConv actuals Nothing $ always $ LastJump e
mkReturn actuals = lastWithArgs cmmResConv actuals Nothing $ always LastReturn
--mkJump e actuals = mkMiddle (CopyOut cmmArgConv actuals) <*> mkLast (LastJump e)
--mkReturn actuals = mkMiddle (CopyOut cmmResConv actuals) <*> mkLast LastReturn

mkFinalCall f conv actuals =
  lastWithArgs (ConventionStandard conv Arguments) actuals Nothing
      $ always $ LastCall f Nothing --mkFinalCall  f conv actuals =
--    mkMiddle (CopyOut (ConventionStandard conv Arguments) actuals) <*>
--    mkLast   (LastCall f Nothing)
--

mkCmmCall f results actuals srt = mkCall f CmmCallConv results actuals srt

-- I'm dropping the SRT, but that should be okay: we plan to reconstruct it later.
mkCall f conv results actuals _ =
  lastWithArgs (ConventionStandard conv Arguments) actuals (Just results)
        $ \k -> LastCall f (Just k)
--mkCall f conv results actuals srt = 
--    withFreshLabel "call successor" $ \k ->
--      mkMiddle (CopyOut (ConventionStandard conv Arguments) actuals) <*>
--      mkLast (LastCall f (Just k)) <*>
--      mkLabel k <*>
--      mkMiddle (CopyIn (ConventionStandard conv Results) results srt)
