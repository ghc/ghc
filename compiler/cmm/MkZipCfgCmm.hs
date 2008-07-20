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

import BlockId
import CmmExpr
import Cmm ( GenCmm(..), GenCmmTop(..), CmmStatic, CmmInfo
           , CmmCallTarget(..), CmmActuals, CmmFormals, CmmFormalsWithoutKinds
           , CmmKinded (..)
           )
import MachOp (MachHint(..), wordRep)
import ZipCfgCmmRep hiding (CmmGraph, CmmAGraph, CmmBlock, CmmZ, CmmTopZ)
  -- to make this module more self-contained, the above definitions are
  -- duplicated below
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

---------- Context manipulation ("return via")
mkAddToContext :: CmmExpr -> [CmmExpr] -> CmmAGraph

---------- Control transfer
mkJump       	:: Area    -> CmmExpr -> CmmActuals -> CmmAGraph
mkCbranch    	:: CmmExpr -> BlockId -> BlockId    -> CmmAGraph
mkSwitch     	:: CmmExpr -> [Maybe BlockId]       -> CmmAGraph
mkReturn     	:: Area    -> CmmActuals            -> CmmAGraph

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

cmmResConv :: Convention
cmmResConv = ConventionStandard CmmCallConv Results

copyIn :: Convention -> Area -> CmmFormals -> [Middle]
copyIn _ area formals = reverse $ snd $ foldl ci (1, []) formals
  where ci (n, ms) v = (n+1, MidAssign (CmmLocal $ kindlessCmm v)
                                       (CmmLoad (CmmStackSlot area n) wordRep) : ms)

copyOut :: Convention -> Area -> CmmActuals -> [Middle]
copyOut conv area actuals = moveSP conv $ snd $ foldl co (1, []) actuals
  where moveSP (ConventionStandard _ Arguments) args =
           MidAssign spReg (outgoingSlot area) : reverse args
        moveSP _ args = reverse $ MidAssign spReg (outgoingSlot area) : args
        co (n, ms) v = (n+1, MidStore (CmmStackSlot area n) (kindlessCmm v) : ms)
mkEntry :: Area -> Convention -> CmmFormalsWithoutKinds -> [Middle]
mkEntry area conv formals = copyIn conv area fs
  where fs = map (\f -> CmmKinded f NoHint) formals

-- I'm not sure how to get the calling conventions right yet,
-- and I suspect this should not be resolved until sometime after
-- Simon's patch is applied.
-- For now, I apply a bogus calling convention: all arguments go on the
-- stack, using the same amount of stack space.
lastWithArgs' :: BlockId -> Area -> Convention -> CmmActuals -> Maybe CmmFormals ->
                 (BlockId -> Last) -> CmmAGraph
lastWithArgs' k area conv actuals formals toLast =
  (mkMiddles $ copyOut conv area actuals) <*>
  -- adjust the sp
  mkLast (toLast k) <*>
  case formals of
    Just formals -> mkLabel k <*> (mkMiddles $ copyIn conv area formals)
    Nothing      -> emptyAGraph
lastWithArgs :: Convention -> CmmActuals -> Maybe CmmFormals -> (BlockId -> Last) -> CmmAGraph
lastWithArgs c a f l =
  withFreshLabel "call successor" $ \k -> lastWithArgs' k (mkCallArea k a f) c a f l

always :: a -> b -> a
always x _ = x

-- The area created for the jump and return arguments is the same area as the
-- procedure entry.
mkJump   area e actuals =
  lastWithArgs' (areaId area) area cmmResConv actuals Nothing $ always $ LastJump e
mkReturn area   actuals =
  lastWithArgs' (areaId area) area cmmResConv actuals Nothing $ always LastReturn

mkFinalCall f conv actuals =
  lastWithArgs (ConventionStandard conv Arguments) actuals Nothing
      $ always $ LastCall f Nothing --mkFinalCall  f conv actuals =

mkCmmCall f results actuals srt = mkCall f CmmCallConv results actuals srt

-- I'm dropping the SRT, but that should be okay: we plan to reconstruct it later.
mkCall f conv results actuals _ =
  lastWithArgs (ConventionStandard conv Arguments) actuals (Just results)
        $ \k -> LastCall f (Just k)
