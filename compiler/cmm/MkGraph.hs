{-# LANGUAGE GADTs #-}

module MkGraph
  ( CmmAGraph, CgStmt(..)
  , (<*>), catAGraphs
  , mkLabel, mkMiddle, mkLast
  , lgraphOfAGraph, labelAGraph

  , stackStubExpr
  , mkNop, mkAssign, mkStore, mkUnsafeCall, mkFinalCall, lastWithArgs
  , mkJump, mkDirectJump, mkForeignJump, mkJumpGC, mkCbranch, mkSwitch
  , mkReturn, mkReturnSimple, mkComment, mkCallEntry, mkBranch
  , copyInOflow, copyInSlot, copyOutOflow, copyOutSlot
  , toCall, Transfer(..)
  )
where

import BlockId
import Cmm
import CmmCallConv (assignArgumentsPos, ParamLocation(..))


import Compiler.Hoopl hiding (Unique, (<*>), mkFirst, mkMiddle, mkLast, mkLabel, mkBranch, Shape(..))
import FastString
import ForeignCall
import Outputable
import Prelude hiding (succ)
import SMRep (ByteOff)
import UniqSupply
import OrdList

#include "HsVersions.h"


-----------------------------------------------------------------------------
-- Building Graphs


-- | CmmAGraph is a chunk of code consisting of:
--
--   * ordinary statements (assignments, stores etc.)
--   * jumps
--   * labels
--   * out-of-line labelled blocks
--
-- The semantics is that control falls through labels and out-of-line
-- blocks.  Everything after a jump up to the next label is by
-- definition unreachable code, and will be discarded.
--
-- Two CmmAGraphs can be stuck together with <*>, with the meaning that
-- control flows from the first to the second.
--
-- A 'CmmAGraph' can be turned into a 'CmmGraph' (closed at both ends)
-- by providing a label for the entry point; see 'labelAGraph'.
--
type CmmAGraph = OrdList CgStmt

data CgStmt
  = CgLabel BlockId
  | CgStmt  (CmmNode O O)
  | CgLast  (CmmNode O C)
  | CgFork  BlockId CmmAGraph

flattenCmmAGraph :: BlockId -> CmmAGraph -> CmmGraph
flattenCmmAGraph id stmts =
    CmmGraph { g_entry = id,
               g_graph = GMany NothingO body NothingO }
  where
  (block, blocks) = flatten (fromOL stmts)
  entry = blockJoinHead (CmmEntry id) block
  body = foldr addBlock emptyBody (entry:blocks)

  flatten :: [CgStmt] -> (Block CmmNode O C, [Block CmmNode C C])
  flatten [] = panic "flatten []"

  -- A label at the end of a function or fork: this label must not be reachable,
  -- but it might be referred to from another BB that also isn't reachable.
  -- Eliminating these has to be done with a dead-code analysis.  For now,
  -- we just make it into a well-formed block by adding a recursive jump.
  flatten [CgLabel id]
    = (goto_id, [blockJoinHead (CmmEntry id) goto_id] )
    where goto_id = blockJoinTail emptyBlock (CmmBranch id)

  -- A jump/branch: throw away all the code up to the next label, because
  -- it is unreachable.  Be careful to keep forks that we find on the way.
  flatten (CgLast stmt : stmts)
    = case dropWhile isOrdinaryStmt stmts of
        [] ->
            ( sing, [] )
        [CgLabel id] ->
            ( sing, [blockJoin (CmmEntry id) emptyBlock (CmmBranch id)] )
        (CgLabel id : stmts) ->
            ( sing, blockJoinHead (CmmEntry id) block : blocks )
            where (block,blocks) = flatten stmts
        (CgFork fork_id stmts : ss) -> 
            flatten (CgFork fork_id stmts : CgLast stmt : ss)
        _ -> panic "MkGraph.flatten"
    where
      sing = blockJoinTail emptyBlock stmt

  flatten (s:ss) = 
        case s of
          CgStmt stmt -> (blockCons stmt block, blocks)
          CgLabel id  -> (blockJoinTail emptyBlock (CmmBranch id),
                          blockJoinHead (CmmEntry id) block : blocks)
          CgFork fork_id stmts -> 
                (block, blockJoinHead (CmmEntry fork_id) fork_block : fork_blocks ++ blocks)
                where (fork_block, fork_blocks) = flatten (fromOL stmts)
          _ -> panic "MkGraph.flatten"
    where (block,blocks) = flatten ss

isOrdinaryStmt :: CgStmt -> Bool
isOrdinaryStmt (CgStmt _) = True
isOrdinaryStmt (CgLast _) = True
isOrdinaryStmt _          = False



---------- AGraph manipulation

(<*>)          :: CmmAGraph -> CmmAGraph -> CmmAGraph
(<*>)           = appOL

catAGraphs     :: [CmmAGraph] -> CmmAGraph
catAGraphs      = concatOL

-- | created a sequence "goto id; id:" as an AGraph
mkLabel        :: BlockId -> CmmAGraph
mkLabel bid     = unitOL (CgLabel bid)

-- | creates an open AGraph from a given node
mkMiddle        :: CmmNode O O -> CmmAGraph
mkMiddle middle = unitOL (CgStmt middle)

-- | created a closed AGraph from a given node
mkLast         :: CmmNode O C -> CmmAGraph
mkLast last     = unitOL (CgLast last)


-- | allocate a fresh label for the entry point
lgraphOfAGraph :: CmmAGraph -> UniqSM CmmGraph
lgraphOfAGraph g = do u <- getUniqueM
                      return (flattenCmmAGraph (mkBlockId u) g)

-- | use the given BlockId as the label of the entry point
labelAGraph    :: BlockId -> CmmAGraph -> UniqSM CmmGraph
labelAGraph lbl ag = return (flattenCmmAGraph lbl ag)

---------- No-ops
mkNop        :: CmmAGraph
mkNop         = nilOL

mkComment    :: FastString -> CmmAGraph
#ifdef DEBUG
-- SDM: generating all those comments takes time, this saved about 4% for me
mkComment fs  = mkMiddle $ CmmComment fs
#else
mkComment _   = nilOL
#endif

---------- Assignment and store
mkAssign     :: CmmReg  -> CmmExpr -> CmmAGraph
mkAssign l r  = mkMiddle $ CmmAssign l r

mkStore      :: CmmExpr -> CmmExpr -> CmmAGraph
mkStore  l r  = mkMiddle $ CmmStore  l r

---------- Control transfer
mkJump          :: CmmExpr -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkJump e actuals updfr_off =
  lastWithArgs Jump old NativeNodeCall actuals updfr_off $
    toCall e Nothing updfr_off 0

mkDirectJump    :: CmmExpr -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkDirectJump e actuals updfr_off =
  lastWithArgs Jump old NativeDirectCall actuals updfr_off $
    toCall e Nothing updfr_off 0

mkJumpGC        :: CmmExpr -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkJumpGC e actuals updfr_off =
  lastWithArgs Jump old GC actuals updfr_off $
    toCall e Nothing updfr_off 0

mkForeignJump   :: Convention -> CmmExpr -> [CmmActual] -> UpdFrameOffset
                -> CmmAGraph
mkForeignJump conv e actuals updfr_off =
  lastWithArgs Jump old conv actuals updfr_off $
    toCall e Nothing updfr_off 0

mkCbranch       :: CmmExpr -> BlockId -> BlockId -> CmmAGraph
mkCbranch pred ifso ifnot = mkLast (CmmCondBranch pred ifso ifnot)

mkSwitch        :: CmmExpr -> [Maybe BlockId] -> CmmAGraph
mkSwitch e tbl   = mkLast $ CmmSwitch e tbl

mkReturn        :: CmmExpr -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkReturn e actuals updfr_off =
  lastWithArgs Ret  old NativeReturn actuals updfr_off $
    toCall e Nothing updfr_off 0
    -- where e = CmmLoad (CmmStackSlot (CallArea Old) updfr_off) gcWord

mkReturnSimple  :: [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkReturnSimple actuals updfr_off =
  lastWithArgs Ret  old NativeReturn actuals updfr_off $
    toCall e Nothing updfr_off 0
    where e = CmmLoad (CmmStackSlot (CallArea Old) updfr_off) gcWord

mkBranch        :: BlockId -> CmmAGraph
mkBranch bid     = mkLast (CmmBranch bid)

mkFinalCall   :: CmmExpr -> CCallConv -> [CmmActual] -> UpdFrameOffset
              -> CmmAGraph
mkFinalCall f _ actuals updfr_off =
  lastWithArgs Call old NativeDirectCall actuals updfr_off $
    toCall f Nothing updfr_off 0

mkUnsafeCall  :: ForeignTarget -> [CmmFormal] -> [CmmActual] -> CmmAGraph
mkUnsafeCall t fs as = mkMiddle $ CmmUnsafeForeignCall t fs as


--------------------------------------------------------------------------




-- Why are we inserting extra blocks that simply branch to the successors?
-- Because in addition to the branch instruction, @mkBranch@ will insert
-- a necessary adjustment to the stack pointer.


-- For debugging purposes, we can stub out dead stack slots:
stackStubExpr :: Width -> CmmExpr
stackStubExpr w = CmmLit (CmmInt 0 w)

-- When we copy in parameters, we usually want to put overflow
-- parameters on the stack, but sometimes we want to pass
-- the variables in their spill slots.
-- Therefore, for copying arguments and results, we provide different
-- functions to pass the arguments in an overflow area and to pass them in spill slots.
copyInOflow  :: Convention -> Area -> [CmmFormal] -> (Int, CmmAGraph)
copyInSlot   :: Convention -> [CmmFormal] -> [CmmNode O O]
copyOutSlot  :: Convention -> [LocalReg] -> [CmmNode O O]

copyInOflow conv area formals = (offset, catAGraphs $ map mkMiddle nodes)
  where (offset, nodes) = copyIn oneCopyOflowI conv area formals
copyInSlot c f = snd $ copyIn oneCopySlotI c (panic "no area for copying to slots") f

type SlotCopier = Area -> (LocalReg, ByteOff) -> (ByteOff, [CmmNode O O]) ->
                          (ByteOff, [CmmNode O O])
type CopyIn  = SlotCopier -> Convention -> Area -> [CmmFormal] -> (ByteOff, [CmmNode O O])

-- Return the number of bytes used for copying arguments, as well as the
-- instructions to copy the arguments.
copyIn :: CopyIn
copyIn oflow conv area formals =
  foldr ci (init_offset, []) args'
  where ci (reg, RegisterParam r) (n, ms) =
          (n, CmmAssign (CmmLocal reg) (CmmReg $ CmmGlobal r) : ms)
        ci (r, StackParam off) (n, ms) = oflow area (r, off) (n, ms)
        init_offset = widthInBytes wordWidth -- infotable
        args  = assignArgumentsPos conv localRegType formals
        args' = foldl adjust [] args
          where adjust rst (v, StackParam off) = (v, StackParam (off + init_offset)) : rst
                adjust rst x@(_, RegisterParam _) = x : rst

-- Copy-in one arg, using overflow space if needed.
oneCopyOflowI, oneCopySlotI :: SlotCopier
oneCopyOflowI area (reg, off) (n, ms) =
  (max n off, CmmAssign (CmmLocal reg) (CmmLoad (CmmStackSlot area off) ty) : ms)
  where ty = localRegType reg

-- Copy-in one arg, using spill slots if needed -- used for calling conventions at
-- a procpoint that is not a return point. The offset is irrelevant here...
oneCopySlotI _ (reg, _) (n, ms) =
  (n, CmmAssign (CmmLocal reg) (CmmLoad (CmmStackSlot (RegSlot reg) w) ty) : ms)
  where ty = localRegType reg
        w  = widthInBytes (typeWidth ty)


-- Factoring out the common parts of the copyout functions yielded something
-- more complicated:

data Transfer = Call | Jump | Ret deriving Eq

copyOutOflow :: Convention -> Transfer -> Area -> [CmmActual] -> UpdFrameOffset ->
                              (Int, CmmAGraph)

-- Generate code to move the actual parameters into the locations
-- required by the calling convention.  This includes a store for the
-- return address.
--
-- The argument layout function ignores the pointer to the info table,
-- so we slot that in here. When copying-out to a young area, we set
-- the info table for return and adjust the offsets of the other
-- parameters.  If this is a call instruction, we adjust the offsets
-- of the other parameters.
copyOutOflow conv transfer area@(CallArea a) actuals updfr_off
  = foldr co (init_offset, mkNop) args'
  where 
    co (v, RegisterParam r) (n, ms) = (n, mkAssign (CmmGlobal r) v <*> ms)
    co (v, StackParam off)  (n, ms) = (max n off, mkStore (CmmStackSlot area off) v <*> ms)

    (setRA, init_offset) =
      case a of Young id -> id `seq` -- Generate a store instruction for
           	    	     	     -- the return address if making a call
                  if transfer == Call then
                    ([(CmmLit (CmmBlock id), StackParam init_offset)],
                     widthInBytes wordWidth)
                  else ([], 0)
                Old -> ([], updfr_off)

    args :: [(CmmExpr, ParamLocation)]   -- The argument and where to put it
    args = assignArgumentsPos conv cmmExprType actuals

    args' = foldl adjust setRA args
      where adjust rst   (v, StackParam off)  = (v, StackParam (off + init_offset)) : rst
            adjust rst x@(_, RegisterParam _) = x : rst

copyOutOflow _ _ (RegSlot _) _ _ = panic "cannot copy arguments into a register slot"

-- Args passed only in registers and stack slots; no overflow space.
-- No return address may apply!
copyOutSlot conv actuals = foldr co [] args
  where co (v, RegisterParam r) ms = CmmAssign (CmmGlobal r) (toExp v) : ms
        co (v, StackParam off)  ms = CmmStore  (CmmStackSlot (RegSlot v) off) (toExp v) : ms
        toExp r = CmmReg (CmmLocal r)
        args = assignArgumentsPos conv localRegType actuals

mkCallEntry :: Convention -> [CmmFormal] -> (Int, CmmAGraph)
mkCallEntry conv formals = copyInOflow conv (CallArea Old) formals

lastWithArgs :: Transfer -> Area -> Convention -> [CmmActual] -> UpdFrameOffset ->
                (ByteOff -> CmmAGraph) -> CmmAGraph
lastWithArgs transfer area conv actuals updfr_off last =
  let (outArgs, copies) = copyOutOflow conv transfer area actuals updfr_off in
  copies <*> last outArgs

-- The area created for the jump and return arguments is the same area as the
-- procedure entry.
old :: Area
old = CallArea Old

toCall :: CmmExpr -> Maybe BlockId -> UpdFrameOffset -> ByteOff -> ByteOff
       -> CmmAGraph
toCall e cont updfr_off res_space arg_space =
  mkLast $ CmmCall e cont arg_space res_space updfr_off
