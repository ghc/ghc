{-# LANGUAGE GADTs #-}

module MkGraph
  ( CmmAGraph, CgStmt(..)
  , (<*>), catAGraphs
  , mkLabel, mkMiddle, mkLast, outOfLine
  , lgraphOfAGraph, labelAGraph

  , stackStubExpr
  , mkNop, mkAssign, mkStore, mkUnsafeCall, mkFinalCall, mkCallReturnsTo
  , mkJumpReturnsTo
  , mkJump, mkDirectJump, mkForeignJump, mkForeignJumpExtra, mkJumpGC
  , mkCbranch, mkSwitch
  , mkReturn, mkReturnSimple, mkComment, mkCallEntry, mkBranch
  , copyInOflow, copyOutOflow
  , noExtraStack
  , toCall, Transfer(..)
  )
where

import BlockId
import Cmm
import CmmCallConv (assignArgumentsPos, ParamLocation(..))


import Compiler.Hoopl hiding (Unique, (<*>), mkFirst, mkMiddle, mkLast, mkLabel, mkBranch, Shape(..))
import DynFlags
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

-- | A labelled code block; should end in a last node
outOfLine      :: BlockId -> CmmAGraph -> CmmAGraph
outOfLine l g   = unitOL (CgFork l g)

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
mkAssign l (CmmReg r) | l == r  = mkNop
mkAssign l r  = mkMiddle $ CmmAssign l r

mkStore      :: CmmExpr -> CmmExpr -> CmmAGraph
mkStore  l r  = mkMiddle $ CmmStore  l r

---------- Control transfer
mkJump          :: DynFlags -> CmmExpr -> [CmmActual] -> UpdFrameOffset
                -> CmmAGraph
mkJump dflags e actuals updfr_off =
  lastWithArgs dflags Jump Old NativeNodeCall actuals updfr_off $
    toCall e Nothing updfr_off 0

mkDirectJump    :: DynFlags -> CmmExpr -> [CmmActual] -> UpdFrameOffset
                -> CmmAGraph
mkDirectJump dflags e actuals updfr_off =
  lastWithArgs dflags Jump Old NativeDirectCall actuals updfr_off $
    toCall e Nothing updfr_off 0

mkJumpGC        :: DynFlags -> CmmExpr -> [CmmActual] -> UpdFrameOffset
                -> CmmAGraph
mkJumpGC dflags e actuals updfr_off =
  lastWithArgs dflags Jump Old GC actuals updfr_off $
    toCall e Nothing updfr_off 0

mkForeignJump   :: DynFlags
                -> Convention -> CmmExpr -> [CmmActual] -> UpdFrameOffset
                -> CmmAGraph
mkForeignJump dflags conv e actuals updfr_off =
  mkForeignJumpExtra dflags conv e actuals updfr_off noExtraStack

mkForeignJumpExtra :: DynFlags -> Convention -> CmmExpr -> [CmmActual]
                -> UpdFrameOffset -> (ByteOff, [(CmmExpr, ByteOff)])
                -> CmmAGraph
mkForeignJumpExtra dflags conv e actuals updfr_off extra_stack =
  lastWithArgsAndExtraStack dflags Jump Old conv actuals updfr_off extra_stack $
    toCall e Nothing updfr_off 0

mkCbranch       :: CmmExpr -> BlockId -> BlockId -> CmmAGraph
mkCbranch pred ifso ifnot = mkLast (CmmCondBranch pred ifso ifnot)

mkSwitch        :: CmmExpr -> [Maybe BlockId] -> CmmAGraph
mkSwitch e tbl   = mkLast $ CmmSwitch e tbl

mkReturn        :: DynFlags -> CmmExpr -> [CmmActual] -> UpdFrameOffset
                -> CmmAGraph
mkReturn dflags e actuals updfr_off =
  lastWithArgs dflags Ret  Old NativeReturn actuals updfr_off $
    toCall e Nothing updfr_off 0

mkReturnSimple  :: DynFlags -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkReturnSimple dflags actuals updfr_off =
  mkReturn dflags e actuals updfr_off
  where e = CmmLoad (CmmStackSlot Old updfr_off) gcWord

mkBranch        :: BlockId -> CmmAGraph
mkBranch bid     = mkLast (CmmBranch bid)

mkFinalCall   :: DynFlags
              -> CmmExpr -> CCallConv -> [CmmActual] -> UpdFrameOffset
              -> CmmAGraph
mkFinalCall dflags f _ actuals updfr_off =
  lastWithArgs dflags Call Old NativeDirectCall actuals updfr_off $
    toCall f Nothing updfr_off 0

mkCallReturnsTo :: DynFlags -> CmmExpr -> Convention -> [CmmActual]
                -> BlockId
                -> ByteOff
                -> UpdFrameOffset
                -> (ByteOff, [(CmmExpr,ByteOff)])
                -> CmmAGraph
mkCallReturnsTo dflags f callConv actuals ret_lbl ret_off updfr_off extra_stack = do
  lastWithArgsAndExtraStack dflags Call (Young ret_lbl) callConv actuals
     updfr_off extra_stack $
       toCall f (Just ret_lbl) updfr_off ret_off

-- Like mkCallReturnsTo, but does not push the return address (it is assumed to be
-- already on the stack).
mkJumpReturnsTo :: DynFlags -> CmmExpr -> Convention -> [CmmActual]
                -> BlockId
                -> ByteOff
                -> UpdFrameOffset
                -> CmmAGraph
mkJumpReturnsTo dflags f callConv actuals ret_lbl ret_off updfr_off  = do
  lastWithArgs dflags JumpRet (Young ret_lbl) callConv actuals updfr_off $
       toCall f (Just ret_lbl) updfr_off ret_off

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
copyInOflow  :: DynFlags -> Convention -> Area -> [CmmFormal]
             -> (Int, CmmAGraph)

copyInOflow dflags conv area formals = (offset, catAGraphs $ map mkMiddle nodes)
  where (offset, nodes) = copyIn dflags oneCopyOflowI conv area formals

type SlotCopier = Area -> (LocalReg, ByteOff) -> (ByteOff, [CmmNode O O]) ->
                          (ByteOff, [CmmNode O O])
type CopyIn  = DynFlags -> SlotCopier -> Convention -> Area -> [CmmFormal] -> (ByteOff, [CmmNode O O])

-- Return the number of bytes used for copying arguments, as well as the
-- instructions to copy the arguments.
copyIn :: CopyIn
copyIn dflags oflow conv area formals =
  foldr ci (init_offset, []) args'
  where ci (reg, RegisterParam r) (n, ms) =
          (n, CmmAssign (CmmLocal reg) (CmmReg $ CmmGlobal r) : ms)
        ci (r, StackParam off) (n, ms) = oflow area (r, off) (n, ms)
        init_offset = widthInBytes wordWidth -- infotable
        args  = assignArgumentsPos dflags conv localRegType formals
        args' = foldl adjust [] args
          where adjust rst (v, StackParam off) = (v, StackParam (off + init_offset)) : rst
                adjust rst x@(_, RegisterParam _) = x : rst

-- Copy-in one arg, using overflow space if needed.
oneCopyOflowI :: SlotCopier
oneCopyOflowI area (reg, off) (n, ms) =
  (max n off, CmmAssign (CmmLocal reg) (CmmLoad (CmmStackSlot area off) ty) : ms)
  where ty = localRegType reg

-- Factoring out the common parts of the copyout functions yielded something
-- more complicated:

data Transfer = Call | JumpRet | Jump | Ret deriving Eq

copyOutOflow :: DynFlags -> Convention -> Transfer -> Area -> [CmmActual]
             -> UpdFrameOffset
             -> (ByteOff, [(CmmExpr,ByteOff)]) -- extra stack stuff
             -> (Int, [GlobalReg], CmmAGraph)

-- Generate code to move the actual parameters into the locations
-- required by the calling convention.  This includes a store for the
-- return address.
--
-- The argument layout function ignores the pointer to the info table,
-- so we slot that in here. When copying-out to a young area, we set
-- the info table for return and adjust the offsets of the other
-- parameters.  If this is a call instruction, we adjust the offsets
-- of the other parameters.
copyOutOflow dflags conv transfer area actuals updfr_off
  (extra_stack_off, extra_stack_stuff)
  = foldr co (init_offset, [], mkNop) (args' ++ stack_params)
  where 
    co (v, RegisterParam r) (n, rs, ms)
       = (n, r:rs, mkAssign (CmmGlobal r) v <*> ms)
    co (v, StackParam off)  (n, rs, ms)
       = (max n off, rs, mkStore (CmmStackSlot area off) v <*> ms)

    stack_params = [ (e, StackParam (off + init_offset))
                   | (e,off) <- extra_stack_stuff ]

    (setRA, init_offset) =
      case area of
            Young id -> id `seq` -- Generate a store instruction for
                                 -- the return address if making a call
                  case transfer of
                     Call ->
                       ([(CmmLit (CmmBlock id), StackParam init_offset)],
                       widthInBytes wordWidth)
                     JumpRet ->
                       ([],
                       widthInBytes wordWidth)
                     _other ->
                       ([], 0)
            Old -> ([], updfr_off)

    arg_offset = init_offset + extra_stack_off

    args :: [(CmmExpr, ParamLocation)]   -- The argument and where to put it
    args = assignArgumentsPos dflags conv cmmExprType actuals

    args' = foldl adjust setRA args
      where adjust rst   (v, StackParam off)  = (v, StackParam (off + arg_offset)) : rst
            adjust rst x@(_, RegisterParam _) = x : rst



mkCallEntry :: DynFlags -> Convention -> [CmmFormal] -> (Int, CmmAGraph)
mkCallEntry dflags conv formals = copyInOflow dflags conv Old formals

lastWithArgs :: DynFlags -> Transfer -> Area -> Convention -> [CmmActual]
             -> UpdFrameOffset
             -> (ByteOff -> [GlobalReg] -> CmmAGraph)
             -> CmmAGraph
lastWithArgs dflags transfer area conv actuals updfr_off last =
  lastWithArgsAndExtraStack dflags transfer area conv actuals
                            updfr_off noExtraStack last

lastWithArgsAndExtraStack :: DynFlags
             -> Transfer -> Area -> Convention -> [CmmActual]
             -> UpdFrameOffset -> (ByteOff, [(CmmExpr,ByteOff)])
             -> (ByteOff -> [GlobalReg] -> CmmAGraph)
             -> CmmAGraph
lastWithArgsAndExtraStack dflags transfer area conv actuals updfr_off
                          extra_stack last =
  copies <*> last outArgs regs
 where
  (outArgs, regs, copies) = copyOutOflow dflags conv transfer area actuals
                               updfr_off extra_stack


noExtraStack :: (ByteOff, [(CmmExpr,ByteOff)])
noExtraStack = (0,[])

toCall :: CmmExpr -> Maybe BlockId -> UpdFrameOffset -> ByteOff
       -> ByteOff -> [GlobalReg]
       -> CmmAGraph
toCall e cont updfr_off res_space arg_space regs =
  mkLast $ CmmCall e cont regs arg_space res_space updfr_off
