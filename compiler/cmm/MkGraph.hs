{-# LANGUAGE BangPatterns, CPP, GADTs #-}

module MkGraph
  ( CmmAGraph, CgStmt(..)
  , (<*>), catAGraphs
  , mkLabel, mkMiddle, mkLast, outOfLine
  , lgraphOfAGraph, labelAGraph

  , stackStubExpr
  , mkNop, mkAssign, mkStore, mkUnsafeCall, mkFinalCall, mkCallReturnsTo
  , mkJumpReturnsTo
  , mkJump, mkJumpExtra
  , mkRawJump
  , mkCbranch, mkSwitch
  , mkReturn, mkComment, mkCallEntry, mkBranch
  , copyInOflow, copyOutOflow
  , noExtraStack
  , toCall, Transfer(..)
  )
where

import BlockId
import Cmm
import CmmCallConv

import Compiler.Hoopl hiding (Unique, (<*>), mkFirst, mkMiddle, mkLast, mkLabel, mkBranch, Shape(..))
import DynFlags
import FastString
import ForeignCall
import SMRep (ByteOff)
import UniqSupply
import OrdList

import Control.Monad
import Data.List
import Data.Maybe
import Prelude (($),Int,Eq(..)) -- avoid importing (<*>)

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
  body = foldr addBlock emptyBody $ flatten id stmts []

  --
  -- flatten: given an entry label and a CmmAGraph, make a list of blocks.
  --
  -- NB. avoid the quadratic-append trap by passing in the tail of the
  -- list.  This is important for Very Long Functions (e.g. in T783).
  --
  flatten :: Label -> CmmAGraph -> [Block CmmNode C C] -> [Block CmmNode C C]
  flatten id g blocks
      = flatten1 (fromOL g) (blockJoinHead (CmmEntry id) emptyBlock) blocks

  --
  -- flatten0: we are outside a block at this point: any code before
  -- the first label is unreachable, so just drop it.
  --
  flatten0 :: [CgStmt] -> [Block CmmNode C C] -> [Block CmmNode C C]
  flatten0 [] blocks = blocks

  flatten0 (CgLabel id : stmts) blocks
    = flatten1 stmts block blocks
    where !block = blockJoinHead (CmmEntry id) emptyBlock

  flatten0 (CgFork fork_id stmts : rest) blocks
    = flatten fork_id stmts $ flatten0 rest blocks

  flatten0 (CgLast _ : stmts) blocks = flatten0 stmts blocks
  flatten0 (CgStmt _ : stmts) blocks = flatten0 stmts blocks

  --
  -- flatten1: we have a partial block, collect statements until the
  -- next last node to make a block, then call flatten0 to get the rest
  -- of the blocks
  --
  flatten1 :: [CgStmt] -> Block CmmNode C O
           -> [Block CmmNode C C] -> [Block CmmNode C C]

  -- The current block falls through to the end of a function or fork:
  -- this code should not be reachable, but it may be referenced by
  -- other code that is not reachable.  We'll remove it later with
  -- dead-code analysis, but for now we have to keep the graph
  -- well-formed, so we terminate the block with a branch to the
  -- beginning of the current block.
  flatten1 [] block blocks
    = blockJoinTail block (CmmBranch (entryLabel block)) : blocks

  flatten1 (CgLast stmt : stmts) block blocks
    = block' : flatten0 stmts blocks
    where !block' = blockJoinTail block stmt

  flatten1 (CgStmt stmt : stmts) block blocks
    = flatten1 stmts block' blocks
    where !block' = blockSnoc block stmt

  flatten1 (CgFork fork_id stmts : rest) block blocks
    = flatten fork_id stmts $ flatten1 rest block blocks

  -- a label here means that we should start a new block, and the
  -- current block should fall through to the new block.
  flatten1 (CgLabel id : stmts) block blocks
    = blockJoinTail block (CmmBranch id) :
      flatten1 stmts (blockJoinHead (CmmEntry id) emptyBlock) blocks



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
                      return (labelAGraph (mkBlockId u) g)

-- | use the given BlockId as the label of the entry point
labelAGraph    :: BlockId -> CmmAGraph -> CmmGraph
labelAGraph lbl ag = flattenCmmAGraph lbl ag

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
mkJump          :: DynFlags -> Convention -> CmmExpr
                -> [CmmActual]
                -> UpdFrameOffset
                -> CmmAGraph
mkJump dflags conv e actuals updfr_off =
  lastWithArgs dflags Jump Old conv actuals updfr_off $
    toCall e Nothing updfr_off 0

-- | A jump where the caller says what the live GlobalRegs are.  Used
-- for low-level hand-written Cmm.
mkRawJump       :: DynFlags -> CmmExpr -> UpdFrameOffset -> [GlobalReg]
                -> CmmAGraph
mkRawJump dflags e updfr_off vols =
  lastWithArgs dflags Jump Old NativeNodeCall [] updfr_off $
    \arg_space _  -> toCall e Nothing updfr_off 0 arg_space vols


mkJumpExtra :: DynFlags -> Convention -> CmmExpr -> [CmmActual]
                -> UpdFrameOffset -> [CmmActual]
                -> CmmAGraph
mkJumpExtra dflags conv e actuals updfr_off extra_stack =
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
                -> [CmmActual]
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
-- parameters on the stack, but sometimes we want to pass the
-- variables in their spill slots.  Therefore, for copying arguments
-- and results, we provide different functions to pass the arguments
-- in an overflow area and to pass them in spill slots.
copyInOflow  :: DynFlags -> Convention -> Area
             -> [CmmFormal]
             -> [CmmFormal]
             -> (Int, [GlobalReg], CmmAGraph)

copyInOflow dflags conv area formals extra_stk
  = (offset, gregs, catAGraphs $ map mkMiddle nodes)
  where (offset, gregs, nodes) = copyIn dflags conv area formals extra_stk

-- Return the number of bytes used for copying arguments, as well as the
-- instructions to copy the arguments.
copyIn :: DynFlags -> Convention -> Area
       -> [CmmFormal]
       -> [CmmFormal]
       -> (ByteOff, [GlobalReg], [CmmNode O O])
copyIn dflags conv area formals extra_stk
  = (stk_size, [r | (_, RegisterParam r) <- args], map ci (stk_args ++ args))
  where
     ci (reg, RegisterParam r) =
          CmmAssign (CmmLocal reg) (CmmReg (CmmGlobal r))
     ci (reg, StackParam off) =
          CmmAssign (CmmLocal reg) (CmmLoad (CmmStackSlot area off) ty)
          where ty = localRegType reg

     init_offset = widthInBytes (wordWidth dflags) -- infotable

     (stk_off, stk_args) = assignStack dflags init_offset localRegType extra_stk

     (stk_size, args) = assignArgumentsPos dflags stk_off conv
                                           localRegType formals

-- Factoring out the common parts of the copyout functions yielded something
-- more complicated:

data Transfer = Call | JumpRet | Jump | Ret deriving Eq

copyOutOflow :: DynFlags -> Convention -> Transfer -> Area -> [CmmActual]
             -> UpdFrameOffset
             -> [CmmActual] -- extra stack args
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
copyOutOflow dflags conv transfer area actuals updfr_off extra_stack_stuff
  = (stk_size, regs, graph)
  where
    (regs, graph) = foldr co ([], mkNop) (setRA ++ args ++ stack_params)

    co (v, RegisterParam r) (rs, ms)
       = (r:rs, mkAssign (CmmGlobal r) v <*> ms)
    co (v, StackParam off)  (rs, ms)
       = (rs, mkStore (CmmStackSlot area off) v <*> ms)

    (setRA, init_offset) =
      case area of
            Young id ->  -- Generate a store instruction for
                         -- the return address if making a call
                  case transfer of
                     Call ->
                       ([(CmmLit (CmmBlock id), StackParam init_offset)],
                       widthInBytes (wordWidth dflags))
                     JumpRet ->
                       ([],
                       widthInBytes (wordWidth dflags))
                     _other ->
                       ([], 0)
            Old -> ([], updfr_off)

    (extra_stack_off, stack_params) =
       assignStack dflags init_offset (cmmExprType dflags) extra_stack_stuff

    args :: [(CmmExpr, ParamLocation)]   -- The argument and where to put it
    (stk_size, args) = assignArgumentsPos dflags extra_stack_off conv
                                          (cmmExprType dflags) actuals



mkCallEntry :: DynFlags -> Convention -> [CmmFormal] -> [CmmFormal]
            -> (Int, [GlobalReg], CmmAGraph)
mkCallEntry dflags conv formals extra_stk
  = copyInOflow dflags conv Old formals extra_stk

lastWithArgs :: DynFlags -> Transfer -> Area -> Convention -> [CmmActual]
             -> UpdFrameOffset
             -> (ByteOff -> [GlobalReg] -> CmmAGraph)
             -> CmmAGraph
lastWithArgs dflags transfer area conv actuals updfr_off last =
  lastWithArgsAndExtraStack dflags transfer area conv actuals
                            updfr_off noExtraStack last

lastWithArgsAndExtraStack :: DynFlags
             -> Transfer -> Area -> Convention -> [CmmActual]
             -> UpdFrameOffset -> [CmmActual]
             -> (ByteOff -> [GlobalReg] -> CmmAGraph)
             -> CmmAGraph
lastWithArgsAndExtraStack dflags transfer area conv actuals updfr_off
                          extra_stack last =
  copies <*> last outArgs regs
 where
  (outArgs, regs, copies) = copyOutOflow dflags conv transfer area actuals
                               updfr_off extra_stack


noExtraStack :: [CmmActual]
noExtraStack = []

toCall :: CmmExpr -> Maybe BlockId -> UpdFrameOffset -> ByteOff
       -> ByteOff -> [GlobalReg]
       -> CmmAGraph
toCall e cont updfr_off res_space arg_space regs =
  mkLast $ CmmCall e cont regs arg_space res_space updfr_off
