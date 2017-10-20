{-# LANGUAGE BangPatterns, GADTs #-}

module MkGraph
  ( CmmAGraph, CmmAGraphScoped, CgStmt(..)
  , (<*>), catAGraphs
  , mkLabel, mkMiddle, mkLast, outOfLine
  , lgraphOfAGraph, labelAGraph

  , stackStubExpr
  , mkNop, mkAssign, mkStore
  , mkUnsafeCall, mkFinalCall, mkCallReturnsTo
  , mkJumpReturnsTo
  , mkJump, mkJumpExtra
  , mkRawJump
  , mkCbranch, mkSwitch
  , mkReturn, mkComment, mkCallEntry, mkBranch
  , mkUnwind
  , copyInOflow, copyOutOflow
  , noExtraStack
  , toCall, Transfer(..)
  )
where

import GhcPrelude hiding ( (<*>) ) -- avoid importing (<*>)

import BlockId
import Cmm
import CmmCallConv
import CmmSwitch (SwitchTargets)

import Hoopl.Block
import Hoopl.Graph
import Hoopl.Label
import DynFlags
import FastString
import ForeignCall
import OrdList
import SMRep (ByteOff)
import UniqSupply
import Util
import Panic


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
-- by providing a label for the entry point and a tick scope; see
-- 'labelAGraph'.
type CmmAGraph = OrdList CgStmt
-- | Unlabeled graph with tick scope
type CmmAGraphScoped = (CmmAGraph, CmmTickScope)

data CgStmt
  = CgLabel BlockId CmmTickScope
  | CgStmt  (CmmNode O O)
  | CgLast  (CmmNode O C)
  | CgFork  BlockId CmmAGraph CmmTickScope

flattenCmmAGraph :: BlockId -> CmmAGraphScoped -> CmmGraph
flattenCmmAGraph id (stmts_t, tscope) =
    CmmGraph { g_entry = id,
               g_graph = GMany NothingO body NothingO }
  where
  body = foldr addBlock emptyBody $ flatten id stmts_t tscope []

  --
  -- flatten: given an entry label and a CmmAGraph, make a list of blocks.
  --
  -- NB. avoid the quadratic-append trap by passing in the tail of the
  -- list.  This is important for Very Long Functions (e.g. in T783).
  --
  flatten :: Label -> CmmAGraph -> CmmTickScope -> [Block CmmNode C C]
          -> [Block CmmNode C C]
  flatten id g tscope blocks
      = flatten1 (fromOL g) block' blocks
      where !block' = blockJoinHead (CmmEntry id tscope) emptyBlock
  --
  -- flatten0: we are outside a block at this point: any code before
  -- the first label is unreachable, so just drop it.
  --
  flatten0 :: [CgStmt] -> [Block CmmNode C C] -> [Block CmmNode C C]
  flatten0 [] blocks = blocks

  flatten0 (CgLabel id tscope : stmts) blocks
    = flatten1 stmts block blocks
    where !block = blockJoinHead (CmmEntry id tscope) emptyBlock

  flatten0 (CgFork fork_id stmts_t tscope : rest) blocks
    = flatten fork_id stmts_t tscope $ flatten0 rest blocks

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

  flatten1 (CgFork fork_id stmts_t tscope : rest) block blocks
    = flatten fork_id stmts_t tscope $ flatten1 rest block blocks

  -- a label here means that we should start a new block, and the
  -- current block should fall through to the new block.
  flatten1 (CgLabel id tscp : stmts) block blocks
    = blockJoinTail block (CmmBranch id) :
      flatten1 stmts (blockJoinHead (CmmEntry id tscp) emptyBlock) blocks



---------- AGraph manipulation

(<*>)          :: CmmAGraph -> CmmAGraph -> CmmAGraph
(<*>)           = appOL

catAGraphs     :: [CmmAGraph] -> CmmAGraph
catAGraphs      = concatOL

-- | creates a sequence "goto id; id:" as an AGraph
mkLabel        :: BlockId -> CmmTickScope -> CmmAGraph
mkLabel bid scp = unitOL (CgLabel bid scp)

-- | creates an open AGraph from a given node
mkMiddle        :: CmmNode O O -> CmmAGraph
mkMiddle middle = unitOL (CgStmt middle)

-- | creates a closed AGraph from a given node
mkLast         :: CmmNode O C -> CmmAGraph
mkLast last     = unitOL (CgLast last)

-- | A labelled code block; should end in a last node
outOfLine      :: BlockId -> CmmAGraphScoped -> CmmAGraph
outOfLine l (c,s) = unitOL (CgFork l c s)

-- | allocate a fresh label for the entry point
lgraphOfAGraph :: CmmAGraphScoped -> UniqSM CmmGraph
lgraphOfAGraph g = do
  u <- getUniqueM
  return (labelAGraph (mkBlockId u) g)

-- | use the given BlockId as the label of the entry point
labelAGraph    :: BlockId -> CmmAGraphScoped -> CmmGraph
labelAGraph lbl ag = flattenCmmAGraph lbl ag

---------- No-ops
mkNop        :: CmmAGraph
mkNop         = nilOL

mkComment    :: FastString -> CmmAGraph
mkComment fs
  -- SDM: generating all those comments takes time, this saved about 4% for me
  | debugIsOn = mkMiddle $ CmmComment fs
  | otherwise = nilOL

---------- Assignment and store
mkAssign     :: CmmReg  -> CmmExpr -> CmmAGraph
mkAssign l (CmmReg r) | l == r  = mkNop
mkAssign l r  = mkMiddle $ CmmAssign l r

mkStore      :: CmmExpr -> CmmExpr -> CmmAGraph
mkStore  l r  = mkMiddle $ CmmStore  l r

---------- Control transfer
mkJump          :: DynFlags -> Convention -> CmmExpr
                -> [CmmExpr]
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


mkJumpExtra :: DynFlags -> Convention -> CmmExpr -> [CmmExpr]
                -> UpdFrameOffset -> [CmmExpr]
                -> CmmAGraph
mkJumpExtra dflags conv e actuals updfr_off extra_stack =
  lastWithArgsAndExtraStack dflags Jump Old conv actuals updfr_off extra_stack $
    toCall e Nothing updfr_off 0

mkCbranch       :: CmmExpr -> BlockId -> BlockId -> Maybe Bool -> CmmAGraph
mkCbranch pred ifso ifnot likely =
  mkLast (CmmCondBranch pred ifso ifnot likely)

mkSwitch        :: CmmExpr -> SwitchTargets -> CmmAGraph
mkSwitch e tbl   = mkLast $ CmmSwitch e tbl

mkReturn        :: DynFlags -> CmmExpr -> [CmmExpr] -> UpdFrameOffset
                -> CmmAGraph
mkReturn dflags e actuals updfr_off =
  lastWithArgs dflags Ret  Old NativeReturn actuals updfr_off $
    toCall e Nothing updfr_off 0

mkBranch        :: BlockId -> CmmAGraph
mkBranch bid     = mkLast (CmmBranch bid)

mkFinalCall   :: DynFlags
              -> CmmExpr -> CCallConv -> [CmmExpr] -> UpdFrameOffset
              -> CmmAGraph
mkFinalCall dflags f _ actuals updfr_off =
  lastWithArgs dflags Call Old NativeDirectCall actuals updfr_off $
    toCall f Nothing updfr_off 0

mkCallReturnsTo :: DynFlags -> CmmExpr -> Convention -> [CmmExpr]
                -> BlockId
                -> ByteOff
                -> UpdFrameOffset
                -> [CmmExpr]
                -> CmmAGraph
mkCallReturnsTo dflags f callConv actuals ret_lbl ret_off updfr_off extra_stack = do
  lastWithArgsAndExtraStack dflags Call (Young ret_lbl) callConv actuals
     updfr_off extra_stack $
       toCall f (Just ret_lbl) updfr_off ret_off

-- Like mkCallReturnsTo, but does not push the return address (it is assumed to be
-- already on the stack).
mkJumpReturnsTo :: DynFlags -> CmmExpr -> Convention -> [CmmExpr]
                -> BlockId
                -> ByteOff
                -> UpdFrameOffset
                -> CmmAGraph
mkJumpReturnsTo dflags f callConv actuals ret_lbl ret_off updfr_off  = do
  lastWithArgs dflags JumpRet (Young ret_lbl) callConv actuals updfr_off $
       toCall f (Just ret_lbl) updfr_off ret_off

mkUnsafeCall  :: ForeignTarget -> [CmmFormal] -> [CmmActual] -> CmmAGraph
mkUnsafeCall t fs as = mkMiddle $ CmmUnsafeForeignCall t fs as

-- | Construct a 'CmmUnwind' node for the given register and unwinding
-- expression.
mkUnwind     :: GlobalReg -> CmmExpr -> CmmAGraph
mkUnwind r e  = mkMiddle $ CmmUnwind [(r, Just e)]

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
    -- See Note [Width of parameters]
    ci (reg, RegisterParam r@(VanillaReg {})) =
        let local = CmmLocal reg
            global = CmmReg (CmmGlobal r)
            width = cmmRegWidth dflags local
            expr
                | width == wordWidth dflags = global
                | width < wordWidth dflags =
                    CmmMachOp (MO_XX_Conv (wordWidth dflags) width) [global]
                | otherwise = panic "Parameter width greater than word width"

        in CmmAssign local expr

    -- Non VanillaRegs
    ci (reg, RegisterParam r) =
        CmmAssign (CmmLocal reg) (CmmReg (CmmGlobal r))

    ci (reg, StackParam off)
      | isBitsType $ localRegType reg
      , typeWidth (localRegType reg) < wordWidth dflags =
        let
          stack_slot = (CmmLoad (CmmStackSlot area off) (cmmBits $ wordWidth dflags))
          local = CmmLocal reg
          width = cmmRegWidth dflags local
          expr  = CmmMachOp (MO_XX_Conv (wordWidth dflags) width) [stack_slot]
        in CmmAssign local expr 
         
      | otherwise =
         CmmAssign (CmmLocal reg) (CmmLoad (CmmStackSlot area off) ty)
         where ty = localRegType reg

    init_offset = widthInBytes (wordWidth dflags) -- infotable

    (stk_off, stk_args) = assignStack dflags init_offset localRegType extra_stk

    (stk_size, args) = assignArgumentsPos dflags stk_off conv
                                          localRegType formals

-- Factoring out the common parts of the copyout functions yielded something
-- more complicated:

data Transfer = Call | JumpRet | Jump | Ret deriving Eq

copyOutOflow :: DynFlags -> Convention -> Transfer -> Area -> [CmmExpr]
             -> UpdFrameOffset
             -> [CmmExpr] -- extra stack args
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

    -- See Note [Width of parameters]
    co (v, RegisterParam r@(VanillaReg {})) (rs, ms) =
        let width = cmmExprWidth dflags v
            value
                | width == wordWidth dflags = v
                | width < wordWidth dflags =
                    CmmMachOp (MO_XX_Conv width (wordWidth dflags)) [v]
                | otherwise = panic "Parameter width greater than word width"

        in (r:rs, mkAssign (CmmGlobal r) value <*> ms)

    -- Non VanillaRegs
    co (v, RegisterParam r) (rs, ms) =
        (r:rs, mkAssign (CmmGlobal r) v <*> ms)

    -- See Note [Width of parameters]
    co (v, StackParam off)  (rs, ms)
      = (rs, mkStore (CmmStackSlot area off) (value v) <*> ms)

    width v = cmmExprWidth dflags v
    value v
      | isBitsType $ cmmExprType dflags v
      , width v < wordWidth dflags =
        CmmMachOp (MO_XX_Conv (width v) (wordWidth dflags)) [v]
      | otherwise = v

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


-- Note [Width of parameters]
--
-- Consider passing a small (< word width) primitive like Int8# to a function.
-- It's actually non-trivial to do this without extending/narrowing:
-- * Global registers are considered to have native word width (i.e., 64-bits on
--   x86-64), so CmmLint would complain if we assigned an 8-bit parameter to a
--   global register.
-- * Same problem exists with LLVM IR.
-- * Lowering gets harder since on x86-32 not every register exposes its lower
--   8 bits (e.g., for %eax we can use %al, but there isn't a corresponding
--   8-bit register for %edi). So we would either need to extend/narrow anyway,
--   or complicate the calling convention.
-- * Passing a small integer in a stack slot, which has native word width,
--   requires extending to word width when writing to the stack and narrowing
--   when reading off the stack (see #16258).
-- So instead, we always extend every parameter smaller than native word width
-- in copyOutOflow and then truncate it back to the expected width in copyIn.
-- Note that we do this in cmm using MO_XX_Conv to avoid requiring
-- zero-/sign-extending - it's up to a backend to handle this in a most
-- efficient way (e.g., a simple register move or a smaller size store).
-- This convention (of ignoring the upper bits) is different from some C ABIs,
-- e.g. all PowerPC ELF ABIs, that require sign or zero extending parameters.
--
-- There was some discussion about this on this PR:
-- https://github.com/ghc-proposals/ghc-proposals/pull/74


mkCallEntry :: DynFlags -> Convention -> [CmmFormal] -> [CmmFormal]
            -> (Int, [GlobalReg], CmmAGraph)
mkCallEntry dflags conv formals extra_stk
  = copyInOflow dflags conv Old formals extra_stk

lastWithArgs :: DynFlags -> Transfer -> Area -> Convention -> [CmmExpr]
             -> UpdFrameOffset
             -> (ByteOff -> [GlobalReg] -> CmmAGraph)
             -> CmmAGraph
lastWithArgs dflags transfer area conv actuals updfr_off last =
  lastWithArgsAndExtraStack dflags transfer area conv actuals
                            updfr_off noExtraStack last

lastWithArgsAndExtraStack :: DynFlags
             -> Transfer -> Area -> Convention -> [CmmExpr]
             -> UpdFrameOffset -> [CmmExpr]
             -> (ByteOff -> [GlobalReg] -> CmmAGraph)
             -> CmmAGraph
lastWithArgsAndExtraStack dflags transfer area conv actuals updfr_off
                          extra_stack last =
  copies <*> last outArgs regs
 where
  (outArgs, regs, copies) = copyOutOflow dflags conv transfer area actuals
                               updfr_off extra_stack


noExtraStack :: [CmmExpr]
noExtraStack = []

toCall :: CmmExpr -> Maybe BlockId -> UpdFrameOffset -> ByteOff
       -> ByteOff -> [GlobalReg]
       -> CmmAGraph
toCall e cont updfr_off res_space arg_space regs =
  mkLast $ CmmCall e cont regs arg_space res_space updfr_off
