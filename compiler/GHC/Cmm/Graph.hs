{-# LANGUAGE GADTs #-}

module GHC.Cmm.Graph
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

import GHC.Prelude hiding ( (<*>) ) -- avoid importing (<*>)

import GHC.Platform.Profile

import GHC.Cmm.BlockId
import GHC.Cmm
import GHC.Cmm.CallConv
import GHC.Cmm.Switch (SwitchTargets)

import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Data.FastString
import GHC.Types.ForeignCall
import GHC.Data.OrdList
import GHC.Runtime.Heap.Layout (ByteOff)
import GHC.Types.Unique.DSM
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Panic


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

flattenCmmAGraph :: BlockId -> CmmAGraphScoped -> DCmmGraph
flattenCmmAGraph id (stmts_t, tscope) =
    CmmGraph { g_entry = id,
               g_graph = GMany NothingO body NothingO }
  where
  body = DWrap [(entryLabel b, b) | b <- flatten id stmts_t tscope [] ]

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
lgraphOfAGraph :: CmmAGraphScoped -> UniqDSM DCmmGraph
lgraphOfAGraph g = do
  u <- getUniqueDSM
  return (labelAGraph (mkBlockId u) g)

-- | use the given BlockId as the label of the entry point
labelAGraph    :: BlockId -> CmmAGraphScoped -> DCmmGraph
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

-- | Assumes natural alignment
mkStore      :: CmmExpr -> CmmExpr -> CmmAGraph
mkStore  l r  = mkMiddle $ CmmStore  l r NaturallyAligned

---------- Control transfer
mkJump          :: Profile -> Convention -> CmmExpr
                -> [CmmExpr]
                -> UpdFrameOffset
                -> CmmAGraph
mkJump profile conv e actuals updfr_off =
  lastWithArgs profile Jump Old conv actuals updfr_off $
    toCall e Nothing updfr_off 0

-- | A jump where the caller says what the live GlobalRegs are.  Used
-- for low-level hand-written Cmm.
mkRawJump       :: Profile -> CmmExpr -> UpdFrameOffset -> [GlobalReg]
                -> CmmAGraph
mkRawJump profile e updfr_off vols =
  lastWithArgs profile Jump Old NativeNodeCall [] updfr_off $
    \arg_space _  -> toCall e Nothing updfr_off 0 arg_space vols


mkJumpExtra :: Profile -> Convention -> CmmExpr -> [CmmExpr]
                -> UpdFrameOffset -> [CmmExpr]
                -> CmmAGraph
mkJumpExtra profile conv e actuals updfr_off extra_stack =
  lastWithArgsAndExtraStack profile Jump Old conv actuals updfr_off extra_stack $
    toCall e Nothing updfr_off 0

mkCbranch       :: CmmExpr -> BlockId -> BlockId -> Maybe Bool -> CmmAGraph
mkCbranch pred ifso ifnot likely =
  mkLast (CmmCondBranch pred ifso ifnot likely)

mkSwitch        :: CmmExpr -> SwitchTargets -> CmmAGraph
mkSwitch e tbl   = mkLast $ CmmSwitch e tbl

mkReturn        :: Profile -> CmmExpr -> [CmmExpr] -> UpdFrameOffset
                -> CmmAGraph
mkReturn profile e actuals updfr_off =
  lastWithArgs profile Ret  Old NativeReturn actuals updfr_off $
    toCall e Nothing updfr_off 0

mkBranch        :: BlockId -> CmmAGraph
mkBranch bid     = mkLast (CmmBranch bid)

mkFinalCall   :: Profile
              -> CmmExpr -> CCallConv -> [CmmExpr] -> UpdFrameOffset
              -> CmmAGraph
mkFinalCall profile f _ actuals updfr_off =
  lastWithArgs profile Call Old NativeDirectCall actuals updfr_off $
    toCall f Nothing updfr_off 0

mkCallReturnsTo :: Profile -> CmmExpr -> Convention -> [CmmExpr]
                -> BlockId
                -> ByteOff
                -> UpdFrameOffset
                -> [CmmExpr]
                -> CmmAGraph
mkCallReturnsTo profile f callConv actuals ret_lbl ret_off updfr_off extra_stack =
  lastWithArgsAndExtraStack profile Call (Young ret_lbl) callConv actuals
    updfr_off extra_stack $
      toCall f (Just ret_lbl) updfr_off ret_off

-- Like mkCallReturnsTo, but does not push the return address (it is assumed to be
-- already on the stack).
mkJumpReturnsTo :: Profile -> CmmExpr -> Convention -> [CmmExpr]
                -> BlockId
                -> ByteOff
                -> UpdFrameOffset
                -> CmmAGraph
mkJumpReturnsTo profile f callConv actuals ret_lbl ret_off updfr_off =
  lastWithArgs profile JumpRet (Young ret_lbl) callConv actuals updfr_off $
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
copyInOflow  :: Profile -> Convention -> Area
             -> [CmmFormal]
             -> [CmmFormal]
             -> (Int, [GlobalReg], CmmAGraph)

copyInOflow profile conv area formals extra_stk
  = (offset, gregs, catAGraphs $ map mkMiddle nodes)
  where (offset, gregs, nodes) = copyIn profile conv area formals extra_stk

-- Return the number of bytes used for copying arguments, as well as the
-- instructions to copy the arguments.
copyIn :: Profile -> Convention -> Area
       -> [CmmFormal]
       -> [CmmFormal]
       -> (ByteOff, [GlobalReg], [CmmNode O O])
copyIn profile conv area formals extra_stk
  = (stk_size, [r | (_, RegisterParam r) <- args], map ci (stk_args ++ args))
  where
    platform = profilePlatform profile

    ci :: (LocalReg, ParamLocation) -> CmmNode O O
    ci (reg, RegisterParam r@(VanillaReg {})) =
        let local = CmmLocal reg
            width = cmmRegWidth local
            (expr, ty)
              -- See Note [Width of parameters]
                | width == wordWidth platform
                = (global, localRegType reg)
                | width < wordWidth platform
                = (CmmMachOp (MO_XX_Conv (wordWidth platform) width) [global]
                  ,setCmmTypeWidth (wordWidth platform) (localRegType reg))
                | otherwise
                = panic "Parameter width greater than word width"
            global = CmmReg (CmmGlobal $ GlobalRegUse r ty)

        in CmmAssign local expr

    -- Non VanillaRegs
    ci (reg, RegisterParam r) =
        CmmAssign (CmmLocal reg) (CmmReg (CmmGlobal $ GlobalRegUse r (localRegType reg)))

    ci (reg, StackParam off)
      | isBitsType $ localRegType reg
      -- See Note [Width of parameters]
      , typeWidth (localRegType reg) < wordWidth platform =
        let
          stack_slot = CmmLoad (CmmStackSlot area off) (cmmBits $ wordWidth platform) NaturallyAligned
          local = CmmLocal reg
          width = cmmRegWidth local
          expr  = CmmMachOp (MO_XX_Conv (wordWidth platform) width) [stack_slot]
        in CmmAssign local expr

      | otherwise =
         CmmAssign (CmmLocal reg) (CmmLoad (CmmStackSlot area off) ty NaturallyAligned)
         where ty = localRegType reg

    init_offset = widthInBytes (wordWidth platform) -- infotable

    (stk_off, stk_args) = assignStack platform init_offset localRegType extra_stk

    (stk_size, args) = assignArgumentsPos profile stk_off conv
                                          localRegType formals

-- Factoring out the common parts of the copyout functions yielded something
-- more complicated:

data Transfer = Call | JumpRet | Jump | Ret deriving Eq

copyOutOflow :: Profile -> Convention -> Transfer -> Area -> [CmmExpr]
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
copyOutOflow profile conv transfer area actuals updfr_off extra_stack_stuff
  = (stk_size, regs, graph)
  where
    platform = profilePlatform profile
    (regs, graph) = foldr co ([], mkNop) (setRA ++ args ++ stack_params)

    co :: (CmmExpr, ParamLocation)
       -> ([GlobalReg], CmmAGraph)
       -> ([GlobalReg], CmmAGraph)
    co (v, RegisterParam r@(VanillaReg {})) (rs, ms) =
        let width = cmmExprWidth platform v
            value
              -- See Note [Width of parameters]
                | width == wordWidth platform = v
                | width < wordWidth platform =
                    CmmMachOp (MO_XX_Conv width (wordWidth platform)) [v]
                | otherwise = panic "Parameter width greater than word width"

        in (r:rs, mkAssign (CmmGlobal $ GlobalRegUse r (cmmExprType platform value)) value <*> ms)

    -- Non VanillaRegs
    co (v, RegisterParam r) (rs, ms) =
        (r:rs, mkAssign (CmmGlobal $ GlobalRegUse r (cmmExprType platform v)) v <*> ms)

    co (v, StackParam off)  (rs, ms)
      = (rs, mkStore (CmmStackSlot area off) (value v) <*> ms)

    -- See Note [Width of parameters]
    width v = cmmExprWidth platform v
    value v
      | isBitsType $ cmmExprType platform v
      , width v < wordWidth platform =
        CmmMachOp (MO_XX_Conv (width v) (wordWidth platform)) [v]
      | otherwise = v

    (setRA, init_offset) =
      case area of
            Young id ->  -- Generate a store instruction for
                         -- the return address if making a call
                  case transfer of
                     Call ->
                       ([(CmmLit (CmmBlock id), StackParam init_offset)],
                       widthInBytes (wordWidth platform))
                     JumpRet ->
                       ([],
                       widthInBytes (wordWidth platform))
                     _other ->
                       ([], 0)
            Old -> ([], updfr_off)

    (extra_stack_off, stack_params) =
       assignStack platform init_offset (cmmExprType platform) extra_stack_stuff

    args :: [(CmmExpr, ParamLocation)]   -- The argument and where to put it
    (stk_size, args) = assignArgumentsPos profile extra_stack_off conv
                                          (cmmExprType platform) actuals


-- Note [Width of parameters]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Consider passing a small (< word width) primitive like Int8# to a function.
-- It's actually non-trivial to do this without extending/narrowing:
-- * Lowering gets harder, since on x86-32 not every register exposes its lower
--   8 bits (e.g., for %eax we can use %al, but there isn't a corresponding
--   8-bit register for %edi). So we would either need to extend/narrow anyway,
--   or complicate the calling convention.
-- * Passing a small integer in a stack slot, which has native word width,
--   requires extending to word width when writing to the stack and narrowing
--   when reading off the stack (see #16258).
--   This is because the generated Cmm application functions (such as stg_ap_n)
--   always load the full width from the stack.
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


mkCallEntry :: Profile -> Convention -> [CmmFormal] -> [CmmFormal]
            -> (Int, [GlobalReg], CmmAGraph)
mkCallEntry profile conv formals extra_stk
  = copyInOflow profile conv Old formals extra_stk

lastWithArgs :: Profile -> Transfer -> Area -> Convention -> [CmmExpr]
             -> UpdFrameOffset
             -> (ByteOff -> [GlobalReg] -> CmmAGraph)
             -> CmmAGraph
lastWithArgs profile transfer area conv actuals updfr_off last =
  lastWithArgsAndExtraStack profile transfer area conv actuals
                            updfr_off noExtraStack last

lastWithArgsAndExtraStack :: Profile
             -> Transfer -> Area -> Convention -> [CmmExpr]
             -> UpdFrameOffset -> [CmmExpr]
             -> (ByteOff -> [GlobalReg] -> CmmAGraph)
             -> CmmAGraph
lastWithArgsAndExtraStack profile transfer area conv actuals updfr_off
                          extra_stack last =
  copies <*> last outArgs regs
 where
  (outArgs, regs, copies) = copyOutOflow profile conv transfer area actuals
                               updfr_off extra_stack


noExtraStack :: [CmmExpr]
noExtraStack = []

toCall :: CmmExpr -> Maybe BlockId -> UpdFrameOffset -> ByteOff
       -> ByteOff -> [GlobalReg]
       -> CmmAGraph
toCall e cont updfr_off res_space arg_space regs =
  mkLast $ CmmCall e cont regs arg_space res_space updfr_off
