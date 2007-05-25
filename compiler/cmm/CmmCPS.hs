module CmmCPS (
  -- | Converts C-- with full proceedures and parameters
  -- to a CPS transformed C-- with the stack made manifest.
  cmmCPS
) where

#include "HsVersions.h"

import Cmm
import CmmLint
import PprCmm

import Dataflow (fixedpoint)
import CmmLive
import CmmBrokenBlock
import CmmProcPoint

import MachOp
import ForeignCall
import CLabel
import SMRep
import Constants

import DynFlags
import ErrUtils
import Maybes
import Outputable
import UniqSupply
import UniqFM
import UniqSet
import Unique

import Monad
import IO
import Data.List

-----------------------------------------------------------------------------
-- |Top level driver for the CPS pass
-----------------------------------------------------------------------------
cmmCPS :: DynFlags -- ^ Dynamic flags: -dcmm-lint -ddump-cps-cmm
       -> [Cmm]    -- ^ Input C-- with Proceedures
       -> IO [Cmm] -- ^ Output CPS transformed C--
cmmCPS dflags abstractC = do
  when (dopt Opt_DoCmmLinting dflags) $
       do showPass dflags "CmmLint"
	  case firstJust $ map cmmLint abstractC of
	    Just err -> do printDump err
			   ghcExit dflags 1
	    Nothing  -> return ()
  showPass dflags "CPS"

  -- TODO: more lint checking
  --        check for use of branches to non-existant blocks
  --        check for use of Sp, SpLim, R1, R2, etc.

  uniqSupply <- mkSplitUniqSupply 'p'
  let supplies = listSplitUniqSupply uniqSupply
  let doCpsProc s (Cmm c) =
          Cmm $ concat $ zipWith cpsProc (listSplitUniqSupply s) c
  let continuationC = zipWith doCpsProc supplies abstractC

  dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "CPS Cmm" (pprCmms continuationC)

  -- TODO: add option to dump Cmm to file

  return continuationC

-----------------------------------------------------------------------------
-- |CPS a single CmmTop (proceedure)
-- Only 'CmmProc' are transformed 'CmmData' will be left alone.
-----------------------------------------------------------------------------

cpsProc :: UniqSupply 
        -> CmmTop     -- ^Input proceedure
        -> [CmmTop]   -- ^Output proceedure and continuations
cpsProc uniqSupply x@(CmmData _ _) = [x]
cpsProc uniqSupply x@(CmmProc info_table ident params blocks) = cps_procs
    where
      uniqes :: [[Unique]]
      uniqes = map uniqsFromSupply $ listSplitUniqSupply uniqSupply

      -- Break the block at each function call.
      -- The part after the function call will have to become a continuation.
      broken_blocks :: [BrokenBlock]
      broken_blocks =
          concat $ zipWith3 breakBlock uniqes blocks
                     (FunctionEntry ident params:repeat ControlEntry)

      -- Calculate live variables for each broken block.
      --
      -- Nothing can be live on entry to the first block
      -- so we could take the tail, but for now we wont
      -- to help future proof the code.
      live :: BlockEntryLiveness
      live = cmmLiveness $ map cmmBlockFromBrokenBlock broken_blocks

      -- Calculate which blocks must be made into full fledged procedures.
      proc_points :: UniqSet BlockId
      proc_points = calculateProcPoints broken_blocks

      -- Construct a map so we can lookup a broken block by its 'BlockId'.
      block_env :: BlockEnv BrokenBlock
      block_env = blocksToBlockEnv broken_blocks

      -- Group the blocks into continuations based on the set of proc-points.
      continuations :: [Continuation]
      continuations = map (gatherBlocksIntoContinuation proc_points block_env)
                          (uniqSetToList proc_points)

      -- Select the stack format on entry to each continuation.
      --
      -- This is an association list instead of a UniqFM because
      -- CLabel's don't have a 'Uniqueable' instance.
      formats :: [(CLabel, StackFormat)]
      formats = selectStackFormat live continuations

      -- Do the actual CPS transform.
      cps_procs :: [CmmTop]
      cps_procs = map (continuationToProc formats) continuations

--------------------------------------------------------------------------------

-- The format for the call to a continuation
-- The fst is the arguments that must be passed to the continuation
-- by the continuation's caller.
-- The snd is the live values that must be saved on stack.
-- A Nothing indicates an ignored slot.
-- The head of each list is the stack top or the first parameter.

-- The format for live values for a particular continuation
-- All on stack for now.
-- Head element is the top of the stack (or just under the header).
-- Nothing means an empty slot.
-- Future possibilities include callee save registers (i.e. passing slots in register)
-- and heap memory (not sure if that's usefull at all though, but it may
-- be worth exploring the design space).

continuationLabel (Continuation _ _ l _ _) = l
data Continuation =
  Continuation
     Bool              -- True => Function entry, False => Continuation/return point
     [CmmStatic]       -- Info table, may be empty
     CLabel            -- Used to generate both info & entry labels
     CmmFormals        -- Argument locals live on entry (C-- procedure params)
     [BrokenBlock]   -- Code, may be empty.  The first block is
                       -- the entry point.  The order is otherwise initially 
                       -- unimportant, but at some point the code gen will
                       -- fix the order.

		       -- the BlockId of the first block does not give rise
		       -- to a label.  To jump to the first block in a Proc,
		       -- use the appropriate CLabel.

-- Describes the layout of a stack frame for a continuation
data StackFormat
    = StackFormat
         (Maybe CLabel)		-- The label occupying the top slot
         WordOff		-- Total frame size in words
         [(CmmReg, WordOff)]	-- local reg offsets from stack top

-- A block can be a continuation of a call
-- A block can be a continuation of another block (w/ or w/o joins)
-- A block can be an entry to a function

-----------------------------------------------------------------------------

collectNonProcPointTargets ::
    UniqSet BlockId -> BlockEnv BrokenBlock
    -> UniqSet BlockId -> BlockId -> UniqSet BlockId
collectNonProcPointTargets proc_points blocks current_targets block =
    if sizeUniqSet current_targets == sizeUniqSet new_targets
       then current_targets
       else foldl (collectNonProcPointTargets proc_points blocks) new_targets targets
    where
      block' = lookupWithDefaultUFM blocks (panic "TODO") block
      targets =
        -- Note the subtlety that since the extra branch after a call
        -- will always be to a block that is a proc-point,
        -- this subtraction will always remove that case
        uniqSetToList $ (mkUniqSet $ brokenBlockTargets block') `minusUniqSet` proc_points
        -- TODO: remove redundant uniqSetToList
      new_targets = current_targets `unionUniqSets` (mkUniqSet targets)

-- TODO: insert proc point code here
--  * Branches and switches to proc points may cause new blocks to be created
--    (or proc points could leave behind phantom blocks that just jump to them)
--  * Proc points might get some live variables passed as arguments

gatherBlocksIntoContinuation ::
    UniqSet BlockId -> BlockEnv BrokenBlock
    -> BlockId -> Continuation
gatherBlocksIntoContinuation proc_points blocks start =
  Continuation is_entry info_table clabel params body
    where
      children = (collectNonProcPointTargets proc_points blocks (unitUniqSet start) start) `delOneFromUniqSet` start
      start_block = lookupWithDefaultUFM blocks (panic "TODO") start
      children_blocks = map (lookupWithDefaultUFM blocks (panic "TODO")) (uniqSetToList children)
      body = start_block : children_blocks
      info_table = [] -- TODO
      start_block_entry = brokenBlockEntry start_block
      is_entry = case start_block_entry of
                   FunctionEntry _ _ -> True
                   _ -> False
      clabel = case start_block_entry of
                 FunctionEntry label _ -> label
                 _ -> mkReturnPtLabel $ getUnique start
      params = case start_block_entry of
                 FunctionEntry _ args -> args
                 ContinuationEntry args -> args
                 ControlEntry -> [] -- TODO: it's a proc-point, we could pass lives in parameter registers

--------------------------------------------------------------------------------
-- For now just select the continuation orders in the order they are in the set with no gaps

selectStackFormat :: BlockEnv CmmLive -> [Continuation] -> [(CLabel, StackFormat)]
selectStackFormat live continuations =
    map (\c -> (continuationLabel c, selectStackFormat' c)) continuations
    where
      selectStackFormat' (Continuation True info_table label formals blocks) =
          --let ident = brokenBlockId $ head blocks -- TODO: CLabel isn't a uniquable, but we need a better way than this
          --in
          StackFormat (Just label) 0 []
      selectStackFormat' (Continuation False info_table label formals blocks) =
          -- TODO: assumes the first block is the entry block
          let ident = brokenBlockId $ head blocks -- TODO: CLabel isn't a uniquable, but we need a better way than this
          in live_to_format label formals $ lookupWithDefaultUFM live unknown_block ident

      live_to_format :: CLabel -> CmmFormals -> CmmLive -> StackFormat
      live_to_format label formals live =
          foldl extend_format
                    (StackFormat (Just label) retAddrSizeW [])
                    (uniqSetToList (live `minusUniqSet` mkUniqSet (cmmFormalsToLiveLocals formals)))

      extend_format :: StackFormat -> LocalReg -> StackFormat
      extend_format (StackFormat label size offsets) reg =
          StackFormat label (slot_size reg + size) ((CmmLocal reg, size) : offsets)

      slot_size :: LocalReg -> Int
      slot_size reg = ((machRepByteWidth (localRegRep reg) - 1) `div` wORD_SIZE) + 1

      unknown_block = panic "unknown BlockId in selectStackFormat"

continuationToProc :: [(CLabel, StackFormat)] -> Continuation -> CmmTop
continuationToProc formats (Continuation is_entry info label formals blocks) =
    CmmProc info label formals (map (continuationToProc' label formats) blocks)
    where
      continuationToProc' :: CLabel -> [(CLabel, StackFormat)] -> BrokenBlock
                             -> CmmBasicBlock
      continuationToProc' curr_ident formats (BrokenBlock ident entry stmts _ exit) =
          BasicBlock ident (prefix++stmts++postfix)
          where
            curr_format = maybe unknown_block id $ lookup curr_ident formats
            unknown_block = panic "unknown BlockId in continuationToProc"
            prefix = case entry of
                       ControlEntry -> []
                       FunctionEntry _ _ -> []
                       ContinuationEntry formals ->
                           unpack_continuation curr_format
            postfix = case exit of
                        FinalBranch next -> [CmmBranch next]
                        FinalSwitch expr targets -> [CmmSwitch expr targets]
                        FinalReturn arguments ->
                            exit_function curr_format
                                (CmmLoad (CmmReg spReg) wordRep)
                                arguments
                        FinalJump target arguments ->
                            exit_function curr_format target arguments
                        -- TODO: do something about global saves
                        FinalCall next (CmmForeignCall target CmmCallConv)
                            results arguments saves ->
                                pack_continuation curr_format cont_format ++
                                [CmmJump target arguments]
                            where
                              cont_format = maybe unknown_block id $
                                            lookup (mkReturnPtLabel $ getUnique next) formats
                        FinalCall next _ results arguments saves -> panic "unimplemented CmmCall"

--------------------------------------------------------------------------------
-- Functions that generate CmmStmt sequences
-- for packing/unpacking continuations
-- and entering/exiting functions

exit_function :: StackFormat -> CmmExpr -> CmmActuals -> [CmmStmt]
exit_function (StackFormat curr_id curr_frame_size curr_offsets) target arguments
  = adjust_spReg ++ jump where
    adjust_spReg =
        if curr_frame_size == 0
        then []
        else [CmmAssign spReg
                 (CmmRegOff spReg (curr_frame_size*wORD_SIZE))]
    jump = [CmmJump target arguments]

enter_function :: WordOff -> [CmmStmt]
enter_function max_frame_size
  = check_stack_limit where
    check_stack_limit = [
     CmmCondBranch
     (CmmMachOp (MO_U_Lt $ cmmRegRep spReg)
                    [CmmRegOff spReg max_frame_size, CmmReg spLimReg])
     gc_block]
    gc_block = undefined -- TODO: get stack and heap checks to go to same

-- TODO: fix branches to proc point (we have to insert a new block to marshel the continuation)
pack_continuation :: StackFormat -> StackFormat -> [CmmStmt]
pack_continuation (StackFormat curr_id curr_frame_size curr_offsets)
                       (StackFormat cont_id cont_frame_size cont_offsets)
  = save_live_values ++ set_stack_header ++ adjust_spReg where
    -- TODO: only save variables when actually needed
    save_live_values =
        [CmmStore
         (CmmRegOff
          spReg (wORD_SIZE*(curr_frame_size - cont_frame_size + offset)))
         (CmmReg reg)
         | (reg, offset) <- cont_offsets]
    needs_header =
      case (curr_id, cont_id) of
        (Just x, Just y) -> x /= y
        _ -> isJust cont_id
    set_stack_header =
      if not needs_header
         then []
         else [CmmStore (CmmRegOff spReg (wORD_SIZE*(curr_frame_size - cont_frame_size))) continuation_function]
    continuation_function = CmmLit $ CmmLabel $ fromJust cont_id
    adjust_spReg =
        if curr_frame_size == cont_frame_size
        then []
        else [CmmAssign spReg (CmmRegOff spReg ((curr_frame_size - cont_frame_size)*wORD_SIZE))]

-- Lazy adjustment of stack headers assumes all blocks
-- that could branch to eachother (i.e. control blocks)
-- have the same stack format (this causes a problem
-- only for proc-point).
unpack_continuation :: StackFormat -> [CmmStmt]
unpack_continuation (StackFormat curr_id curr_frame_size curr_offsets)
  = load_live_values where
    -- TODO: only save variables when actually needed
    load_live_values =
        [CmmAssign
         reg
         (CmmLoad (CmmRegOff spReg (wORD_SIZE*offset)) (cmmRegRep reg))
         | (reg, offset) <- curr_offsets]

