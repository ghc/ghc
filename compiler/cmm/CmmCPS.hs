module CmmCPS (cmmCPS) where

#include "HsVersions.h"

import Cmm
import CmmLint
import PprCmm

import Dataflow (fixedpoint)
import CmmLive

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

data BrokenBlock
  = BrokenBlock {
      brokenBlockId :: BlockId, -- Like a CmmBasicBlock
      brokenBlockEntry :: BlockEntryInfo,
                                -- How this block can be entered

      brokenBlockStmts :: [CmmStmt],
                                -- Like a CmmBasicBlock
                                -- (but without the last statement)

      brokenBlockTargets :: [BlockId],
                                -- Blocks that this block could
                                -- branch to one either by conditional
                                -- branches or via the last statement

      brokenBlockExit :: FinalStmt
                                -- How the block can be left
    }

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

data BlockEntryInfo
  = FunctionEntry		-- Beginning of a function
      CLabel                    -- The function name
      CmmFormals                -- Aguments to function

  | ContinuationEntry 		-- Return point of a call
      CmmFormals                -- return values (argument to continuation)
  -- TODO:
  -- | ProcPointEntry -- no return values, but some live might end up as params or possibly in the frame

  | ControlEntry		-- A label in the input

-- Final statement in a BlokenBlock
-- Constructors and arguments match those in Cmm,
-- but are restricted to branches, returns, jumps, calls and switches
data FinalStmt
  = FinalBranch
      BlockId -- next block (must be a ControlEntry)

  | FinalReturn
      CmmActuals -- return values

  | FinalJump
      CmmExpr -- the function to call
      CmmActuals -- arguments to call

  | FinalCall
      BlockId -- next block after call (must be a ContinuationEntry)
      CmmCallTarget -- the function to call
      CmmFormals -- results from call (redundant with ContinuationEntry)
      CmmActuals -- arguments to call
      (Maybe [GlobalReg]) -- registers that must be saved (TODO)

  | FinalSwitch
      CmmExpr [Maybe BlockId]   -- Table branch

  -- TODO: | ProcPointExit (needed?)

data StackFormat
    = StackFormat
         BlockId {- block that is the start of the continuation. may or may not be the current block -}
         WordOff {- total frame size -}
         [(CmmReg, WordOff)] {- local reg offsets from stack top -}

-- A block can be a continuation of a call
-- A block can be a continuation of another block (w/ or w/o joins)
-- A block can be an entry to a function

blocksToBlockEnv :: [BrokenBlock] -> BlockEnv BrokenBlock
blocksToBlockEnv blocks = listToUFM $ map (\b -> (brokenBlockId b, b)) blocks

-----------------------------------------------------------------------------
calculateOwnership :: UniqSet BlockId -> [BrokenBlock] -> BlockEnv (UniqSet BlockId)
calculateOwnership proc_points blocks =
    fixedpoint dependants update (map brokenBlockId blocks) emptyUFM
    where
      blocks_ufm :: BlockEnv BrokenBlock
      blocks_ufm = blocksToBlockEnv blocks

      dependants :: BlockId -> [BlockId]
      dependants ident =
          brokenBlockTargets $ lookupWithDefaultUFM
                                 blocks_ufm unknown_block ident

      update :: BlockId -> Maybe BlockId
             -> BlockEnv (UniqSet BlockId) -> Maybe (BlockEnv (UniqSet BlockId))
      update ident cause owners =
          case (cause, ident `elementOfUniqSet` proc_points) of
            (Nothing, True) -> Just $ addToUFM owners ident (unitUniqSet ident)
            (Nothing, False) -> Nothing
            (Just cause', True) -> Nothing
            (Just cause', False) ->
                if (sizeUniqSet old) == (sizeUniqSet new)
                   then Nothing
                   else Just $ addToUFM owners ident new
                where
                  old = lookupWithDefaultUFM owners emptyUniqSet ident
                  new = old `unionUniqSets` lookupWithDefaultUFM owners emptyUniqSet cause'

      unknown_block = panic "unknown BlockId in selectStackFormat"

calculateProcPoints :: [BrokenBlock] -> UniqSet BlockId
calculateProcPoints blocks = calculateProcPoints' init_proc_points blocks
    where
      init_proc_points = mkUniqSet $
                         map brokenBlockId $
                         filter always_proc_point blocks
      always_proc_point BrokenBlock {
                              brokenBlockEntry = FunctionEntry _ _ } = True
      always_proc_point BrokenBlock {
                              brokenBlockEntry = ContinuationEntry _ } = True
      always_proc_point _ = False

calculateProcPoints' :: UniqSet BlockId -> [BrokenBlock] -> UniqSet BlockId
calculateProcPoints' old_proc_points blocks =
    if sizeUniqSet old_proc_points == sizeUniqSet new_proc_points
      then old_proc_points
      else calculateProcPoints' new_proc_points blocks
    where
      owners = calculateOwnership old_proc_points blocks
      new_proc_points = unionManyUniqSets (old_proc_points:(map (calculateProcPoints'' owners) blocks))

calculateProcPoints'' :: BlockEnv (UniqSet BlockId) -> BrokenBlock -> UniqSet BlockId
calculateProcPoints''  owners block =
    unionManyUniqSets (map (f parent_id) child_ids)
    where
      parent_id = brokenBlockId block
      child_ids = brokenBlockTargets block
      -- TODO: name for f
      f parent_id child_id = 
          if needs_proc_point
            then unitUniqSet child_id
            else emptyUniqSet
          where
            parent_owners = lookupWithDefaultUFM owners emptyUniqSet parent_id
            child_owners = lookupWithDefaultUFM owners emptyUniqSet child_id
            needs_proc_point = not $ isEmptyUniqSet $ child_owners `minusUniqSet` parent_owners

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

buildContinuation ::
    UniqSet BlockId -> BlockEnv BrokenBlock
    -> BlockId -> Continuation
buildContinuation proc_points blocks start =
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

selectStackFormat :: BlockEnv CmmLive -> [BrokenBlock] -> BlockEnv StackFormat
selectStackFormat live blocks =
    fixedpoint dependants update (map brokenBlockId blocks) emptyUFM
    where
      blocks_ufm :: BlockEnv BrokenBlock
      blocks_ufm = listToUFM $ map (\b -> (brokenBlockId b, b)) blocks

      dependants :: BlockId -> [BlockId]
      dependants ident =
          brokenBlockTargets $ lookupWithDefaultUFM
                                 blocks_ufm unknown_block ident

      update :: BlockId -> Maybe BlockId
             -> BlockEnv StackFormat -> Maybe (BlockEnv StackFormat)
      update ident cause formats =
          if ident `elemUFM` formats
             then Nothing -- Blocks only need to be updated once
             else case (cause,
                        brokenBlockEntry $ lookupWithDefaultUFM blocks_ufm
                                             unknown_block ident) of
                    -- Propagate only to blocks entered by branches
                    -- (not function entry blocks or continuation entry blocks)
                    (Just cause_name, ControlEntry) ->
                        Just $ addToUFM formats ident cause_format
                            where cause_format = lookupWithDefaultUFM
                                                   formats unknown_block
                                                   cause_name
                    -- Do initial calculates for function blocks
                    (Nothing, FunctionEntry _ _) ->
                        Just $
                             addToUFM formats ident $
                             StackFormat ident 0 []
                    -- Do initial calculates for continuation blocks
                    (Nothing, ContinuationEntry _) ->
                        Just $
                             addToUFM formats ident $
                             live_to_format ident $
                             lookupWithDefaultUFM live unknown_block ident
                    _ -> Nothing

      unknown_block = panic "unknown BlockId in selectStackFormat"

      live_to_format :: BlockId -> CmmLive -> StackFormat
      live_to_format label live =
          foldl extend_format
                    (StackFormat label retAddrSizeW [])
                    (uniqSetToList live)

      extend_format :: StackFormat -> LocalReg -> StackFormat
      extend_format (StackFormat block size offsets) reg =
          StackFormat block (slot_size reg + size) ((CmmLocal reg, size) : offsets)

selectStackFormat2 :: BlockEnv CmmLive -> [Continuation] -> [(CLabel, StackFormat)]
selectStackFormat2 live continuations =
    map (\c -> (continuationLabel c, selectStackFormat' c)) continuations
    where
      selectStackFormat' (Continuation True info_table label formals blocks) =
          let ident = brokenBlockId $ head blocks -- TODO: CLabel isn't a uniquable, but we need a better way than this
          in StackFormat ident 0 []
      selectStackFormat' (Continuation False info_table label formals blocks) =
          let ident = brokenBlockId $ head blocks -- TODO: CLabel isn't a uniquable, but we need a better way than this
          in live_to_format ident $ lookupWithDefaultUFM live unknown_block ident          

      live_to_format :: BlockId -> CmmLive -> StackFormat
      live_to_format label live =
          foldl extend_format
                    (StackFormat label retAddrSizeW [])
                    (uniqSetToList live)

      extend_format :: StackFormat -> LocalReg -> StackFormat
      extend_format (StackFormat block size offsets) reg =
          StackFormat block (slot_size reg + size) ((CmmLocal reg, size) : offsets)

      unknown_block = panic "unknown BlockId in selectStackFormat"

slot_size reg = ((machRepByteWidth (localRegRep reg) - 1) `div` wORD_SIZE) + 1

constructContinuation :: [(CLabel, StackFormat)] -> Continuation -> CmmTop
constructContinuation formats (Continuation is_entry info label formals blocks) =
    CmmProc info label formals (map (constructContinuation2' label formats) blocks)

constructContinuation2' :: CLabel -> [(CLabel, StackFormat)] -> BrokenBlock
                       -> CmmBasicBlock
constructContinuation2' curr_ident formats (BrokenBlock ident entry stmts _ exit) =
    BasicBlock ident (prefix++stmts++postfix)
    where
      curr_format = maybe unknown_block id $ lookup curr_ident formats
      unknown_block = panic "unknown BlockId in constructContinuation"
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
    adjust_spReg = [
     CmmAssign spReg
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
    set_stack_header = -- TODO: only set when needed
        [CmmStore (CmmRegOff spReg (wORD_SIZE*(curr_frame_size - cont_frame_size))) continuation_function]
    continuation_function = CmmLit $ CmmLabel $ mkReturnPtLabel {-TODO: use the correct function -} $ getUnique cont_id
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

-----------------------------------------------------------------------------
-- Breaking basic blocks on function calls
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Takes a basic block and breaks it up into a list of broken blocks
--
-- Takes a basic block and returns a list of basic blocks that
-- each have at most 1 CmmCall in them which must occur at the end.
-- Also returns with each basic block, the variables that will
-- be arguments to the continuation of the block once the call (if any)
-- returns.

breakBlock :: [Unique] -> CmmBasicBlock -> BlockEntryInfo -> [BrokenBlock]
breakBlock uniques (BasicBlock ident stmts) entry =
    breakBlock' uniques ident entry [] [] stmts where
        breakBlock' uniques current_id entry exits accum_stmts stmts =
            case stmts of
              [] -> panic "block doesn't end in jump, goto or return"
              [CmmJump target arguments] ->
                  [BrokenBlock current_id entry accum_stmts
                               exits
                               (FinalJump target arguments)]
              [CmmReturn arguments] ->
                  [BrokenBlock current_id entry accum_stmts
                               exits
                               (FinalReturn arguments)]
              [CmmBranch target] ->
                  [BrokenBlock current_id entry accum_stmts
                               (target:exits)
                               (FinalBranch target)]
              [CmmSwitch expr targets] ->
                  [BrokenBlock current_id entry accum_stmts
                               (mapMaybe id targets ++ exits)
                               (FinalSwitch expr targets)]
              (CmmJump _ _:_) ->
                  panic "jump in middle of block"
              (CmmReturn _:_) ->
                  panic "return in middle of block"
              (CmmBranch _:_) ->
                  panic "branch in middle of block"
              (CmmSwitch _ _:_) ->
                  panic ("switch in middle of block" ++ (showSDoc $ ppr stmts))
              (CmmCall target results arguments saves:stmts) -> block : rest
                  where
                    new_id = BlockId $ head uniques
                    block = BrokenBlock current_id entry accum_stmts
                            (new_id:exits)
                            (FinalCall new_id target results arguments saves)
                    rest = breakBlock' (tail uniques) new_id
                           (ContinuationEntry results) [] [] stmts
              (s@(CmmCondBranch test target):stmts) ->
                  breakBlock' uniques current_id entry
                              (target:exits) (accum_stmts++[s]) stmts
              (s:stmts) ->
                  breakBlock' uniques current_id entry
                              exits (accum_stmts++[s]) stmts

--------------------------------
-- Convert from a BrokenBlock
-- to a CmmBasicBlock so the
-- liveness analysis can run
-- on it.
--------------------------------
cmmBlockFromBrokenBlock :: BrokenBlock -> CmmBasicBlock
cmmBlockFromBrokenBlock (BrokenBlock ident _ stmts _ exit) =
    BasicBlock ident (stmts++exit_stmt)
    where
      exit_stmt =
          case exit of
            FinalBranch target -> [CmmBranch target]
            FinalReturn arguments -> [CmmReturn arguments]
            FinalJump target arguments -> [CmmJump target arguments]
            FinalSwitch expr targets -> [CmmSwitch expr targets]
            FinalCall branch_target call_target results arguments saves ->
                [CmmCall call_target results arguments saves,
                 CmmBranch branch_target]

-----------------------------------------------------------------------------
-- CPS a single CmmTop (proceedure)
-----------------------------------------------------------------------------

cpsProc :: UniqSupply -> CmmTop -> [CmmTop]
cpsProc uniqSupply x@(CmmData _ _) = [x]
cpsProc uniqSupply x@(CmmProc info_table ident params blocks) =
    --[CmmProc info_table ident params cps_blocks]
    cps_continuations
    where
      uniqes :: [[Unique]]
      uniqes = map uniqsFromSupply $ listSplitUniqSupply uniqSupply

      -- Break the block at each function call
      broken_blocks :: [BrokenBlock]
      broken_blocks = concat $ zipWith3 breakBlock uniqes blocks
                                        (FunctionEntry ident params:repeat ControlEntry)

      -- Calculate live variables for each broken block
      live :: BlockEntryLiveness
      live = cmmLiveness $ map cmmBlockFromBrokenBlock broken_blocks
             -- nothing can be live on entry to the first block so we could take the tail

      proc_points :: UniqSet BlockId
      proc_points = calculateProcPoints broken_blocks

      continuations :: [Continuation]
      continuations = map (buildContinuation proc_points (blocksToBlockEnv broken_blocks)) (uniqSetToList proc_points)

      -- TODO: insert proc point code here
      --  * Branches and switches to proc points may cause new blocks to be created
      --    (or proc points could leave behind phantom blocks that just jump to them)
      --  * Proc points might get some live variables passed as arguments

      -- TODO: let blocks_with_live = map (cmmLivenessComment live . snd) broken_blocks

      --procs = groupBlocksIntoContinuations live broken_blocks

      -- Select the stack format on entry to each block
      formats2 :: [(CLabel, StackFormat)]
      formats2 = selectStackFormat2 live continuations

      -- Do the actual CPS transform
      cps_continuations :: [CmmTop]
      cps_continuations = map (constructContinuation formats2) continuations

--------------------------------------------------------------------------------
cmmCPS :: DynFlags
       -> [Cmm]                 -- C-- with Proceedures
       -> IO [Cmm]		-- Output: CPS transformed C--

cmmCPS dflags abstractC = do
  when (dopt Opt_DoCmmLinting dflags) $
       do showPass dflags "CmmLint"
	  case firstJust $ map cmmLint abstractC of
	    Just err -> do printDump err
			   ghcExit dflags 1
	    Nothing  -> return ()
  showPass dflags "CPS"
  -- TODO: check for use of branches to non-existant blocks
  -- TODO: check for use of Sp, SpLim, R1, R2, etc.
  -- TODO: find out if it is valid to create a new unique source like this
  uniqSupply <- mkSplitUniqSupply 'p'
  let supplies = listSplitUniqSupply uniqSupply
  let continuationC = zipWith (\s (Cmm c) -> Cmm $ concat $ zipWith (cpsProc) (listSplitUniqSupply s) c) supplies abstractC

  dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "CPS Cmm" (pprCmms continuationC)
  -- TODO: add option to dump Cmm to file
  return continuationC
