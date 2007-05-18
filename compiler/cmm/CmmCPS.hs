module CmmCPS (cmmCPS) where

#include "HsVersions.h"

import Cmm
import CmmLint
import PprCmm

import Dataflow -- (fixedpoint, cmmLivenessComment, cmmLiveness, CmmLive)

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

--------------------------------------------------------------------------------
-- Monad for the CPSer
-- Contains:
--  * State for the uniqSupply

data CPSState = CPSState { cps_uniqs :: UniqSupply }

data CPS a = CPS { runCPS :: CPSState -> (CPSState, a) }

instance Monad CPS where
  return a = CPS $ \s -> (s, a)
  (CPS m) >>= f = CPS $ \s ->
    let (s', m') = m s
    in runCPS (f m') s'

--------------------------------------------------------------------------------
-- Utility functions

getState = CPS $ \s -> (s, s)
putState s = CPS $ \_ -> (s, ())

newLabelCPS = do
  state <- getState
  let (us1, us2) = splitUniqSupply (cps_uniqs state)
  putState $ state { cps_uniqs = us1 }
  return $ BlockId (uniqFromSupply us2)

mapMCmmTop :: (Monad m) => (CmmTop -> m [CmmTop]) -> Cmm -> m Cmm
mapMCmmTop f (Cmm xs) = liftM Cmm $ liftM concat $ mapM f xs

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
  = BrokenBlock		
       BlockId			-- Like a CmmBasicBlock
       BlockEntryInfo		-- How this block can be entered
       [CmmStmt]		-- Like a CmmBasicBlock (but without
				--	the last statement)
       BlockExitInfo		-- How the block can be left

data BlockEntryInfo
  = FunctionEntry		-- Beginning of function

  | ContinuationEntry 		-- Return point of a call
	CmmFormals {- return values -}
  -- TODO | ProcPointEntry {- no return values, but some live might end up as params -}

  | ControlEntry		-- A label in the input

data BlockExitInfo
  = ControlExit [BlockId] -- blocks branched to conditionally 
    BlockId -- next block (must be a ControlEntry)

  | ReturnExit [BlockId] -- blocks branched to conditionally 
    CmmActuals -- return values

  | TailCallExit [BlockId] -- blocks branched to conditionally 
    CmmExpr -- the function to call
    CmmActuals -- arguments to call

  | CallExit [BlockId] -- blocks branched to conditionally 
    BlockId -- next block after call (must be a ContinuationEntry)
    CmmCallTarget -- the function to call
    CmmFormals -- results from call (redundant with ContinuationEntry)
    CmmActuals -- arguments to call
    (Maybe [GlobalReg]) -- registers that must be saved (TODO)
  -- TODO: | ProcPointExit (needed?)

data CPSBlockInfo
  = ControlBlock -- Consider whether a proc-point might want arguments on stack
  | ContinuationBlock [(CmmReg,MachHint)] {- params -}
  | EntryBlock

--type StackFormat = [Maybe LocalReg] -- TODO: consider params as part of format
data StackFormat
    = StackFormat
      	 BlockId {- block that is the start of the continuation. may or may not be the current block -}
      	 WordOff {- total frame size -}
      	 [(CmmReg, WordOff)] {- local reg offsets from stack top -}

-- A block can be a continuation of a call
-- A block can be a continuation of another block (w/ or w/o joins)
-- A block can be an entry to a function

--------------------------------------------------------------------------------
-- For now just select the continuation orders in the order they are in the set with no gaps
-- TODO: select a format that keeps blocks that can jump to each other the same
-- Assumed that jumps, calls 
selectStackFormat :: UniqFM {-BlockId-} CmmFormals -> UniqFM {-BlockId-} CmmLive -> UniqFM {-BlockId-} [(CPSBlockInfo, CmmBasicBlock)] -> UniqFM {-BlockId-} StackFormat
selectStackFormat = undefined
{-
selectStackFormat param live blocks = fixedpoint 
listToUFM $ map live_to_format $ ufmToList live
    where
      live_to_format (unique, live) = (unique, format) where
          format = foldl extend_format
                    (StackFormat (BlockId unique) retAddrSizeW [])
                    (uniqSetToList live)
      extend_format :: StackFormat -> LocalReg -> StackFormat
      extend_format (StackFormat block size offsets) reg =
          StackFormat block (slot_size reg + size) ((CmmLocal reg, size) : offsets)
-}

selectStackFormat2 :: UniqFM {-BlockId-} CmmLive -> [BrokenBlock] -> UniqFM {-BlockId-} StackFormat
selectStackFormat2 live blocks = fixedpoint dependants update (map brokenBlockId blocks) emptyUFM where
  blocks_ufm = listToUFM $ map (\b -> (brokenBlockId b, b)) blocks
  dependants ident =
      case lookupWithDefaultUFM blocks_ufm (panic "TODO") ident of
        (BrokenBlock _ _ _ (ControlExit exits next)) -> next:exits
        (BrokenBlock _ _ _ (ReturnExit exits _)) -> exits
        (BrokenBlock _ _ _ (TailCallExit exits _ _)) -> exits
        (BrokenBlock _ _ _ (CallExit exits _ _ _ _ _)) -> exits
  update ident cause formats =
    let BrokenBlock _ entry _ _ = lookupWithDefaultUFM blocks_ufm (panic "unknown BlockId in selectStackFormat:live") ident in
    case cause of
      -- Propagate only to blocks entered by branches (not function entry blocks or continuation entry blocks)
      Just cause_name ->
          let cause_format = lookupWithDefaultUFM formats (panic "update signaled for block not in format") cause_name
          in case entry of
            ControlEntry -> Just $ addToUFM formats ident cause_format
            FunctionEntry -> Nothing
            ContinuationEntry _ -> Nothing
      -- Do initial calculates for function blocks
      Nothing ->
          case entry of
            ControlEntry -> Nothing
            FunctionEntry -> Just $ addToUFM formats ident $ StackFormat ident 0 []
            ContinuationEntry _ -> Just $ addToUFM formats ident $ live_to_format ident $ lookupWithDefaultUFM live (panic "TODO") ident
  live_to_format label live =
      foldl extend_format
                (StackFormat label retAddrSizeW [])
                (uniqSetToList live)
  extend_format :: StackFormat -> LocalReg -> StackFormat
  extend_format (StackFormat block size offsets) reg =
      StackFormat block (slot_size reg + size) ((CmmLocal reg, size) : offsets)

slot_size reg = ((machRepByteWidth (localRegRep reg) - 1) `div` wORD_SIZE) + 1

transformReturn :: UniqFM {-BlockId-} CPSBlockInfo -> UniqFM {-BlockId-} StackFormat -> CmmBasicBlock -> CmmBasicBlock
transformReturn block_infos formats (BasicBlock ident stmts) =
  -- NOTE: assumes that return/jump can *only* appear at end of block
  case last stmts of
    CmmReturn arguments ->
        BasicBlock ident $
                  (init stmts) ++
                  exit_function curr_format (CmmLoad (CmmReg spReg) wordRep) arguments
    CmmJump target arguments ->
        BasicBlock ident $
                  (init stmts) ++
                  exit_function curr_format target arguments
    _ -> BasicBlock ident stmts
  where
  curr_format = lookupWithDefaultUFM formats (panic $ "format: unknown block " ++ (showSDoc $ ppr $ getUnique ident)) ident

destructContinuation :: UniqFM {-BlockId-} CPSBlockInfo -> UniqFM {-BlockId-} StackFormat -> CmmBasicBlock -> CmmBasicBlock
destructContinuation block_infos formats (BasicBlock ident stmts) =
  case info of
    ControlBlock -> BasicBlock ident stmts
    ContinuationBlock _ -> BasicBlock ident (unpack_continuation curr_format ++ stmts)
  where
  info = lookupWithDefaultUFM block_infos (panic $ "info: unknown block " ++ (showSDoc $ ppr $ getUnique ident)) ident
  curr_format = lookupWithDefaultUFM formats (panic $ "format: unknown block " ++ (showSDoc $ ppr $ getUnique ident)) ident

constructContinuation2 :: UniqFM {-BlockId-} StackFormat -> BrokenBlock -> CmmBasicBlock
constructContinuation2 formats (BrokenBlock ident entry stmts exit) =
    BasicBlock ident (prefix++stmts++postfix)
    where
      curr_format = lookupWithDefaultUFM formats (panic $ "format: unknown block " ++ (showSDoc $ ppr $ getUnique ident)) ident
      prefix = case entry of
                 ControlEntry -> []
                 FunctionEntry -> []
                 ContinuationEntry formals -> unpack_continuation curr_format
      postfix = case exit of
                  ControlExit _ next -> [CmmBranch next]
                  ReturnExit _ arguments -> exit_function curr_format (CmmLoad (CmmReg spReg) wordRep) arguments
                  TailCallExit _ target arguments -> exit_function curr_format target arguments
                  -- TODO: do something about global saves
                  CallExit _ next (CmmForeignCall target CmmCallConv) results arguments saves ->
                      let cont_format = lookupWithDefaultUFM formats (panic $ "format: unknown block " ++ (showSDoc $ ppr $ getUnique next)) next
                      in pack_continuation curr_format cont_format ++
                             [CmmJump target arguments]
                  CallExit _ next _ results arguments saves -> panic "unimplemented CmmCall"

constructContinuation :: UniqFM {-BlockId-} CPSBlockInfo -> UniqFM {-BlockId-} StackFormat -> CmmBasicBlock -> CmmBasicBlock
constructContinuation block_infos formats (BasicBlock ident stmts) =
  case last $ init stmts of
    -- TODO: global_saves
    --CmmCall (CmmForeignCall target CmmCallConv) results arguments (Just []) -> --TODO: handle globals
    CmmCall (CmmForeignCall target CmmCallConv) results arguments _ ->
        BasicBlock ident $
                   init (init stmts) ++
                   pack_continuation curr_format cont_format ++
                   [CmmJump target arguments]
    CmmCall target results arguments _ -> panic "unimplemented CmmCall"
    -- TODO: branches for proc-points
    -- _ -> BasicBlock ident $ (init stmts) ++ build_block_branch
    _ -> BasicBlock ident stmts
  where
  info = lookupWithDefaultUFM block_infos (panic $ "info: unknown block " ++ (showSDoc $ ppr $ getUnique next_block)) next_block
  cont_format = lookupWithDefaultUFM formats (panic $ "format: unknown block " ++ (showSDoc $ ppr $ getUnique next_block)) next_block
  curr_format = lookupWithDefaultUFM formats (panic $ "format: unknown block " ++ (showSDoc $ ppr $ getUnique next_block)) ident
  next_block = case last stmts of
    CmmBranch next -> next
    -- TODO: blocks with jump at end
    -- TODO: blocks with return at end
    _ -> panic $ "basic block without a branch at the end (unimplemented) " ++ (showSDoc $ ppr $ stmts)
  next_block_as_proc_expr = CmmLit $ CmmLabel $ mkReturnPtLabel $ getUnique next_block
  block_needs_call = True -- TODO: use a table (i.e. proc-point)
  build_block_branch =
    if block_needs_call
       then [CmmJump next_block_as_proc_expr [] {- TODO: pass live -}] {- NOTE: a block can never be both a continuation and a controll block -}
       else [CmmBranch next_block]

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
    continuation_function = CmmLit $ CmmLabel $ mkReturnPtLabel $ getUnique cont_id
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

-- TODO: TBD when to adjust the stack

cpsProc :: CmmTop -> CPS [CmmTop]
cpsProc x@(CmmData _ _) = return [x]
cpsProc x@(CmmProc info_table ident params blocks) = do

  broken_blocks <- liftM concat $ mapM breakBlock blocks
  broken_blocks2 <- liftM concat (zipWithM breakBlock2 blocks (FunctionEntry:repeat ControlEntry))
	-- broken_blocks :: [BrokenBlock]

   let live = cmmLiveness (map snd broken_blocks)
  let live2 :: BlockEntryLiveness
      live2 = cmmLiveness2 broken_blocks2

  let blocks_with_live = map (cmmLivenessComment live . snd) broken_blocks

  let formats = selectStackFormat (panic "params to selectStackFormat" {-TODO-}) live (undefined)
  let formats2 :: BlockEnv StackFormat	-- Stack format on entry
      formats2 = selectStackFormat2 live2 broken_blocks2

  let block_infos = listToUFM $ map (\(info, block) -> (blockId block, info)) broken_blocks
  --let blocks_with_live' = map (constructContinuation block_infos formats) blocks_with_live
  --let blocks_with_live'' = map (destructContinuation block_infos formats) blocks_with_live'
  --let blocks_with_live''' = map (transformReturn block_infos formats) blocks_with_live''

  return $ [CmmProc info_table ident params $ map (constructContinuation2 formats2) broken_blocks2]
{-  
  return $ [CmmProc info_table ident params $
            map (constructContinuation block_infos formats .
                 destructContinuation block_infos formats .
                 transformReturn block_infos formats)
            blocks_with_live]
-}

--------------------------------------------------------------------------------
-- Takes a basic block and returns a list of basic blocks that
-- each have at most 1 CmmCall in them which must occur at the end.
-- Also returns with each basic block, the variables that will
-- be arguments to the continuation of the block once the call (if any) returns.

cmmBlockifyCalls :: [CmmBasicBlock] -> CPS [(CPSBlockInfo, CmmBasicBlock)]
cmmBlockifyCalls blocks = liftM concat $ mapM breakBlock blocks

-- [(CmmReg,MachHint)] is the results from the previous block that are expected as parameters
--breakBlock :: CmmBasicBlock -> CPS [(Maybe BlockId, CmmBasicBlock)]
breakBlock :: CmmBasicBlock -> CPS [(CPSBlockInfo, CmmBasicBlock)]
breakBlock (BasicBlock ident stmts) = breakBlock' ident ControlBlock [] stmts

breakBlock' current_id block_info accum_stmts [] =
  return [(block_info, BasicBlock current_id accum_stmts)]
-- TODO: notice a call just before a branch, jump, call, etc.
breakBlock' current_id block_info accum_stmts (stmt@(CmmCall _ results _ _):stmts) = do
  new_id <- newLabelCPS
  let new_block = (block_info, BasicBlock current_id (accum_stmts ++ [stmt, CmmBranch new_id]))
  rest <- breakBlock' new_id (ContinuationBlock results) [] stmts
  return $ (new_block:rest)
breakBlock' current_id arguments accum_stmts (stmt:stmts) =
  breakBlock' current_id arguments (accum_stmts ++ [stmt]) stmts

breakBlock2 (BasicBlock ident stmts) entry = breakBlock2' ident entry [] [] stmts

breakBlock2' current_id block_info exits accum_stmts [] =
    panic "block doesn't end in jump, goto or return"
breakBlock2' current_id entry exits accum_stmts [CmmJump target arguments] =
    return [BrokenBlock current_id entry accum_stmts (TailCallExit exits target arguments)]
breakBlock2' current_id entry exits accum_stmts [CmmReturn arguments] =
    return [BrokenBlock current_id entry accum_stmts (ReturnExit exits arguments)]
breakBlock2' current_id entry exits accum_stmts [CmmBranch target] =
    return [BrokenBlock current_id entry accum_stmts (ControlExit exits target)]
breakBlock2' _ _ _ _ (CmmJump _ _:_) = panic "jump in middle of block"
breakBlock2' _ _ _ _ (CmmReturn _:_) = panic "return in middle of block"
breakBlock2' _ _ _ _ (CmmBranch _:_) = panic "branch in middle of block"
breakBlock2' _ _ _ _ (CmmSwitch _ _:_) = panic "switch in block not implemented"
breakBlock2' current_id entry exits accum_stmts (CmmCall target results arguments saves:stmts) = do
  new_id <- newLabelCPS
  rest <- breakBlock2' new_id (ContinuationEntry results) [] [] stmts
  return $ BrokenBlock current_id entry accum_stmts (CallExit exits new_id target results arguments saves) : rest
breakBlock2' current_id entry exits accum_stmts (s@(CmmCondBranch test target):stmts) =
    breakBlock2' current_id entry (target:exits) (accum_stmts++[s]) stmts
breakBlock2' current_id entry exits accum_stmts (s:stmts) =
    breakBlock2' current_id entry exits (accum_stmts++[s]) stmts

brokenBlockTargets (BrokenBlock _ _ _ (TailCallExit exits _ _)) = exits
brokenBlockTargets (BrokenBlock _ _ _ (ReturnExit exits _)) = exits
brokenBlockTargets (BrokenBlock _ _ _ (ControlExit exits target)) = target:exits
brokenBlockTargets (BrokenBlock _ _ _ (CallExit exits next _ _ _ _)) = next:exits

brokenBlockId (BrokenBlock ident _ _ _) = ident

cmmBrokenBlockSources ::
    [BrokenBlock] -> UniqFM {-BlockId-} (UniqSet BlockId)
cmmBrokenBlockSources blocks = foldr aux emptyUFM blocks where
    aux block sourcesUFM  =
        foldr add_source_edges sourcesUFM targets where
            add_source_edges t ufm =
                addToUFM_Acc (flip addOneToUniqSet) unitUniqSet ufm t ident
            targets = brokenBlockTargets block
            ident = brokenBlockId block

cmmBrokenBlockNames :: [BrokenBlock] -> UniqFM {-BlockId-} BrokenBlock
cmmBrokenBlockNames blocks = listToUFM $ map block_name blocks where
    block_name b = (brokenBlockId b, b)

cmmBrokenBlockDependants :: UniqFM {-BlockId-} (UniqSet BlockId) -> BlockId -> [BlockId]
cmmBrokenBlockDependants sources ident =
    uniqSetToList $ lookupWithDefaultUFM sources emptyUniqSet ident

cmmBrokenBlockLive :: UniqFM {-BlockId-} CmmLive -> BrokenBlock -> CmmLive
cmmBrokenBlockLive other_live (BrokenBlock _ _ stmts exit) =
    foldr ((.) . (cmmStmtLive other_live)) id stmts live_at_end
    where
      live_at_end =
          case exit of
            ControlExit _ _ -> emptyUniqSet
            ReturnExit _ actuals -> foldr ((.) . cmmExprLive) id (map fst actuals) emptyUniqSet
            TailCallExit _ target actuals -> 
                cmmExprLive target $ foldr ((.) . cmmExprLive) id (map fst actuals) $ emptyUniqSet
            CallExit _ _ target _ actuals live ->
                target_liveness $
                foldr ((.) . cmmExprLive) id (map fst actuals) $
                emptyUniqSet
                where
                  only_local_regs [] = []
                  only_local_regs ((CmmGlobal _,_):args) = only_local_regs args
                  only_local_regs ((CmmLocal r,_):args) = r:only_local_regs args
                  target_liveness =
                    case target of
                      (CmmForeignCall target _) -> cmmExprLive target
                      (CmmPrim _) -> id


cmmBrokenBlockUpdate ::
    UniqFM {-BlockId-} BrokenBlock
    -> BlockId
    -> Maybe BlockId
    -> UniqFM {-BlockId-} CmmLive
    -> Maybe (UniqFM {-BlockId-} CmmLive)
cmmBrokenBlockUpdate blocks node _ state =
    let old_live = lookupWithDefaultUFM state (panic "unknown block id during liveness analysis") node
        block = lookupWithDefaultUFM blocks (panic "unknown block id during liveness analysis") node
        new_live = cmmBrokenBlockLive state block
    in if (sizeUniqSet old_live) == (sizeUniqSet new_live)
       then Nothing
       else Just $ addToUFM state node new_live


cmmLiveness2 :: [BrokenBlock] -> UniqFM {-BlockId-} CmmLive
cmmLiveness2 blocks =
    fixedpoint (cmmBrokenBlockDependants sources) (cmmBrokenBlockUpdate blocks')
               (map brokenBlockId blocks) (listToUFM [(brokenBlockId b, emptyUniqSet) | b <- blocks]) where
                   sources = cmmBrokenBlockSources blocks
                   blocks' = cmmBrokenBlockNames blocks

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
  -- continuationC <- return abstractC
  -- TODO: find out if it is valid to create a new unique source like this
  uniqSupply <- mkSplitUniqSupply 'p'
  let (_, continuationC) = runCPS (mapM (mapMCmmTop cpsProc) abstractC) (CPSState uniqSupply)

  dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "CPS Cmm" (pprCmms continuationC)
  -- TODO: add option to dump Cmm to file
  return continuationC
