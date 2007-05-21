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


data BlockEntryInfo
  = FunctionEntry		-- Beginning of function

  | ContinuationEntry 		-- Return point of a call
      CmmFormals                -- return values
  -- TODO:
  -- | ProcPointEntry -- no return values, but some live might end up as params or possibly in the frame

  | ControlEntry		-- A label in the input

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
  -- TODO: | ProcPointExit (needed?)

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

selectStackFormat2 :: BlockEnv CmmLive -> [BrokenBlock] -> BlockEnv StackFormat
selectStackFormat2 live blocks = fixedpoint dependants update (map brokenBlockId blocks) emptyUFM where
  blocks_ufm = listToUFM $ map (\b -> (brokenBlockId b, b)) blocks
  dependants ident =
      brokenBlockTargets $ lookupWithDefaultUFM blocks_ufm (panic "TODO") ident
  update ident cause formats =
    let BrokenBlock _ entry _ _ _ = lookupWithDefaultUFM blocks_ufm (panic "unknown BlockId in selectStackFormat:live") ident in
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

constructContinuation2 :: BlockEnv StackFormat -> BrokenBlock -> CmmBasicBlock
constructContinuation2 formats (BrokenBlock ident entry stmts _ exit) =
    BasicBlock ident (prefix++stmts++postfix)
    where
      curr_format = lookupWithDefaultUFM formats (panic $ "format: unknown block " ++ (showSDoc $ ppr $ getUnique ident)) ident
      prefix = case entry of
                 ControlEntry -> []
                 FunctionEntry -> []
                 ContinuationEntry formals -> unpack_continuation curr_format
      postfix = case exit of
                  FinalBranch next -> [CmmBranch next]
                  FinalReturn arguments -> exit_function curr_format (CmmLoad (CmmReg spReg) wordRep) arguments
                  FinalJump target arguments -> exit_function curr_format target arguments
                  -- TODO: do something about global saves
                  FinalCall next (CmmForeignCall target CmmCallConv) results arguments saves ->
                      let cont_format = lookupWithDefaultUFM formats (panic $ "format: unknown block " ++ (showSDoc $ ppr $ getUnique next)) next
                      in pack_continuation curr_format cont_format ++
                             [CmmJump target arguments]
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

-----------------------------------------------------------------------------
-- Breaking basic blocks on function calls
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Takes a basic block and returns a list of basic blocks that
-- each have at most 1 CmmCall in them which must occur at the end.
-- Also returns with each basic block, the variables that will
-- be arguments to the continuation of the block once the call (if any)
-- returns.

breakBlock uniques (BasicBlock ident stmts) entry =
    breakBlock' uniques ident entry [] [] stmts where
        breakBlock' uniques current_id entry exits accum_stmts stmts =
            case stmts of
              [] -> panic "block doesn't end in jump, goto or return"
              [CmmJump target arguments] ->
                  [BrokenBlock current_id entry accum_stmts exits
                                   (FinalJump target arguments)]
              [CmmReturn arguments] ->
                  [BrokenBlock current_id entry accum_stmts exits
                                   (FinalReturn arguments)]
              [CmmBranch target] ->
                  [BrokenBlock current_id entry accum_stmts (target:exits)
                                   (FinalBranch target)]
              (CmmJump _ _:_) ->
                  panic "jump in middle of block"
              (CmmReturn _:_) ->
                  panic "return in middle of block"
              (CmmBranch _:_) ->
                  panic "branch in middle of block"
              (CmmSwitch _ _:_) ->
                  panic "switch in block not implemented"
              (CmmCall target results arguments saves:stmts) ->
                  let new_id = BlockId $ head uniques
                      rest = breakBlock' (tail uniques) new_id (ContinuationEntry results) [] [] stmts
                  in BrokenBlock current_id entry accum_stmts (new_id:exits)
                         (FinalCall new_id target results arguments saves) : rest
              (s@(CmmCondBranch test target):stmts) ->
                  breakBlock' uniques current_id entry (target:exits) (accum_stmts++[s]) stmts
              (s:stmts) ->
                  breakBlock' uniques current_id entry exits (accum_stmts++[s]) stmts

-----------------------------------------------------------------------------
cmmBlockFromBrokenBlock :: BrokenBlock -> CmmBasicBlock
cmmBlockFromBrokenBlock (BrokenBlock ident _ stmts _ exit) = BasicBlock ident (stmts++exit_stmt)
    where
      exit_stmt =
          case exit of
            FinalBranch target -> [CmmBranch target]
            FinalReturn arguments -> [CmmReturn arguments]
            FinalJump target arguments -> [CmmJump target arguments]
            FinalCall branch_target call_target results arguments saves -> [CmmCall call_target results arguments saves, CmmBranch branch_target]

-----------------------------------------------------------------------------
-- CPS a single CmmTop (proceedure)
-----------------------------------------------------------------------------

cpsProc :: UniqSupply -> CmmTop -> [CmmTop]
cpsProc uniqSupply x@(CmmData _ _) = [x]
cpsProc uniqSupply x@(CmmProc info_table ident params blocks) =
  [CmmProc info_table ident params $ map (constructContinuation2 formats) broken_blocks]
    where
      uniqes :: [[Unique]]
      uniqes = map uniqsFromSupply $ listSplitUniqSupply uniqSupply

      broken_blocks :: [BrokenBlock]
      broken_blocks = concat $ zipWith3 breakBlock uniqes blocks (FunctionEntry:repeat ControlEntry)
  
      live :: BlockEntryLiveness
      live = cmmLiveness $ map cmmBlockFromBrokenBlock broken_blocks

      -- TODO: branches for proc points
      -- TODO: let blocks_with_live = map (cmmLivenessComment live . snd) broken_blocks

      formats :: BlockEnv StackFormat	-- Stack format on entry
      formats = selectStackFormat2 live broken_blocks


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
  let supplies = listSplitUniqSupply uniqSupply
  let continuationC = zipWith (\s (Cmm c) -> Cmm $ concat $ zipWith (cpsProc) (listSplitUniqSupply s) c) supplies abstractC

  dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "CPS Cmm" (pprCmms continuationC)
  -- TODO: add option to dump Cmm to file
  return continuationC
