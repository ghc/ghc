module CmmCPSGen (
  -- | Converts continuations into full proceedures.
  -- The main work of the CPS transform that everything else is setting-up.
  continuationToProc,
  Continuation(..), continuationLabel,
  ContinuationFormat(..),
) where

#include "HsVersions.h"

import Cmm
import CLabel
import CmmBrokenBlock -- Data types only
import MachOp
import CmmUtils
import CmmCallConv

import CgProf (curCCS, curCCSAddr)
import CgUtils (cmmOffsetW)
import CgInfoTbls (entryCode)
import SMRep
import ForeignCall

import Constants
import StaticFlags
import Unique
import Maybe

import Panic

import MachRegs (callerSaveVolatileRegs)
  -- HACK: this is part of the NCG so we shouldn't use this, but we need
  -- it for now to eliminate the need for saved regs to be in CmmCall.
  -- The long term solution is to factor callerSaveVolatileRegs
  -- from nativeGen into CPS

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

continuationLabel (Continuation _ l _ _ _) = l
data Continuation info =
  Continuation
     info              -- Left <=> Continuation created by the CPS
                       -- Right <=> Function or Proc point
     CLabel            -- Used to generate both info & entry labels
     CmmFormals        -- Argument locals live on entry (C-- procedure params)
     Bool              -- ^ True <=> GC block so ignore stack size
     [BrokenBlock]     -- Code, may be empty.  The first block is
                       -- the entry point.  The order is otherwise initially 
                       -- unimportant, but at some point the code gen will
                       -- fix the order.

		       -- the BlockId of the first block does not give rise
		       -- to a label.  To jump to the first block in a Proc,
		       -- use the appropriate CLabel.

data ContinuationFormat
    = ContinuationFormat {
        continuation_formals :: CmmFormals,
        continuation_label :: Maybe CLabel,	-- The label occupying the top slot
        continuation_frame_size :: WordOff,	-- Total frame size in words (not including arguments)
        continuation_stack :: [Maybe LocalReg]	-- local reg offsets from stack top
      }

-- A block can be a continuation of a call
-- A block can be a continuation of another block (w/ or w/o joins)
-- A block can be an entry to a function

-----------------------------------------------------------------------------
continuationToProc :: (WordOff, WordOff, [(CLabel, ContinuationFormat)])
                   -> CmmReg
                   -> [[Unique]]
                   -> Continuation CmmInfo
                   -> CmmTop
continuationToProc (max_stack, update_frame_size, formats) stack_use uniques
                   (Continuation info label formals _ blocks) =
    CmmProc info label formals (concat $ zipWith3 continuationToProc' uniques blocks (True : repeat False))
    where
      curr_format = maybe unknown_block id $ lookup label formats
      unknown_block = panic "unknown BlockId in continuationToProc"
      curr_stack = continuation_frame_size curr_format
      arg_stack = argumentsSize localRegRep formals

      param_stmts :: [CmmStmt]
      param_stmts = function_entry curr_format

      gc_stmts :: [CmmStmt]
      gc_stmts =
        assign_gc_stack_use stack_use arg_stack (max_stack - curr_stack)

      update_stmts :: [CmmStmt]
      update_stmts =
          case info of
            CmmInfo _ (Just (UpdateFrame target args)) _ ->
                pack_frame curr_stack update_frame_size (Just target) (map Just args) ++
                adjust_sp_reg (curr_stack - update_frame_size)
            CmmInfo _ Nothing _ -> []

-- At present neither the Cmm parser nor the code generator
-- produce code that will allow the target of a CmmCondBranch
-- or a CmmSwitch to become a continuation or a proc-point.
-- If future revisions, might allow these to happen
-- then special care will have to be take to allow for that case.
      continuationToProc' :: [Unique]
                          -> BrokenBlock
                          -> Bool
                          -> [CmmBasicBlock]
      continuationToProc' uniques (BrokenBlock ident entry stmts _ exit) is_entry =
          prefix_blocks ++ [main_block]
          where
            prefix_blocks =
                if is_entry
                then [BasicBlock
                      (BlockId prefix_unique)
                      (param_stmts ++ [CmmBranch ident])]
                else []

            prefix_unique : call_uniques = uniques
            toCLabel = mkReturnPtLabel . getUnique

            block_for_branch unique next
                | (Just cont_format) <- lookup (toCLabel next) formats
                = let
                    new_next = BlockId unique
                    cont_stack = continuation_frame_size cont_format
                    arguments = map formal_to_actual (continuation_formals cont_format)
                  in (new_next,
                     [BasicBlock new_next $
                      pack_continuation False curr_format cont_format ++
                      tail_call (curr_stack - cont_stack)
                              (CmmLit $ CmmLabel $ toCLabel next)
                              arguments])
                | otherwise
                = (next, [])

            block_for_branch' :: Unique -> Maybe BlockId -> (Maybe BlockId, [CmmBasicBlock])
            block_for_branch' _ Nothing = (Nothing, [])
            block_for_branch' unique (Just next) = (Just new_next, new_blocks)
              where (new_next, new_blocks) = block_for_branch unique next

            main_block =
                case entry of
                  FunctionEntry _ _ _ ->
                      -- Ugh, the statements for an update frame must come
                      -- *after* the GC check that was added at the beginning
                      -- of the CPS pass.  So we have do edit the statements
                      -- a bit.  This depends on the knowledge that the
                      -- statements in the first block are only the GC check.
                      -- That's fragile but it works for now.
                      BasicBlock ident (gc_stmts ++ stmts ++ update_stmts ++ postfix_stmts)
                  ControlEntry -> BasicBlock ident (stmts ++ postfix_stmts)
                  ContinuationEntry _ _ _ -> BasicBlock ident (stmts ++ postfix_stmts)
            postfix_stmts = case exit of
                        FinalBranch next ->
                            if (mkReturnPtLabel $ getUnique next) == label
                            then [CmmBranch next]
                            else case lookup (mkReturnPtLabel $ getUnique next) formats of
                              Nothing -> [CmmBranch next]
                              Just cont_format ->
                                pack_continuation True curr_format cont_format ++
                                tail_call (curr_stack - cont_stack)
                                          (CmmLit $ CmmLabel $ mkReturnPtLabel $ getUnique next)
                                          arguments
                                where
                                  cont_stack = continuation_frame_size cont_format
                                  arguments = map formal_to_actual (continuation_formals cont_format)
                        FinalSwitch expr targets -> [CmmSwitch expr targets]
                        FinalReturn arguments ->
                            tail_call curr_stack
                                (entryCode (CmmLoad (CmmReg spReg) wordRep))
                                arguments
                        FinalJump target arguments ->
                            tail_call curr_stack target arguments

                        -- A regular Cmm function call
                        FinalCall next (CmmForeignCall target CmmCallConv)
                            results arguments _ _ ->
                                pack_continuation True curr_format cont_format ++
                                tail_call (curr_stack - cont_stack)
                                              target arguments
                            where
                              cont_format = maybe unknown_block id $
                                            lookup (mkReturnPtLabel $ getUnique next) formats
                              cont_stack = continuation_frame_size cont_format

                        -- A safe foreign call
                        FinalCall next (CmmForeignCall target conv)
                            results arguments _ _ ->
                                target_stmts ++
                                foreignCall call_uniques' (CmmForeignCall new_target conv)
                                            results arguments
                            where
                              (call_uniques', target_stmts, new_target) =
                                  maybeAssignTemp call_uniques target

                        -- A safe prim call
                        FinalCall next (CmmPrim target)
                            results arguments _ _ ->
                                foreignCall call_uniques (CmmPrim target)
                                            results arguments

formal_to_actual reg = (CmmReg (CmmLocal reg), NoHint)

foreignCall :: [Unique] -> CmmCallTarget -> CmmHintFormals -> CmmActuals -> [CmmStmt]
foreignCall uniques call results arguments =
    arg_stmts ++
    saveThreadState ++
    caller_save ++
    [CmmCall (CmmForeignCall suspendThread CCallConv)
		 [ (id,PtrHint) ]
		 [ (CmmReg (CmmGlobal BaseReg), PtrHint) ]
		 CmmUnsafe,
     CmmCall call results new_args CmmUnsafe,
     CmmCall (CmmForeignCall resumeThread CCallConv)
                 [ (new_base, PtrHint) ]
		 [ (CmmReg (CmmLocal id), PtrHint) ]
		 CmmUnsafe,
     -- Assign the result to BaseReg: we
     -- might now have a different Capability!
     CmmAssign (CmmGlobal BaseReg) (CmmReg (CmmLocal new_base))] ++
    caller_load ++
    loadThreadState tso_unique ++
    [CmmJump (CmmReg spReg) (map (formal_to_actual . fst) results)]
    where
      (_, arg_stmts, new_args) =
          loadArgsIntoTemps argument_uniques arguments
      (caller_save, caller_load) =
          callerSaveVolatileRegs (Just [{-only system regs-}])
      new_base = LocalReg base_unique (cmmRegRep (CmmGlobal BaseReg)) KindNonPtr
      id = LocalReg id_unique wordRep KindNonPtr
      tso_unique : base_unique : id_unique : argument_uniques = uniques

-- -----------------------------------------------------------------------------
-- Save/restore the thread state in the TSO

suspendThread = CmmLit (CmmLabel (mkRtsCodeLabel SLIT("suspendThread")))
resumeThread  = CmmLit (CmmLabel (mkRtsCodeLabel SLIT("resumeThread")))

-- This stuff can't be done in suspendThread/resumeThread, because it
-- refers to global registers which aren't available in the C world.

saveThreadState =
  -- CurrentTSO->sp = Sp;
  [CmmStore (cmmOffset stgCurrentTSO tso_SP) stgSp,
  closeNursery] ++
  -- and save the current cost centre stack in the TSO when profiling:
  if opt_SccProfilingOn
  then [CmmStore (cmmOffset stgCurrentTSO tso_CCCS) curCCS]
  else []

   -- CurrentNursery->free = Hp+1;
closeNursery = CmmStore nursery_bdescr_free (cmmOffsetW stgHp 1)

loadThreadState tso_unique =
  [
	-- tso = CurrentTSO;
  	CmmAssign (CmmLocal tso) stgCurrentTSO,
	-- Sp = tso->sp;
	CmmAssign sp (CmmLoad (cmmOffset (CmmReg (CmmLocal tso)) tso_SP)
	                      wordRep),
	-- SpLim = tso->stack + RESERVED_STACK_WORDS;
	CmmAssign spLim (cmmOffsetW (cmmOffset (CmmReg (CmmLocal tso)) tso_STACK)
			            rESERVED_STACK_WORDS)
  ] ++
  openNursery ++
  -- and load the current cost centre stack from the TSO when profiling:
  if opt_SccProfilingOn 
  then [CmmStore curCCSAddr 
	(CmmLoad (cmmOffset (CmmReg (CmmLocal tso)) tso_CCCS) wordRep)]
  else []
  where tso = LocalReg tso_unique wordRep KindNonPtr -- TODO FIXME NOW


openNursery = [
        -- Hp = CurrentNursery->free - 1;
	CmmAssign hp (cmmOffsetW (CmmLoad nursery_bdescr_free wordRep) (-1)),

        -- HpLim = CurrentNursery->start + 
	--		CurrentNursery->blocks*BLOCK_SIZE_W - 1;
	CmmAssign hpLim
	    (cmmOffsetExpr
		(CmmLoad nursery_bdescr_start wordRep)
		(cmmOffset
		  (CmmMachOp mo_wordMul [
		    CmmMachOp (MO_S_Conv I32 wordRep)
		      [CmmLoad nursery_bdescr_blocks I32],
		    CmmLit (mkIntCLit bLOCK_SIZE)
		   ])
		  (-1)
		)
	    )
   ]


nursery_bdescr_free   = cmmOffset stgCurrentNursery oFFSET_bdescr_free
nursery_bdescr_start  = cmmOffset stgCurrentNursery oFFSET_bdescr_start
nursery_bdescr_blocks = cmmOffset stgCurrentNursery oFFSET_bdescr_blocks

tso_SP    = tsoFieldB     oFFSET_StgTSO_sp
tso_STACK = tsoFieldB     oFFSET_StgTSO_stack
tso_CCCS  = tsoProfFieldB oFFSET_StgTSO_CCCS

-- The TSO struct has a variable header, and an optional StgTSOProfInfo in
-- the middle.  The fields we're interested in are after the StgTSOProfInfo.
tsoFieldB :: ByteOff -> ByteOff
tsoFieldB off
  | opt_SccProfilingOn = off + sIZEOF_StgTSOProfInfo + fixedHdrSize * wORD_SIZE
  | otherwise          = off + fixedHdrSize * wORD_SIZE

tsoProfFieldB :: ByteOff -> ByteOff
tsoProfFieldB off = off + fixedHdrSize * wORD_SIZE

stgSp		  = CmmReg sp
stgHp		  = CmmReg hp
stgCurrentTSO	  = CmmReg currentTSO
stgCurrentNursery = CmmReg currentNursery

sp		  = CmmGlobal Sp
spLim		  = CmmGlobal SpLim
hp		  = CmmGlobal Hp
hpLim		  = CmmGlobal HpLim
currentTSO	  = CmmGlobal CurrentTSO
currentNursery 	  = CmmGlobal CurrentNursery

-----------------------------------------------------------------------------
-- Functions that generate CmmStmt sequences
-- for packing/unpacking continuations
-- and entering/exiting functions

tail_call :: WordOff -> CmmExpr -> CmmActuals -> [CmmStmt]
tail_call spRel target arguments
  = store_arguments ++ adjust_sp_reg spRel ++ jump where
    store_arguments =
        [stack_put spRel expr offset
         | ((expr, _), StackParam offset) <- argument_formats] ++
        [global_put expr global
         | ((expr, _), RegisterParam global) <- argument_formats]
    jump = [CmmJump target arguments]

    argument_formats = assignArguments (cmmExprRep . fst) arguments

adjust_sp_reg spRel =
    if spRel == 0
    then []
    else [CmmAssign spReg (CmmRegOff spReg (spRel*wORD_SIZE))]

assign_gc_stack_use stack_use arg_stack max_frame_size =
    if max_frame_size > arg_stack
    then [CmmAssign stack_use (CmmRegOff spReg (-max_frame_size*wORD_SIZE))]
    else [CmmAssign stack_use (CmmReg spLimReg)]
         -- Trick the optimizer into eliminating the branch for us
  
gc_stack_check :: BlockId -> WordOff -> [CmmStmt]
gc_stack_check gc_block max_frame_size
  = check_stack_limit where
    check_stack_limit = [
     CmmCondBranch
     (CmmMachOp (MO_U_Lt $ cmmRegRep spReg)
                    [CmmRegOff spReg (-max_frame_size*wORD_SIZE),
                     CmmReg spLimReg])
     gc_block]


pack_continuation :: Bool               -- ^ Whether to set the top/header
                                        -- of the stack.  We only need to
                                        -- set it if we are calling down
                                        -- as opposed to continuation
                                        -- adaptors.
                  -> ContinuationFormat -- ^ The current format
                  -> ContinuationFormat -- ^ The return point format
                  -> [CmmStmt]
pack_continuation allow_header_set
                      (ContinuationFormat _ curr_id curr_frame_size _)
                      (ContinuationFormat _ cont_id cont_frame_size live_regs)
  = pack_frame curr_frame_size cont_frame_size maybe_header continuation_args
  where
    continuation_args = map (maybe Nothing (Just . CmmReg . CmmLocal))
                            live_regs
    needs_header_set =
        case (curr_id, cont_id) of
          (Just x, Just y) -> x /= y
          _ -> isJust cont_id

    maybe_header = if allow_header_set && needs_header_set
                   then maybe Nothing (Just . CmmLit . CmmLabel . entryLblToInfoLbl) cont_id
                   else Nothing

pack_frame :: WordOff         -- ^ Current frame size
           -> WordOff         -- ^ Next frame size
           -> Maybe CmmExpr   -- ^ Next frame header if any
           -> [Maybe CmmExpr] -- ^ Next frame data
           -> [CmmStmt]
pack_frame curr_frame_size next_frame_size next_frame_header frame_args =
    store_live_values ++ set_stack_header
    where
    -- TODO: only save variables when actually needed
    -- (may be handled by latter pass)
    store_live_values =
        [stack_put spRel expr offset
         | (expr, offset) <- cont_offsets]
    set_stack_header =
        case next_frame_header of
          Nothing -> []
          Just expr -> [stack_put spRel expr 0]

    -- TODO: factor with function_entry and CmmInfo.hs(?)
    cont_offsets = mkOffsets label_size frame_args

    label_size = 1 :: WordOff

    mkOffsets size [] = []
    mkOffsets size (Nothing:exprs) = mkOffsets (size+1) exprs
    mkOffsets size (Just expr:exprs) = (expr, size):mkOffsets (size + width) exprs
        where
          width = machRepByteWidth (cmmExprRep expr) `quot` wORD_SIZE
          -- TODO: it would be better if we had a machRepWordWidth

    spRel = curr_frame_size - next_frame_size


-- Lazy adjustment of stack headers assumes all blocks
-- that could branch to eachother (i.e. control blocks)
-- have the same stack format (this causes a problem
-- only for proc-point).
function_entry :: ContinuationFormat -> [CmmStmt]
function_entry (ContinuationFormat formals _ _ live_regs)
  = load_live_values ++ load_args where
    -- TODO: only save variables when actually needed
    -- (may be handled by latter pass)
    load_live_values =
        [stack_get 0 reg offset
         | (reg, offset) <- curr_offsets]
    load_args =
        [stack_get 0 reg offset
         | (reg, StackParam offset) <- argument_formats] ++
        [global_get reg global
         | (reg, RegisterParam global) <- argument_formats]

    argument_formats = assignArguments (localRegRep) formals

    -- TODO: eliminate copy/paste with pack_continuation
    curr_offsets = mkOffsets label_size live_regs

    label_size = 1 :: WordOff

    mkOffsets size [] = []
    mkOffsets size (Nothing:regs) = mkOffsets (size+1) regs
    mkOffsets size (Just reg:regs) = (reg, size):mkOffsets (size + width) regs
        where
          width = machRepByteWidth (localRegRep reg) `quot` wORD_SIZE
          -- TODO: it would be better if we had a machRepWordWidth

-----------------------------------------------------------------------------
-- Section: Stack and argument register puts and gets
-----------------------------------------------------------------------------
-- TODO: document

-- |Construct a 'CmmStmt' that will save a value on the stack
stack_put :: WordOff            -- ^ Offset from the real 'Sp' that 'offset'
                                -- is relative to (added to offset)
          -> CmmExpr            -- ^ What to store onto the stack
          -> WordOff            -- ^ Where on the stack to store it
                                -- (positive <=> higher addresses)
          -> CmmStmt
stack_put spRel expr offset =
    CmmStore (CmmRegOff spReg (wORD_SIZE*(spRel + offset))) expr

--------------------------------
-- |Construct a 
stack_get :: WordOff
          -> LocalReg
          -> WordOff
          -> CmmStmt
stack_get spRel reg offset =
    CmmAssign (CmmLocal reg)
              (CmmLoad (CmmRegOff spReg (wORD_SIZE*(spRel + offset)))
                       (localRegRep reg))
global_put :: CmmExpr -> GlobalReg -> CmmStmt
global_put expr global = CmmAssign (CmmGlobal global) expr
global_get :: LocalReg -> GlobalReg -> CmmStmt
global_get reg global = CmmAssign (CmmLocal reg) (CmmReg (CmmGlobal global))
