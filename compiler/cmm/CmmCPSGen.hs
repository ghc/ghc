module CmmCPSGen (
  -- | Converts continuations into full proceedures.
  -- The main work of the CPS transform that everything else is setting-up.
  continuationToProc,
  Continuation(..), continuationLabel,
  ContinuationFormat(..),
) where

import BlockId
import Cmm
import CLabel
import CmmBrokenBlock -- Data types only
import CmmUtils
import CmmCallConv
import ClosureInfo

import CgProf
import CgUtils
import CgInfoTbls
import SMRep
import ForeignCall

import Module
import Constants
import StaticFlags
import Unique
import Data.Maybe
import FastString

import Panic

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

continuationLabel :: Continuation (Either C_SRT CmmInfo) -> CLabel
continuationLabel (Continuation _ l _ _ _) = l
data Continuation info =
  Continuation
     info              -- Left <=> Continuation created by the CPS
                       -- Right <=> Function or Proc point
     CLabel            -- Used to generate both info & entry labels
     CmmFormals        -- Argument locals live on entry (C-- procedure params)
     Bool              -- True <=> GC block so ignore stack size
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
                   -> [[[Unique]]]
                   -> Continuation CmmInfo
                   -> CmmTop
continuationToProc (max_stack, update_frame_size, formats) stack_use uniques
                   (Continuation info label formals _ blocks) =
    CmmProc info label formals (ListGraph blocks')
    where
      blocks' = concat $ zipWith3 continuationToProc' uniques blocks
                         (True : repeat False)
      curr_format = maybe unknown_block id $ lookup label formats
      unknown_block = panic "unknown BlockId in continuationToProc"
      curr_stack = continuation_frame_size curr_format
      arg_stack = argumentsSize localRegType formals

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

      continuationToProc' :: [[Unique]]
                          -> BrokenBlock
                          -> Bool
                          -> [CmmBasicBlock]
      continuationToProc' uniques (BrokenBlock ident entry stmts _ exit) is_entry =
          prefix_blocks ++ [BasicBlock ident fixed_main_stmts] ++ concat new_blocks
          where
            prefix_blocks =
                if is_entry
                then [BasicBlock
                      (BlockId prefix_unique)
                      (param_stmts ++ [CmmBranch ident])]
                else []

            (prefix_unique : call_uniques) : new_block_uniques = uniques
            toCLabel = mkReturnPtLabel . getUnique

            block_for_branch :: Unique -> BlockId -> (BlockId, [CmmBasicBlock])
            block_for_branch unique next
                -- branches to the current function don't have to jump
                | (mkReturnPtLabel $ getUnique next) == label
                = (next, [])

                -- branches to any other function have to jump
                | (Just cont_format) <- lookup (toCLabel next) formats
                = let
                    new_next = BlockId unique
                    cont_stack = continuation_frame_size cont_format
                    arguments = map formal_to_actual (continuation_formals cont_format)
                  in (new_next,
                     [BasicBlock new_next $
                      pack_continuation curr_format cont_format ++
                      tail_call (curr_stack - cont_stack)
                                (CmmLit $ CmmLabel $ toCLabel next)
                                arguments])

                -- branches to blocks in the current function don't have to jump
                | otherwise
                = (next, [])

            -- Wrapper for block_for_branch for when the target
            -- is inside a 'Maybe'.
            block_for_branch' :: Unique -> Maybe BlockId -> (Maybe BlockId, [CmmBasicBlock])
            block_for_branch' _ Nothing = (Nothing, [])
            block_for_branch' unique (Just next) = (Just new_next, new_blocks)
              where (new_next, new_blocks) = block_for_branch unique next

            -- If the target of a switch, branch or cond branch becomes a proc point
            -- then we have to make a new block what will then *jump* to the original target.
            proc_point_fix unique (CmmCondBranch test target)
                = (CmmCondBranch test new_target, new_blocks)
                  where (new_target, new_blocks) = block_for_branch (head unique) target
            proc_point_fix unique (CmmSwitch test targets)
                = (CmmSwitch test new_targets, concat new_blocks)
                  where (new_targets, new_blocks) =
                            unzip $ zipWith block_for_branch' unique targets
            proc_point_fix unique (CmmBranch target)
                = (CmmBranch new_target, new_blocks)
                  where (new_target, new_blocks) = block_for_branch (head unique) target
            proc_point_fix _ other = (other, [])

            (fixed_main_stmts, new_blocks) = unzip $ zipWith proc_point_fix new_block_uniques main_stmts
            main_stmts =
                case entry of
                  FunctionEntry _ _ _ ->
                      -- The statements for an update frame must come /after/
                      -- the GC check that was added at the beginning of the
                      -- CPS pass.  So we have do edit the statements a bit.
                      -- This depends on the knowledge that the statements in
                      -- the first block are only the GC check.  That's
                      -- fragile but it works for now.
                      gc_stmts ++ stmts ++ update_stmts ++ postfix_stmts
                  ControlEntry -> stmts ++ postfix_stmts
                  ContinuationEntry _ _ _ -> stmts ++ postfix_stmts
            postfix_stmts = case exit of
                        -- Branches and switches may get modified by proc_point_fix
                        FinalBranch next -> [CmmBranch next]
                        FinalSwitch expr targets -> [CmmSwitch expr targets]

                        -- A return is a tail call to the stack top
                        FinalReturn arguments ->
                            tail_call curr_stack
                                (entryCode (CmmLoad (CmmReg spReg) bWord))
                                arguments

                        -- A tail call
                        FinalJump target arguments ->
                            tail_call curr_stack target arguments

                        -- A regular Cmm function call
                        FinalCall next (CmmCallee target CmmCallConv)
                            _ arguments _ _ _ ->
                                pack_continuation curr_format cont_format ++
                                tail_call (curr_stack - cont_stack)
                                              target arguments
                            where
                              cont_format = maybe unknown_block id $
                                            lookup (mkReturnPtLabel $ getUnique next) formats
                              cont_stack = continuation_frame_size cont_format

                        -- A safe foreign call
                        FinalCall _ (CmmCallee target conv)
                            results arguments _ _ _ ->
                                target_stmts ++
                                foreignCall call_uniques' (CmmCallee new_target conv)
                                            results arguments
                            where
                              (call_uniques', target_stmts, new_target) =
                                  maybeAssignTemp call_uniques target

                        -- A safe prim call
                        FinalCall _ (CmmPrim target)
                            results arguments _ _ _ ->
                                foreignCall call_uniques (CmmPrim target)
                                            results arguments

formal_to_actual :: LocalReg -> CmmHinted CmmExpr
formal_to_actual reg = CmmHinted (CmmReg (CmmLocal reg)) NoHint

foreignCall :: [Unique] -> CmmCallTarget -> HintedCmmFormals -> HintedCmmActuals -> [CmmStmt]
foreignCall uniques call results arguments =
    arg_stmts ++
    saveThreadState ++
    caller_save ++
    [CmmCall (CmmCallee suspendThread CCallConv)
		 [ CmmHinted id AddrHint ]
		 [ CmmHinted (CmmReg (CmmGlobal BaseReg)) AddrHint
		 -- XXX: allow for interruptible suspension
		 , CmmHinted (CmmLit (CmmInt 0 wordWidth)) NoHint ]
		 CmmUnsafe
                 CmmMayReturn,
     CmmCall call results new_args CmmUnsafe CmmMayReturn,
     CmmCall (CmmCallee resumeThread CCallConv)
                 [ CmmHinted new_base AddrHint ]
		 [ CmmHinted (CmmReg (CmmLocal id)) AddrHint ]
		 CmmUnsafe
                 CmmMayReturn,
     -- Assign the result to BaseReg: we
     -- might now have a different Capability!
     CmmAssign (CmmGlobal BaseReg) (CmmReg (CmmLocal new_base))] ++
    caller_load ++
    loadThreadState tso_unique ++
    [CmmJump (CmmReg spReg) (map (formal_to_actual . hintlessCmm) results)]
    where
      (_, arg_stmts, new_args) =
          loadArgsIntoTemps argument_uniques arguments
      (caller_save, caller_load) =
          callerSaveVolatileRegs (Just [{-only system regs-}])
      new_base = LocalReg base_unique (cmmRegType (CmmGlobal BaseReg))
      id = LocalReg id_unique bWord
      tso_unique : base_unique : id_unique : argument_uniques = uniques

-- -----------------------------------------------------------------------------
-- Save/restore the thread state in the TSO

suspendThread, resumeThread :: CmmExpr
suspendThread = CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "suspendThread")))
resumeThread  = CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "resumeThread")))

-- This stuff can't be done in suspendThread/resumeThread, because it
-- refers to global registers which aren't available in the C world.

saveThreadState :: [CmmStmt]
saveThreadState =
  -- CurrentTSO->sp = Sp;
  [CmmStore (cmmOffset stgCurrentTSO tso_SP) stgSp,
  closeNursery] ++
  -- and save the current cost centre stack in the TSO when profiling:
  if opt_SccProfilingOn
  then [CmmStore (cmmOffset stgCurrentTSO tso_CCCS) curCCS]
  else []

   -- CurrentNursery->free = Hp+1;
closeNursery :: CmmStmt
closeNursery = CmmStore nursery_bdescr_free (cmmOffsetW stgHp 1)

loadThreadState :: Unique -> [CmmStmt]
loadThreadState tso_unique =
  [
	-- tso = CurrentTSO;
  	CmmAssign (CmmLocal tso) stgCurrentTSO,
	-- Sp = tso->sp;
	CmmAssign sp (CmmLoad (cmmOffset (CmmReg (CmmLocal tso)) tso_SP)
	                      bWord),
	-- SpLim = tso->stack + RESERVED_STACK_WORDS;
	CmmAssign spLim (cmmOffsetW (cmmOffset (CmmReg (CmmLocal tso)) tso_STACK)
			            rESERVED_STACK_WORDS)
  ] ++
  openNursery ++
  -- and load the current cost centre stack from the TSO when profiling:
  if opt_SccProfilingOn 
  then [CmmStore curCCSAddr 
	(CmmLoad (cmmOffset (CmmReg (CmmLocal tso)) tso_CCCS) bWord)]
  else []
  where tso = LocalReg tso_unique bWord -- TODO FIXME NOW


openNursery :: [CmmStmt]
openNursery = [
        -- Hp = CurrentNursery->free - 1;
	CmmAssign hp (cmmOffsetW (CmmLoad nursery_bdescr_free bWord) (-1)),

        -- HpLim = CurrentNursery->start + 
	--		CurrentNursery->blocks*BLOCK_SIZE_W - 1;
	CmmAssign hpLim
	    (cmmOffsetExpr
		(CmmLoad nursery_bdescr_start bWord)
		(cmmOffset
		  (CmmMachOp mo_wordMul [
		    CmmMachOp (MO_SS_Conv W32 wordWidth)
		      [CmmLoad nursery_bdescr_blocks b32],
		    CmmLit (mkIntCLit bLOCK_SIZE)
		   ])
		  (-1)
		)
	    )
   ]


nursery_bdescr_free, nursery_bdescr_start, nursery_bdescr_blocks :: CmmExpr
nursery_bdescr_free   = cmmOffset stgCurrentNursery oFFSET_bdescr_free
nursery_bdescr_start  = cmmOffset stgCurrentNursery oFFSET_bdescr_start
nursery_bdescr_blocks = cmmOffset stgCurrentNursery oFFSET_bdescr_blocks

tso_SP, tso_STACK, tso_CCCS :: ByteOff
tso_SP    = tsoFieldB     undefined --oFFSET_StgTSO_sp
tso_STACK = tsoFieldB     undefined --oFFSET_StgTSO_stack
tso_CCCS  = tsoProfFieldB oFFSET_StgTSO_CCCS

-- The TSO struct has a variable header, and an optional StgTSOProfInfo in
-- the middle.  The fields we're interested in are after the StgTSOProfInfo.
tsoFieldB :: ByteOff -> ByteOff
tsoFieldB off
  | opt_SccProfilingOn = off + sIZEOF_StgTSOProfInfo + fixedHdrSize * wORD_SIZE
  | otherwise          = off + fixedHdrSize * wORD_SIZE

tsoProfFieldB :: ByteOff -> ByteOff
tsoProfFieldB off = off + fixedHdrSize * wORD_SIZE

stgSp, stgHp, stgCurrentTSO, stgCurrentNursery :: CmmExpr
stgSp		  = CmmReg sp
stgHp		  = CmmReg hp
stgCurrentTSO	  = CmmReg currentTSO
stgCurrentNursery = CmmReg currentNursery

sp, spLim, hp, hpLim, currentTSO, currentNursery :: CmmReg
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

tail_call :: WordOff -> CmmExpr -> HintedCmmActuals -> [CmmStmt]
tail_call spRel target arguments
  = store_arguments ++ adjust_sp_reg spRel ++ jump where
    store_arguments =
        [stack_put spRel expr offset
         | ((CmmHinted expr _), StackParam offset) <- argument_formats] ++
        [global_put expr global
         | ((CmmHinted expr _), RegisterParam global) <- argument_formats]
    jump = [CmmJump target arguments]

    argument_formats = assignArguments (cmmExprType . hintlessCmm) arguments

adjust_sp_reg :: Int -> [CmmStmt]
adjust_sp_reg spRel =
    if spRel == 0
    then []
    else [CmmAssign spReg (CmmRegOff spReg (spRel*wORD_SIZE))]

assign_gc_stack_use :: CmmReg -> Int -> Int -> [CmmStmt]
assign_gc_stack_use stack_use arg_stack max_frame_size =
    if max_frame_size > arg_stack
    then [CmmAssign stack_use (CmmRegOff spReg (-max_frame_size*wORD_SIZE))]
    else [CmmAssign stack_use (CmmReg spLimReg)]
         -- Trick the optimizer into eliminating the branch for us
  
{-
UNUSED 2008-12-29

gc_stack_check :: BlockId -> WordOff -> [CmmStmt]
gc_stack_check gc_block max_frame_size
  = check_stack_limit where
    check_stack_limit = [
     CmmCondBranch
     (CmmMachOp (MO_U_Lt (typeWidth (cmmRegType spReg)))
                [CmmRegOff spReg (-max_frame_size*wORD_SIZE),
                     CmmReg spLimReg])
     gc_block]
-}

pack_continuation :: ContinuationFormat -- ^ The current format
                  -> ContinuationFormat -- ^ The return point format
                  -> [CmmStmt]
pack_continuation (ContinuationFormat _ curr_id curr_frame_size _)
                  (ContinuationFormat _ cont_id cont_frame_size live_regs)
  = pack_frame curr_frame_size cont_frame_size maybe_header continuation_args
  where
    continuation_args = map (maybe Nothing (Just . CmmReg . CmmLocal))
                            live_regs
    needs_header_set =
        case (curr_id, cont_id) of
          (Just x, Just y) -> x /= y
          _ -> isJust cont_id

    maybe_header = if needs_header_set
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

    mkOffsets _    [] = []
    mkOffsets size (Nothing:exprs) = mkOffsets (size+1) exprs
    mkOffsets size (Just expr:exprs) = (expr, size):mkOffsets (size + width) exprs
        where
          width = (widthInBytes $ typeWidth $ cmmExprType expr) `quot` wORD_SIZE
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

    argument_formats = assignArguments (localRegType) formals

    -- TODO: eliminate copy/paste with pack_continuation
    curr_offsets = mkOffsets label_size live_regs

    label_size = 1 :: WordOff

    mkOffsets _    [] = []
    mkOffsets size (Nothing:regs) = mkOffsets (size+1) regs
    mkOffsets size (Just reg:regs) = (reg, size):mkOffsets (size + width) regs
        where
          width = (widthInBytes $ typeWidth $ localRegType reg) `quot` wORD_SIZE
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
                       (localRegType reg))
global_put :: CmmExpr -> GlobalReg -> CmmStmt
global_put expr global = CmmAssign (CmmGlobal global) expr
global_get :: LocalReg -> GlobalReg -> CmmStmt
global_get reg global = CmmAssign (CmmLocal reg) (CmmReg (CmmGlobal global))
