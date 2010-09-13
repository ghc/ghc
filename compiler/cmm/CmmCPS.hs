module CmmCPS (
  -- | Converts C-- with full proceedures and parameters
  -- to a CPS transformed C-- with the stack made manifest.
  cmmCPS
) where

#include "HsVersions.h"

import BlockId
import Cmm
import CmmLint
import PprCmm

import CmmLive
import CmmBrokenBlock
import CmmProcPoint
import CmmCallConv
import CmmCPSGen
import CmmUtils

import ClosureInfo
import CLabel
import SMRep
import Constants

import DynFlags
import ErrUtils
import Maybes
import Outputable
import UniqSupply
import UniqSet
import Unique

import Control.Monad

-----------------------------------------------------------------------------
-- |Top level driver for the CPS pass
-----------------------------------------------------------------------------
cmmCPS :: DynFlags -- ^ Dynamic flags: -dcmm-lint -ddump-cps-cmm
       -> [Cmm]    -- ^ Input C-- with Proceedures
       -> IO [Cmm] -- ^ Output CPS transformed C--
cmmCPS dflags cmm_with_calls
  = do	{ when (dopt Opt_DoCmmLinting dflags) $
	       do showPass dflags "CmmLint"
		  case firstJust $ map cmmLint cmm_with_calls of
		    Just err -> do printDump err
				   ghcExit dflags 1
		    Nothing  -> return ()
	; showPass dflags "CPS"

  -- TODO: more lint checking
  --        check for use of branches to non-existant blocks
  --        check for use of Sp, SpLim, R1, R2, etc.

	; uniqSupply <- mkSplitUniqSupply 'p'
	; let supplies = listSplitUniqSupply uniqSupply
	; let cpsd_cmm = zipWith doCpsProc supplies cmm_with_calls

	; dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "CPS Cmm" (pprCmms cpsd_cmm)

  -- TODO: add option to dump Cmm to file

	; return cpsd_cmm }


-----------------------------------------------------------------------------
-- |CPS a single CmmTop (proceedure)
-- Only 'CmmProc' are transformed 'CmmData' will be left alone.
-----------------------------------------------------------------------------

doCpsProc :: UniqSupply -> Cmm -> Cmm
doCpsProc s (Cmm c) 
  = Cmm $ concat $ zipWith cpsProc (listSplitUniqSupply s) c

cpsProc :: UniqSupply 
        -> CmmTop     -- ^Input procedure
        -> [CmmTop]   -- ^Output procedures; 
		      --   a single input procedure is converted to
		      --   multiple output procedures

-- Data blocks don't need to be CPS transformed
cpsProc _ proc@(CmmData _ _) = [proc]

-- Empty functions just don't work with the CPS algorithm, but
-- they don't need the transformation anyway so just output them directly
cpsProc _ proc@(CmmProc _ _ _ (ListGraph []))
  = pprTrace "cpsProc: unexpected empty proc" (ppr proc) [proc]

-- CPS transform for those procs that actually need it
-- The plan is this:
--
--   * Introduce a stack-check block as the first block
--   * The first blocks gets a FunctionEntry; the rest are ControlEntry
--   * Now break each block into a bunch of blocks (at call sites); 
--	all but the first will be ContinuationEntry
--
cpsProc uniqSupply (CmmProc info ident params (ListGraph blocks)) = cps_procs
    where
      -- We need to be generating uniques for several things.
      -- We could make this function monadic to handle that
      -- but since there is no other reason to make it monadic,
      -- we instead will just split them all up right here.
      (uniqSupply1, uniqSupply2) = splitUniqSupply uniqSupply
      uniques :: [[Unique]]
      uniques = map uniqsFromSupply $ listSplitUniqSupply uniqSupply1
      (stack_check_block_unique:stack_use_unique:adaptor_uniques) :
       block_uniques = uniques
      proc_uniques = map (map (map uniqsFromSupply . listSplitUniqSupply) . listSplitUniqSupply) $ listSplitUniqSupply uniqSupply2

      stack_use = CmmLocal (LocalReg stack_use_unique (cmmRegType spReg))
      stack_check_block_id = BlockId stack_check_block_unique
      stack_check_block = make_stack_check stack_check_block_id info stack_use (blockId $ head blocks)

      forced_blocks = stack_check_block : blocks

      CmmInfo maybe_gc_block_id update_frame _ = info

      -- Break the block at each function call.
      -- The part after the function call will have to become a continuation.
      broken_blocks :: ([(BlockId, ContFormat)], [BrokenBlock])
      broken_blocks =
          (\x -> (concatMap fst x, concatMap snd x)) $
          zipWith3 (breakBlock (maybeToList maybe_gc_block_id))
                     block_uniques
                     forced_blocks
                     (FunctionEntry info ident params :
                      repeat ControlEntry)

      f' = selectContinuations (fst broken_blocks)
      broken_blocks' = map (makeContinuationEntries f') $
                       concat $
                       zipWith (adaptBlockToFormat f')
                               adaptor_uniques
                               (snd broken_blocks)

      -- Calculate live variables for each broken block.
      --
      -- Nothing can be live on entry to the first block
      -- so we could take the tail, but for now we wont
      -- to help future proof the code.
      live :: BlockEntryLiveness
      live = cmmLiveness $ map cmmBlockFromBrokenBlock broken_blocks'

      -- Calculate which blocks must be made into full fledged procedures.
      proc_points :: UniqSet BlockId
      proc_points = calculateProcPoints broken_blocks'

      -- Construct a map so we can lookup a broken block by its 'BlockId'.
      block_env :: BlockEnv BrokenBlock
      block_env = blocksToBlockEnv broken_blocks'

      -- Group the blocks into continuations based on the set of proc-points.
      continuations :: [Continuation (Either C_SRT CmmInfo)]
      continuations = map (gatherBlocksIntoContinuation live proc_points block_env)
                          (uniqSetToList proc_points)

      -- Select the stack format on entry to each continuation.
      -- Return the max stack offset and an association list
      --
      -- This is an association list instead of a UniqFM because
      -- CLabel's don't have a 'Uniqueable' instance.
      formats :: [(CLabel,              -- key
                   (CmmFormals,         -- arguments
                    Maybe CLabel,       -- label in top slot
                    [Maybe LocalReg]))] -- slots
      formats = selectContinuationFormat live continuations

      -- Do a little meta-processing on the stack formats such as
      -- getting the individual frame sizes and the maximum frame size
      formats' :: (WordOff, WordOff, [(CLabel, ContinuationFormat)])
      formats'@(_, _, format_list) = processFormats formats update_frame continuations

      -- Update the info table data on the continuations with
      -- the selected stack formats.
      continuations' :: [Continuation CmmInfo]
      continuations' = map (applyContinuationFormat format_list) continuations

      -- Do the actual CPS transform.
      cps_procs :: [CmmTop]
      cps_procs = zipWith (continuationToProc formats' stack_use) proc_uniques continuations'

make_stack_check :: BlockId -> CmmInfo -> CmmReg -> BlockId
                 -> GenBasicBlock CmmStmt
make_stack_check stack_check_block_id info stack_use next_block_id =
    BasicBlock stack_check_block_id $
                   check_stmts ++ [CmmBranch next_block_id]
    where
      check_stmts =
          case info of
            -- If we are given a stack check handler,
            -- then great, well check the stack.
            CmmInfo (Just gc_block) _ _
                -> [CmmCondBranch
                    (CmmMachOp (MO_U_Lt (typeWidth (cmmRegType spReg)))
                     [CmmReg stack_use, CmmReg spLimReg])
                    gc_block]
            -- If we aren't given a stack check handler,
            -- then humph! we just won't check the stack for them.
            CmmInfo Nothing _ _
                -> []
-----------------------------------------------------------------------------

collectNonProcPointTargets ::
    UniqSet BlockId -> BlockEnv BrokenBlock
    -> UniqSet BlockId -> [BlockId] -> UniqSet BlockId
collectNonProcPointTargets proc_points blocks current_targets new_blocks =
    if sizeUniqSet current_targets == sizeUniqSet new_targets
       then current_targets
       else foldl
                (collectNonProcPointTargets proc_points blocks)
                new_targets
                (map (:[]) targets)
    where
      blocks' = map (lookupWithDefaultBEnv blocks (panic "TODO")) new_blocks
      targets =
        -- Note the subtlety that since the extra branch after a call
        -- will always be to a block that is a proc-point,
        -- this subtraction will always remove that case
        uniqSetToList $ (unionManyUniqSets $ map (mkUniqSet . brokenBlockTargets) blocks')
                          `minusUniqSet` proc_points
        -- TODO: remove redundant uniqSetToList
      new_targets = current_targets `unionUniqSets` (mkUniqSet targets)

-- TODO: insert proc point code here
--  * Branches and switches to proc points may cause new blocks to be created
--    (or proc points could leave behind phantom blocks that just jump to them)
--  * Proc points might get some live variables passed as arguments

gatherBlocksIntoContinuation ::
    BlockEntryLiveness -> UniqSet BlockId -> BlockEnv BrokenBlock
    -> BlockId -> Continuation (Either C_SRT CmmInfo)
gatherBlocksIntoContinuation live proc_points blocks start =
  Continuation info_table clabel params is_gc_cont body
    where
      children = (collectNonProcPointTargets proc_points blocks (unitUniqSet start) [start]) `minusUniqSet` (unitUniqSet start)
      start_block = lookupWithDefaultBEnv blocks unknown_block start
      children_blocks = map (lookupWithDefaultBEnv blocks unknown_block) (uniqSetToList children)
      unknown_block :: a    -- Used at more than one type
      unknown_block = panic "unknown block in gatherBlocksIntoContinuation"
      body = start_block : children_blocks

      -- We can't properly annotate the continuation's stack parameters
      -- at this point because this is before stack selection
      -- but we want to keep the C_SRT around so we use 'Either'.
      info_table = case start_block_entry of
                     FunctionEntry info _ _ -> Right info
                     ContinuationEntry _ srt _ -> Left srt
                     ControlEntry -> Right (CmmInfo Nothing Nothing CmmNonInfoTable)

      is_gc_cont = case start_block_entry of
                     FunctionEntry _ _ _ -> False
                     ContinuationEntry _ _ gc_cont -> gc_cont
                     ControlEntry -> False

      start_block_entry = brokenBlockEntry start_block
      clabel = case start_block_entry of
                 FunctionEntry _ label _ -> label
                 _ -> mkReturnPtLabel $ getUnique start
      params = case start_block_entry of
                 FunctionEntry _ _ args -> args
                 ContinuationEntry args _ _ -> args
                 ControlEntry ->
                     uniqSetToList $
                     lookupWithDefaultBEnv live unknown_block start
                     -- it's a proc-point, pass lives in parameter registers

--------------------------------------------------------------------------------
-- For now just select the continuation orders in the order they are in the set with no gaps

selectContinuationFormat :: BlockEnv CmmLive
                  -> [Continuation (Either C_SRT CmmInfo)]
                  -> [(CLabel, (CmmFormals, Maybe CLabel, [Maybe LocalReg]))]
selectContinuationFormat live continuations =
    map (\c -> (continuationLabel c, selectContinuationFormat' c)) continuations
    where
      -- User written continuations
      selectContinuationFormat' (Continuation
                          (Right (CmmInfo _ _ (CmmInfoTable _ _ _ (ContInfo format _))))
                          label formals _ _) =
          (formals, Just label, format)
      -- Either user written non-continuation code
      -- or CPS generated proc-points
      selectContinuationFormat' (Continuation (Right _) _ formals _ _) =
          (formals, Nothing, [])
      -- CPS generated continuations
      selectContinuationFormat' (Continuation (Left _) label formals _ blocks) =
          -- TODO: assumes the first block is the entry block
          let ident = brokenBlockId $ head blocks -- TODO: CLabel isn't a uniquable, but we need a better way than this
          in (formals,
              Just label,
              map Just $ uniqSetToList $
              lookupWithDefaultBEnv live unknown_block ident)

      unknown_block = panic "unknown BlockId in selectContinuationFormat"

processFormats :: [(CLabel, (CmmFormals, Maybe CLabel, [Maybe LocalReg]))]
               -> Maybe UpdateFrame
               -> [Continuation (Either C_SRT CmmInfo)]
               -> (WordOff, WordOff, [(CLabel, ContinuationFormat)])
processFormats formats update_frame continuations =
    (max_size + update_frame_size, update_frame_size, formats')
    where
      max_size = maximum $
                 0 : map (continuationMaxStack formats') continuations
      formats' = map make_format formats
      make_format (label, (formals, top, stack)) =
          (label,
           ContinuationFormat {
             continuation_formals = formals,
             continuation_label = top,
             continuation_frame_size = stack_size stack +
                                if isJust top
                                then label_size
                                else 0,
             continuation_stack = stack })

      update_frame_size = case update_frame of
                            Nothing -> 0
                            (Just (UpdateFrame _ args))
                                -> label_size + update_size args

      update_size [] = 0
      update_size (expr:exprs) = width + update_size exprs
          where
            width = (widthInBytes $ typeWidth $ cmmExprType expr) `quot` wORD_SIZE
            -- TODO: it would be better if we had a machRepWordWidth

      -- TODO: get rid of "+ 1" etc.
      label_size = 1 :: WordOff

      stack_size [] = 0
      stack_size (Nothing:formats) = 1 + stack_size formats -- one dead word
      stack_size (Just reg:formats) = width + stack_size formats
          where
            width = (widthInBytes $ typeWidth $ localRegType reg) `quot` wORD_SIZE
            -- TODO: it would be better if we had a machRepWordWidth

continuationMaxStack :: [(CLabel, ContinuationFormat)]
                     -> Continuation a
                     -> WordOff
continuationMaxStack _ (Continuation _ _ _ True _) = 0
continuationMaxStack formats (Continuation _ label _ False blocks) =
    max_arg_size + continuation_frame_size stack_format
    where
      stack_format = maybe unknown_format id $ lookup label formats
      unknown_format = panic "Unknown format in continuationMaxStack"

      max_arg_size = maximum $ 0 : map block_max_arg_size blocks

      block_max_arg_size block =
          maximum (final_arg_size (brokenBlockExit block) :
                   map stmt_arg_size (brokenBlockStmts block))

      final_arg_size (FinalReturn args) =
          argumentsSize (cmmExprType . hintlessCmm) args
      final_arg_size (FinalJump _ args) =
          argumentsSize (cmmExprType . hintlessCmm) args
      final_arg_size (FinalCall _    _ _ _    _ _ True) = 0
      final_arg_size (FinalCall next _ _ args _ _ False) =
          -- We have to account for the stack used when we build a frame
          -- for the *next* continuation from *this* continuation
          argumentsSize (cmmExprType . hintlessCmm) args +
          continuation_frame_size next_format
          where 
            next_format = maybe unknown_format id $ lookup next' formats
            next' = mkReturnPtLabel $ getUnique next

      final_arg_size _ = 0

      stmt_arg_size (CmmJump _ args) =
          argumentsSize (cmmExprType . hintlessCmm) args
      stmt_arg_size (CmmCall _ _ _ (CmmSafe _) _) =
          panic "Safe call in processFormats"
      stmt_arg_size (CmmReturn _) =
          panic "CmmReturn in processFormats"
      stmt_arg_size _ = 0

-----------------------------------------------------------------------------
applyContinuationFormat :: [(CLabel, ContinuationFormat)]
                 -> Continuation (Either C_SRT CmmInfo)
                 -> Continuation CmmInfo

-- User written continuations
applyContinuationFormat formats
   (Continuation (Right (CmmInfo gc update_frame
                             (CmmInfoTable clos prof tag (ContInfo _ srt))))
                 label formals is_gc blocks) =
    Continuation (CmmInfo gc update_frame (CmmInfoTable clos prof tag (ContInfo format srt)))
                 label formals is_gc blocks
    where
      format = continuation_stack $ maybe unknown_block id $ lookup label formats
      unknown_block = panic "unknown BlockId in applyContinuationFormat"

-- Either user written non-continuation code or CPS generated proc-point
applyContinuationFormat _ (Continuation
                          (Right info) label formals is_gc blocks) =
    Continuation info label formals is_gc blocks

-- CPS generated continuations
applyContinuationFormat formats (Continuation
                          (Left srt) label formals is_gc blocks) =
    Continuation (CmmInfo gc Nothing (CmmInfoTable undefined prof tag (ContInfo (continuation_stack $ format) srt)))
                 label formals is_gc blocks
    where
      gc = Nothing -- Generated continuations never need a stack check
      -- TODO prof: this is the same as the current implementation
      -- but I think it could be improved
      prof = ProfilingInfo zeroCLit zeroCLit
      tag = rET_SMALL -- cmmToRawCmm may convert it to rET_BIG
      format = maybe unknown_block id $ lookup label formats
      unknown_block = panic "unknown BlockId in applyContinuationFormat"

