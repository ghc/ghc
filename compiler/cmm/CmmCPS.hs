module CmmCPS (
  -- | Converts C-- with full proceedures and parameters
  -- to a CPS transformed C-- with the stack made manifest.
  cmmCPS
) where

#include "HsVersions.h"

import Cmm
import CmmLint
import PprCmm

import CmmLive
import CmmBrokenBlock
import CmmProcPoint
import CmmCallConv
import CmmCPSGen
import CmmInfo
import CmmUtils

import ClosureInfo
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
       -> [GenCmm CmmStatic CmmInfo CmmStmt]    -- ^ Input C-- with Proceedures
       -> IO [GenCmm CmmStatic [CmmStatic] CmmStmt] -- ^ Output CPS transformed C--
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

stg_gc_gen = mkRtsApFastLabel SLIT("gen_cg_TODO") --panic "Need the label for gc"
make_gc_block block_id fun_label formals safety = BasicBlock block_id stmts
    where
      stmts = [CmmCall stg_gc_gen_target [] [] safety,
               CmmJump fun_expr actuals]
      stg_gc_gen_target =
          CmmForeignCall (CmmLit (CmmLabel stg_gc_gen)) CmmCallConv
      actuals = map (\x -> (CmmReg (CmmLocal x), NoHint)) formals
      fun_expr = CmmLit (CmmLabel fun_label)

make_gc_check stack_use gc_block =
    [CmmCondBranch
     (CmmMachOp (MO_U_Lt $ cmmRegRep spReg)
                    [CmmReg stack_use, CmmReg spLimReg])
    gc_block]

force_gc_block old_info stack_use block_id fun_label formals =
    case old_info of
      CmmNonInfo (Just existing) -> (old_info, [], make_gc_check stack_use existing)
      CmmInfo _ (Just existing) _ _ -> (old_info, [], make_gc_check stack_use existing)
      CmmNonInfo Nothing
          -> (CmmNonInfo (Just block_id),
              [make_gc_block block_id fun_label formals (CmmSafe NoC_SRT)],
              make_gc_check stack_use block_id)
      CmmInfo prof Nothing type_tag type_info
          -> (CmmInfo prof (Just block_id) type_tag type_info,
              [make_gc_block block_id fun_label formals (CmmSafe srt)],
              make_gc_check stack_use block_id)
             where
               srt = case type_info of
                       ConstrInfo _ _ _ -> NoC_SRT
                       FunInfo _ srt' _ _ _ _ -> srt'
                       ThunkInfo _ srt' -> srt'
                       ThunkSelectorInfo _ srt' -> srt'
                       ContInfo _ srt' -> srt'

-----------------------------------------------------------------------------
-- |CPS a single CmmTop (proceedure)
-- Only 'CmmProc' are transformed 'CmmData' will be left alone.
-----------------------------------------------------------------------------

cpsProc :: UniqSupply 
        -> GenCmmTop CmmStatic CmmInfo CmmStmt     -- ^Input proceedure
        -> [GenCmmTop CmmStatic [CmmStatic] CmmStmt]   -- ^Output proceedure and continuations
cpsProc uniqSupply (CmmData sec dat) = [CmmData sec dat]
cpsProc uniqSupply (CmmProc info ident params blocks) = info_procs
    where
      (uniqSupply1, uniqSupply2) = splitUniqSupply uniqSupply
      uniques :: [[Unique]]
      uniques = map uniqsFromSupply $ listSplitUniqSupply uniqSupply1
      (gc_unique:stack_use_unique:info_uniques):adaptor_uniques:block_uniques = uniques
      proc_uniques = map (map uniqsFromSupply . listSplitUniqSupply) $ listSplitUniqSupply uniqSupply2

      stack_use = CmmLocal (LocalReg stack_use_unique (cmmRegRep spReg) KindPtr)

      -- TODO: doc
      forced_gc :: (CmmInfo, [CmmBasicBlock], [CmmStmt])
      forced_gc = force_gc_block info stack_use (BlockId gc_unique) ident params
      (forced_info, gc_blocks, check_stmts) = forced_gc

      forced_blocks =
          case blocks of
            (BasicBlock id stmts) : bs ->
                (BasicBlock id (check_stmts ++ stmts)) : (bs ++ gc_blocks)
            [] -> [] -- If there is no code then we don't need a stack check

      forced_gc_id = case forced_info of
                       CmmNonInfo (Just x) -> x
                       CmmInfo _ (Just x) _ _ -> x

      -- Break the block at each function call.
      -- The part after the function call will have to become a continuation.
      broken_blocks :: ([(BlockId, ContFormat)], [BrokenBlock])
      broken_blocks =
          (\x -> (concatMap fst x, concatMap snd x)) $
          zipWith3 (breakBlock [forced_gc_id])
                     block_uniques
                     forced_blocks
                     (FunctionEntry forced_info ident params :
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
      formats' :: (WordOff, [(CLabel, ContinuationFormat)])
      formats' = processFormats formats continuations

      -- Update the info table data on the continuations with
      -- the selected stack formats.
      continuations' :: [Continuation CmmInfo]
      continuations' = map (applyContinuationFormat (snd formats')) continuations

      -- Do the actual CPS transform.
      cps_procs :: [CmmTop]
      cps_procs = zipWith (continuationToProc formats' stack_use) proc_uniques continuations'

      -- Convert the info tables from CmmInfo to [CmmStatic]
      -- We might want to put this in another pass eventually
      info_procs :: [RawCmmTop]
      info_procs = concat (zipWith mkInfoTable info_uniques cps_procs)

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
      blocks' = map (lookupWithDefaultUFM blocks (panic "TODO")) new_blocks
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
      start_block = lookupWithDefaultUFM blocks (panic "TODO") start
      unknown_block = panic "unknown block in gatherBlocksIntoContinuation"
      children_blocks = map (lookupWithDefaultUFM blocks (panic "TODO")) (uniqSetToList children)
      body = start_block : children_blocks

      -- We can't properly annotate the continuation's stack parameters
      -- at this point because this is before stack selection
      -- but we want to keep the C_SRT around so we use 'Either'.
      info_table = case start_block_entry of
                     FunctionEntry info _ _ -> Right info
                     ContinuationEntry _ srt _ -> Left srt
                     ControlEntry -> Right (CmmNonInfo Nothing)

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
                     lookupWithDefaultUFM live unknown_block start
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
                          (Right (CmmInfo _ _ _ (ContInfo format srt)))
                          label formals _ _) =
          (formals, Just label, format)
      -- Either user written non-continuation code
      -- or CPS generated proc-points
      selectContinuationFormat' (Continuation (Right _) _ formals _ _) =
          (formals, Nothing, [])
      -- CPS generated continuations
      selectContinuationFormat' (Continuation (Left srt) label formals _ blocks) =
          -- TODO: assumes the first block is the entry block
          let ident = brokenBlockId $ head blocks -- TODO: CLabel isn't a uniquable, but we need a better way than this
          in (formals,
              Just label,
              map Just $ uniqSetToList $
              lookupWithDefaultUFM live unknown_block ident)

      unknown_block = panic "unknown BlockId in selectContinuationFormat"

processFormats :: [(CLabel, (CmmFormals, Maybe CLabel, [Maybe LocalReg]))]
               -> [Continuation (Either C_SRT CmmInfo)]
               -> (WordOff, [(CLabel, ContinuationFormat)])
processFormats formats continuations = (max_size, formats')
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

      -- TODO: get rid of "+ 1" etc.
      label_size = 1 :: WordOff

      stack_size [] = 0
      stack_size (Nothing:formats) = 1 + stack_size formats -- one dead word
      stack_size (Just reg:formats) = width + stack_size formats
          where
            width = machRepByteWidth (localRegRep reg) `quot` wORD_SIZE
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
          argumentsSize (cmmExprRep . fst) args
      final_arg_size (FinalJump _ args) =
          argumentsSize (cmmExprRep . fst) args
      final_arg_size (FinalCall next _ _ args _ True) = 0
      final_arg_size (FinalCall next _ _ args _ False) =
          -- We have to account for the stack used when we build a frame
          -- for the *next* continuation from *this* continuation
          argumentsSize (cmmExprRep . fst) args +
          continuation_frame_size next_format
          where 
            next_format = maybe unknown_format id $ lookup next' formats
            next' = mkReturnPtLabel $ getUnique next

      final_arg_size _ = 0

      stmt_arg_size (CmmJump _ args) =
          argumentsSize (cmmExprRep . fst) args
      stmt_arg_size (CmmCall _ _ _ (CmmSafe _)) =
          panic "Safe call in processFormats"
      stmt_arg_size (CmmReturn _) =
          panic "CmmReturn in processFormats"
      stmt_arg_size _ = 0

-----------------------------------------------------------------------------
applyContinuationFormat :: [(CLabel, ContinuationFormat)]
                 -> Continuation (Either C_SRT CmmInfo)
                 -> Continuation CmmInfo

-- User written continuations
applyContinuationFormat formats (Continuation
                          (Right (CmmInfo prof gc tag (ContInfo _ srt)))
                          label formals is_gc blocks) =
    Continuation (CmmInfo prof gc tag (ContInfo format srt))
                 label formals is_gc blocks
    where
      format = continuation_stack $ maybe unknown_block id $ lookup label formats
      unknown_block = panic "unknown BlockId in applyContinuationFormat"

-- Either user written non-continuation code or CPS generated proc-point
applyContinuationFormat formats (Continuation
                          (Right info) label formals is_gc blocks) =
    Continuation info label formals is_gc blocks

-- CPS generated continuations
applyContinuationFormat formats (Continuation
                          (Left srt) label formals is_gc blocks) =
    Continuation (CmmInfo prof gc tag (ContInfo (continuation_stack $ format) srt))
                 label formals is_gc blocks
    where
      gc = Nothing -- Generated continuations never need a stack check
      -- TODO prof: this is the same as the current implementation
      -- but I think it could be improved
      prof = ProfilingInfo zeroCLit zeroCLit
      tag = rET_SMALL -- cmmToRawCmm may convert it to rET_BIG
      format = maybe unknown_block id $ lookup label formats
      unknown_block = panic "unknown BlockId in applyContinuationFormat"

