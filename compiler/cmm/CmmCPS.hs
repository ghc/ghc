module CmmCPS (cmmCPS) where

#include "HsVersions.h"

import Cmm
import CmmLint
import PprCmm

import Dataflow (cmmLivenessComment, cmmLiveness, CmmLive)

import MachOp
import ForeignCall
import CLabel

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

data CPSBlockInfo
  = ControlBlock -- Consider whether a proc-point might want arguments on stack
  | ContinuationBlock [(CmmReg,MachHint)] {- params -}

type ContinuationFormat = [Maybe LocalReg] -- TODO: consider params as part of format

-- A block can be a continuation of a call
-- A block can be a continuation of another block (w/ or w/o joins)
-- A block can be an entry to a function

type CmmParam = [(CmmReg,MachHint)]

-- For now just select the continuation orders in the order they are in the set with no gaps
selectContinuationFormat :: UniqFM {-BlockId-} CmmParam -> UniqFM {-BlockId-} CmmLive -> UniqFM {-BlockId-} ContinuationFormat
selectContinuationFormat param live = mapUFM (map Just . uniqSetToList) live

transformReturn block_infos formats (BasicBlock ident stmts) =
  case last $ init stmts of
    CmmReturn arguments ->
        BasicBlock ident $ (init $ init stmts) ++ 
                         [CmmJump (CmmReg spReg) arguments]
    -- TODO: tail calls
    -- TODO: return direct at the end of a block
    _ -> BasicBlock ident stmts

destructContinuation :: UniqFM {-BlockId-} CPSBlockInfo -> UniqFM {-BlockId-} ContinuationFormat -> CmmBasicBlock -> CmmBasicBlock
destructContinuation block_infos formats (BasicBlock ident stmts) =
  case info of
    ControlBlock -> BasicBlock ident stmts
    ContinuationBlock _ -> BasicBlock ident (unpack_continuation ++ stmts)
  where
  info = lookupWithDefaultUFM block_infos (panic $ "info: unknown block " ++ (showSDoc $ ppr $ getUnique ident)) ident
  format = lookupWithDefaultUFM formats (panic $ "format: unknown block " ++ (showSDoc $ ppr $ getUnique ident)) ident
  unpack_continuation = CmmAssign spReg (CmmRegOff spReg frame_size) :
                        [CmmAssign (CmmLocal reg) (CmmLoad (CmmRegOff spReg (i*stack_slot_size)) (localRegRep reg))
                         | (i, Just reg) <- zip [1..] format]
  frame_size = stack_header_size + stack_slot_size * (length format)
  stack_header_size = stack_slot_size -- TODO: check if this could be different than stack_slot_size
  stack_slot_size = 4 -- TODO: find actual variables to be used instead of this

constructContinuation :: UniqFM {-BlockId-} CPSBlockInfo -> UniqFM {-BlockId-} ContinuationFormat -> CmmBasicBlock -> CmmBasicBlock
constructContinuation block_infos formats (BasicBlock ident stmts) =
  case last $ init stmts of
    -- TODO: global_saves
    --CmmCall (CmmForeignCall target CmmCallConv) results arguments (Just []) -> --TODO: handle globals
    CmmCall (CmmForeignCall target CmmCallConv) results arguments _ ->
        BasicBlock ident $
                   init (init stmts) ++
                   pack_continuation ++
                   [CmmJump target arguments]
    CmmCall target results arguments _ -> panic "unimplemented CmmCall"
    _ -> BasicBlock ident $ (init stmts) ++ build_block_branch
  where
  info = lookupWithDefaultUFM block_infos (panic $ "info: unknown block " ++ (showSDoc $ ppr $ getUnique next_block)) next_block
  format = lookupWithDefaultUFM formats (panic $ "format: unknown block " ++ (showSDoc $ ppr $ getUnique next_block)) next_block
  next_block = case last stmts of
    CmmBranch next -> next
    -- TODO: blocks with jump at end
    -- TODO: blocks with return at end
    _ -> panic "basic block without a branch at the end (unimplemented)"
  next_block_as_proc_expr = CmmLit $ CmmLabel $ mkReturnPtLabel $ getUnique next_block
  pack_continuation = CmmAssign spReg (CmmRegOff spReg (-frame_size)) :
                       CmmStore (CmmReg spReg) next_block_as_proc_expr :
                       [CmmStore (CmmRegOff spReg (i*stack_slot_size)) (CmmReg $ CmmLocal reg)
                        | (i, Just reg) <- zip [1..] format]
  frame_size = stack_header_size + stack_slot_size * (length format)
  stack_header_size = stack_slot_size -- TODO: check if this could be different than stack_slot_size (e.g. fixedHdrSize depends on PAR and GRAN)
  stack_slot_size = 4 -- TODO: find actual variables to be used instead of this (e.g. cgRepSizeW)
  block_needs_call = True -- TODO: use a table (i.e. proc-point)
  build_block_branch =
    if block_needs_call
       then [CmmJump next_block_as_proc_expr [] {- TODO: pass live -}] {- NOTE: a block can never be both a continuation and a controll block -}
       else [CmmBranch next_block]

-- TODO: TBD when to adjust the stack

cpsProc :: CmmTop -> CPS [CmmTop]
cpsProc x@(CmmData _ _) = return [x]
cpsProc x@(CmmProc info_table ident params blocks) = do
  broken_blocks <- liftM concat $ mapM breakBlock blocks
  let live = cmmLiveness (map snd broken_blocks)
  let blocks_with_live = map (cmmLivenessComment live . snd) broken_blocks
  let formats = selectContinuationFormat (undefined {-TODO-}) live
  let block_infos = listToUFM $ map (\(info, block) -> (blockId block, info)) broken_blocks
  let blocks_with_live' = map (constructContinuation block_infos formats) blocks_with_live
  let blocks_with_live'' = map (destructContinuation block_infos formats) blocks_with_live'
  let blocks_with_live''' = map (transformReturn block_infos formats) blocks_with_live''
  
  return $ [CmmProc info_table ident params blocks_with_live''']

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
