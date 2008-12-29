module CmmLive (
        CmmLive,
        BlockEntryLiveness,
        cmmLiveness,
        cmmFormalsToLiveLocals,
  ) where

#include "HsVersions.h"

import BlockId
import Cmm
import Dataflow

import Maybes
import Panic
import UniqSet

-----------------------------------------------------------------------------
-- Calculating what variables are live on entry to a basic block
-----------------------------------------------------------------------------

-- | The variables live on entry to a block
type CmmLive = UniqSet LocalReg

-- | A mapping from block labels to the variables live on entry
type BlockEntryLiveness = BlockEnv CmmLive

-- | A mapping from block labels to the blocks that target it
type BlockSources = BlockEnv (UniqSet BlockId)

-- | A mapping from block labels to the statements in the block
type BlockStmts = BlockEnv [CmmStmt]

-----------------------------------------------------------------------------
-- | Calculated liveness info for a list of 'CmmBasicBlock'
-----------------------------------------------------------------------------
cmmLiveness :: [CmmBasicBlock] -> BlockEntryLiveness
cmmLiveness blocks =
    fixedpoint (cmmBlockDependants sources)
               (cmmBlockUpdate blocks')
               (map blockId blocks)
               (mkBlockEnv [(blockId b, emptyUniqSet) | b <- blocks])
    where
      sources :: BlockSources
      sources = cmmBlockSources blocks

      blocks' :: BlockStmts
      blocks' = mkBlockEnv $ map block_name blocks

      block_name :: CmmBasicBlock -> (BlockId, [CmmStmt])
      block_name b = (blockId b, blockStmts b)

{-
-- For debugging, annotate each block with a comment indicating
-- the calculated live variables
cmmLivenessComment ::
    BlockEnv (UniqSet LocalReg) -> CmmBasicBlock -> CmmBasicBlock
cmmLivenessComment live (BasicBlock ident stmts) =
    BasicBlock ident stmts' where
        stmts' = (CmmComment $ mkFastString $ showSDoc $ ppr $ live'):stmts
        live' = map CmmLocal $ uniqSetToList $ lookupWithDefaultUFM live emptyUniqSet ident
-}


-----------------------------------------------------------------------------
-- | Calculates a table of where one can lookup the blocks that might
-- need updating after a given block is updated in the liveness analysis
-----------------------------------------------------------------------------
cmmBlockSources :: [CmmBasicBlock] -> BlockSources
cmmBlockSources blocks = foldr aux emptyBlockEnv blocks
    where
      aux :: CmmBasicBlock
          -> BlockSources
          -> BlockSources
      aux block sourcesUFM =
          foldUniqSet (add_source_edges $ blockId block)
                      sourcesUFM
                      (branch_targets $ blockStmts block)

      add_source_edges :: BlockId -> BlockId
                       -> BlockSources
                       -> BlockSources
      add_source_edges source target ufm =
          addToBEnv_Acc (flip addOneToUniqSet) unitUniqSet ufm target source

      branch_targets :: [CmmStmt] -> UniqSet BlockId
      branch_targets stmts =
          mkUniqSet $ concatMap target stmts where
              target (CmmBranch ident) = [ident]
              target (CmmCondBranch _ ident) = [ident]
              target (CmmSwitch _ blocks) = mapMaybe id blocks
              target _ = []

-----------------------------------------------------------------------------
-- | Given the table calculated by 'cmmBlockSources', list all blocks
-- that depend on the result of a particular block.
--
-- Used by the call to 'fixedpoint'.
-----------------------------------------------------------------------------
cmmBlockDependants :: BlockSources -> BlockId -> [BlockId]
cmmBlockDependants sources ident =
    uniqSetToList $ lookupWithDefaultBEnv sources emptyUniqSet ident

-----------------------------------------------------------------------------
-- | Given the table of type 'BlockStmts' and a block that was updated,
-- calculate an updated BlockEntryLiveness
-----------------------------------------------------------------------------
cmmBlockUpdate ::
    BlockStmts
    -> BlockId
    -> Maybe BlockId
    -> BlockEntryLiveness
    -> Maybe BlockEntryLiveness
cmmBlockUpdate blocks node _ state =
    if (sizeUniqSet old_live) == (sizeUniqSet new_live)
      then Nothing
      else Just $ extendBlockEnv state node new_live
    where
      new_live, old_live :: CmmLive
      new_live = cmmStmtListLive state block_stmts
      old_live = lookupWithDefaultBEnv state missing_live node

      block_stmts :: [CmmStmt]
      block_stmts = lookupWithDefaultBEnv blocks missing_block node

      missing_live = panic "unknown block id during liveness analysis"
      missing_block = panic "unknown block id during liveness analysis"

-----------------------------------------------------------------------------
-- Section: 
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- CmmBlockLive, cmmStmtListLive and helpers
-----------------------------------------------------------------------------

-- Calculate the live registers for a local block (list of statements)

cmmStmtListLive :: BlockEntryLiveness -> [CmmStmt] -> CmmLive
cmmStmtListLive other_live stmts =
    foldr ((.) . (cmmStmtLive other_live)) id stmts emptyUniqSet

-----------------------------------------------------------------------------
-- This code is written in the style of a state monad,
-- but since Control.Monad.State is not in the core
-- we can't use it in GHC, so we'll fake one here.
-- We don't need a return value so well leave it out.
-- Thus 'bind' reduces to function composition.

type CmmLivenessTransformer = CmmLive -> CmmLive

-- Helpers for the "Monad"
addLive, addKilled :: CmmLive -> CmmLivenessTransformer
addLive new_live live = live `unionUniqSets` new_live
addKilled new_killed live = live `minusUniqSet` new_killed

--------------------------------
-- Liveness of a CmmStmt
--------------------------------
cmmFormalsToLiveLocals :: HintedCmmFormals -> [LocalReg]
cmmFormalsToLiveLocals formals = map hintlessCmm formals

cmmStmtLive :: BlockEntryLiveness -> CmmStmt -> CmmLivenessTransformer
cmmStmtLive _ (CmmNop) = id
cmmStmtLive _ (CmmComment _) = id
cmmStmtLive _ (CmmAssign reg expr) =
    cmmExprLive expr . reg_liveness where
        reg_liveness =
            case reg of
              (CmmLocal reg') -> addKilled $ unitUniqSet reg'
              (CmmGlobal _) -> id
cmmStmtLive _ (CmmStore expr1 expr2) =
    cmmExprLive expr2 . cmmExprLive expr1
cmmStmtLive _ (CmmCall target results arguments _ _) =
    target_liveness .
    foldr ((.) . cmmExprLive) id (map hintlessCmm arguments) .
    addKilled (mkUniqSet $ cmmFormalsToLiveLocals results) where
        target_liveness =
            case target of
              (CmmCallee target _) -> cmmExprLive target
              (CmmPrim _) -> id
cmmStmtLive other_live (CmmBranch target) =
    addLive (lookupWithDefaultBEnv other_live emptyUniqSet target)
cmmStmtLive other_live (CmmCondBranch expr target) =
    cmmExprLive expr .
    addLive (lookupWithDefaultBEnv other_live emptyUniqSet target)
cmmStmtLive other_live (CmmSwitch expr targets) =
    cmmExprLive expr .
    (foldr ((.) . (addLive .
                   lookupWithDefaultBEnv other_live emptyUniqSet))
           id
           (mapCatMaybes id targets))
cmmStmtLive _ (CmmJump expr params) =
    const (cmmExprLive expr $ foldr ((.) . cmmExprLive) id (map hintlessCmm params) $ emptyUniqSet)
cmmStmtLive _ (CmmReturn params) =
    const (foldr ((.) . cmmExprLive) id (map hintlessCmm params) $ emptyUniqSet)

--------------------------------
-- Liveness of a CmmExpr
--------------------------------
cmmExprLive :: CmmExpr -> CmmLivenessTransformer
cmmExprLive expr = addLive (mkUniqSet $ expr_liveness expr) where
    expr_liveness :: CmmExpr -> [LocalReg]
    expr_liveness (CmmLit _) = []
    expr_liveness (CmmLoad expr _) = expr_liveness expr
    expr_liveness (CmmReg reg) = reg_liveness reg
    expr_liveness (CmmMachOp _ exprs) = concatMap expr_liveness exprs
    expr_liveness (CmmRegOff reg _) = reg_liveness reg
    expr_liveness (CmmStackSlot _ _) = panic "cmmExprLive CmmStackSlot"

    reg_liveness :: CmmReg -> [LocalReg]
    reg_liveness (CmmLocal reg) = [reg]
    reg_liveness (CmmGlobal _) = []
