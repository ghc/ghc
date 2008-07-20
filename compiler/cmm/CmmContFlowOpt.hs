
module CmmContFlowOpt
    ( runCmmOpts, cmmCfgOpts, cmmCfgOptsZ
    , branchChainElimZ, removeUnreachableBlocksZ, predMap
    , replaceLabelsZ
    )
where

import BlockId
import Cmm
import CmmTx
import qualified ZipCfg as G
import ZipCfgCmmRep

import Maybes
import Monad
import Panic
import Prelude hiding (unzip, zip)
import Util
import UniqFM

------------------------------------
mapProcs :: Tx (GenCmmTop d h s) -> Tx (GenCmm d h s)
mapProcs f (Cmm tops) = fmap Cmm (mapTx f tops)


------------------------------------
cmmCfgOpts  :: Tx (ListGraph CmmStmt)
cmmCfgOptsZ :: Tx CmmGraph

cmmCfgOpts  = branchChainElim  -- boring, but will get more exciting later
cmmCfgOptsZ =
  branchChainElimZ `seqTx` blockConcatZ `seqTx` removeUnreachableBlocksZ
        -- Here branchChainElim can ultimately be replaced
        -- with a more exciting combination of optimisations

runCmmOpts :: Tx g -> Tx (GenCmm d h g)
runCmmOpts opt = mapProcs (optGraph opt)

optGraph :: Tx g -> Tx (GenCmmTop d h g)
optGraph _   top@(CmmData {}) = noTx top
optGraph opt (CmmProc info lbl formals g) = fmap (CmmProc info lbl formals) (opt g)

----------------------------------------------------------------
branchChainElim :: Tx (ListGraph CmmStmt)
-- Remove any basic block of the form L: goto L',
-- and replace L with L' everywhere else
branchChainElim (ListGraph blocks)
  | null lone_branch_blocks     -- No blocks to remove
  = noTx (ListGraph blocks)
  | otherwise
  = aTx (ListGraph new_blocks)
  where
    (lone_branch_blocks, others) = partitionWith isLoneBranch blocks
    new_blocks = map (replaceLabels env) others
    env = mkClosureBlockEnv lone_branch_blocks

isLoneBranch :: CmmBasicBlock -> Either (BlockId, BlockId) CmmBasicBlock
isLoneBranch (BasicBlock id [CmmBranch target]) | id /= target = Left (id, target)
isLoneBranch other_block                                       = Right other_block
   -- An infinite loop is not a link in a branch chain!

replaceLabels :: BlockEnv BlockId -> CmmBasicBlock -> CmmBasicBlock
replaceLabels env (BasicBlock id stmts)
  = BasicBlock id (map replace stmts)
  where
    replace (CmmBranch id)       = CmmBranch (lookup id)
    replace (CmmCondBranch e id) = CmmCondBranch e (lookup id)
    replace (CmmSwitch e tbl)    = CmmSwitch e (map (fmap lookup) tbl)
    replace other_stmt           = other_stmt

    lookup id = lookupBlockEnv env id `orElse` id 
----------------------------------------------------------------
branchChainElimZ :: Tx CmmGraph
-- Remove any basic block of the form L: goto L',
-- and replace L with L' everywhere else
branchChainElimZ g@(G.LGraph eid _)
  | null lone_branch_blocks     -- No blocks to remove
  = noTx g
  | otherwise
  = aTx $ replaceLabelsZ env $ G.of_block_list eid (self_branches ++ others)
  where
    (lone_branch_blocks, others) = partitionWith isLoneBranchZ (G.to_block_list g)
    env = mkClosureBlockEnv lone_branch_blocks
    self_branches =
        let loop_to (id, _) =
                if lookup id == id then
                    Just (G.Block id (G.ZLast (G.mkBranchNode id)))
                else
                    Nothing
        in  mapMaybe loop_to lone_branch_blocks
    lookup id = lookupBlockEnv env id `orElse` id 

isLoneBranchZ :: CmmBlock -> Either (BlockId, BlockId) CmmBlock
isLoneBranchZ (G.Block id (G.ZLast (G.LastOther (LastBranch target))))
    | id /= target  = Left (id,target)
isLoneBranchZ other = Right other
   -- An infinite loop is not a link in a branch chain!

replaceLabelsZ :: BlockEnv BlockId -> CmmGraph -> CmmGraph
replaceLabelsZ env = replace_eid . G.map_nodes id id last
  where
    replace_eid (G.LGraph eid blocks) = G.LGraph (lookup eid) blocks
    last (LastBranch id)              = LastBranch (lookup id)
    last (LastCondBranch e ti fi)     = LastCondBranch e (lookup ti) (lookup fi)
    last (LastSwitch e tbl)           = LastSwitch e (map (fmap lookup) tbl)
    last (LastCall tgt (Just id))     = LastCall tgt (Just $ lookup id) 
    last exit_jump_return             = exit_jump_return
    lookup id = lookupBlockEnv env id `orElse` id 

----------------------------------------------------------------
-- Build a map from a block to its set of predecessors. Very useful.
predMap :: G.LastNode l => G.LGraph m l -> BlockEnv BlockSet
predMap g = G.fold_blocks add_preds emptyBlockEnv g -- find the back edges
  where add_preds b env = foldl (add b) env (G.succs b)
        add (G.Block bid _) env b' =
          extendBlockEnv env b' $
                extendBlockSet (lookupBlockEnv env b' `orElse` emptyBlockSet) bid
----------------------------------------------------------------
blockConcatZ  :: Tx CmmGraph
-- If a block B branches to a label L, and L has no other predecessors,
-- then we can splice the block starting with L onto the end of B.
-- Because this optmization can be inhibited by unreachable blocks,
-- we bundle it with a pass that drops unreachable blocks.
-- Order matters, so we work bottom up (reverse postorder DFS).
-- Note: This optimization does _not_ subsume branch chain elimination.
blockConcatZ = removeUnreachableBlocksZ  `seqTx` blockConcatZ'
blockConcatZ' :: Tx CmmGraph
blockConcatZ' g@(G.LGraph eid blocks) = tx $ G.LGraph eid blocks'
  where (changed, blocks') = foldr maybe_concat (False, blocks) $ G.postorder_dfs g
        maybe_concat b@(G.Block bid _) (changed, blocks') =
          let unchanged = (changed, extendBlockEnv blocks' bid b)
          in case G.goto_end $ G.unzip b of
               (h, G.LastOther (LastBranch b')) ->
                  if num_preds b' == 1 then
                    (True, extendBlockEnv blocks' bid $ splice blocks' h b')
                  else unchanged
               _ -> unchanged
        num_preds bid = liftM sizeBlockSet (lookupBlockEnv backEdges bid) `orElse` 0
        backEdges = predMap g
        splice blocks' h bid' =
          case lookupBlockEnv blocks' bid' of
            Just (G.Block _ t) -> G.zip $ G.ZBlock h t
            Nothing -> panic "unknown successor block"
        tx = if changed then aTx else noTx
----------------------------------------------------------------
mkClosureBlockEnv :: [(BlockId, BlockId)] -> BlockEnv BlockId
mkClosureBlockEnv blocks = mkBlockEnv $ map follow blocks
    where singleEnv = mkBlockEnv blocks
          follow (id, next) = (id, endChain id next)
          endChain orig id = case lookupBlockEnv singleEnv id of
                               Just id' | id /= orig -> endChain orig id'
                               _ -> id
----------------------------------------------------------------
removeUnreachableBlocksZ :: Tx CmmGraph
removeUnreachableBlocksZ g@(G.LGraph id blocks) =
      if length blocks' < sizeUFM blocks then aTx $ G.of_block_list id blocks'
      else noTx g
    where blocks' = G.postorder_dfs g
