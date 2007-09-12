
module CmmContFlowOpt
    ( runCmmOpts, cmmCfgOpts, cmmCfgOptsZ
    , branchChainElimZ, removeUnreachableBlocksZ
    )
where

import Cmm
import CmmTx
import qualified ZipCfg as G
import ZipCfgCmmRep
import Maybes
import Util
import UniqFM

------------------------------------
mapProcs :: Tx (GenCmmTop d h s) -> Tx (GenCmm d h s)
mapProcs f (Cmm tops) = fmap Cmm (mapTx f tops)


------------------------------------
cmmCfgOpts  :: Tx (ListGraph CmmStmt)
cmmCfgOptsZ :: Tx CmmGraph

cmmCfgOpts  = branchChainElim  -- boring, but will get more exciting later
cmmCfgOptsZ = branchChainElimZ `seqTx` removeUnreachableBlocksZ
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
   -- ^ An infinite loop is not a link in a branch chain!

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
    lookup id = G.lookupBlockEnv env id `orElse` id 

isLoneBranchZ :: CmmBlock -> Either (G.BlockId, G.BlockId) CmmBlock
isLoneBranchZ (G.Block id (G.ZLast (G.LastOther (LastBranch target []))))
    | id /= target  = Left (id,target)
isLoneBranchZ other = Right other
   -- ^ An infinite loop is not a link in a branch chain!

replaceLabelsZ :: BlockEnv G.BlockId -> CmmGraph -> CmmGraph
replaceLabelsZ env = replace_eid . G.map_nodes id id last
  where
    replace_eid (G.LGraph eid blocks) = G.LGraph (lookup eid) blocks
    last (LastBranch id args)         = LastBranch (lookup id) args
    last (LastCondBranch e ti fi)     = LastCondBranch e (lookup ti) (lookup fi)
    last (LastSwitch e tbl)           = LastSwitch e (map (fmap lookup) tbl)
    last (LastCall tgt (Just id))     = LastCall tgt (Just $ lookup id) 
    last exit_jump_return             = exit_jump_return
    lookup id = G.lookupBlockEnv env id `orElse` id 
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
