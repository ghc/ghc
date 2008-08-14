
module CmmContFlowOpt
    ( runCmmOpts, cmmCfgOpts, cmmCfgOptsZ
    , branchChainElimZ, removeUnreachableBlocksZ, predMap
    , replaceLabelsZ, runCmmContFlowOptsZs
    )
where

import BlockId
import Cmm
import CmmTx
import qualified ZipCfg as G
import ZipCfg
import ZipCfgCmmRep

import Maybes
import Monad
import Outputable
import Panic
import Prelude hiding (unzip, zip)
import Util
import UniqFM

------------------------------------
runCmmContFlowOptsZs :: [CmmZ] -> [CmmZ]
runCmmContFlowOptsZs prog
  = [ runTx (runCmmOpts cmmCfgOptsZ) cmm_top
    | cmm_top <- prog ]

cmmCfgOpts  :: Tx (ListGraph CmmStmt)
cmmCfgOptsZ :: Tx CmmGraph

cmmCfgOpts  = branchChainElim  -- boring, but will get more exciting later
cmmCfgOptsZ = branchChainElimZ `seqTx` blockConcatZ `seqTx` removeUnreachableBlocksZ
        -- Here branchChainElim can ultimately be replaced
        -- with a more exciting combination of optimisations

runCmmOpts :: Tx g -> Tx (GenCmm d h g)
runCmmOpts opt = mapProcs (optGraph opt)

optGraph :: Tx g -> Tx (GenCmmTop d h g)
optGraph _   top@(CmmData {}) = noTx top
optGraph opt (CmmProc info lbl formals g) = fmap (CmmProc info lbl formals) (opt g)

------------------------------------
mapProcs :: Tx (GenCmmTop d h s) -> Tx (GenCmm d h s)
mapProcs f (Cmm tops) = fmap Cmm (mapTx f tops)

----------------------------------------------------------------
branchChainElim :: Tx (ListGraph CmmStmt)
-- If L is not captured in an instruction, we can remove any
-- basic block of the form L: goto L', and replace L with L' everywhere else.
-- How does L get captured? In a CallArea.
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
branchChainElimZ g@(G.LGraph eid args _)
  | null lone_branch_blocks     -- No blocks to remove
  = noTx g
  | otherwise
  = aTx $ replaceLabelsZ env $ G.of_block_list eid args (self_branches ++ others)
  where
    (lone_branch_blocks, others) = partitionWith isLoneBranchZ (G.to_block_list g)
    env = mkClosureBlockEnvZ lone_branch_blocks
    self_branches =
        let loop_to (id, _) =
                if lookup id == id then
                    Just (G.Block id Nothing (G.ZLast (G.mkBranchNode id)))
                else
                    Nothing
        in  mapMaybe loop_to lone_branch_blocks
    lookup id = lookupBlockEnv env id `orElse` id 

isLoneBranchZ :: CmmBlock -> Either (BlockId, BlockId) CmmBlock
isLoneBranchZ (G.Block id Nothing (G.ZLast (G.LastOther (LastBranch target))))
    | id /= target  = Left (id,target)
isLoneBranchZ other = Right other
   -- An infinite loop is not a link in a branch chain!

replaceLabelsZ :: BlockEnv BlockId -> CmmGraph -> CmmGraph
replaceLabelsZ env = replace_eid . G.map_nodes id middle last
  where
    replace_eid (G.LGraph eid off blocks) = G.LGraph (lookup eid) off blocks
    middle m@(MidComment _)            = m
    middle   (MidAssign r e)           = MidAssign r (exp e)
    middle   (MidStore addr e)         = MidStore (exp addr) (exp e)
    middle   (MidUnsafeCall tgt fs as) = MidUnsafeCall (midcall tgt) fs (map exp as)
    middle   (MidAddToContext e es)    = MidAddToContext (exp e) (map exp es)
    last (LastBranch id)             = LastBranch (lookup id)
    last (LastCondBranch e ti fi)    = LastCondBranch (exp e) (lookup ti) (lookup fi)
    last (LastSwitch e tbl)          = LastSwitch (exp e) (map (fmap lookup) tbl)
    last (LastCall tgt mb_id s)      = LastCall (exp tgt) (fmap lookup mb_id) s
    last (LastJump e s)              = LastJump (exp e) s
    last (LastReturn s)              = LastReturn s
    midcall   (ForeignTarget e c)    = ForeignTarget (exp e) c
    midcall m@(PrimTarget _)         = m
    exp e@(CmmLit _)         = e
    exp   (CmmLoad addr ty)  = CmmLoad (exp addr) ty
    exp e@(CmmReg _)         = e
    exp   (CmmMachOp op es)  = CmmMachOp op $ map exp es
    exp e@(CmmRegOff _ _)    = e
    exp   (CmmStackSlot (CallArea (Young id)) i) =
      CmmStackSlot (CallArea (Young (lookup id))) i
    exp e@(CmmStackSlot _ _) = e
    lookup id = fmap lookup (lookupBlockEnv env id) `orElse` id 

----------------------------------------------------------------
-- Build a map from a block to its set of predecessors. Very useful.
predMap :: G.LastNode l => G.LGraph m l -> BlockEnv BlockSet
predMap g = G.fold_blocks add_preds emptyBlockEnv g -- find the back edges
  where add_preds b env = foldl (add b) env (G.succs b)
        add (G.Block bid _ _) env b' =
          extendBlockEnv env b' $
                extendBlockSet (lookupBlockEnv env b' `orElse` emptyBlockSet) bid
----------------------------------------------------------------
-- If a block B branches to a label L, and L has no other predecessors,
-- then we can splice the block starting with L onto the end of B.
-- Because this optmization can be inhibited by unreachable blocks,
-- we first take a pass to drops unreachable blocks.
-- Order matters, so we work bottom up (reverse postorder DFS).
--
-- To ensure correctness, we have to make sure that the BlockId of the block
-- we are about to eliminate is not named in another instruction
-- (except an adjacent stack pointer adjustment, which we expect and also eliminate).
-- For 
--
-- Note: This optimization does _not_ subsume branch chain elimination.
blockConcatZ  :: Tx CmmGraph
blockConcatZ = removeUnreachableBlocksZ `seqTx` blockConcatZ'
blockConcatZ' :: Tx CmmGraph
blockConcatZ' g@(G.LGraph eid off blocks) =
  tx $ pprTrace "concatMap" (ppr concatMap) $ replaceLabelsZ concatMap $ G.LGraph eid off blocks'
  where (changed, blocks', concatMap) =
           foldr maybe_concat (False, blocks, emptyBlockEnv) $ G.postorder_dfs g
        maybe_concat b@(G.Block bid _ _) (changed, blocks', concatMap) =
          let unchanged = (changed, extendBlockEnv blocks' bid b, concatMap)
          in case G.goto_end $ G.unzip b of
               (h, G.LastOther (LastBranch b')) ->
                  if num_preds b' == 1 then
                    (True, extendBlockEnv blocks' bid $ splice blocks' h b',
                     extendBlockEnv concatMap b' bid)
                  else unchanged
               _ -> unchanged
        num_preds bid = liftM sizeBlockSet (lookupBlockEnv backEdges bid) `orElse` 0
        backEdges = predMap g
        splice blocks' h bid' =
          case lookupBlockEnv blocks' bid' of
            Just (G.Block _ Nothing t) -> G.zip $ G.ZBlock h t
            Just (G.Block _ (Just _) _) ->
              panic "trying to concatenate but successor block has incoming args"
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
mkClosureBlockEnvZ :: [(BlockId, BlockId)] -> BlockEnv BlockId
mkClosureBlockEnvZ blocks = mkBlockEnv $ map follow blocks
    where singleEnv = mkBlockEnv blocks
          follow (id, next) = (id, endChain id next)
          endChain orig id = case lookupBlockEnv singleEnv id of
                               Just id' | id /= orig -> endChain orig id'
                               _ -> id
----------------------------------------------------------------
removeUnreachableBlocksZ :: Tx CmmGraph
removeUnreachableBlocksZ g@(G.LGraph id off blocks) =
      if length blocks' < sizeUFM blocks then aTx $ G.of_block_list id off blocks'
      else noTx g
    where blocks' = G.postorder_dfs g
