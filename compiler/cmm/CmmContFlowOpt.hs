
module CmmContFlowOpt
    ( runCmmOpts, cmmCfgOpts, cmmCfgOptsZ
    , branchChainElimZ, removeUnreachableBlocksZ, predMap
    , replaceLabelsZ, replaceBranches, runCmmContFlowOptsZs
    )
where

import BlockId
import Cmm
import CmmTx
import qualified ZipCfg as G
import ZipCfg
import ZipCfgCmmRep

import Maybes
import Control.Monad
import Outputable
import Prelude hiding (unzip, zip)
import Util

------------------------------------
runCmmContFlowOptsZs :: [CmmZ] -> [CmmZ]
runCmmContFlowOptsZs prog
  = [ runTx (runCmmOpts cmmCfgOptsZ) cmm_top
    | cmm_top <- prog ]

cmmCfgOpts  :: Tx (ListGraph CmmStmt)
cmmCfgOptsZ :: Tx (a, CmmGraph)

cmmCfgOpts  = branchChainElim  -- boring, but will get more exciting later
cmmCfgOptsZ g =
  optGraph
    (branchChainElimZ `seqTx` blockConcatZ `seqTx` removeUnreachableBlocksZ) g
        -- Here branchChainElim can ultimately be replaced
        -- with a more exciting combination of optimisations

runCmmOpts :: Tx g -> Tx (GenCmm d h g)
-- Lifts a transformer on a single graph to one on the whole program
runCmmOpts opt = mapProcs (optProc opt)

optProc :: Tx g -> Tx (GenCmmTop d h g)
optProc _   top@(CmmData {}) = noTx top
optProc opt (CmmProc info lbl formals g) =
  fmap (CmmProc info lbl formals) (opt g)

optGraph :: Tx g -> Tx (a, g)
optGraph opt (a, g) = fmap (\g' -> (a, g')) (opt g)

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
-- and replace L with L' everywhere else,
-- unless L is the successor of a call instruction and L'
-- is the entry block. You don't want to set the successor
-- of a function call to the entry block because there is no good way
-- to store both the infotables for the call and from the callee,
-- while putting the stack pointer in a consistent place.
--
-- JD isn't quite sure when it's safe to share continuations for different
-- function calls -- have to think about where the SP will be,
-- so we'll table that problem for now by leaving all call successors alone.
branchChainElimZ g@(G.LGraph eid _)
  | null lone_branch_blocks     -- No blocks to remove
  = noTx g
  | otherwise
  = aTx $ replaceLabelsZ env $ G.of_block_list eid (self_branches ++ others)
  where
    blocks = G.to_block_list g
    (lone_branch_blocks, others) = partitionWith isLoneBranchZ blocks
    env = mkClosureBlockEnvZ lone_branch_blocks
    self_branches =
      let loop_to (id, _) =
            if lookup id == id then
              Just (G.Block id (G.ZLast (G.mkBranchNode id)))
            else
              Nothing
      in  mapMaybe loop_to lone_branch_blocks
    lookup id = lookupBlockEnv env id `orElse` id 

    call_succs = foldl add emptyBlockSet blocks
      where add succs b =
              case G.last (G.unzip b) of
                LastOther (LastCall _ (Just k) _ _ _) -> extendBlockSet succs k
                _ -> succs
    isLoneBranchZ :: CmmBlock -> Either (BlockId, BlockId) CmmBlock
    isLoneBranchZ (G.Block id (G.ZLast (G.LastOther (LastBranch target))))
        | id /= target && not (elemBlockSet id call_succs) = Left (id,target)
    isLoneBranchZ other = Right other
       -- An infinite loop is not a link in a branch chain!

maybeReplaceLabels :: (Last -> Bool) -> BlockEnv BlockId -> CmmGraph -> CmmGraph
maybeReplaceLabels lpred env =
  replace_eid . G.map_nodes id middle last
   where
     replace_eid (G.LGraph eid blocks) = G.LGraph (lookup eid) blocks
     middle = mapExpDeepMiddle exp
     last l = if lpred l then mapExpDeepLast exp (last' l) else l
     last' (LastBranch bid) = LastBranch (lookup bid)
     last' (LastCondBranch p t f) = LastCondBranch p (lookup t) (lookup f)
     last' (LastSwitch e arms) = LastSwitch e (map (liftM lookup) arms)
     last' (LastCall t k a res r) = LastCall t (liftM lookup k) a res r
     exp (CmmLit (CmmBlock bid)) = CmmLit (CmmBlock (lookup bid))
     exp   (CmmStackSlot (CallArea (Young id)) i) =
       CmmStackSlot (CallArea (Young (lookup id))) i
     exp e = e
     lookup id = fmap lookup (lookupBlockEnv env id) `orElse` id 

replaceLabelsZ :: BlockEnv BlockId -> CmmGraph -> CmmGraph
replaceLabelsZ = maybeReplaceLabels (const True)

-- replaceBranchLabels :: BlockEnv BlockId -> CmmGraph -> CmmGraph
-- replaceBranchLabels env g@(LGraph _ _) = maybeReplaceLabels lpred env g
--   where lpred (LastBranch _) = True
--         lpred _ = False

replaceBranches :: BlockEnv BlockId -> CmmGraph -> CmmGraph
replaceBranches env g = map_nodes id id last g
  where
    last (LastBranch id)          = LastBranch (lookup id)
    last (LastCondBranch e ti fi) = LastCondBranch e (lookup ti) (lookup fi)
    last (LastSwitch e tbl)       = LastSwitch e (map (fmap lookup) tbl)
    last l@(LastCall {})          = l
    lookup id = fmap lookup (lookupBlockEnv env id) `orElse` id 

----------------------------------------------------------------
-- Build a map from a block to its set of predecessors. Very useful.
predMap :: G.LastNode l => G.LGraph m l -> BlockEnv BlockSet
predMap g = G.fold_blocks add_preds emptyBlockEnv g -- find the back edges
  where add_preds b env = foldl (add b) env (G.succs b)
        add (G.Block bid _) env b' =
          extendBlockEnv env b' $
                extendBlockSet (lookupBlockEnv env b' `orElse` emptyBlockSet) bid
----------------------------------------------------------------
-- If a block B branches to a label L, L is not the entry block,
-- and L has no other predecessors,
-- then we can splice the block starting with L onto the end of B.
-- Because this optimization can be inhibited by unreachable blocks,
-- we first take a pass to drops unreachable blocks.
-- Order matters, so we work bottom up (reverse postorder DFS).
--
-- To ensure correctness, we have to make sure that the BlockId of the block
-- we are about to eliminate is not named in another instruction.
--
-- Note: This optimization does _not_ subsume branch chain elimination.
blockConcatZ  :: Tx CmmGraph
blockConcatZ = removeUnreachableBlocksZ `seqTx` blockConcatZ'
blockConcatZ' :: Tx CmmGraph
blockConcatZ' g@(G.LGraph eid blocks) =
  tx $ replaceLabelsZ concatMap $ G.LGraph eid blocks'
  where (changed, blocks', concatMap) =
           foldr maybe_concat (False, blocks, emptyBlockEnv) $ G.postorder_dfs g
        maybe_concat b@(G.Block bid _) (changed, blocks', concatMap) =
          let unchanged = (changed, extendBlockEnv blocks' bid b, concatMap)
          in case G.goto_end $ G.unzip b of
               (h, G.LastOther (LastBranch b')) ->
                  if canConcatWith b' then
                    (True, extendBlockEnv blocks' bid $ splice blocks' h b',
                     extendBlockEnv concatMap b' bid)
                  else unchanged
               _ -> unchanged
        num_preds bid = liftM sizeBlockSet (lookupBlockEnv backEdges bid) `orElse` 0
        canConcatWith b' = b' /= eid && num_preds b' == 1
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
mkClosureBlockEnvZ :: [(BlockId, BlockId)] -> BlockEnv BlockId
mkClosureBlockEnvZ blocks = mkBlockEnv $ map follow blocks
    where singleEnv = mkBlockEnv blocks
          follow (id, next) = (id, endChain id next)
          endChain orig id = case lookupBlockEnv singleEnv id of
                               Just id' | id /= orig -> endChain orig id'
                               _ -> id
----------------------------------------------------------------
removeUnreachableBlocksZ :: Tx CmmGraph
removeUnreachableBlocksZ g@(G.LGraph id blocks) =
  if length blocks' < sizeBEnv blocks then aTx $ G.of_block_list id blocks'
  else noTx g
    where blocks' = G.postorder_dfs g
