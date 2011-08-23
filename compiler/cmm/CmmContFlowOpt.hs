{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-incomplete-patterns #-}

module CmmContFlowOpt
    ( runCmmOpts, oldCmmCfgOpts, cmmCfgOpts
    , branchChainElim, removeUnreachableBlocks, predMap
    , replaceLabels, replaceBranches, runCmmContFlowOpts
    )
where

import BlockId
import Cmm
import CmmUtils
import qualified OldCmm as Old

import Maybes
import Compiler.Hoopl
import Control.Monad
import Outputable
import Prelude hiding (succ, unzip, zip)
import Util

------------------------------------
runCmmContFlowOpts :: CmmGroup -> CmmGroup
runCmmContFlowOpts prog = runCmmOpts cmmCfgOpts prog

oldCmmCfgOpts :: Old.ListGraph Old.CmmStmt -> Old.ListGraph Old.CmmStmt
cmmCfgOpts    :: CmmGraph -> CmmGraph

oldCmmCfgOpts = oldBranchChainElim  -- boring, but will get more exciting later
cmmCfgOpts    =
  removeUnreachableBlocks . blockConcat . branchChainElim
        -- Here branchChainElim can ultimately be replaced
        -- with a more exciting combination of optimisations

runCmmOpts :: (g -> g) -> GenCmmGroup d h g -> GenCmmGroup d h g
-- Lifts a transformer on a single graph to one on the whole program
runCmmOpts opt = map (optProc opt)

optProc :: (g -> g) -> GenCmmDecl d h g -> GenCmmDecl d h g
optProc _   top@(CmmData {}) = top
optProc opt (CmmProc info lbl g) = CmmProc info lbl (opt g)

----------------------------------------------------------------
oldBranchChainElim :: Old.ListGraph Old.CmmStmt -> Old.ListGraph Old.CmmStmt
-- If L is not captured in an instruction, we can remove any
-- basic block of the form L: goto L', and replace L with L' everywhere else.
-- How does L get captured? In a CallArea.
oldBranchChainElim (Old.ListGraph blocks)
  | null lone_branch_blocks     -- No blocks to remove
  = Old.ListGraph blocks
  | otherwise
  = Old.ListGraph new_blocks
  where
    (lone_branch_blocks, others) = partitionWith isLoneBranch blocks
    new_blocks = map (replaceLabels env) others
    env = mkClosureBlockEnv lone_branch_blocks

    isLoneBranch :: Old.CmmBasicBlock -> Either (BlockId, BlockId) Old.CmmBasicBlock
    isLoneBranch (Old.BasicBlock id [Old.CmmBranch target]) | id /= target = Left (id, target)
    isLoneBranch other_block                                           = Right other_block
       -- An infinite loop is not a link in a branch chain!

    replaceLabels :: BlockEnv BlockId -> Old.CmmBasicBlock -> Old.CmmBasicBlock
    replaceLabels env (Old.BasicBlock id stmts)
      = Old.BasicBlock id (map replace stmts)
      where
        replace (Old.CmmBranch id)       = Old.CmmBranch (lookup id)
        replace (Old.CmmCondBranch e id) = Old.CmmCondBranch e (lookup id)
        replace (Old.CmmSwitch e tbl)    = Old.CmmSwitch e (map (fmap lookup) tbl)
        replace other_stmt           = other_stmt

        lookup id = mapLookup id env `orElse` id 

----------------------------------------------------------------
branchChainElim :: CmmGraph -> CmmGraph
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
branchChainElim g
  | null lone_branch_blocks     -- No blocks to remove
  = g
  | otherwise
  = replaceLabels env $ ofBlockList (g_entry g) (self_branches ++ others)
  where
    blocks = toBlockList g
    (lone_branch_blocks, others) = partitionWith isLoneBranch blocks
    env = mkClosureBlockEnv lone_branch_blocks
    self_branches =
      let loop_to (id, _) =
            if lookup id == id then
              Just $ blockOfNodeList (JustC (CmmEntry id), [], JustC (mkBranchNode id))
            else
              Nothing
      in  mapMaybe loop_to lone_branch_blocks
    lookup id = mapLookup id env `orElse` id

    call_succs = foldl add emptyBlockSet blocks
      where add :: BlockSet -> CmmBlock -> BlockSet
            add succs b =
              case lastNode b of
                (CmmCall _ (Just k) _ _ _) -> setInsert k succs
                (CmmForeignCall {succ=k})  -> setInsert k succs
                _                          -> succs
    isLoneBranch :: CmmBlock -> Either (BlockId, BlockId) CmmBlock
    isLoneBranch block | (JustC (CmmEntry id), [], JustC (CmmBranch target)) <- blockToNodeList block,
                         id /= target && not (setMember id call_succs)
                       = Left (id,target)
    isLoneBranch other = Right other
       -- An infinite loop is not a link in a branch chain!

maybeReplaceLabels :: (CmmNode O C -> Bool) -> BlockEnv BlockId -> CmmGraph -> CmmGraph
maybeReplaceLabels lpred env =
  replace_eid . mapGraphNodes (id, middle, last)
   where
     replace_eid g = g {g_entry = lookup (g_entry g)}
     lookup id = fmap lookup (mapLookup id env) `orElse` id
     
     middle = mapExpDeep exp
     last l = if lpred l then mapExpDeep exp (last' l) else l
     last' :: CmmNode O C -> CmmNode O C
     last' (CmmBranch bid)             = CmmBranch (lookup bid)
     last' (CmmCondBranch p t f)       = CmmCondBranch p (lookup t) (lookup f)
     last' (CmmSwitch e arms)          = CmmSwitch e (map (liftM lookup) arms)
     last' (CmmCall t k a res r)       = CmmCall t (liftM lookup k) a res r
     last' (CmmForeignCall t r a bid u i) = CmmForeignCall t r a (lookup bid) u i

     exp (CmmLit (CmmBlock bid))                = CmmLit (CmmBlock (lookup bid))
     exp (CmmStackSlot (CallArea (Young id)) i) = CmmStackSlot (CallArea (Young (lookup id))) i
     exp e                                      = e


replaceLabels :: BlockEnv BlockId -> CmmGraph -> CmmGraph
replaceLabels = maybeReplaceLabels (const True)

replaceBranches :: BlockEnv BlockId -> CmmGraph -> CmmGraph
replaceBranches env g = mapGraphNodes (id, id, last) g
  where
    last :: CmmNode O C -> CmmNode O C
    last (CmmBranch id)          = CmmBranch (lookup id)
    last (CmmCondBranch e ti fi) = CmmCondBranch e (lookup ti) (lookup fi)
    last (CmmSwitch e tbl)       = CmmSwitch e (map (fmap lookup) tbl)
    last l@(CmmCall {})          = l
    last l@(CmmForeignCall {})   = l
    lookup id = fmap lookup (mapLookup id env) `orElse` id

----------------------------------------------------------------
-- Build a map from a block to its set of predecessors. Very useful.
predMap :: [CmmBlock] -> BlockEnv BlockSet
predMap blocks = foldr add_preds mapEmpty blocks -- find the back edges
  where add_preds block env = foldl (add (entryLabel block)) env (successors block)
        add bid env b' =
          mapInsert b' (setInsert bid (mapLookup b' env `orElse` setEmpty)) env
----------------------------------------------------------------
-- If a block B branches to a label L, L is not the entry block,
-- and L has no other predecessors,
-- then we can splice the block starting with L onto the end of B.
-- Order matters, so we work bottom up (reverse postorder DFS).
-- This optimization can be inhibited by unreachable blocks, but
-- the reverse postorder DFS returns only reachable blocks.
--
-- To ensure correctness, we have to make sure that the BlockId of the block
-- we are about to eliminate is not named in another instruction.
--
-- Note: This optimization does _not_ subsume branch chain elimination.
blockConcat  :: CmmGraph -> CmmGraph
blockConcat g@(CmmGraph {g_entry=eid}) =
  replaceLabels concatMap $ ofBlockMap (g_entry g) blocks'
  where blocks = postorderDfs g
        (blocks', concatMap) =
           foldr maybe_concat (toBlockMap g, mapEmpty) $ blocks
        maybe_concat :: CmmBlock -> (LabelMap CmmBlock, LabelMap Label) -> (LabelMap CmmBlock, LabelMap Label)
        maybe_concat b unchanged@(blocks', concatMap) =
          let bid = entryLabel b
          in case blockToNodeList b of
               (JustC h, m, JustC (CmmBranch b')) ->
                  if canConcatWith b' then
                    (mapInsert bid (splice blocks' h m b') blocks',
                     mapInsert b' bid concatMap)
                  else unchanged
               _ -> unchanged
        num_preds bid = liftM setSize (mapLookup bid backEdges) `orElse` 0
        canConcatWith b' = b' /= eid && num_preds b' == 1
        backEdges = predMap blocks
        splice :: forall map n e x.
                  IsMap map =>
                  map (Block n e x) -> n C O -> [n O O] -> KeyOf map -> Block n C x
        splice blocks' h m bid' =
          case mapLookup bid' blocks' of
            Nothing -> panic "unknown successor block"
            Just block | (_, m', l') <- blockToNodeList block -> blockOfNodeList (JustC h, (m ++ m'), l')
----------------------------------------------------------------
mkClosureBlockEnv :: [(BlockId, BlockId)] -> BlockEnv BlockId
mkClosureBlockEnv blocks = mapFromList $ map follow blocks
    where singleEnv = mapFromList blocks :: BlockEnv BlockId
          follow (id, next) = (id, endChain id next)
          endChain orig id = case mapLookup id singleEnv of
                               Just id' | id /= orig -> endChain orig id'
                               _ -> id
----------------------------------------------------------------
removeUnreachableBlocks :: CmmGraph -> CmmGraph
removeUnreachableBlocks g =
  if length blocks < mapSize (toBlockMap g) then ofBlockList (g_entry g) blocks
                                           else g
    where blocks = postorderDfs g
