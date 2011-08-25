{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-incomplete-patterns #-}

module CmmContFlowOpt
    ( runCmmContFlowOpts
    , removeUnreachableBlocks, replaceBranches
    )
where

import BlockId
import Cmm
import CmmUtils
import Digraph
import Maybes
import Outputable

import Compiler.Hoopl
import Control.Monad
import Prelude hiding (succ, unzip, zip)

-----------------------------------------------------------------------------
--
-- Control-flow optimisations
--
-----------------------------------------------------------------------------

runCmmContFlowOpts :: CmmGroup -> CmmGroup
runCmmContFlowOpts = map (optProc cmmCfgOpts)

cmmCfgOpts :: CmmGraph -> CmmGraph
cmmCfgOpts = removeUnreachableBlocks . blockConcat . branchChainElim
        -- Here branchChainElim can ultimately be replaced
        -- with a more exciting combination of optimisations

optProc :: (g -> g) -> GenCmmDecl d h g -> GenCmmDecl d h g
optProc opt (CmmProc info lbl g) = CmmProc info lbl (opt g)
optProc _   top                  = top

-----------------------------------------------------------------------------
--
-- Branch Chain Elimination
--
-----------------------------------------------------------------------------

-- | Remove any basic block of the form L: goto L', and replace L with
-- L' everywhere else, unless L is the successor of a call instruction
-- and L' is the entry block. You don't want to set the successor of a
-- function call to the entry block because there is no good way to
-- store both the infotables for the call and from the callee, while
-- putting the stack pointer in a consistent place.
--
-- JD isn't quite sure when it's safe to share continuations for different
-- function calls -- have to think about where the SP will be,
-- so we'll table that problem for now by leaving all call successors alone.

branchChainElim :: CmmGraph -> CmmGraph
branchChainElim g
  | null lone_branch_blocks = g    -- No blocks to remove
  | otherwise               = {- pprTrace "branchChainElim" (ppr forest) $ -}
                              replaceLabels (mapFromList edges) g
  where
    blocks = toBlockList g

    lone_branch_blocks :: [(BlockId, BlockId)]
      -- each (L,K) is a block of the form
      --   L : goto K
    lone_branch_blocks = mapCatMaybes isLoneBranch blocks

    call_succs = foldl add emptyBlockSet blocks
      where add :: BlockSet -> CmmBlock -> BlockSet
            add succs b =
              case lastNode b of
                (CmmCall _ (Just k) _ _ _) -> setInsert k succs
                (CmmForeignCall {succ=k})  -> setInsert k succs
                _                          -> succs

    isLoneBranch :: CmmBlock -> Maybe (BlockId, BlockId)
    isLoneBranch block
      | (JustC (CmmEntry id), [], JustC (CmmBranch target)) <- blockToNodeList block
      , not (setMember id call_succs)
      = Just (id,target)
      | otherwise
      = Nothing

    -- We build a graph from lone_branch_blocks (every node has only
    -- one out edge).  Then we
    --   - topologically sort the graph: if from A we can reach B,
    --     then A occurs before B in the result list.
    --   - depth-first search starting from the nodes in this list.
    --     This gives us a [[node]], in which each list is a dependency
    --     chain.
    --   - for each list [a1,a2,...an] replace branches to ai with an.
    --
    -- This approach nicely deals with cycles by ignoring them.
    -- Branches in a cycle will be redirected to somewhere in the
    -- cycle, but we don't really care where.  A cycle should be dead code,
    -- and so will be eliminated by removeUnreachableBlocks.
    --
    fromNode (b,_) = b
    toNode   a     = (a,a)

    all_block_ids :: LabelSet
    all_block_ids = setFromList (map fst lone_branch_blocks)
                      `setUnion`
                    setFromList (map snd lone_branch_blocks)

    forest = dfsTopSortG $ graphFromVerticesAndAdjacency nodes lone_branch_blocks
        where nodes = map toNode $ setElems $ all_block_ids

    edges  = [ (fromNode y, fromNode x)
             | (x:xs) <- map reverse forest, y <- xs ]

----------------------------------------------------------------

replaceLabels :: BlockEnv BlockId -> CmmGraph -> CmmGraph
replaceLabels env =
  replace_eid . mapGraphNodes1 txnode
   where
     replace_eid g = g {g_entry = lookup (g_entry g)}
     lookup id = mapLookup id env `orElse` id

     txnode :: CmmNode e x -> CmmNode e x
     txnode (CmmBranch bid)         = CmmBranch (lookup bid)
     txnode (CmmCondBranch p t f)   = CmmCondBranch (exp p) (lookup t) (lookup f)
     txnode (CmmSwitch e arms)      = CmmSwitch (exp e) (map (liftM lookup) arms)
     txnode (CmmCall t k a res r)   = CmmCall (exp t) (liftM lookup k) a res r
     txnode fc@CmmForeignCall{}     = fc{ args = map exp (args fc)
                                        , succ = lookup (succ fc) }
     txnode other                   = mapExpDeep exp other

     exp :: CmmExpr -> CmmExpr
     exp (CmmLit (CmmBlock bid))                = CmmLit (CmmBlock (lookup bid))
     exp (CmmStackSlot (CallArea (Young id)) i) = CmmStackSlot (CallArea (Young (lookup id))) i
     exp e                                      = e


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
            -- XXX: this is a recursive lookup, it follows chains until the lookup
            -- returns Nothing, at which point we return the last BlockId

----------------------------------------------------------------
-- Build a map from a block to its set of predecessors. Very useful.
predMap :: [CmmBlock] -> BlockEnv BlockSet
predMap blocks = foldr add_preds mapEmpty blocks -- find the back edges
  where add_preds block env = foldl (add (entryLabel block)) env (successors block)
        add bid env b' =
          mapInsert b' (setInsert bid (mapLookup b' env `orElse` setEmpty)) env

-----------------------------------------------------------------------------
--
-- Block concatenation
--
-----------------------------------------------------------------------------

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
  where
     blocks = postorderDfs g

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
            Just block | (_, m', l') <- blockToNodeList block
                -> blockOfNodeList (JustC h, (m ++ m'), l')


-----------------------------------------------------------------------------
--
-- Removing unreachable blocks
--
-----------------------------------------------------------------------------

removeUnreachableBlocks :: CmmGraph -> CmmGraph
removeUnreachableBlocks g
  | length blocks < mapSize (toBlockMap g) = ofBlockList (g_entry g) blocks
  | otherwise = g
  where blocks = postorderDfs g
