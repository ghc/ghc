module Dataflow (mapCmmTop, onBasicBlock, cmmLivenessComment, cmmLiveness) where

import Cmm
import PprCmm ()

import UniqSet
import UniqFM

import FastString
import Outputable

import Maybes

import Data.List
import Data.Maybe

cmmBranchSources :: [(BlockId, [BlockId])] -> [(BlockId, [BlockId])]
cmmBranchSources input =
    [(target, [s | (s, ts) <- input, target `elem` ts])
     | target <- targets] where
        targets = nub [t | (s, ts) <- input, t <- ts]

cmmBranchTargets :: CmmBasicBlock -> UniqSet BlockId
cmmBranchTargets (BasicBlock _ stmts) =
    mkUniqSet $ concatMap target stmts where
        target (CmmBranch ident) = [ident]
        target (CmmCondBranch _ ident) = [ident]
        target (CmmSwitch _ blocks) = mapMaybe id blocks
        target _ = []

--------------------------------------------------------------------------------

-- This should really be a state monad, but that is not in the core libraries
-- so we'll hack around it here.
-- The monad we're using is: type State a = s -> s

-- The variables that were made live and killed respectively
type CmmLive = UniqSet LocalReg
addLive new_live live = live `unionUniqSets` new_live
addKilled new_killed live = live `minusUniqSet` new_killed

-- Calculate the live and killed registers for a local block
cmmBlockLive :: UniqFM {-BlockId-} CmmLive -> CmmBasicBlock -> CmmLive
cmmBlockLive other_live (BasicBlock _ stmts) =
    foldr ((.) . (cmmStmtLive other_live)) id stmts emptyUniqSet

-- Helper for cmmLocalLiveness
cmmStmtLive :: UniqFM {-BlockId-} CmmLive -> CmmStmt -> (CmmLive -> CmmLive)
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
cmmStmtLive _ (CmmCall target results arguments _) =
    target_liveness .
    foldr ((.) . cmmExprLive) id (map fst arguments) .
    addKilled (mkUniqSet $ only_local_regs results) where
        only_local_regs [] = []
        only_local_regs ((CmmGlobal _,_):args) = only_local_regs args
        only_local_regs ((CmmLocal r,_):args) = r:only_local_regs args
        target_liveness =
            case target of
              (CmmForeignCall target _) -> cmmExprLive target
              (CmmPrim _) -> id
cmmStmtLive other_live (CmmBranch target) = addLive (lookupWithDefaultUFM other_live emptyUniqSet target)
cmmStmtLive other_live (CmmCondBranch expr target) = cmmExprLive expr . addLive (lookupWithDefaultUFM other_live emptyUniqSet target)
cmmStmtLive other_live (CmmSwitch expr targets) =
    cmmExprLive expr .
    (foldr ((.) . (addLive . lookupWithDefaultUFM other_live emptyUniqSet)) id (mapCatMaybes id targets))
cmmStmtLive _ (CmmJump expr params) =
    const (cmmExprLive expr $ foldr ((.) . cmmExprLive) id (map fst params) $ emptyUniqSet)
cmmStmtLive _ (CmmReturn params) =
    const (foldr ((.) . cmmExprLive) id (map fst params) $ emptyUniqSet)

--------

-- Helper for cmmLocalLiveness
cmmExprLive :: CmmExpr -> (CmmLive -> CmmLive)
cmmExprLive expr = addLive (mkUniqSet $ expr_liveness expr) where
    expr_liveness (CmmLit _) = []
    expr_liveness (CmmLoad expr _) = expr_liveness expr
    expr_liveness (CmmReg reg) = reg_liveness reg
    expr_liveness (CmmMachOp _ exprs) = concatMap expr_liveness exprs
    expr_liveness (CmmRegOff reg _) = reg_liveness reg
    reg_liveness (CmmLocal reg) = [reg]
    reg_liveness (CmmGlobal _) = []

cmmBlockUpdate ::
    UniqFM {-BlockId-} CmmBasicBlock
    -> BlockId
    -> Maybe BlockId
    -> UniqFM {-BlockId-} CmmLive
    -> Maybe (UniqFM {-BlockId-} CmmLive)
cmmBlockUpdate blocks node _ state =
    let old_live = lookupWithDefaultUFM state emptyUniqSet node
        block = lookupWithDefaultUFM blocks (panic "unknown block id during liveness analysis") node
        new_live = cmmBlockLive state block
    in if (sizeUniqSet old_live) == (sizeUniqSet new_live)
       then Nothing
       else Just $ addToUFM state node new_live

cmmBlockDependants :: UniqFM {-BlockId-} (UniqSet BlockId) -> BlockId -> [BlockId]
cmmBlockDependants sources ident =
    uniqSetToList $ lookupWithDefaultUFM sources emptyUFM ident

cmmBlockSourcesAndTargets ::
    [CmmBasicBlock]
    -> (UniqFM {-BlockId-} (UniqSet BlockId), UniqFM (UniqSet BlockId))
cmmBlockSourcesAndTargets blocks = foldr aux (emptyUFM, emptyUFM) blocks where
    aux block (sourcesUFM, targetsUFM)  =
        (foldUniqSet add_source_edges sourcesUFM targets,
         addToUFM_Acc unionUniqSets id targetsUFM ident targets) where
            add_source_edges t ufm =
                addToUFM_Acc (flip addOneToUniqSet) unitUniqSet ufm t ident
            targets = cmmBranchTargets block
            ident = blockId block

cmmBlockNames :: [CmmBasicBlock] -> UniqFM {-BlockId-} CmmBasicBlock
cmmBlockNames blocks = listToUFM $ map block_name blocks where
    block_name b = (blockId b, b)

cmmLiveness :: [CmmBasicBlock] -> UniqFM {-BlockId-} CmmLive
cmmLiveness blocks =
    fixedpoint (cmmBlockDependants sources) (cmmBlockUpdate blocks')
               (map blockId blocks) emptyUFM where
                   (sources, targets) = cmmBlockSourcesAndTargets blocks
                   blocks' = cmmBlockNames blocks

cmmLivenessComment ::
    UniqFM {-BlockId-} (UniqSet LocalReg)
    -> CmmBasicBlock -> CmmBasicBlock
cmmLivenessComment live (BasicBlock ident stmts) =
    BasicBlock ident stmts' where
        stmts' = (CmmComment $ mkFastString $ showSDoc $ ppr $ live'):stmts
        live' = map CmmLocal $ uniqSetToList $ lookupWithDefaultUFM live emptyUniqSet ident

onBasicBlock f (CmmProc ds ident args blocks) = CmmProc ds ident args (f blocks)
onBasicBlock f x = x

mapCmmTop f (Cmm xs) = Cmm (map f xs)

--------------------------------------------------------------------------------

-- Solve a fixed-point of a dataflow problem.
-- O(N+H*E) calls to update where
--   N = number of nodes,
--   E = number of edges,
--   H = maximum height of the lattice for any particular node.
-- dependants: map from nodes to those who's value depend on the argument node
-- update:
--   Given the node which needs to be updated, and
--   which node caused that node to need to be updated,
--   update the state.
--   (The causing node will be 'Nothing' if this is the initial update.)
--   Must return 'Nothing' if no change,
--   otherwise returrn 'Just' of the new state
-- nodes: a set of nodes that initially need updating
-- state: some sort of state (usually a map)
--        containing the initial value for each node
--
-- Sketch for proof of complexity:
-- Note that the state is threaded through the entire execution.
-- Also note that the height of the latice at any particular node
-- is the number of times 'update' can return non-Nothing for a particular node.
-- Every call (except for the top level one) must be caused by a non-Nothing
-- result and each non-Nothing result causes as many calls as it has
-- out-going edges.  Thus any particular node, n, may cause in total
-- at most H*out(n) further calls.  When summed over all nodes,
-- that is H*E.  The N term of the complexity is from the initial call
-- when 'update' will be passed 'Nothing'.
fixedpoint ::
    (node -> [node])
    -> (node -> Maybe node -> s -> Maybe s)
    -> [node] -> s -> s
fixedpoint dependants update nodes state =
    foldr (fixedpoint' Nothing) state nodes where
        fixedpoint' cause node state =
            case update node cause state of
              Nothing -> state
              Just state' ->
                  foldr (fixedpoint' (Just node)) state' (dependants node)
