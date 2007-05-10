module Dataflow (mapCmmTop, onBasicBlock, cmmLivenessComment, cmmLiveness) where

import Cmm
import PprCmm

import Unique
import UniqSet
import UniqFM

import FastString
import Outputable

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
type CmmLiveness = (UniqSet LocalReg, UniqSet LocalReg)
addLocalLive new_live (live, killed) =
    (live `unionUniqSets` new_live, killed `minusUniqSet` new_live)
addLocalKilled new_killed (live, killed) =
    (live `minusUniqSet` new_killed, killed `unionUniqSets` new_killed)

-- Calculate the live and killed registers for a local block
cmmLocalLiveness :: CmmBasicBlock -> CmmLiveness
cmmLocalLiveness (BasicBlock _ stmts) =
    foldr ((.) . cmmStmtLocalLiveness) id stmts (emptyUniqSet, emptyUniqSet)

-- Helper for cmmLocalLiveness
cmmStmtLocalLiveness :: CmmStmt -> (CmmLiveness -> CmmLiveness)
cmmStmtLocalLiveness (CmmNop) = id
cmmStmtLocalLiveness (CmmComment _) = id
cmmStmtLocalLiveness (CmmAssign reg expr) =
    cmmExprLocalLiveness expr . reg_liveness where
        reg_liveness =
            case reg of
              (CmmLocal reg') -> addLocalKilled $ unitUniqSet reg'
              (CmmGlobal _) -> id
cmmStmtLocalLiveness (CmmStore expr1 expr2) =
    cmmExprLocalLiveness expr2 . cmmExprLocalLiveness expr1
cmmStmtLocalLiveness (CmmCall target results arguments _) =
    target_liveness .
    foldr ((.) . cmmExprLocalLiveness) id (map fst arguments) .
    addLocalKilled (mkUniqSet $ only_local_regs results) where
        only_local_regs [] = []
        only_local_regs ((CmmGlobal _,_):args) = only_local_regs args
        only_local_regs ((CmmLocal r,_):args) = r:only_local_regs args
        target_liveness =
            case target of
              (CmmForeignCall target _) -> cmmExprLocalLiveness target
              (CmmPrim _) -> id
cmmStmtLocalLiveness (CmmBranch _) = const (emptyUniqSet, emptyUniqSet)
cmmStmtLocalLiveness (CmmCondBranch expr _) = cmmExprLocalLiveness expr
cmmStmtLocalLiveness (CmmSwitch expr _) = cmmExprLocalLiveness expr
cmmStmtLocalLiveness (CmmJump expr params) =
    const (cmmExprLocalLiveness expr (mkUniqSet params, emptyUniqSet))

-- Helper for cmmLocalLiveness
cmmExprLocalLiveness :: CmmExpr -> (CmmLiveness -> CmmLiveness)
cmmExprLocalLiveness expr = addLocalLive (mkUniqSet $ expr_liveness expr) where
    expr_liveness (CmmLit _) = []
    expr_liveness (CmmLoad expr _) = expr_liveness expr
    expr_liveness (CmmReg reg) = reg_liveness reg
    expr_liveness (CmmMachOp _ exprs) = concatMap expr_liveness exprs
    expr_liveness (CmmRegOff reg _) = reg_liveness reg
    reg_liveness (CmmLocal reg) = [reg]
    reg_liveness (CmmGlobal _) = []

{-
branch_update ::
    UniqFM {-BlockId-} (UniqSet BlockId)
    -> UniqFM {-BlockId-} CmmLiveness
    -> BlockId
    -> UniqFM {-BlockId-} (UniqSet LocalReg)
    -> Maybe (UniqFM {-BlockId-} (UniqSet LocalReg))
branch_update targets local_liveness ident input_state =
    if (sizeUniqSet old_live) >= (sizeUniqSet new_live)
      then Nothing
      else Just $ addToUFM input_state ident new_live
    where
      old_live = lookupWithDefaultUFM input_state emptyUniqSet ident
      (born, killed) =
          lookupWithDefaultUFM
          local_liveness (emptyUniqSet, emptyUniqSet) ident
      target_live = unionManyUniqSets $
                    map (lookupWithDefaultUFM input_state emptyUniqSet) target
      target = uniqSetToList $ lookupWithDefaultUFM targets emptyUniqSet ident
      new_live = (target_live `minusUniqSet` killed) `unionUniqSets` born
-}

cmmBlockUpdate ::
    UniqFM {-BlockId-} CmmLiveness
    -> BlockId
    -> Maybe BlockId
    -> UniqFM {-BlockId-} (UniqSet LocalReg)
    -> Maybe (UniqFM {-BlockId-} (UniqSet LocalReg))
cmmBlockUpdate local_liveness ident cause input_state =
    let (born, killed) = lookupWithDefaultUFM
                         local_liveness (emptyUniqSet, emptyUniqSet) ident
        old_live = lookupWithDefaultUFM input_state emptyUniqSet ident
        cause_live =
            case cause of
              Just cause' -> lookupWithDefaultUFM input_state emptyUniqSet cause'
              Nothing -> emptyUniqSet
        new_live = old_live
                   `unionUniqSets` (cause_live `minusUniqSet` killed)
                   `unionUniqSets` born
    in {-trace (--(showSDoc $ ppr $ getUnique cause) ++ "-->" ++
              (showSDoc $ ppr $ getUnique ident) ++ ":" ++ 
              (showSDoc $ ppr $ map CmmLocal $ uniqSetToList $ cause_live) ++ ":" ++
              (showSDoc $ ppr $ map CmmLocal $ uniqSetToList $ old_live) ++ ":" ++
              (showSDoc $ ppr $ map CmmLocal $ uniqSetToList $ new_live) ++ "|" ++
              (show $ map (\(k,v) -> (k, showSDoc $ ppr $ map CmmLocal $ uniqSetToList v)) $ ufmToList input_state)) $-}
         if (sizeUniqSet old_live) == (sizeUniqSet new_live)
           then Nothing
           else Just $ addToUFM input_state ident new_live

cmmBlockDependants :: UniqFM {-BlockId-} (UniqSet BlockId) -> BlockId -> [BlockId]
cmmBlockDependants sources ident =
    uniqSetToList $ lookupWithDefaultUFM sources emptyUFM ident

cmmBlockLiveness :: [CmmBasicBlock] -> UniqFM {-BlockId-} CmmLiveness
cmmBlockLiveness blocks = listToUFM $ map block_info blocks where
    block_info block = (blockId block, cmmLocalLiveness block)

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

cmmLiveness :: [CmmBasicBlock] -> UniqFM {-BlockId-} (UniqSet LocalReg)
cmmLiveness blocks =
    fixedpoint (cmmBlockDependants sources) (cmmBlockUpdate liveness)
               (map blockId blocks) emptyUFM where
                   (sources, targets) = cmmBlockSourcesAndTargets blocks
                   liveness = cmmBlockLiveness blocks

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
