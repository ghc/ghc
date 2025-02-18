{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TupleSections              #-}
--
-- Copyright (c) 2018 Andreas Klebinger
--

module GHC.CmmToAsm.CFG
    ( CFG, CfgEdge(..), EdgeInfo(..), EdgeWeight(..)
    , TransitionSource(..)

    --Modify the CFG
    , addWeightEdge, addEdge
    , delEdge
    , addNodesBetween, shortcutWeightMap
    , reverseEdges, filterEdges
    , addImmediateSuccessor
    , mkWeightInfo, adjustEdgeWeight, setEdgeWeight

    --Query the CFG
    , infoEdgeList, edgeList
    , getSuccessorEdges, getSuccessors
    , getSuccEdgesSorted
    , getEdgeInfo
    , getCfgNodes, hasNode

    -- Loop Information
    , loopMembers, loopLevels, loopInfo

    --Construction/Misc
    , getCfg, getCfgProc, pprEdgeWeights, sanityCheckCfg

    --Find backedges and update their weight
    , optimizeCFG
    , mkGlobalWeights

     )
where

import GHC.Prelude
import GHC.Platform

import GHC.Cmm.BlockId
import GHC.Cmm as Cmm

import GHC.Cmm.Switch
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dataflow.Block
import qualified GHC.Cmm.Dataflow.Graph as G

import GHC.Utils.Misc
import GHC.Data.Graph.Directed
import GHC.Data.Maybe

import GHC.Types.Unique
import qualified GHC.CmmToAsm.CFG.Dominators as Dom
import GHC.CmmToAsm.CFG.Weight
import GHC.Data.Word64Map.Strict (Word64Map)
import GHC.Data.Word64Set (Word64Set)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)

import qualified Data.IntMap.Strict as IM
import qualified GHC.Data.Word64Map.Strict as WM
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified GHC.Data.Word64Set as WS
import qualified Data.Set as S
import Data.Tree
import Data.Bifunctor

import GHC.Utils.Outputable
import GHC.Utils.Panic
-- DEBUGGING ONLY
--import GHC.Cmm.DebugBlock
--import GHC.Data.OrdList
--import GHC.Cmm.DebugBlock.Trace

import Data.List (sort, nub, partition)
import Data.STRef.Strict
import Control.Monad.ST

import Data.Array.MArray
import Data.Array.ST
import Data.Array.IArray
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.Base (unsafeRead, unsafeWrite)

import Control.Monad
import GHC.Data.UnionFind
import Data.Word

type Prob = Double

type Edge = (BlockId, BlockId)
type Edges = [Edge]

newtype EdgeWeight
  = EdgeWeight { weightToDouble :: Double }
  deriving (Eq,Ord,Enum,Num,Real,Fractional)

instance Outputable EdgeWeight where
  ppr (EdgeWeight w) = doublePrec 5 w

type EdgeInfoMap edgeInfo = LabelMap (LabelMap edgeInfo)

-- | A control flow graph where edges have been annotated with a weight.
-- Implemented as IntMap (IntMap \<edgeData>)
-- We must uphold the invariant that for each edge A -> B we must have:
-- A entry B in the outer map.
-- A entry B in the map we get when looking up A.
-- Maintaining this invariant is useful as any failed lookup now indicates
-- an actual error in code which might go unnoticed for a while
-- otherwise.
type CFG = EdgeInfoMap EdgeInfo

data CfgEdge
  = CfgEdge
  { edgeFrom :: !BlockId
  , edgeTo :: !BlockId
  , edgeInfo :: !EdgeInfo
  }

-- | Careful! Since we assume there is at most one edge from A to B
--   the Eq instance does not consider weight.
instance Eq CfgEdge where
  (==) (CfgEdge from1 to1 _) (CfgEdge from2 to2 _)
    = from1 == from2 && to1 == to2

-- | Edges are sorted ascending pointwise by weight, source and destination
instance Ord CfgEdge where
  compare (CfgEdge from1 to1 (EdgeInfo {edgeWeight = weight1}))
          (CfgEdge from2 to2 (EdgeInfo {edgeWeight = weight2}))
    | weight1 < weight2 || weight1 == weight2 && from1 < from2 ||
      weight1 == weight2 && from1 == from2 && to1 < to2
    = LT
    | from1 == from2 && to1 == to2 && weight1 == weight2
    = EQ
    | otherwise
    = GT

instance Outputable CfgEdge where
  ppr (CfgEdge from1 to1 edgeInfo)
    = parens (ppr from1 <+> text "-(" <> ppr edgeInfo <> text ")->" <+> ppr to1)

-- | Can we trace back a edge to a specific Cmm Node
-- or has it been introduced during assembly codegen. We use this to maintain
-- some information which would otherwise be lost during the
-- Cmm \<-> asm transition.
-- See also Note [Inverting conditions]
data TransitionSource
  = CmmSource { trans_cmmNode :: (CmmNode O C)
              , trans_info :: BranchInfo }
  | AsmCodeGen
  deriving (Eq)

data BranchInfo = NoInfo         -- ^ Unknown, but not heap or stack check.
                | HeapStackCheck -- ^ Heap or stack check
    deriving Eq

instance Outputable BranchInfo where
    ppr NoInfo = text "regular"
    ppr HeapStackCheck = text "heap/stack"

isHeapOrStackCheck :: TransitionSource -> Bool
isHeapOrStackCheck (CmmSource { trans_info = HeapStackCheck}) = True
isHeapOrStackCheck _ = False

-- | Information about edges
data EdgeInfo
  = EdgeInfo
  { transitionSource :: !TransitionSource
  , edgeWeight :: !EdgeWeight
  } deriving (Eq)

instance Outputable EdgeInfo where
  ppr edgeInfo = text "weight:" <+> ppr (edgeWeight edgeInfo)

-- | Convenience function, generate edge info based
--   on weight not originating from cmm.
mkWeightInfo :: EdgeWeight -> EdgeInfo
mkWeightInfo = EdgeInfo AsmCodeGen

-- | Adjust the weight between the blocks using the given function.
--   If there is no such edge returns the original map.
adjustEdgeWeight :: CFG -> (EdgeWeight -> EdgeWeight)
                 -> BlockId -> BlockId -> CFG
adjustEdgeWeight cfg f from to
  | Just info <- getEdgeInfo from to cfg
  , !weight <- edgeWeight info
  , !newWeight <- f weight
  = addEdge from to (info { edgeWeight = newWeight}) cfg
  | otherwise = cfg

-- | Set the weight between the blocks to the given weight.
--   If there is no such edge returns the original map.
setEdgeWeight :: CFG -> EdgeWeight
              -> BlockId -> BlockId -> CFG
setEdgeWeight cfg !weight from to
  | Just info <- getEdgeInfo from to cfg
  = addEdge from to (info { edgeWeight = weight}) cfg
  | otherwise = cfg


getCfgNodes :: CFG -> [BlockId]
getCfgNodes m =
    mapKeys m

-- | Is this block part of this graph?
hasNode :: CFG -> BlockId -> Bool
hasNode m node =
  -- Check the invariant that each node must exist in the first map or not at all.
  assert (found || not (any (mapMember node) m))
  found
    where
      found = mapMember node m



-- | Check if the nodes in the cfg and the set of blocks are the same.
--   In a case of a mismatch we panic and show the difference.
sanityCheckCfg :: CFG -> LabelSet -> SDoc -> Bool
sanityCheckCfg m blockSet msg
    | blockSet == cfgNodes
    = True
    | otherwise =
        pprPanic "Block list and cfg nodes don't match" (
            text "difference:" <+> ppr diff $$
            text "blocks:" <+> ppr blockSet $$
            text "cfg:" <+> pprEdgeWeights m $$
            msg )
            False
    where
      cfgNodes = setFromList $ getCfgNodes m :: LabelSet
      diff = (setUnion cfgNodes blockSet) `setDifference` (setIntersection cfgNodes blockSet) :: LabelSet

-- | Filter the CFG with a custom function f.
--   Parameters are `f from to edgeInfo`
filterEdges :: (BlockId -> BlockId -> EdgeInfo -> Bool) -> CFG -> CFG
filterEdges f cfg =
    mapMapWithKey filterSources cfg
    where
      filterSources from m =
        mapFilterWithKey (\to w -> f from to w) m


{- Note [Updating the CFG during shortcutting]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Note [What is shortcutting] in the control flow optimization
code (GHC.Cmm.ContFlowOpt) for a slightly more in depth explanation on shortcutting.

In the native backend we shortcut jumps at the assembly level. ("GHC.CmmToAsm")
This means we remove blocks containing only one jump from the code
and instead redirecting all jumps targeting this block to the deleted
blocks jump target.

However we want to have an accurate representation of control
flow in the CFG. So we add/remove edges accordingly to account
for the eliminated blocks and new edges.

If we shortcut A -> B -> C to A -> C:
* We delete edges A -> B and B -> C
* Replacing them with the edge A -> C

We also try to preserve jump weights while doing so.

Note that:
* The edge B -> C can't have interesting weights since
  the block B consists of a single unconditional jump without branching.
* We delete the edge A -> B and add the edge A -> C.
* The edge A -> B can be one of many edges originating from A so likely
  has edge weights we want to preserve.

For this reason we simply store the edge info from the original A -> B
edge and apply this information to the new edge A -> C.

Sometimes we have a scenario where jump target C is not represented by an
BlockId but an immediate value. I'm only aware of this happening without
tables next to code currently.

Then we go from A ---> B - -> IMM   to   A - -> IMM where the dashed arrows
are not stored in the CFG.

In that case we simply delete the edge A -> B.

In terms of implementation the native backend first builds a mapping
from blocks suitable for shortcutting to their jump targets.
Then it redirects all jump instructions to these blocks using the
built up mapping.
This function (shortcutWeightMap) takes the same mapping and
applies the mapping to the CFG in the way laid out above.

-}
shortcutWeightMap :: LabelMap (Maybe BlockId) -> CFG -> CFG
shortcutWeightMap cuts cfg
  | mapNull cuts = cfg
  | otherwise = normalised_cfg
    where
      -- First take the cuts map and collapse any shortcuts, for example
      -- if the cuts map has A -> B and B -> C then we want to rewrite
      -- A -> C and B -> C directly.
      normalised_cuts_st :: forall s . ST s (LabelMap (Maybe BlockId))
      normalised_cuts_st = do
        (null :: Point s (Maybe BlockId)) <- fresh Nothing
        let cuts_list = mapToList cuts
        -- Create a unification variable for each of the nodes in a rewrite
        cuts_vars <- traverse (\p -> (p,) <$> fresh (Just p)) (concatMap (\(a, b) -> [a] ++ maybe [] (:[]) b) cuts_list)
        let cuts_map = mapFromList cuts_vars :: LabelMap (Point s (Maybe BlockId))
        -- Then unify according to the rewrites in the cuts map
        mapM_ (\(from, to) -> expectJust (mapLookup from cuts_map)
                              `union` expectJust (maybe (Just null) (flip mapLookup cuts_map) to) ) cuts_list
        -- Then recover the unique representative, which is the result of following
        -- the chain to the end.
        mapM find cuts_map

      normalised_cuts = runST normalised_cuts_st

      cuts_domain :: LabelSet
      cuts_domain = setFromList $ mapKeys cuts

      -- The CFG is shortcutted using the normalised cuts map
      normalised_cfg :: CFG
      normalised_cfg = mapFoldlWithKey update_edge mapEmpty cfg

      update_edge :: CFG -> Label -> LabelMap EdgeInfo -> CFG
      update_edge new_map from edge_map
        -- If the from edge is in the cuts map then delete the edge
        | setMember from cuts_domain = new_map
        -- Otherwise we are keeping the edge, but might have shortcutted some of
        -- the target nodes.
        | otherwise = mapInsert from (mapFoldlWithKey update_from_edge mapEmpty edge_map) new_map

      update_from_edge :: LabelMap a -> Label -> a -> LabelMap a
      update_from_edge new_map to_edge edge_info
        -- Edge is in the normalised cuts
        | Just new_edge <- mapLookup to_edge normalised_cuts =
            case new_edge of
              -- The result was Nothing, so edge is deleted
              Nothing -> new_map
              -- The new target for the edge, write it with the old edge_info.
              Just new_to -> mapInsert new_to edge_info new_map
        -- Node wasn't in the cuts map, so just add it back
        | otherwise = mapInsert to_edge edge_info new_map


-- | Sometimes we insert a block which should unconditionally be executed
--   after a given block. This function updates the CFG for these cases.
--  So we get A -> B    => A -> A' -> B
--             \                  \
--              -> C    =>         -> C
--
addImmediateSuccessor :: Weights -> BlockId -> BlockId -> CFG -> CFG
addImmediateSuccessor weights node follower cfg
    = updateEdges . addWeightEdge node follower weight $ cfg
    where
        weight = fromIntegral (uncondWeight weights)
        targets = getSuccessorEdges cfg node
        successors = map fst targets :: [BlockId]
        updateEdges = addNewSuccs . remOldSuccs
        remOldSuccs m = foldl' (flip (delEdge node)) m successors
        addNewSuccs m =
          foldl' (\m' (t,info) -> addEdge follower t info m') m targets

-- | Adds a new edge, overwrites existing edges if present
addEdge :: BlockId -> BlockId -> EdgeInfo -> CFG -> CFG
addEdge from to info cfg =
    mapAlter addFromToEdge from $
    mapAlter addDestNode to cfg
    where
        -- Simply insert the edge into the edge list.
        addFromToEdge Nothing = Just $ mapSingleton to info
        addFromToEdge (Just wm) = Just $ mapInsert to info wm
        -- We must add the destination node explicitly
        addDestNode Nothing = Just $ mapEmpty
        addDestNode n@(Just _) = n


-- | Adds a edge with the given weight to the cfg
--   If there already existed an edge it is overwritten.
--   `addWeightEdge from to weight cfg`
addWeightEdge :: BlockId -> BlockId -> EdgeWeight -> CFG -> CFG
addWeightEdge from to weight cfg =
    addEdge from to (mkWeightInfo weight) cfg

delEdge :: BlockId -> BlockId -> CFG -> CFG
delEdge from to m =
    mapAdjust (mapDelete to) from m


-- | Destinations from bid ordered by weight (descending)
getSuccEdgesSorted :: CFG -> BlockId -> [(BlockId,EdgeInfo)]
getSuccEdgesSorted m bid =
    let destMap = mapFindWithDefault mapEmpty bid m
        cfgEdges = mapToList destMap
        sortedEdges = sortWith (negate . edgeWeight . snd) cfgEdges
    in  --pprTrace "getSuccEdgesSorted" (ppr bid <+> text "map:" <+> ppr m)
        sortedEdges

-- | Get successors of a given node with edge weights.
getSuccessorEdges :: HasDebugCallStack => CFG -> BlockId -> [(BlockId,EdgeInfo)]
getSuccessorEdges m bid = maybe lookupError mapToList (mapLookup bid m)
  where
    lookupError = pprPanic "getSuccessorEdges: Block does not exist" $
                    ppr bid <+> pprEdgeWeights m

getEdgeInfo :: BlockId -> BlockId -> CFG -> Maybe EdgeInfo
getEdgeInfo from to m
    | Just wm <- mapLookup from m
    , Just info <- mapLookup to wm
    = Just $! info
    | otherwise
    = Nothing

getEdgeWeight :: CFG -> BlockId -> BlockId -> EdgeWeight
getEdgeWeight cfg from to = edgeWeight $ expectJust $ getEdgeInfo from to cfg

getTransitionSource :: BlockId -> BlockId -> CFG -> TransitionSource
getTransitionSource from to cfg = transitionSource $ expectJust $ getEdgeInfo from to cfg

reverseEdges :: CFG -> CFG
reverseEdges cfg = mapFoldlWithKey (\cfg from toMap -> go (addNode cfg from) from toMap) mapEmpty cfg
  where
    -- We must preserve nodes without outgoing edges!
    addNode :: CFG -> BlockId -> CFG
    addNode cfg b = mapInsertWith mapUnion b mapEmpty cfg
    go :: CFG -> BlockId -> (LabelMap EdgeInfo) -> CFG
    go cfg from toMap = mapFoldlWithKey (\cfg to info -> addEdge to from info cfg) cfg toMap  :: CFG


-- | Returns a unordered list of all edges with info
infoEdgeList :: CFG -> [CfgEdge]
infoEdgeList m =
    go (mapToList m) []
  where
    -- We avoid foldMap to avoid thunk buildup
    go :: [(BlockId,LabelMap EdgeInfo)] -> [CfgEdge] -> [CfgEdge]
    go [] acc = acc
    go ((from,toMap):xs) acc
      = go' xs from (mapToList toMap) acc
    go' :: [(BlockId,LabelMap EdgeInfo)] -> BlockId -> [(BlockId,EdgeInfo)] -> [CfgEdge] -> [CfgEdge]
    go' froms _    []              acc = go froms acc
    go' froms from ((to,info):tos) acc
      = go' froms from tos (CfgEdge from to info : acc)

-- | Returns a unordered list of all edges without weights
edgeList :: CFG -> [Edge]
edgeList m =
    go (mapToList m) []
  where
    -- We avoid foldMap to avoid thunk buildup
    go :: [(BlockId,LabelMap EdgeInfo)] -> [Edge] -> [Edge]
    go [] acc = acc
    go ((from,toMap):xs) acc
      = go' xs from (mapKeys toMap) acc
    go' :: [(BlockId,LabelMap EdgeInfo)] -> BlockId -> [BlockId] -> [Edge] -> [Edge]
    go' froms _    []              acc = go froms acc
    go' froms from (to:tos) acc
      = go' froms from tos ((from,to) : acc)

-- | Get successors of a given node without edge weights.
getSuccessors :: HasDebugCallStack => CFG -> BlockId -> [BlockId]
getSuccessors m bid
    | Just wm <- mapLookup bid m
    = mapKeys wm
    | otherwise = lookupError
    where
      lookupError = pprPanic "getSuccessors: Block does not exist" $
                    ppr bid <+> pprEdgeWeights m

pprEdgeWeights :: CFG -> SDoc
pprEdgeWeights m =
    let edges = sort $ infoEdgeList m :: [CfgEdge]
        printEdge (CfgEdge from to (EdgeInfo { edgeWeight = weight }))
            = text "\t" <> ppr from <+> text "->" <+> ppr to <>
              text "[label=\"" <> ppr weight <> text "\",weight=\"" <>
              ppr weight <> text "\"];\n"
        --for the case that there are no edges from/to this node.
        --This should rarely happen but it can save a lot of time
        --to immediately see it when it does.
        printNode node
            = text "\t" <> ppr node <> text ";\n"
        getEdgeNodes (CfgEdge from to _) = [from,to]
        edgeNodes = setFromList $ concatMap getEdgeNodes edges :: LabelSet
        nodes = filter (\n -> (not . setMember n) edgeNodes) . mapKeys $ mapFilter null m
    in
    text "digraph {\n" <>
        (foldl' (<>) empty (map printEdge edges)) <>
        (foldl' (<>) empty (map printNode nodes)) <>
    text "}\n"

{-# INLINE updateEdgeWeight #-} --Allows eliminating the tuple when possible
-- | Invariant: The edge **must** exist already in the graph.
updateEdgeWeight :: (EdgeWeight -> EdgeWeight) -> Edge -> CFG -> CFG
updateEdgeWeight f (from, to) cfg
    | Just oldInfo <- getEdgeInfo from to cfg
    = let !oldWeight = edgeWeight oldInfo
          !newWeight = f oldWeight
      in addEdge from to (oldInfo {edgeWeight = newWeight}) cfg
    | otherwise
    = panic "Trying to update invalid edge"

-- from to oldWeight => newWeight
mapWeights :: (BlockId -> BlockId -> EdgeWeight -> EdgeWeight) -> CFG -> CFG
mapWeights f cfg =
  foldl' (\cfg (CfgEdge from to info) ->
            let oldWeight = edgeWeight info
                newWeight = f from to oldWeight
            in addEdge from to (info {edgeWeight = newWeight}) cfg)
          cfg (infoEdgeList cfg)


-- | Insert a block in the control flow between two other blocks.
-- We pass a list of tuples (A,B,C) where
-- * A -> C: Old edge
-- * A -> B -> C : New Arc, where B is the new block.
-- It's possible that a block has two jumps to the same block
-- in the assembly code. However we still only store a single edge for
-- these cases.
-- We assign the old edge info to the edge A -> B and assign B -> C the
-- weight of an unconditional jump.
addNodesBetween :: Weights -> CFG -> [(BlockId,BlockId,BlockId)] -> CFG
addNodesBetween weights m updates =
  foldl'  updateWeight m .
          weightUpdates $ updates
    where
      weight = fromIntegral (uncondWeight weights)
      -- We might add two blocks for different jumps along a single
      -- edge. So we end up with edges:   A -> B -> C   ,   A -> D -> C
      -- in this case after applying the first update the weight for A -> C
      -- is no longer available. So we calculate future weights before updates.
      weightUpdates = map getWeight
      getWeight :: (BlockId,BlockId,BlockId) -> (BlockId,BlockId,BlockId,EdgeInfo)
      getWeight (from,between,old)
        | Just edgeInfo <- getEdgeInfo from old m
        = (from,between,old,edgeInfo)
        | otherwise
        = pprPanic "Can't find weight for edge that should have one" (
            text "triple" <+> ppr (from,between,old) $$
            text "updates" <+> ppr updates $$
            text "cfg:" <+> pprEdgeWeights m )
      updateWeight :: CFG -> (BlockId,BlockId,BlockId,EdgeInfo) -> CFG
      updateWeight m (from,between,old,edgeInfo)
        = addEdge from between edgeInfo .
          addWeightEdge between old weight .
          delEdge from old $ m

{-
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ~~~       Note [CFG Edge Weights]    ~~~
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Edge weights assigned do not currently represent a specific
  cost model and rather just a ranking of which blocks should
  be placed next to each other given their connection type in
  the CFG.
  This is especially relevant if we whenever two blocks will
  jump to the same target.

                     A   B
                      \ /
                       C

  Should A or B be placed in front of C? The block layout algorithm
  decides this based on which edge (A,C)/(B,C) is heavier. So we
  make a educated guess on which branch should be preferred.

  We rank edges in this order:
  * Unconditional Control Transfer - They will always
    transfer control to their target. Unless there is a info table
    we can turn the jump into a fallthrough as well.
    We use 20k as default, so it's easy to spot if values have been
    modified but unlikely that we run into issues with overflow.
  * If branches (likely) - We assume branches marked as likely
    are taken more than 80% of the time.
    By ranking them below unconditional jumps we make sure we
    prefer the unconditional if there is a conditional and
    unconditional edge towards a block.
  * If branches (regular) - The false branch can potentially be turned
    into a fallthrough so we prefer it slightly over the true branch.
  * Unlikely branches - These can be assumed to be taken less than 20%
    of the time. So we given them one of the lowest priorities.
  * Switches - Switches at this level are implemented as jump tables
    so have a larger number of successors. So without more information
    we can only say that each individual successor is unlikely to be
    jumped to and we rank them accordingly.
  * Calls - We currently ignore calls completely:
        * By the time we return from a call there is a good chance
          that the address we return to has already been evicted from
          cache eliminating a main advantage sequential placement brings.
        * Calls always require a info table in front of their return
          address. This reduces the chance that we return to the same
          cache line further.

-}
-- | Generate weights for a Cmm proc based on some simple heuristics.
getCfgProc :: Platform -> Weights -> GenCmmDecl d h CmmGraph -> CFG
getCfgProc _        _       (CmmData {}) = mapEmpty
getCfgProc platform weights (CmmProc _info _lab _live graph) = getCfg platform weights graph

getCfg :: Platform -> Weights -> CmmGraph -> CFG
getCfg platform weights graph =
  foldl' insertEdge edgelessCfg $ concatMap getBlockEdges blocks
  where
    Weights
            { uncondWeight = uncondWeight
            , condBranchWeight = condBranchWeight
            , switchWeight = switchWeight
            , callWeight = callWeight
            , likelyCondWeight = likelyCondWeight
            , unlikelyCondWeight = unlikelyCondWeight
            --  Last two are used in other places
            --, infoTablePenalty = infoTablePenalty
            --, backEdgeBonus = backEdgeBonus
            } = weights
    -- Explicitly add all nodes to the cfg to ensure they are part of the
    -- CFG.
    edgelessCfg = mapFromList $ zip (map G.entryLabel blocks) (repeat mapEmpty)
    insertEdge :: CFG -> ((BlockId,BlockId),EdgeInfo) -> CFG
    insertEdge m ((from,to),weight) =
      mapAlter f from m
        where
          f :: Maybe (LabelMap EdgeInfo) -> Maybe (LabelMap EdgeInfo)
          f Nothing = Just $ mapSingleton to weight
          f (Just destMap) = Just $ mapInsert to weight destMap
    getBlockEdges :: CmmBlock -> [((BlockId,BlockId),EdgeInfo)]
    getBlockEdges block =
      case branch of
        CmmBranch dest -> [mkEdge dest uncondWeight]
        CmmCondBranch cond t f l
          | l == Nothing ->
              [mkEdge f condBranchWeight,   mkEdge t condBranchWeight]
          | l == Just True ->
              [mkEdge f unlikelyCondWeight, mkEdge t likelyCondWeight]
          | l == Just False ->
              [mkEdge f likelyCondWeight,   mkEdge t unlikelyCondWeight]
          where
            mkEdgeInfo = -- pprTrace "Info" (ppr branchInfo <+> ppr cond)
                         EdgeInfo (CmmSource branch branchInfo) . fromIntegral
            mkEdge target weight = ((bid,target), mkEdgeInfo weight)
            branchInfo =
              foldRegsUsed
                (panic "GHC.CmmToAsm.CFG.getCfg: foldRegsUsed")
                (\info r -> if r == SpLim || r == HpLim || r == BaseReg
                    then HeapStackCheck else info)
                NoInfo cond

        (CmmSwitch _e ids) ->
          let switchTargets = switchTargetsToList ids
              --Compiler performance hack - for very wide switches don't
              --consider targets for layout.
              adjustedWeight =
                if (length switchTargets > 10) then -1 else switchWeight
          in map (\x -> mkEdge x adjustedWeight) switchTargets
        (CmmCall { cml_cont = Just cont})  -> [mkEdge cont callWeight]
        (CmmForeignCall {Cmm.succ = cont}) -> [mkEdge cont callWeight]
        (CmmCall { cml_cont = Nothing })   -> []
        other ->
            panic "Foo" $
            assertPpr False (text "Unknown successor cause:" <>
              (pdoc platform branch <+> text "=>" <> pdoc platform (G.successors other))) $
            map (\x -> ((bid,x),mkEdgeInfo 0)) $ G.successors other
      where
        bid = G.entryLabel block
        mkEdgeInfo = EdgeInfo (CmmSource branch NoInfo) . fromIntegral
        mkEdge target weight = ((bid,target), mkEdgeInfo weight)
        branch = lastNode block :: CmmNode O C

    blocks = revPostorder graph :: [CmmBlock]

--Find back edges by BFS
findBackEdges :: HasDebugCallStack => BlockId -> CFG -> Edges
findBackEdges root cfg =
    --pprTraceIt "Backedges:" $
    map fst .
    filter (\x -> snd x == Backward) $ typedEdges
  where
    edges = edgeList cfg :: [(BlockId,BlockId)]
    getSuccs = getSuccessors cfg :: BlockId -> [BlockId]
    typedEdges =
      classifyEdges root getSuccs edges :: [((BlockId,BlockId),EdgeType)]

optimizeCFG :: Bool -> Weights -> RawCmmDecl -> CFG -> CFG
optimizeCFG _ _ (CmmData {}) cfg = cfg
optimizeCFG doStaticPred weights proc@(CmmProc _info _lab _live graph) cfg =
  (if doStaticPred then staticPredCfg (g_entry graph) else id) $
    optHsPatterns weights proc $ cfg

-- | Modify branch weights based on educated guess on
-- patterns GHC tends to produce and how they affect
-- performance.
--
-- Most importantly we penalize jumps across info tables.
optHsPatterns :: Weights -> RawCmmDecl -> CFG -> CFG
optHsPatterns _ (CmmData {}) cfg = cfg
optHsPatterns weights (CmmProc info _lab _live graph) cfg =
    {-# SCC optHsPatterns #-}
    -- pprTrace "Initial:" (pprEdgeWeights cfg) $
    -- pprTrace "Initial:" (ppr $ mkGlobalWeights (g_entry graph) cfg) $

    -- pprTrace "LoopInfo:" (ppr $ loopInfo cfg (g_entry graph)) $
    favourFewerPreds  .
    penalizeInfoTables info .
    increaseBackEdgeWeight (g_entry graph) $ cfg
  where

    -- Increase the weight of all backedges in the CFG
    -- this helps to make loop jumpbacks the heaviest edges
    increaseBackEdgeWeight :: BlockId -> CFG -> CFG
    increaseBackEdgeWeight root cfg =
        let backedges = findBackEdges root cfg
            update weight
              --Keep irrelevant edges irrelevant
              | weight <= 0 = 0
              | otherwise
              = weight + fromIntegral (backEdgeBonus weights)
        in  foldl'  (\cfg edge -> updateEdgeWeight update edge cfg)
                    cfg backedges

    -- Since we cant fall through info tables we penalize these.
    penalizeInfoTables :: LabelMap a -> CFG -> CFG
    penalizeInfoTables info cfg =
        mapWeights fupdate cfg
      where
        fupdate :: BlockId -> BlockId -> EdgeWeight -> EdgeWeight
        fupdate _ to weight
          | mapMember to info
          = weight - (fromIntegral $ infoTablePenalty weights)
          | otherwise = weight

    -- If a block has two successors, favour the one with fewer
    -- predecessors and/or the one allowing fall through.
    favourFewerPreds :: CFG -> CFG
    favourFewerPreds cfg =
        let
            revCfg =
              reverseEdges $ filterEdges
                              (\_from -> fallthroughTarget)  cfg

            predCount n = length $ getSuccessorEdges revCfg n
            nodes = getCfgNodes cfg

            modifiers :: Int -> Int -> (EdgeWeight, EdgeWeight)
            modifiers preds1 preds2
              | preds1 <  preds2 = ( 1,-1)
              | preds1 == preds2 = ( 0, 0)
              | otherwise        = (-1, 1)

            update :: CFG -> BlockId -> CFG
            update cfg node
              | [(s1,e1),(s2,e2)] <- getSuccessorEdges cfg node
              , !w1 <- edgeWeight e1
              , !w2 <- edgeWeight e2
              --Only change the weights if there isn't already a ordering.
              , w1 == w2
              , (mod1,mod2) <- modifiers (predCount s1) (predCount s2)
              = (\cfg' ->
                  (adjustEdgeWeight cfg' (+mod2) node s2))
                    (adjustEdgeWeight cfg  (+mod1) node s1)
              | otherwise
              = cfg
        in foldl' update cfg nodes
      where
        fallthroughTarget :: BlockId -> EdgeInfo -> Bool
        fallthroughTarget to (EdgeInfo source _weight)
          | mapMember to info = False
          | AsmCodeGen <- source = True
          | CmmSource { trans_cmmNode = CmmBranch {} } <- source = True
          | CmmSource { trans_cmmNode = CmmCondBranch {} } <- source = True
          | otherwise = False

-- | Convert block-local branch weights to global weights.
staticPredCfg :: BlockId -> CFG -> CFG
staticPredCfg entry cfg = cfg'
  where
    (_, globalEdgeWeights) = {-# SCC mkGlobalWeights #-}
                             mkGlobalWeights entry cfg
    cfg' = {-# SCC rewriteEdges #-}
            mapFoldlWithKey
                (\cfg from m ->
                    mapFoldlWithKey
                        (\cfg to w -> setEdgeWeight cfg (EdgeWeight w) from to )
                        cfg m )
                cfg
                globalEdgeWeights

-- | Determine loop membership of blocks based on SCC analysis
--   This is faster but only gives yes/no answers.
loopMembers :: HasDebugCallStack => CFG -> LabelMap Bool
loopMembers cfg =
    foldl' (flip setLevel) mapEmpty sccs
  where
    mkNode :: BlockId -> Node BlockId BlockId
    mkNode bid = DigraphNode bid bid (getSuccessors cfg bid)
    nodes = map mkNode (getCfgNodes cfg)

    sccs = stronglyConnCompFromEdgedVerticesOrd nodes

    setLevel :: SCC BlockId -> LabelMap Bool -> LabelMap Bool
    setLevel (AcyclicSCC bid) m = mapInsert bid False m
    setLevel (CyclicSCC bids) m = foldl' (\m k -> mapInsert k True m) m bids

loopLevels :: CFG -> BlockId -> LabelMap Int
loopLevels cfg root = liLevels loopInfos
    where
      loopInfos = loopInfo cfg root

data LoopInfo = LoopInfo
  { liBackEdges :: [(Edge)] -- ^ List of back edges
  , liLevels :: LabelMap Int -- ^ BlockId -> LoopLevel mapping
  , liLoops :: [(Edge, LabelSet)] -- ^ (backEdge, loopBody), body includes header
  }

instance Outputable LoopInfo where
    ppr (LoopInfo _ _lvls loops) =
        text "Loops:(backEdge, bodyNodes)" $$
            (vcat $ map ppr loops)

{-  Note [Determining the loop body]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Starting with the knowledge that:
    * head dominates the loop
    * `tail` -> `head` is a backedge

    We can determine all nodes by:
    * Deleting the loop head from the graph.
    * Collect all blocks which are reachable from the `tail`.

    We do so by performing bfs from the tail node towards the head.
 -}

-- | Determine loop membership of blocks based on Dominator analysis.
--   This is slower but gives loop levels instead of just loop membership.
--   However it only detects natural loops. Irreducible control flow is not
--   recognized even if it loops. But that is rare enough that we don't have
--   to care about that special case.
loopInfo :: HasDebugCallStack => CFG -> BlockId -> LoopInfo
loopInfo cfg root = LoopInfo  { liBackEdges = backEdges
                              , liLevels = mapFromList loopCounts
                              , liLoops = loopBodies }
  where
    revCfg = reverseEdges cfg

    graph = -- pprTrace "CFG - loopInfo" (pprEdgeWeights cfg) $
            fmap (setFromList . mapKeys ) cfg :: LabelMap LabelSet


    --TODO - This should be a no op: Export constructors? Use unsafeCoerce? ...
    rooted = ( fromBlockId root
              , toWord64Map $ fmap toWord64Set graph) :: (Word64, Word64Map Word64Set)
    tree = fmap toBlockId $ Dom.domTree rooted :: Tree BlockId

    -- Map from Nodes to their dominators
    domMap :: LabelMap LabelSet
    domMap = mkDomMap tree

    edges = edgeList cfg :: [(BlockId, BlockId)]
    -- We can't recompute nodes from edges, there might be blocks not connected via edges.
    nodes = getCfgNodes cfg :: [BlockId]

    -- identify back edges
    isBackEdge (from,to)
      | Just doms <- mapLookup from domMap
      , setMember to doms
      = True
      | otherwise = False

    -- See Note [Determining the loop body]
    -- Get the loop body associated with a back edge.
    findBody edge@(tail, head)
      = ( edge, setInsert head $ go (setSingleton tail) (setSingleton tail) )
      where
        -- See Note [Determining the loop body]


        go :: LabelSet -> LabelSet -> LabelSet
        go found current
          | setNull current = found
          | otherwise = go  (setUnion newSuccessors found)
                            newSuccessors
          where
            -- Really predecessors, since we use the reversed cfg.
            newSuccessors = setFilter (\n -> not $ setMember n found) successors :: LabelSet
            successors = setDelete head $ setUnions $ map
                                      (\x -> if x == head then setEmpty else setFromList (getSuccessors revCfg x))
                                      (setElems current) :: LabelSet

    backEdges = filter isBackEdge edges
    loopBodies = map findBody backEdges :: [(Edge, LabelSet)]

    -- Block b is part of n loop bodies => loop nest level of n
    loopCounts =
      let bodies = map (first snd) loopBodies -- [(Header, Body)]
          loopCount n = length $ nub . map fst . filter (setMember n . snd) $ bodies
      in  map (\n -> (n, loopCount n)) $ nodes :: [(BlockId, Int)]

    toWord64Set :: LabelSet -> Word64Set
    toWord64Set s = WS.fromList . map fromBlockId . setElems $ s
    toWord64Map :: LabelMap a -> Word64Map a
    toWord64Map m = WM.fromList $ map (\(x,y) -> (fromBlockId x,y)) $ mapToList m

    mkDomMap :: Tree BlockId -> LabelMap LabelSet
    mkDomMap root = mapFromList $ go setEmpty root
      where
        go :: LabelSet -> Tree BlockId -> [(Label,LabelSet)]
        go parents (Node lbl [])
          =  [(lbl, parents)]
        go parents (Node _ leaves)
          = let nodes = map rootLabel leaves
                entries = map (\x -> (x,parents)) nodes
            in  entries ++ concatMap
                            (\n -> go (setInsert (rootLabel n) parents) n)
                            leaves

    fromBlockId :: BlockId -> Word64
    fromBlockId = getKey . getUnique

    toBlockId :: Word64 -> BlockId
    toBlockId = mkBlockId . mkUniqueGrimily

-- We make the CFG a Hoopl Graph, so we can reuse revPostOrder.
newtype BlockNode (e :: Extensibility) (x :: Extensibility) = BN (BlockId,[BlockId])

instance G.NonLocal (BlockNode) where
  entryLabel (BN (lbl,_))   = lbl
  successors (BN (_,succs)) = succs

revPostorderFrom :: HasDebugCallStack => CFG -> BlockId -> [BlockId]
revPostorderFrom cfg root =
    map fromNode $ G.revPostorderFrom hooplGraph root
  where
    nodes = getCfgNodes cfg
    hooplGraph = foldl' (\m n -> mapInsert n (toNode n) m) mapEmpty nodes

    fromNode :: BlockNode C C -> BlockId
    fromNode (BN x) = fst x

    toNode :: BlockId -> BlockNode C C
    toNode bid =
        BN (bid,getSuccessors cfg $ bid)


-- | We take in a CFG which has on its edges weights which are
--   relative only to other edges originating from the same node.
--
--   We return a CFG for which each edge represents a GLOBAL weight.
--   This means edge weights are comparable across the whole graph.
--
--   For irreducible control flow results might be imprecise, otherwise they
--   are reliable.
--
--   The algorithm is based on the Paper
--   "Static Branch Prediction and Program Profile Analysis" by Y Wu, JR Larus
--   The only big change is that we go over the nodes in the body of loops in
--   reverse post order. Which is required for diamond control flow to work probably.
--
--   We also apply a few prediction heuristics (based on the same paper)
--
--   The returned result represents frequences.
--   For blocks it's the expected number of executions and
--   for edges is the number of traversals.

{-# NOINLINE mkGlobalWeights #-}
{-# SCC mkGlobalWeights #-}
mkGlobalWeights :: HasDebugCallStack => BlockId -> CFG -> (LabelMap Double, LabelMap (LabelMap Double))
mkGlobalWeights root localCfg
  | null localCfg = panic "Error - Empty CFG"
  | otherwise
  = (blockFreqs', edgeFreqs')
  where
    -- Calculate fixpoints
    (blockFreqs, edgeFreqs) = calcFreqs nodeProbs backEdges' bodies' revOrder'
    blockFreqs' = mapFromList $ map (first fromVertex) (assocs blockFreqs) :: LabelMap Double
    edgeFreqs' = fmap fromVertexMap $ fromVertexMap edgeFreqs

    fromVertexMap :: IM.IntMap x -> LabelMap x
    fromVertexMap m = mapFromList . map (first fromVertex) $ IM.toList m

    revOrder = revPostorderFrom localCfg root :: [BlockId]
    loopResults@(LoopInfo backedges _levels bodies) = loopInfo localCfg root

    revOrder' = map toVertex revOrder
    backEdges' = map (bimap toVertex toVertex) backedges
    bodies' = map calcBody bodies

    estimatedCfg = staticBranchPrediction root loopResults localCfg
    -- Normalize the weights to probabilities and apply heuristics
    nodeProbs = cfgEdgeProbabilities estimatedCfg toVertex

    -- By mapping vertices to numbers in reverse post order we can bring any subset into reverse post
    -- order simply by sorting.
    -- TODO: The sort is redundant if we can guarantee that setElems returns elements ascending
    calcBody (backedge, blocks) =
        (toVertex $ snd backedge, sort . map toVertex $ (setElems blocks))

    vertexMapping = mapFromList $ zip revOrder [0..] :: LabelMap Int
    blockMapping = listArray (0,mapSize vertexMapping - 1) revOrder :: Array Int BlockId
    -- Map from blockId to indices starting at zero
    toVertex :: BlockId -> Int
    toVertex   blockId  = expectJust $ mapLookup blockId vertexMapping
    -- Map from indices starting at zero to blockIds
    fromVertex :: Int -> BlockId
    fromVertex vertex   = blockMapping ! vertex

{- Note [Static Branch Prediction]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The work here has been based on the paper
"Static Branch Prediction and Program Profile Analysis" by Y Wu, JR Larus.

The primary differences are that if we branch on the result of a heap
check we do not apply any of the heuristics.
The reason is simple: They look like loops in the control flow graph
but are usually never entered, and if at most once.

Currently implemented is a heuristic to predict that we do not exit
loops (lehPredicts) and one to predict that backedges are more likely
than any other edge.

The back edge case is special as it supersedes any other heuristic if it
applies.

Do NOT rely solely on nofib results for benchmarking this. I recommend at least
comparing megaparsec and container benchmarks. Nofib does not seem to have
many instances of "loopy" Cmm where these make a difference.

TODO:
* The paper containers more benchmarks which should be implemented.
* If we turn the likelihood on if/else branches into a probability
  instead of true/false we could implement this as a Cmm pass.
  + The complete Cmm code still exists and can be accessed by the heuristics
  + There is no chance of register allocation/codegen inserting branches/blocks
  + making the TransitionSource info wrong.
  + potential to use this information in CmmPasses.
  - Requires refactoring of all the code relying on the binary nature of likelihood.
  - Requires refactoring `loopInfo` to work on both, Cmm Graphs and the backend CFG.
-}

-- | Combination of target node id and information about the branch
--   we are looking at.
type TargetNodeInfo = (BlockId, EdgeInfo)


-- | Update branch weights based on certain heuristics.
-- See Note [Static Branch Prediction]
-- TODO: This should be combined with optimizeCFG
{-# SCC staticBranchPrediction #-}
staticBranchPrediction :: BlockId -> LoopInfo -> CFG -> CFG
staticBranchPrediction _root (LoopInfo l_backEdges loopLevels l_loops) cfg =
    -- pprTrace "staticEstimatesOn" (ppr (cfg)) $
    foldl' update cfg nodes
  where
    nodes = getCfgNodes cfg
    backedges = S.fromList $ l_backEdges
    -- Loops keyed by their back edge
    loops = M.fromList $ l_loops :: M.Map Edge LabelSet
    loopHeads = S.fromList $ map snd $ M.keys loops

    update :: CFG -> BlockId -> CFG
    update cfg node
        -- No successors, nothing to do.
        | null successors = cfg

        -- Mix of backedges and others:
        -- Always predict the backedges.
        | not (null m) && length m < length successors
        -- Heap/Stack checks "loop", but only once.
        -- So we simply exclude any case involving them.
        , not $ any (isHeapOrStackCheck  . transitionSource . snd) successors
        = let   loopChance = repeat $! pred_LBH / (fromIntegral $ length m)
                exitChance = repeat $! (1 - pred_LBH) / fromIntegral (length not_m)
                updates = zip (map fst m) loopChance ++ zip (map fst not_m) exitChance
        in  -- pprTrace "mix" (ppr (node,successors)) $
            foldl' (\cfg (to,weight) -> setEdgeWeight cfg weight node to) cfg updates

        -- For (regular) non-binary branches we keep the weights from the STG -> Cmm translation.
        | length successors /= 2
        = cfg

        -- Only backedges - no need to adjust
        | length m > 0
        = cfg

        -- A regular binary branch, we can plug addition predictors in here.
        | [(s1,s1_info),(s2,s2_info)] <- successors
        , not $ any (isHeapOrStackCheck  . transitionSource . snd) successors
        = -- Normalize weights to total of 1
            let !w1 = max (edgeWeight s1_info) (0)
                !w2 = max (edgeWeight s2_info) (0)
                -- Of both weights are <= 0 we set both to 0.5
                normalizeWeight w = if w1 + w2 == 0 then 0.5 else w/(w1+w2)
                !cfg'  = setEdgeWeight cfg  (normalizeWeight w1) node s1
                !cfg'' = setEdgeWeight cfg' (normalizeWeight w2) node s2

                -- Figure out which heuristics apply to these successors
                heuristics = map ($ ((s1,s1_info),(s2,s2_info)))
                            [lehPredicts, phPredicts, ohPredicts, ghPredicts, lhhPredicts, chPredicts
                            , shPredicts, rhPredicts]
                -- Apply result of a heuristic. Argument is the likelihood
                -- predicted for s1.
                applyHeuristic :: CFG -> Maybe Prob -> CFG
                applyHeuristic cfg Nothing = cfg
                applyHeuristic cfg (Just (s1_pred :: Double))
                  | s1_old == 0 || s2_old == 0 ||
                    isHeapOrStackCheck (transitionSource s1_info) ||
                    isHeapOrStackCheck (transitionSource s2_info)
                  = cfg
                  | otherwise =
                    let -- Predictions from heuristic
                        s1_prob = EdgeWeight s1_pred :: EdgeWeight
                        s2_prob = 1.0 - s1_prob
                        -- Update
                        d = (s1_old * s1_prob) + (s2_old * s2_prob) :: EdgeWeight
                        s1_prob' = s1_old * s1_prob / d
                        !s2_prob' = s2_old * s2_prob / d
                        !cfg_s1 = setEdgeWeight cfg    s1_prob' node s1
                    in  -- pprTrace "Applying heuristic!" (ppr (node,s1,s2) $$ ppr (s1_prob', s2_prob')) $
                        setEdgeWeight cfg_s1 s2_prob' node s2
                  where
                    -- Old weights
                    s1_old = getEdgeWeight cfg node s1
                    s2_old = getEdgeWeight cfg node s2

            in
            -- pprTraceIt "RegularCfgResult" $
            foldl' applyHeuristic cfg'' heuristics

        -- Branch on heap/stack check
        | otherwise = cfg

      where
        -- Chance that loops are taken.
        pred_LBH = 0.875
        -- successors
        successors = getSuccessorEdges cfg node
        -- backedges
        (m,not_m) = partition (\succ -> S.member (node, fst succ) backedges) successors

        -- Heuristics return nothing if they don't say anything about this branch
        -- or Just (prob_s1) where prob_s1 is the likelihood for s1 to be the
        -- taken branch. s1 is the branch in the true case.

        -- Loop exit heuristic.
        -- We are unlikely to leave a loop unless it's to enter another one.
        pred_LEH = 0.75
        -- If and only if no successor is a loopheader,
        -- then we will likely not exit the current loop body.
        lehPredicts :: (TargetNodeInfo,TargetNodeInfo) -> Maybe Prob
        lehPredicts ((s1,_s1_info),(s2,_s2_info))
          | S.member s1 loopHeads || S.member s2 loopHeads
          = Nothing

          | otherwise
          = --pprTrace "lehPredict:" (ppr $ compare s1Level s2Level) $
            case compare s1Level s2Level of
                EQ -> Nothing
                LT -> Just (1-pred_LEH) --s1 exits to a shallower loop level (exits loop)
                GT -> Just (pred_LEH)   --s1 exits to a deeper loop level
            where
                s1Level = mapLookup s1 loopLevels
                s2Level = mapLookup s2 loopLevels

        -- Comparing to a constant is unlikely to be equal.
        ohPredicts (s1,_s2)
            | CmmSource { trans_cmmNode = src1 } <- getTransitionSource node (fst s1) cfg
            , CmmCondBranch cond ltrue _lfalse likely <- src1
            , likely == Nothing
            , CmmMachOp mop args <- cond
            , MO_Eq {} <- mop
            , not (null [x | x@CmmLit{} <- args])
            = if fst s1 == ltrue then Just 0.3 else Just 0.7

            | otherwise
            = Nothing

        -- TODO: These are all the other heuristics from the paper.
        -- Not all will apply, for now we just stub them out as Nothing.
        phPredicts = const Nothing
        ghPredicts = const Nothing
        lhhPredicts = const Nothing
        chPredicts = const Nothing
        shPredicts = const Nothing
        rhPredicts = const Nothing

-- We normalize all edge weights as probabilities between 0 and 1.
-- Ignoring rounding errors all outgoing edges sum up to 1.
cfgEdgeProbabilities :: CFG -> (BlockId -> Int) -> IM.IntMap (IM.IntMap Prob)
cfgEdgeProbabilities cfg toVertex
    = mapFoldlWithKey foldEdges IM.empty cfg
  where
    foldEdges = (\m from toMap -> IM.insert (toVertex from) (normalize toMap) m)

    normalize :: (LabelMap EdgeInfo) -> (IM.IntMap Prob)
    normalize weightMap
        | edgeCount <= 1 = mapFoldlWithKey (\m k _ -> IM.insert (toVertex k) 1.0 m) IM.empty weightMap
        | otherwise = mapFoldlWithKey (\m k _ -> IM.insert (toVertex k) (normalWeight k) m) IM.empty weightMap
      where
        edgeCount = mapSize weightMap
        -- Negative weights are generally allowed but are mapped to zero.
        -- We then check if there is at least one non-zero edge and if not
        -- assign uniform weights to all branches.
        minWeight = 0 :: Prob
        weightMap' = fmap (\w -> max (weightToDouble . edgeWeight $ w) minWeight) weightMap
        totalWeight = sum weightMap'

        normalWeight :: BlockId -> Prob
        normalWeight bid
         | totalWeight == 0
         = 1.0 / fromIntegral edgeCount
         | Just w <- mapLookup bid weightMap'
         = w/totalWeight
         | otherwise = panic "impossible"

-- This is the fixpoint algorithm from
--   "Static Branch Prediction and Program Profile Analysis" by Y Wu, JR Larus
-- The adaption to Haskell is my own.
calcFreqs :: IM.IntMap (IM.IntMap Prob) -> [(Int,Int)] -> [(Int, [Int])] -> [Int]
          -> (Array Int Double, IM.IntMap (IM.IntMap Prob))
calcFreqs graph backEdges loops revPostOrder = runST $ do
    visitedNodes <- newArray (0,nodeCount-1) False :: ST s (STUArray s Int Bool)
    blockFreqs <- newArray (0,nodeCount-1) 0.0 :: ST s (STUArray s Int Double)
    edgeProbs <- newSTRef graph
    edgeBackProbs <- newSTRef graph

    -- let traceArray a = do
    --       vs <- forM [0..nodeCount-1] $ \i -> readArray a i >>= (\v -> return (i,v))
          -- trace ("array: " ++ show vs) $ return ()

    let  -- See #1600, we need to inline or unboxing makes perf worse.
        -- {-# INLINE getFreq #-}
        {-# INLINE visited #-}
        visited b = unsafeRead visitedNodes b
        getFreq b = unsafeRead blockFreqs b
        -- setFreq :: forall s. Int -> Double -> ST s ()
        setFreq b f = unsafeWrite blockFreqs b f
        -- setVisited :: forall s. Node -> ST s ()
        setVisited b = unsafeWrite visitedNodes b True
        -- Frequency/probability that edge is taken.
        getProb' arr b1 b2 = readSTRef arr >>=
            (\graph ->
                return .
                        fromMaybe (error "getFreq 1") .
                        IM.lookup b2 .
                        fromMaybe (error "getFreq 2") $
                        (IM.lookup b1 graph)
            )
        setProb' arr b1 b2 prob = do
          g <- readSTRef arr
          let !m = fromMaybe (error "Foo") $ IM.lookup b1 g
              !m' = IM.insert b2 prob m
          writeSTRef arr $! (IM.insert b1 m' g)

        getEdgeFreq b1 b2 = getProb' edgeProbs b1 b2
        setEdgeFreq b1 b2 = setProb' edgeProbs b1 b2
        getProb b1 b2 = fromMaybe (error "getProb") $ do
            m' <- IM.lookup b1 graph
            IM.lookup b2 m'

        getBackProb b1 b2 = getProb' edgeBackProbs b1 b2
        setBackProb b1 b2 = setProb' edgeBackProbs b1 b2


    let -- calcOutFreqs :: Node -> ST s ()
        calcOutFreqs bhead block = do
          !f <- getFreq block
          forM (successors block) $ \bi -> do
            let !prob = getProb block bi
            let !succFreq = f * prob
            setEdgeFreq block bi succFreq
            -- traceM $ "SetOut: " ++ show (block, bi, f, prob, succFreq)
            when (bi == bhead) $ setBackProb block bi succFreq


    let propFreq block head = do
            -- traceM ("prop:" ++ show (block,head))
            -- traceShowM block

            !v <- visited block
            if v then
                return () --Dont look at nodes twice
            else if block == head then
                setFreq block 1.0 -- Loop header frequency is always 1
            else do
                let preds = IS.elems $ predecessors block
                irreducible <- (fmap or) $ forM preds $ \bp -> do
                    !bp_visited <- visited bp
                    let bp_backedge = isBackEdge bp block
                    return (not bp_visited && not bp_backedge)

                if irreducible
                then return () -- Rare we don't care
                else do
                    setFreq block 0
                    !cycleProb <- sum <$> (forM preds $ \pred -> do
                        if isBackEdge pred block
                            then
                                getBackProb pred block
                            else do
                                !f <- getFreq block
                                !prob <- getEdgeFreq pred block
                                setFreq block $! f + prob
                                return 0)
                    -- traceM $ "cycleProb:" ++ show cycleProb
                    let limit = 1 - 1/512 -- Paper uses 1 - epsilon, but this works.
                                          -- determines how large likelyhoods in loops can grow.
                    !cycleProb <- return $ min cycleProb limit -- <- return $ if cycleProb > limit then limit else cycleProb
                    -- traceM $ "cycleProb:" ++ show cycleProb

                    !f <- getFreq block
                    setFreq block (f / (1.0 - cycleProb))

            setVisited block
            calcOutFreqs head block

    -- Loops, by nesting, inner to outer
    forM_ loops $ \(head, body) -> do
        forM_ [0 .. nodeCount - 1] (\i -> unsafeWrite visitedNodes i True) -- Mark all nodes as visited.
        forM_ body (\i -> unsafeWrite visitedNodes i False) -- Mark all blocks reachable from head as not visited
        forM_ body $ \block -> propFreq block head

    -- After dealing with all loops, deal with non-looping parts of the CFG
    forM_ [0 .. nodeCount - 1] (\i -> unsafeWrite visitedNodes i False) -- Everything in revPostOrder is reachable
    forM_ revPostOrder $ \block -> propFreq block (head revPostOrder)

    -- trace ("Final freqs:") $ return ()
    -- let freqString = pprFreqs freqs
    -- trace (unlines freqString) $ return ()
    -- trace (pprFre) $ return ()
    graph' <- readSTRef edgeProbs
    freqs' <- unsafeFreeze  blockFreqs

    return (freqs', graph')
  where
    -- How can these lookups fail? Consider the CFG [A -> B]
    predecessors :: Int -> IS.IntSet
    predecessors b = fromMaybe IS.empty $ IM.lookup b revGraph
    successors :: Int -> [Int]
    successors b = fromMaybe (lookupError "succ" b graph)$ IM.keys <$> IM.lookup b graph
    lookupError s b g = pprPanic ("Lookup error " ++ s) $
                            ( text "node" <+> ppr b $$
                                text "graph" <+>
                                vcat (map (\(k,m) -> ppr (k,m :: IM.IntMap Double)) $ IM.toList g)
                            )

    nodeCount = IM.foldl' (\count toMap -> IM.foldlWithKey' countTargets (count + 1) toMap) 0 graph
      where
        countTargets = (\count k _ -> countNode k + count )
        countNode n = if IM.member n graph then 0 else 1

    isBackEdge from to = S.member (from,to) backEdgeSet
    backEdgeSet = S.fromList backEdges

    revGraph :: IntMap IntSet
    revGraph = IM.foldlWithKey' (\m from toMap -> addEdges m from toMap) IM.empty graph
        where
            addEdges m0 from toMap = IM.foldlWithKey' (\m k _ -> addEdge m from k) m0 toMap
            addEdge m0 from to = IM.insertWith IS.union to (IS.singleton from) m0
