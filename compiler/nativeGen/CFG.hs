--
-- Copyright (c) 2018 Andreas Klebinger
--

{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -fprof-auto-exported #-}

module CFG
    ( CFG, CfgEdge(..), EdgeInfo(..), EdgeWeight(..)
    , TransitionSource(..)

    --Modify the CFG
    , addWeightEdge, addEdge
    , delEdge, delNode
    , addNodesBetween, shortcutWeightMap
    , reverseEdges, filterEdges
    , addImmediateSuccessor
    , mkWeightInfo, adjustEdgeWeight, setEdgeWeight

    --Query the CFG
    , infoEdgeList, edgeList
    , getSuccessorEdges, getSuccessors
    , getSuccEdgesSorted, weightedEdgeList
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

#include "HsVersions.h"

import GhcPrelude

import BlockId
import Cmm ( RawCmmDecl, GenCmmDecl( .. ), CmmBlock, succ, g_entry
           , CmmGraph )
import CmmNode
import CmmUtils
import CmmSwitch
import Hoopl.Collections
import Hoopl.Label
import Hoopl.Block
import qualified Hoopl.Graph as G

import Util
import Digraph
import Maybes

import Unique
import qualified Dominators as Dom
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Tree
import Data.Bifunctor

import Outputable
-- DEBUGGING ONLY
--import Debug
-- import Debug.Trace
--import OrdList
import PprCmm ()
import qualified DynFlags as D

import Data.List

import Data.STRef
import Control.Monad.ST

import Data.Array.MArray
import Data.Array.ST
import Data.Array

import Control.Monad

type Prob = Double

type Edge = (BlockId, BlockId)
type Edges = [Edge]

newtype EdgeWeight
  = EdgeWeight { weightToDouble :: Double }
  deriving (Eq,Ord,Enum,Num,Real,Fractional)

instance Outputable EdgeWeight where
  ppr (EdgeWeight w) = ppr w

type EdgeInfoMap edgeInfo = LabelMap (LabelMap edgeInfo)

-- | A control flow graph where edges have been annotated with a weight.
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
-- or has it been introduced for codegen. We use this to maintain
-- some information which would otherwise be lost during the
-- Cmm <-> asm transition.
-- See also Note [Inverting Conditional Branches]
data TransitionSource
  = CmmSource (CmmNode O C)
  | AsmCodeGen
  deriving (Eq)

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
  , weight <- edgeWeight info
  = addEdge from to (info { edgeWeight = f weight}) cfg
  | otherwise = cfg

-- | Set the weight between the blocks to the given weight.
--   If there is no such edge returns the original map.
setEdgeWeight :: CFG -> EdgeWeight
              -> BlockId -> BlockId -> CFG
setEdgeWeight cfg weight from to
  | Just info <- getEdgeInfo from to cfg
  = addEdge from to (info { edgeWeight = weight}) cfg
  | otherwise = cfg



getCfgNodes :: CFG -> LabelSet
getCfgNodes m = mapFoldMapWithKey (\k v -> setFromList (k:mapKeys v)) m

hasNode :: CFG -> BlockId -> Bool
hasNode m node = mapMember node m || any (mapMember node) m

-- | Check if the nodes in the cfg and the set of blocks are the same.
--   In a case of a missmatch we panic and show the difference.
sanityCheckCfg :: CFG -> LabelSet -> SDoc -> Bool
sanityCheckCfg m blockSet msg
    | blockSet == cfgNodes
    = True
    | otherwise =
        pprPanic "Block list and cfg nodes don't match" (
            text "difference:" <+> ppr diff $$
            text "blocks:" <+> ppr blockSet $$
            text "cfg:" <+> ppr m $$
            msg )
            False
    where
      cfgNodes = getCfgNodes m :: LabelSet
      diff = (setUnion cfgNodes blockSet) `setDifference` (setIntersection cfgNodes blockSet) :: LabelSet

-- | Filter the CFG with a custom function f.
--   Paramaeters are `f from to edgeInfo`
filterEdges :: (BlockId -> BlockId -> EdgeInfo -> Bool) -> CFG -> CFG
filterEdges f cfg =
    mapMapWithKey filterSources cfg
    where
      filterSources from m =
        mapFilterWithKey (\to w -> f from to w) m


{- Note [Updating the CFG during shortcutting]

See Note [What is shortcutting] in the control flow optimization
code (CmmContFlowOpt.hs) for a slightly more in depth explanation on shortcutting.

In the native backend we shortcut jumps at the assembly level. (AsmCodeGen.hs)
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
applies the mapping to the CFG in the way layed out above.

-}
shortcutWeightMap :: CFG -> LabelMap (Maybe BlockId) -> CFG
shortcutWeightMap cfg cuts =
  foldl' applyMapping cfg $ mapToList cuts
    where
-- takes the tuple (B,C) from the notation in [Updating the CFG during shortcutting]
      applyMapping :: CFG -> (BlockId,Maybe BlockId) -> CFG
      --Shortcut immediate
      applyMapping m (from, Nothing) =
        mapDelete from .
        fmap (mapDelete from) $ m
      --Regular shortcut
      applyMapping m (from, Just to) =
        let updatedMap :: CFG
            updatedMap
              = fmap (shortcutEdge (from,to)) $
                (mapDelete from m :: CFG )
        --Sometimes we can shortcut multiple blocks like so:
        -- A -> B -> C -> D -> E => A -> E
        -- so we check for such chains.
        in case mapLookup to cuts of
            Nothing -> updatedMap
            Just dest -> applyMapping updatedMap (to, dest)
      --Redirect edge from B to C
      shortcutEdge :: (BlockId, BlockId) -> LabelMap EdgeInfo -> LabelMap EdgeInfo
      shortcutEdge (from, to) m =
        case mapLookup from m of
          Just info -> mapInsert to info $ mapDelete from m
          Nothing   -> m

-- | Sometimes we insert a block which should unconditionally be executed
--   after a given block. This function updates the CFG for these cases.
--  So we get A -> B    => A -> A' -> B
--             \                  \
--              -> C    =>         -> C
--
addImmediateSuccessor :: BlockId -> BlockId -> CFG -> CFG
addImmediateSuccessor node follower cfg
    = updateEdges . addWeightEdge node follower uncondWeight $ cfg
    where
        uncondWeight = fromIntegral . D.uncondWeight .
                       D.cfgWeightInfo $ D.unsafeGlobalDynFlags
        targets = getSuccessorEdges cfg node
        successors = map fst targets :: [BlockId]
        updateEdges = addNewSuccs . remOldSuccs
        remOldSuccs m = foldl' (flip (delEdge node)) m successors
        addNewSuccs m =
          foldl' (\m' (t,info) -> addEdge follower t info m') m targets

-- | Adds a new edge, overwrites existing edges if present
addEdge :: BlockId -> BlockId -> EdgeInfo -> CFG -> CFG
addEdge from to info cfg =
    mapAlter addDest from cfg
    where
        addDest Nothing = Just $ mapSingleton to info
        addDest (Just wm) = Just $ mapInsert to info wm

-- | Adds a edge with the given weight to the cfg
--   If there already existed an edge it is overwritten.
--   `addWeightEdge from to weight cfg`
addWeightEdge :: BlockId -> BlockId -> EdgeWeight -> CFG -> CFG
addWeightEdge from to weight cfg =
    addEdge from to (mkWeightInfo weight) cfg

delEdge :: BlockId -> BlockId -> CFG -> CFG
delEdge from to m =
    mapAlter remDest from m
    where
        remDest Nothing = Nothing
        remDest (Just wm) = Just $ mapDelete to wm

delNode :: BlockId -> CFG -> CFG
delNode node cfg =
  fmap (mapDelete node)  -- < Edges to the node
    (mapDelete node cfg) -- < Edges from the node

-- | Destinations from bid ordered by weight (descending)
getSuccEdgesSorted :: CFG -> BlockId -> [(BlockId,EdgeInfo)]
getSuccEdgesSorted m bid =
    let destMap = mapFindWithDefault mapEmpty bid m
        cfgEdges = mapToList destMap
        sortedEdges = sortWith (negate . edgeWeight . snd) cfgEdges
    in  --pprTrace "getSuccEdgesSorted" (ppr bid <+> text "map:" <+> ppr m)
        sortedEdges

-- | Get successors of a given node with edge weights.
getSuccessorEdges :: CFG -> BlockId -> [(BlockId,EdgeInfo)]
getSuccessorEdges m bid = maybe [] mapToList $ mapLookup bid m

getEdgeInfo :: BlockId -> BlockId -> CFG -> Maybe EdgeInfo
getEdgeInfo from to m
    | Just wm <- mapLookup from m
    , Just info <- mapLookup to wm
    = Just $! info
    | otherwise
    = Nothing

reverseEdges :: CFG -> CFG
reverseEdges cfg = foldr add mapEmpty flatElems
  where
    elems = mapToList $ fmap mapToList cfg :: [(BlockId,[(BlockId,EdgeInfo)])]
    flatElems =
        concatMap (\(from,ws) -> map (\(to,info) -> (to,from,info)) ws ) elems
    add (to,from,info) m = addEdge to from info m

-- | Returns a unordered list of all edges with info
infoEdgeList :: CFG -> [CfgEdge]
infoEdgeList m =
  mapFoldMapWithKey
    (\from toMap ->
      map (\(to,info) -> CfgEdge from to info) (mapToList toMap))
    m

-- | Unordered list of edges with weight as Tuple (from,to,weight)
weightedEdgeList :: CFG -> [(BlockId,BlockId,EdgeWeight)]
weightedEdgeList m =
  mapFoldMapWithKey
    (\from toMap ->
      map (\(to,info) ->
        (from,to, edgeWeight info)) (mapToList toMap))
    m
      --  (\(from, tos) -> map (\(to,info) -> (from,to, edgeWeight info)) tos )

-- | Returns a unordered list of all edges without weights
edgeList :: CFG -> [Edge]
edgeList m =
        mapFoldMapWithKey (\from toMap -> fmap (from,) (mapKeys toMap)) m

-- | Get successors of a given node without edge weights.
getSuccessors :: CFG -> BlockId -> [BlockId]
getSuccessors m bid
    | Just wm <- mapLookup bid m
    = mapKeys wm
    | otherwise = []

pprEdgeWeights :: CFG -> SDoc
pprEdgeWeights m =
    let edges = sort $ weightedEdgeList m
        printEdge (from, to, weight)
            = text "\t" <> ppr from <+> text "->" <+> ppr to <>
              text "[label=\"" <> ppr weight <> text "\",weight=\"" <>
              ppr weight <> text "\"];\n"
        --for the case that there are no edges from/to this node.
        --This should rarely happen but it can save a lot of time
        --to immediately see it when it does.
        printNode node
            = text "\t" <> ppr node <> text ";\n"
        getEdgeNodes (from, to, _weight) = [from,to]
        edgeNodes = setFromList $ concatMap getEdgeNodes edges :: LabelSet
        nodes = filter (\n -> (not . setMember n) edgeNodes) . mapKeys $ mapFilter null m
    in
    text "digraph {\n" <>
        (foldl' (<>) empty (map printEdge edges)) <>
        (foldl' (<>) empty (map printNode nodes)) <>
    text "}\n"

{-# INLINE updateEdgeWeight #-} --Allows eliminating the tuple when possible
updateEdgeWeight :: (EdgeWeight -> EdgeWeight) -> Edge -> CFG -> CFG
updateEdgeWeight f (from, to) cfg
    | Just oldInfo <- getEdgeInfo from to cfg
    = let oldWeight = edgeWeight oldInfo
          newWeight = f oldWeight
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
addNodesBetween :: CFG -> [(BlockId,BlockId,BlockId)] -> CFG
addNodesBetween m updates =
  foldl'  updateWeight m .
          weightUpdates $ updates
    where
      weight = fromIntegral . D.uncondWeight .
                D.cfgWeightInfo $ D.unsafeGlobalDynFlags
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
            text "updates" <+> ppr updates )
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
  make a educated guess how often execution will transer control
  along each edge as well as how much we gain by placing eg A before
  C.

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
  * Calls - We currently ignore calls completly:
        * By the time we return from a call there is a good chance
          that the address we return to has already been evicted from
          cache eliminating a main advantage sequential placement brings.
        * Calls always require a info table in front of their return
          address. This reduces the chance that we return to the same
          cache line further.


-}
-- | Generate weights for a Cmm proc based on some simple heuristics.
getCfgProc :: D.CfgWeights -> RawCmmDecl -> CFG
getCfgProc _       (CmmData {}) = mapEmpty
getCfgProc weights (CmmProc _info _lab _live graph) = getCfg weights graph

getCfg :: D.CfgWeights -> CmmGraph -> CFG
getCfg weights graph =
  foldl' insertEdge edgelessCfg $ concatMap getBlockEdges blocks
  where
    D.CFGWeights
            { D.uncondWeight = uncondWeight
            , D.condBranchWeight = condBranchWeight
            , D.switchWeight = switchWeight
            , D.callWeight = callWeight
            , D.likelyCondWeight = likelyCondWeight
            , D.unlikelyCondWeight = unlikelyCondWeight
            --  Last two are used in other places
            --, D.infoTablePenalty = infoTablePenalty
            --, D.backEdgeBonus = backEdgeBonus
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
        CmmCondBranch _c t f l
          | l == Nothing ->
              [mkEdge f condBranchWeight,   mkEdge t condBranchWeight]
          | l == Just True ->
              [mkEdge f unlikelyCondWeight, mkEdge t likelyCondWeight]
          | l == Just False ->
              [mkEdge f likelyCondWeight,   mkEdge t unlikelyCondWeight]
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
            ASSERT2(False, ppr "Unkown successor cause:" <>
              (ppr branch <+> text "=>" <> ppr (G.successors other)))
            map (\x -> ((bid,x),mkEdgeInfo 0)) $ G.successors other
      where
        bid = G.entryLabel block
        mkEdgeInfo = EdgeInfo (CmmSource branch) . fromIntegral
        mkEdge target weight = ((bid,target), mkEdgeInfo weight)
        branch = lastNode block :: CmmNode O C

    blocks = revPostorder graph :: [CmmBlock]

--Find back edges by BFS
findBackEdges :: BlockId -> CFG -> Edges
findBackEdges root cfg =
    --pprTraceIt "Backedges:" $
    map fst .
    filter (\x -> snd x == Backward) $ typedEdges
  where
    edges = edgeList cfg :: [(BlockId,BlockId)]
    getSuccs = getSuccessors cfg :: BlockId -> [BlockId]
    typedEdges =
      classifyEdges root getSuccs edges :: [((BlockId,BlockId),EdgeType)]


optimizeCFG :: D.CfgWeights -> RawCmmDecl -> CFG -> CFG
optimizeCFG _ (CmmData {}) cfg = cfg
optimizeCFG weights (CmmProc info _lab _live graph) cfg =
    -- pprTrace "Initial:" (pprEdgeWeights cfg) $
    -- pprTrace "Initial:" (ppr $ mkGlobalWeights (g_entry graph) cfg) $

    -- pprTrace "LoopInfo:" (ppr $ loopInfo cfg (g_entry graph)) $
    favourFewerPreds  .
    penalizeInfoTables info .
    increaseBackEdgeWeight (g_entry graph) $ cfg
  where

    -- | Increase the weight of all backedges in the CFG
    -- this helps to make loop jumpbacks the heaviest edges
    increaseBackEdgeWeight :: BlockId -> CFG -> CFG
    increaseBackEdgeWeight root cfg =
        let backedges = findBackEdges root cfg
            update weight
              --Keep irrelevant edges irrelevant
              | weight <= 0 = 0
              | otherwise
              = weight + fromIntegral (D.backEdgeBonus weights)
        in  foldl'  (\cfg edge -> updateEdgeWeight update edge cfg)
                    cfg backedges

    -- | Since we cant fall through info tables we penalize these.
    penalizeInfoTables :: LabelMap a -> CFG -> CFG
    penalizeInfoTables info cfg =
        mapWeights fupdate cfg
      where
        fupdate :: BlockId -> BlockId -> EdgeWeight -> EdgeWeight
        fupdate _ to weight
          | mapMember to info
          = weight - (fromIntegral $ D.infoTablePenalty weights)
          | otherwise = weight


{- Note [Optimize for Fallthrough]

-}
    -- | If a block has two successors, favour the one with fewer
    -- predecessors. (As that one is more likely to become a fallthrough)
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

            update cfg node
              | [(s1,e1),(s2,e2)] <- getSuccessorEdges cfg node
              , w1 <- edgeWeight e1
              , w2 <- edgeWeight e2
              --Only change the weights if there isn't already a ordering.
              , w1 == w2
              , (mod1,mod2) <- modifiers (predCount s1) (predCount s2)
              = (\cfg' ->
                  (adjustEdgeWeight cfg' (+mod2) node s2))
                  (adjustEdgeWeight cfg  (+mod1) node s1)
              | otherwise
              = cfg
        in setFoldl update cfg nodes
      where
        fallthroughTarget :: BlockId -> EdgeInfo -> Bool
        fallthroughTarget to (EdgeInfo source _weight)
          | mapMember to info = False
          | AsmCodeGen <- source = True
          | CmmSource (CmmBranch {}) <- source = True
          | CmmSource (CmmCondBranch {}) <- source = True
          | otherwise = False

-- | Determine loop membership of blocks based on SCC analysis
--   This is faster but only gives yes/no answers.
loopMembers :: CFG -> LabelMap Bool
loopMembers cfg =
    foldl' (flip setLevel) mapEmpty sccs
  where
    mkNode :: BlockId -> Node BlockId BlockId
    mkNode bid = DigraphNode bid bid (getSuccessors cfg bid)
    nodes = map mkNode (setElems $ getCfgNodes cfg)

    sccs = stronglyConnCompFromEdgedVerticesOrd nodes

    setLevel :: SCC BlockId -> LabelMap Bool -> LabelMap Bool
    setLevel (AcyclicSCC bid) m = mapInsert bid False m
    setLevel (CyclicSCC bids) m = foldl' (\m k -> mapInsert k True m) m bids

loopLevels :: CFG -> BlockId -> LabelMap Int
loopLevels cfg root = liLevels $ loopInfo cfg root

data LoopInfo = LoopInfo
  { liBackEdges :: [(Edge)] -- ^ List of back edges
  , liLevels :: LabelMap Int -- ^ BlockId -> LoopLevel mapping
  , liLoops :: [(Edge, LabelSet)] -- ^ (backEdge, loopBody), body includes header
  }

-- | Determine loop membership of blocks based on Dominator analysis.
--   This is slower but gives loop levels.
--   Only detects natural loops. Irreducible control flow is not
--   recognized even if it loops.
loopInfo :: CFG -> BlockId -> LoopInfo
loopInfo cfg root = LoopInfo  { liBackEdges = backEdges
                              , liLevels = mapFromList loopCounts
                              , liLoops = loopBodies }
  where
    revCfg = reverseEdges cfg
    graph = fmap (setFromList . mapKeys ) cfg :: LabelMap LabelSet

    --TODO - This should be a no op: Export constructors? Use unsafeCoerce? ...
    rooted = ( fromBlockId root
              , toIntMap $ fmap toIntSet graph) :: (Int, IntMap IntSet)
    -- rooted = unsafeCoerce (root, graph)
    tree = fmap toBlockId $ Dom.domTree rooted :: Tree BlockId

    -- Map from Nodes to their dominators
    domMap :: LabelMap LabelSet
    domMap = mkDomMap tree

    edges = edgeList cfg :: [(BlockId, BlockId)]
    nodes = getCfgNodes cfg :: LabelSet --[BlockId]

    -- identify back edges
    isBackEdge (from,to)
      | Just doms <- mapLookup from domMap
      , setMember to doms
      = True
      | otherwise = False

    -- determine the loop body for a back edge
    findBody edge@(tail, head)
      = ( edge, setInsert head $ go (setSingleton tail) (setSingleton tail) )
      where
        -- The reversed cfg makes it easier to look up predecessors
        cfg' = delNode head revCfg
        go :: LabelSet -> LabelSet -> LabelSet
        go found current
          | setNull current = found
          | otherwise = go  (setUnion newSuccessors found)
                            newSuccessors
          where
            newSuccessors = setFilter (\n -> not $ setMember n found) successors :: LabelSet
            successors = setFromList $ concatMap
                                      (getSuccessors cfg')
                                      (setElems current) :: LabelSet

    backEdges = filter isBackEdge edges
    loopBodies = map findBody backEdges :: [(Edge, LabelSet)]

    -- Block b is part of n loop bodies => loop nest level of n
    loopCounts =
      let bodies = map (first snd) loopBodies -- [(Header, Body)]
          loopCount n = length $ nub . map fst . filter (setMember n . snd) $ bodies
      in  map (\n -> (n, loopCount n)) $ setElems nodes :: [(BlockId, Int)]

    toIntSet :: LabelSet -> IntSet
    toIntSet s = IS.fromList . map fromBlockId . setElems $ s
    toIntMap :: LabelMap a -> IntMap a
    toIntMap m = IM.fromList $ map (\(x,y) -> (fromBlockId x,y)) $ mapToList m

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

    fromBlockId :: BlockId -> Int
    fromBlockId = getKey . getUnique

    toBlockId :: Int -> BlockId
    toBlockId = mkBlockId . mkUniqueGrimily

-- We make the CFG a Hoopl Graph, so we can reuse revPostOrder.
newtype BlockNode e x = BN (BlockId,[BlockId])
instance G.NonLocal (BlockNode) where
  entryLabel (BN (lbl,_))   = lbl
  successors (BN (_,succs)) = succs

revPostorderFrom :: CFG -> BlockId -> [BlockId]
revPostorderFrom cfg root =
    map fromNode $ G.revPostorderFrom hooplGraph root
  where
    --TODO: Use fromSet instead.
    nodes = setElems $ getCfgNodes cfg
    hooplGraph = mapFromList $ map (\n -> (n,toNode n)) nodes

    fromNode :: BlockNode C C -> BlockId
    fromNode (BN x) = fst x

    toNode :: BlockId -> BlockNode C C
    toNode bid =
        -- sorted such that heavier successors come first.
        BN (bid,map fst . getSuccEdgesSorted cfg $ bid)


-- | We take in a CFG which has on it's edges weights which are
--   relative only to other edges originating from the same node.
--
--   We return a CFG for which each edge represents a GLOBAL weight.
--   This means edge weights are comparable across the whole graph.
--
--   For irreducible control flow results might be imprecise, otherwise they
--   are reliably.

mkGlobalWeights :: BlockId -> CFG -> (LabelMap Double, LabelMap (LabelMap Double))
mkGlobalWeights root localCfg
  | null localCfg = panic "Error - Empty CFG"
  | otherwise
  = --pprTrace "revOrder" (ppr revOrder) $
    -- undefined --propagate (mapSingleton root 1) (revOrder)
    (blockFreqs', edgeFreqs')
  where
    (blockFreqs, edgeFreqs) = calcFreqs nodeProbs' backEdges' bodies' revOrder'
    blockFreqs' = mapFromList $ map (first fromVertex) (assocs blockFreqs) :: LabelMap Double
    edgeFreqs' = fmap fromVertexMap $ fromVertexMap edgeFreqs

    fromVertexMap :: IM.IntMap x -> LabelMap x
    fromVertexMap m = mapFromList . map (first fromVertex) $ IM.toList m



    revOrder = revPostorderFrom localCfg root :: [BlockId]
    LoopInfo backedges _levels bodies = loopInfo localCfg root

    revOrder' = map toVertex revOrder
    backEdges' = map (bimap toVertex toVertex) backedges
    bodies' = map calcBody bodies
    nodeProbs' = map (bimap
                        toVertex
                        (map (first toVertex))
                     )
                     nodeProbs
    nodeProbs = cfgEdgeProbabilities localCfg

    -- TODO: The sort is redundant if we can guarantee that setElems returns elements ascending
    -- By mapping vertices to numbers in reverse post order we can bring any subset into reverse post
    -- order simply by sorting.
    calcBody (backedge, blocks) = (toVertex $ snd backedge, sort . map toVertex $ (setElems blocks))
    vertexMapping = mapFromList $ zip revOrder [0..] :: LabelMap Int
    blockMapping = IM.fromList $ zip [0 ..] revOrder :: IM.IntMap BlockId
    toVertex :: BlockId -> Int
    toVertex   blockId  = expectJust "mkGlobalWeights" $ mapLookup blockId vertexMapping
    fromVertex :: Int -> BlockId
    fromVertex vertex   = expectJust "mkGlobalWeights" $ IM.lookup vertex blockMapping

cfgEdgeProbabilities :: CFG -> [(BlockId, [(BlockId, Prob)])]
cfgEdgeProbabilities cfg
    = normalized
  where
    nodeInfos = mapToList cfg :: [(BlockId, LabelMap EdgeInfo)]
    weightInfo = map (second (fmap (weightToDouble . edgeWeight))) nodeInfos :: [(BlockId, LabelMap Double)]
    normalized = map (second normalize) weightInfo :: [(BlockId, [(BlockId, Double)])]
    normalize :: (LabelMap Double) -> [(BlockId, Double)]
    normalize weightMap
        | edgeCount <= 1 = zip (mapKeys weightMap) [1.0]
        | otherwise = map normalWeight (mapKeys weightMap)
      where
        edgeCount = mapSize weightMap
        -- Negative weights are generally allowed but lead to nonsense results
        -- when we want *probabilities*.
        -- TODO:
        -- Zeros could be okay if we would check if there is at least one non-zero edge
        -- but for simplicity/performance we just treat all edges <= zero as having a minimal chance of
        -- being taken for now.
        minWeight = 0.01
        weightMap' = fmap (\w -> max w minWeight) weightMap
        normalWeight bid
         | Just w <- mapLookup bid weightMap'
         = (bid, w/totalWeight)
         | otherwise = panic "impossible"

        totalWeight = sum weightMap'


calcFreqs :: [(Int,[(Int,Prob)])] -> [(Int,Int)] -> [(Int, [Int])] -> [Int]
          -> (Array Int Double, IM.IntMap (IM.IntMap Prob))
calcFreqs edges backEdges loops revPostOrder = runST $ do
    visitedNodes <- newArray (0,nodeCount-1) False :: ST s (STArray s Int Bool)
    blockFreqs <- newArray (0,nodeCount-1) 0.0 :: ST s (STArray s Int Double)
    edgeProbs <- newSTRef graph
    edgeBackProbs <- newSTRef graph

    -- let traceArray a = do
    --       vs <- forM [0..nodeCount-1] $ \i -> readArray a i >>= (\v -> return (i,v))
          -- trace ("array: " ++ show vs) $ return ()

    let visited b = readArray visitedNodes b
        getFreq b = readArray blockFreqs b
        -- setFreq :: forall s. Int -> Double -> ST s ()
        setFreq b f = writeArray blockFreqs b f
        -- setVisited :: forall s. Node -> ST s ()
        setVisited b = writeArray visitedNodes b True
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
          let m = fromMaybe (error "Foo") $ IM.lookup b1 g
              m' = IM.insert b2 prob m
          writeSTRef arr (IM.insert b1 m' g)

        getEdgeFreq b1 b2 = getProb' edgeProbs b1 b2
        setEdgeFreq b1 b2 = setProb' edgeProbs b1 b2
        getProb b1 b2 = fromMaybe (error "getProb") $ do
            m' <- IM.lookup b1 graph
            IM.lookup b2 m'

        getBackProb b1 b2 = getProb' edgeBackProbs b1 b2
        setBackProb b1 b2 = setProb' edgeBackProbs b1 b2


    let -- calcOutFreqs :: Node -> ST s ()
        calcOutFreqs bhead block = do
          f <- getFreq block
          forM (successors block) $ \bi -> do
            let prob = getProb block bi
            let succFreq = f * prob
            setEdgeFreq block bi succFreq
            -- traceM $ "SetOut: " ++ show (block, bi, f, prob, succFreq)
            when (bi == bhead) $ setBackProb block bi succFreq


    let propFreq block head = do
            -- traceM ("prop:" ++ show (block,head))
            -- traceShowM block

            v <- visited block
            if v then
                return () --Dont look at nodes twice
            else if block == head then
                setFreq block 1.0 -- Loop header frequency is always 1
            else do
                irreducible <- (fmap or) $ forM (predecessors block) $ \bp -> do
                    bp_visited <- visited bp
                    let bp_backedge = isBackEdge bp block
                    return (not bp_visited && not bp_backedge)

                if irreducible
                then return () -- Rare we don't care
                else do
                    setFreq block 0
                    cycleProb <- sum <$> (forM (predecessors block) $ \pred -> do
                        if isBackEdge pred block
                            then
                                getBackProb pred block
                            else do
                                f <- getFreq block
                                prob <- getEdgeFreq pred block
                                setFreq block $ f + prob
                                return 0)
                    -- traceM $ "cycleProb:" ++ show cycleProb
                    let limit = 0.9999999999 -- Paper uses 1 - epsilon, but this works
                    cycleProb <- return $ min cycleProb limit -- <- return $ if cycleProb > limit then limit else cycleProb
                    -- traceM $ "cycleProb:" ++ show cycleProb

                    f <- getFreq block
                    setFreq block (f / (1.0 - cycleProb))


            setVisited block
            calcOutFreqs head block
            -- traceArray blockFreqs

            -- traceM ""

        --   forM_ (successors block) $ \bi -> do
        --     if (isBackEdge block bi)
        --       then return ()
        --       else do
        --         -- Find unvisited successors
        --         propFreq bi head



    -- Set backedge probs to edge probs - WHY?
    -- Just skip

    -- Loops, by nesting, inner to outer


    forM_ loops $ \(head, body) -> do
        forM_ [0 .. nodeCount - 1] (\i -> writeArray visitedNodes i True) -- Mark all nodes as visited.
        forM_ body (\i -> writeArray visitedNodes i False) -- Mark all blocks reachable from head as not visited
        forM_ body $ \block -> propFreq block head

    -- After dealing with all loops, deal with non-looping parts of the CFG
    forM_ [0 .. nodeCount - 1] (\i -> writeArray visitedNodes i False) -- Everything in revPostOrder is reachable
    forM_ revPostOrder $ \block -> propFreq block (head revPostOrder)

    -- trace ("Final freqs:") $ return ()
    -- let freqString = pprFreqs freqs
    -- trace (unlines freqString) $ return ()
    -- trace (pprFre) $ return ()
    graph' <- readSTRef edgeProbs
    freqs' <- freeze blockFreqs

    return (freqs', graph')
  where
    predecessors :: Int -> [Int]
    predecessors b = fromMaybe (lookupError "pred" b revGraph) $ IM.keys <$> IM.lookup b revGraph :: [Int]
    successors b = fromMaybe (lookupError "succ" b graph)$ IM.keys <$> IM.lookup b graph
    lookupError s b g = pprPanic ("Lookup error " ++ s) $
                            ( text "node" <+> ppr b $$
                                text "graph" <+>
                                vcat (map (\(k,m) -> ppr (k,m :: IM.IntMap Double)) $ IM.toList g) $$

                                text "inputGraph" $$
                                vcat (map ppr $ sort edges)
                                )
    -- visited :: Node ->
    nodeCount = length nodes
    nodes = ordNub $ concatMap (\(x,ys)-> x:ys) $ IM.toList $ fmap (IM.keys) graph

    isBackEdge from to = S.member (from,to) backEdgeSet
    backEdgeSet = S.fromList backEdges

    graph = let inner = map (second IM.fromList) edges
            in IM.fromList inner :: IM.IntMap (IM.IntMap Prob)

    revGraph = foldr add nodeMap flatElems
      where
        nodeMap = IM.fromList $ zip (map fst edges) (repeat IM.empty)
        elems = IM.toList $ fmap IM.toList graph :: [(Int,[(Int,Prob)])]
        flatElems =
            concatMap
                (\(from,ws) ->
                    map (\(to,info) -> (to,from,info)) ws
                ) elems
        add (to,from,info) m = addEdge to from info m
        addEdge from to info cfg =
            IM.alter addDest from cfg
            where
                addDest Nothing = Just $ IM.singleton to info
                addDest (Just wm) = Just $ IM.insert to info wm