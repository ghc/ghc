{-# LANGUAGE TypeFamilies               #-}
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
    , checkFailures, coldBlocks, findBackEdges

    -- Loop Information
    , loopInfo

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
import GHC.Data.Maybe

import qualified GHC.CmmToAsm.CFG.Dominators as Dom
import GHC.CmmToAsm.CFG.Weight
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)

import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Tree
import Data.Bifunctor
import Data.Coerce

import GHC.Utils.Outputable
import GHC.Utils.Panic
-- DEBUGGING ONLY
--import GHC.Cmm.DebugBlock
--import GHC.Data.OrdList
--import GHC.Cmm.DebugBlock.Trace

import Data.List (sort, sortOn, partition)
import Data.STRef.Strict
import Control.Monad.ST

import Data.Array.MArray
import Data.Array.ST
import Data.Array.IArray
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.Base (unsafeRead, unsafeWrite)

import Control.Monad
import GHC.Utils.Monad (anyM)
import GHC.Data.UnionFind

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

-- | Edges are sorted ascending pointwise by weight, source and destination.
-- TODO: edges of equal weight are thus ordered by their labels, which carry no
-- meaning for the layout: sequenceChain sorts its directEdges descending by
-- this instance, and so fuses edges of equal weight from the larger label down.
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
  -- | An edge the code generator adds for an operation's retry, as
  -- cmpxchg_code's: static prediction's rule that a back edge is more likely
  -- than any other edge leaves it out.
  | AsmCodeGenRetry
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

-- | The blocks that follow a failed heap or stack check: those the check's less
-- likely branch enters, such as one calling the garbage collector, and their
-- successors, such as the block the garbage collector returns to, which can be
-- a continuation that a call returns to as well. A heap or stack check is a
-- branch 'getCfg' tags 'HeapStackCheck', one whose condition reads SpLim, HpLim
-- or BaseReg after fixStgRegisters, which has turned every global register
-- without a machine register into a load off BaseReg: on x86-64, where HpLim
-- has none, a heap check reads BaseReg, and so does a test of CurrentTSO or
-- CurrentNursery (the BUG at getCfg). Which side is less likely is read off
-- the weights of the CFG given; for a check annotated likely or unlikely,
-- as GHC.StgToCmm.Heap's are, the same side comes out less likely, under
-- the default -fblock-layout-weights, on the native weights and on those
-- optimizeCFG derives from them.
--
-- TODO: some of these blocks may run often: see
-- Note [Frequencies before register allocation].
checkFailures :: CFG -> LabelSet
checkFailures cfg = mapFoldl node setEmpty cfg
  where
    node !acc outs = mapFoldlWithKey (edge outs) acc outs
    edge outs !acc to e
      | isHeapOrStackCheck (transitionSource e)
      , edgeWeight e < mapFoldl (\w e' -> max w (edgeWeight e')) 0 outs
      = mapFoldlWithKey (\a s _ -> setInsert s a) (setInsert to acc)
                        (mapFindWithDefault mapEmpty to cfg)
      | otherwise
      = acc

-- | The blocks that follow a failed heap or stack check ('checkFailures'),
-- and the blocks that follow one of them and whose one out-edge has the code
-- generator as its source, such as those the register allocator puts there.
--
-- TODO: two shapes escape this: see
-- Note [Frequencies before register allocation].
coldBlocks :: CFG -> LabelSet
coldBlocks cfg = setUnion gc $ setFromList
    [ s | b <- setElems gc, Just outs <- [mapLookup b cfg], s <- mapKeys outs
        , inserted s ]
  where
    gc = checkFailures cfg
    inserted b
      | Just outs <- mapLookup b cfg
      , [EdgeInfo { transitionSource = AsmCodeGen }] <- mapElems outs
      = True
      | otherwise = False

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

BUG: the case of an immediate arises with tables next to code too, as x86's
canShortcut accepts a jump to an immediate: a block that is only a tail call's
jump to a label is removed, and the edge A -> B with it. A's branch then has one
target fewer in the CFG than in the code. This costs only speed, on x86-64 under
-fasm-shortcutting, not on Darwin.

In terms of implementation the native backend first builds a mapping
from blocks suitable for shortcutting to their jump targets.
Then it redirects all jump instructions to these blocks using the
built up mapping.
This function (shortcutWeightMap) takes the same mapping and
applies the mapping to the CFG in the way laid out above.

TODO: where a redirected edge A -> C coincides with an edge A -> C already
there, or with another redirected edge, only one EdgeInfo is kept, chosen by
the order of the fold over A's targets in update_from_edge; the weights are not
summed.

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
    mapUpsert addFromToEdge from $
    mapUpsert addDestNode to cfg
    where
        -- Simply insert the edge into the edge list.
        addFromToEdge Nothing = mapSingleton to info
        addFromToEdge (Just wm) = mapInsert to info wm
        -- We must add the destination node explicitly
        addDestNode = fromMaybe mapEmpty


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
    We use 1000 as default ('defaultWeights' in GHC.CmmToAsm.CFG.Weight).
  * If branches (likely) - We assume branches marked as likely
    are taken 75% of the time: the default weights give the likely side 900 and
    the other side 300.
    By ranking them below unconditional jumps we make sure we
    prefer the unconditional if there is a conditional and
    unconditional edge towards a block.
  * If branches (regular) - Both sides get the same weight, but on x86-64 the
    instruction selector adds 3 to the weight of a floating-point comparison's
    false edge (genCondBranch' in GHC.CmmToAsm.X86.CodeGen). If the weights are
    still equal after optHsPatterns' other adjustments, favourFewerPreds adds
    one to the side with fewer predecessors that could fall through into it, and
    subtracts one from the other side.
  * Unlikely branches - By the same weights we assume that these are taken 25%
    of the time. So we give them one of the lowest priorities.
  * Switches - Switches at this level are implemented as jump tables
    so have a larger number of successors. So without more information
    we can only say that each individual successor is unlikely to be
    jumped to and we rank them accordingly.
  * Calls - A call's edge to its continuation gets callWeight. The layout leaves
    these edges out when it builds chains and when it combines neighbouring
    chains (directEdges in GHC.CmmToAsm.BlockLayout), and divides their weight
    by 64 when it merges the chains at the end:
        * By the time we return from a call there is a good chance
          that the address we return to has already been evicted from
          cache eliminating a main advantage sequential placement brings.
        * Calls always require a info table in front of their return
          address. This reduces the chance that we return to the same
          cache line further.

  Under -fcmm-static-pred (on at -O1 and -O2), static prediction turns
  these weights into estimated frequencies before the layout uses them
  (Note [Static Branch Prediction]); these are the frequencies of
  Note [Layout relevant edge weights] in GHC.CmmToAsm.BlockLayout.

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
      mapUpsert f from m
        where
          f :: Maybe (LabelMap EdgeInfo) -> LabelMap EdgeInfo
          f Nothing = mapSingleton to weight
          f (Just destMap) = mapInsert to weight destMap
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
            -- BUG: this runs after fixStgRegisters, which turns a read of
            -- a global register with no machine register into a load at
            -- its offset from BaseReg. So on x86-64 a test of CurrentTSO
            -- or CurrentNursery, such as `if (v == CurrentTSO)` in
            -- rts/Updates.cmm, is tagged HeapStackCheck too. This costs only
            -- speed, on x86-64: under -fcmm-static-pred or -fblock-layout-cfg,
            -- and, for an annotated test such as that one, in the linear
            -- allocator's cold blocks at any optimisation level.
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
              -- TODO: the layout considers these edges nonetheless.
              -- sequenceChain builds them into chains like the others, after
              -- the heavier ones; under -fcmm-static-pred, on at -O1 and -O2,
              -- static prediction replaces their weights before the layout; and
              -- sequenceBlocks follows no edge out of a block with more than
              -- two successors, whatever its weight. What the hack saves has
              -- not been measured.
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

    -- BUG #28088: the CFG gets only the blocks reachable from the entry.
    -- cmmToCmm runs after the Cmm pipeline has removed the unreachable blocks
    -- and turns a branch on a constant into a jump; instruction selection still
    -- covers the blocks this makes unreachable, which the CFG lacks. Where the
    -- selector updates the CFG at such a block, as the x86 selector's cmpxchg
    -- loop for an atomic and, or, xor or nand does, getSuccessorEdges panics
    -- with "Block does not exist". This is reached on x86 and x86-64, at any
    -- optimisation level.
    blocks = revPostorder graph :: [CmmBlock]

-- | The back edges of a depth-first search from the given block: the edges into
-- a block still on the search's stack, but for an edge from a block to itself.
-- They differ from loopInfo's back edges, each an edge into a block that
-- dominates its source: an edge into the root is one here and none there, and
-- in an irreducible region the search's order of visits picks which edges
-- are. A self loop at a block other than the root is one there and none
-- here. The search takes a block's successors in their labels' order, so in
-- an irreducible region the labels pick which edges are back edges; under
-- -fobject-determinism a Haskell module's labels are the same for the same
-- inputs (Note [Norms of the NCG] in GHC.CmmToAsm).
findBackEdges :: BlockId -> CFG -> Edges
findBackEdges root cfg
  = go root (getSuccessors cfg root) [] (setSingleton root) (setSingleton root)
       []
  where
    -- the block searched from, its successors still to look at, and the
    -- blocks below it on the stack with theirs
    go !n [] stack !seen !onStack acc = case stack of
      []             -> acc
      (p, ps) : rest -> go p ps rest seen (setDelete n onStack) acc
    go !n (s : ss) stack !seen !onStack acc
      | s == n                = go n ss stack seen onStack acc
      | s `setMember` seen    = if s `setMember` onStack  -- a subset of seen
                                  then go n ss stack seen onStack ((n, s) : acc)
                                  else go n ss stack seen onStack acc
      | otherwise             = go s (getSuccessors cfg s) ((n, ss) : stack)
                                   (setInsert s seen) (setInsert s onStack) acc

optimizeCFG :: Bool -> Weights -> RawCmmDecl -> CFG -> CFG
optimizeCFG _ _ (CmmData {}) cfg = cfg
optimizeCFG doStaticPred weights (CmmProc info _lab _live graph) cfg
  -- no bonus for back edges where static prediction follows: it starts each
  -- two-way branch from optHsPatterns' weights, so that the penalties become
  -- probabilities, and overwrites the weights of every branch with a back edge
  -- among its targets except a heap or stack check's and one whose targets are
  -- all back edges; the back edges are loopInfo's: one into the root is
  -- findBackEdges' alone, and a branch with one starts from its weight with no
  -- bonus
  | doStaticPred = staticPredCfg (tagRegisters graph) entry $
                     optHsPatterns [] weights info cfg
  | otherwise    = optHsPatterns (findBackEdges entry cfg) weights info cfg
  where entry = g_entry graph

-- | Modify branch weights based on educated guess on
-- patterns GHC tends to produce and how they affect
-- performance; the back edges given get a bonus.
--
-- Most importantly we penalize jumps across info tables.
optHsPatterns :: Edges -> Weights -> LabelMap a -> CFG -> CFG
optHsPatterns backs weights info cfg =
    {-# SCC optHsPatterns #-}
    -- pprTrace "Initial:" (pprEdgeWeights cfg) $
    -- pprTrace "Initial:" (ppr $ mkGlobalWeights (g_entry graph) cfg) $

    -- pprTrace "LoopInfo:" (ppr $ loopInfo cfg (g_entry graph)) $
    favourFewerPreds  .
    penalizeInfoTables info .
    increaseBackEdgeWeight backs $ cfg
  where

    -- Increase the weight of the given backedges
    -- this helps to make loop jumpbacks the heaviest edges
    increaseBackEdgeWeight :: Edges -> CFG -> CFG
    increaseBackEdgeWeight backedges cfg =
        let update weight
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
          | AsmCodeGenRetry <- source = True
          | CmmSource { trans_cmmNode = CmmBranch {} } <- source = True
          | CmmSource { trans_cmmNode = CmmCondBranch {} } <- source = True
          | otherwise = False

-- | Convert block-local branch weights to global weights.
staticPredCfg :: S.Set LocalReg -> BlockId -> CFG -> CFG
staticPredCfg tags entry cfg = cfg'
  where
    (_, globalEdgeWeights) = {-# SCC mkGlobalWeights #-}
                             mkGlobalWeightsWith tags entry cfg
    cfg' = {-# SCC rewriteEdges #-}
            mapFoldlWithKey
                (\cfg from m ->
                    mapFoldlWithKey
                        (\cfg to w -> setEdgeWeight cfg (EdgeWeight w) from to )
                        cfg m )
                cfg
                globalEdgeWeights

data LoopInfo = LoopInfo
  { liBackEdges :: [(Edge)] -- ^ List of back edges
  , liLoops :: [(Edge, LabelSet)] -- ^ (backEdge, loopBody), body includes header
  }

instance Outputable LoopInfo where
    ppr (LoopInfo _ loops) =
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

{- Note [Back edges in loopInfo]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
loopInfo takes an edge for a back edge where its target is in its source's set
of dominators, as mkDomMap computes them. mkDomMap leaves the root out of every
set, and puts every other block in its own set. Hence:

* BUG #28083: an edge into the entry block is never a back edge, so a loop whose
  head is the entry block is not found. This costs only speed, on x86-64 under
  -fcmm-static-pred.

findBackEdges, which optHsPatterns uses for the back edge bonus where static
prediction does not follow, classifies the edges by a depth-first search
instead. It counts as back edges the edges
into the entry block and the back edges of irreducible cycles, and no self
loop. TODO: the two notions of a back edge disagree on these three kinds of
edge.
-}

-- | Determine loop membership of blocks based on Dominator analysis.
--   This is slower but gives loop bodies instead of just loop membership.
--   However it only detects natural loops. Irreducible control flow is not
--   recognized even if it loops. But that is rare enough that we don't have
--   to care about that special case.
loopInfo :: HasDebugCallStack => CFG -> BlockId -> LoopInfo
loopInfo cfg root = LoopInfo  { liBackEdges = backEdges
                              , liLoops = loopBodies }
  where
    revCfg = reverseEdges cfg

    graph = -- pprTrace "CFG - loopInfo" (pprEdgeWeights cfg) $
            fmap (setFromList . mapKeys ) cfg :: LabelMap LabelSet


    rooted = coerce (root, graph)
    tree = coerce (Dom.domTree rooted) :: Tree BlockId

    -- Map from Nodes to their dominators
    domMap :: LabelMap LabelSet
    domMap = mkDomMap tree

    edges = edgeList cfg :: [(BlockId, BlockId)]

    -- identify back edges: see Note [Back edges in loopInfo]
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

    -- each block but the root to the blocks dominating it, itself included
    -- and the root left out, so that no edge into the root is a back edge
    mkDomMap :: Tree BlockId -> LabelMap LabelSet
    mkDomMap (Node _ kids) = foldl' (go setEmpty) mapEmpty kids
      where
        go :: LabelSet -> LabelMap LabelSet -> Tree BlockId -> LabelMap LabelSet
        go !parents !acc (Node lbl kids')
          = let !doms = setInsert lbl parents
            in  foldl' (go doms) (mapInsert lbl doms acc) kids'

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

mkGlobalWeights :: HasDebugCallStack => BlockId -> CFG -> (LabelMap Double, LabelMap (LabelMap Double))
mkGlobalWeights = mkGlobalWeightsWith S.empty

-- No pragma: with NOINLINE, which master had on mkGlobalWeights, whose body
-- this is, the compiler's code differed only in this not being inlined, and
-- its instructions and allocation moved by under 0.001% compiling T3294, T783,
-- T9961, T12707, !16269's CmmDiamondChain and nofib's puzzle at -O0, -O and
-- -O2, on GHC 10.1.20261108.
-- | 'mkGlobalWeights', told which local registers hold a constructor tag
-- (see 'tagRegisters').
{-# SCC mkGlobalWeightsWith #-}
mkGlobalWeightsWith :: HasDebugCallStack => S.Set LocalReg -> BlockId -> CFG -> (LabelMap Double, LabelMap (LabelMap Double))
mkGlobalWeightsWith tags root localCfg
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
    loopResults@(LoopInfo backedges bodies) = loopInfo localCfg root

    revOrder' = map toVertex revOrder
    backEdges' = map (bimap toVertex toVertex) backedges
    -- calcFreqs takes the loops inner first; an inner loop's body is
    -- a proper subset of an enclosing one's
    bodies' = map calcBody (sortOn (setSize . snd) bodies)

    estimatedCfg = staticBranchPrediction tags root loopResults localCfg
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

{- Note [Where a branch's likely side is known]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Which side of a conditional branch runs more often is known in one of three
places, and the code generator treats each case differently:

* Where the branch is made. GHC.StgToCmm annotates some branches with their
  likely side, and getCfg sets the weights of an annotated branch's edges from
  the annotation. One such branch is that of a heap or stack check to the
  garbage collector, which GHC.StgToCmm.Heap marks as unlikely (`Just False`).
  getCfg tags such a branch HeapStackCheck, and staticBranchPrediction leaves
  alone the weights of any branch with an edge so tagged. getCfg recognises the
  branch by the registers its condition reads, not by the annotation, so other
  tests of the same registers are tagged too (a BUG at getCfg). Of the third
  case's heuristics, two reach any other annotated branch: lehPredicts combines
  its prediction with the weights the branch has by then, and the back edge rule
  replaces them where the branch has both a back edge and another edge (the BUG
  in Note [Static Branch Prediction]). ohPredicts skips an annotated branch.

* Upstream, and not carried down. Core and STG know which constructor a tag
  test selects, but the test that chooses a case alternative carries no
  annotation: cgAlts in GHC.StgToCmm.Expr makes it a CmmSwitch, which
  GHC.Cmm.Switch.Implement turns into CmmCondBranches without one. So ohPredicts
  treats it like any comparison with a constant where it is spelt as an equality
  (the BUG TODO there), and leaves it out where it is spelt as an inequality
  (the TODO there).

* Nowhere. Under -fcmm-static-pred, staticBranchPrediction applies its
  heuristics to every other two-way branch: a loop is not left (lehPredicts),
  an equality test (MO_Eq) with a literal and no likelihood annotation is false
  and such an inequality test (MO_Ne) true unless an operand is bits under a
  mask (MO_And) or a constructor tag (ohPredicts), and a back edge is taken
  (Note [Static Branch Prediction]).

The code generator keeps these trade-offs with that knowledge, each for the
reason given:

* GHC.Cmm.ContFlowOpt inverts a condition so that its true side falls through,
  even where the false side is likely and not inverting would be faster: telling
  those cases apart is neither easy nor cheap, and inverting gives smaller
  binaries and code that is as fast on average (Note [Invert Cmm conditionals]).
* The layout leaves out edges across calls when it builds chains and when it
  combines neighbouring chains (directEdges in GHC.CmmToAsm.BlockLayout):
  control returns from a call to its continuation through the continuation's
  info table, not by falling through.
-}

{- Note [Static Branch Prediction]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The work here has been based on the paper
"Static Branch Prediction and Program Profile Analysis" by Y Wu, JR Larus.

The primary differences are that if we branch on the result of a heap
check we do not apply any of the heuristics.
The reason is simple: They look like loops in the control flow graph
but are usually never entered, and if at most once.

Three heuristics are implemented: that we do not exit loops (lehPredicts),
that an equality test (MO_Eq) with a literal and no likelihood annotation is
false and such an inequality test (MO_Ne) true unless an operand is bits under
a mask (MO_And) or a constructor tag (ohPredicts), and that a back edge is
more likely than any other edge, but for the retries the code generator adds
(AsmCodeGenRetry). WIP: the paper's other heuristics are stubs (phPredicts and
the rest), so a two-way branch to which none of the three applies keeps the
CFG's weights, normalised.

The back edge case is special as it supersedes any other heuristic if it
applies. BUG: it supersedes a likely annotation in the Cmm as well: a branch
with both a back edge and another edge is predicted to take the back edge
whatever its annotation says. This costs only speed, on x86-64 under
-fcmm-static-pred.

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

{- Note [Static prediction of tag tests]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ohPredicts predicts that a comparison with a constant comes out unequal,
giving the equal side 0.3. A case on a pointer tag compares the tag with a
constructor's, as in `(x & 7) == 1`, and tags follow the order in which a type
declares its constructors (Note [Constructor tag allocation] in GHC.Core.TyCon),
so whether the equal side is the likely one depends on the type, which the
CFG does not record; and so it does where a type has more constructors than a
pointer tag tells apart, the case comparing the tag its constructor's info table
holds (Note [Tagging big families] in GHC.StgToCmm.Expr). Where the test for
tag 1 is spelt as an equality, the prediction is right if the test is a loop's
exit and wrong if it is the step down a tree: the trees of Data.Map and Data.Set
declare Bin first, so in a lookup the prediction calls the side that stays in
the loop the unlikely one (the BUG TODO at ohPredicts). ohPredicts matches an
equality, but an inequality only where it tests neither bits under a mask nor a
constructor tag, one read from an info table, an unboxed sum's, one in a local
register every assignment to which is a tag or one less than such a register,
and the control-flow optimisation may have inverted a test so that its true
side falls through (Note [Invert Cmm conditionals] in GHC.Cmm.ContFlowOpt), so
whether a tag test is predicted at all depends on how it came out spelt (the
TODO at ohPredicts).

Telling the two apart needs the constructors' numbers of arguments, or which one
recurses, carried down to the CFG, which takes many changes, so the heuristic
stays as it is for now; and as leaning on the estimate more would hand loops
over such trees its misprediction, code that sets it aside or damps its effect
says so and points here.

The back edge rule of Note [Static Branch Prediction] supersedes it on a branch
that has a back edge, as it does every other heuristic and the branch's likely
annotation. BUG: that is wrong where a rare retry compares with a constant:
the RTS's stg_BLACKHOLE_info retries where an indirectee's info pointer
equals stg_IND_info, and its loop is estimated to repeat. This costs only
speed, on x86-64 under -fcmm-static-pred. Whatever the estimate, the layout
raises a loop's back edge above its entry, so that a loop over a tree, whose
back edge the estimate can make lighter than its entry, still has its back
edge fall through into its head. Note [Layout relevant edge weights] in
GHC.CmmToAsm.BlockLayout lists the exceptions, as where the loop's head is the
entry block's one successor.
-}

{- Note [Frequencies before register allocation]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Static prediction estimates block frequencies for the layout only under
-fcmm-static-pred and only after register allocation
(Note [Static Branch Prediction]); the graph allocator's spill costs hold
mkGlobalWeights' estimate whatever the flag, but as a thunk no spill choice
forces (GHC.CmmToAsm.Reg.Graph.SpillCost). So the code that depends on how often
a block runs uses stand-ins, each marked by a TODO pointing here. Once the
estimate is computed in a Cmm pass, as that Note proposes, these could use it
instead, and they are to be reinvestigated together:

 * coldBlocks misses two shapes. In a procedure needing more stack than
   maxSpillSlots, allocMoreStack splits each entry block into a stack allocation
   jumping to a new block that holds the old code, so where the garbage
   collector returns to such a block and it jumps back to the check, the back
   edge leaving it now leaves the new block, whose out-edges are the old
   block's, not the code generator's. And a fixup block F on a check's failure
   edge H -> G gets that edge's HeapStackCheck tag on H -> F (addNodesBetween),
   so that F counts as the failure target and G as its successor, and the block
   the garbage collector returns to, after G, is missed. With the estimate,
   a block inserted after allocation would take the frequency of the edge it
   splits or of the block it continues, with no tag to copy.

 * Where several blocks give a vreg different destinations, wideMoveDests in
   GHC.CmmToAsm.Reg.Linear takes the last block's; the estimate would pick the
   hottest block's at no extra cost.

 * checkFailures counts as cold a block the garbage collector returns to that a
   call returns to as well, which the hot path runs, and linearRA_SCCs in
   GHC.CmmToAsm.Reg.Linear defers it; it also counts a failure target that
   another edge enters, and, for static prediction and the layout, the lighter
   side of an unannotated test of a register held off BaseReg, as of CurrentTSO,
   once favourFewerPreds has broken its tie. The estimate would give each
   block's frequency, and its loops would give linearRA_SCCs an order taken from
   the loops rather than the SCCs.

 * regClobbers in GHC.CmmToAsm.Reg.Linear counts every write, those in the
   blocks that follow a failed heap or stack check too; with the estimate a
   write could count by how often its block runs.
-}

-- | Combination of target node id and information about the branch
--   we are looking at.
type TargetNodeInfo = (BlockId, EdgeInfo)


-- | Update branch weights based on certain heuristics.
-- See Note [Static Branch Prediction]
-- TODO: This should be combined with optimizeCFG
{-# SCC staticBranchPrediction #-}
staticBranchPrediction :: S.Set LocalReg -> BlockId -> LoopInfo -> CFG -> CFG
staticBranchPrediction tags _root (LoopInfo l_backEdges l_loops) cfg =
    -- pprTrace "staticEstimatesOn" (ppr (cfg)) $
    foldl' update cfg nodes
  where
    nodes = getCfgNodes cfg
    -- each block to the blocks its back edges enter, looked up once a node:
    -- as master's set of the edges' pairs, looked up at each successor, they
    -- made the compiler run 0.047% more instructions and allocate 0.012% more
    -- compiling !16269's CmmDiamondChain at -O1 and -O2, and nothing more
    -- at -O0, where static prediction does not run, on GHC 10.1.20261108
    backedges = mapFromListWith setUnion
      [ (from, setSingleton to) | (from, to) <- l_backEdges ]
    -- The heads of loops but those whose back edges all leave a block of
    -- 'coldBlocks': such a back edge closes the cycle a failed heap or stack
    -- check makes through the garbage collector, and lehPredicts says nothing
    -- about a branch into a loop head, so it would say nothing about the test
    -- that stays in a loop holding a check. A back edge leaving a continuation
    -- that a call returns to as well counts the same, so a loop closed through
    -- such a call loses its head too. The check's own branch already gets no
    -- heuristic (isHeapOrStackCheck in update); this rule is for a branch
    -- into the cycle's head, which is the check's block where the collector's
    -- call returns to it or to a block that jumps back to it, and, where the
    -- call returns to a block that chooses among case alternatives, for that
    -- block's own test, of which lehPredicts would take the branch towards an
    -- alternative without such a check for a loop exit. This saves cycles, not
    -- instructions: with the stay side predicted, the layout can let it fall
    -- through, a taken jump fewer an iteration, which an instruction count does
    -- not show, as a conditional jump is one instruction whether taken or not.
    loopHeads = M.keysSet headBodies
    headBodies = M.fromListWith setUnion
      [ (h, body) | ((from, h), body) <- l_loops, not (from `setMember` cold) ]
    -- each block to the number of those loops whose bodies hold it
    loopLevels = mapFromListWith (+)
      [ (b, 1 :: Int) | body <- M.elems headBodies, b <- setElems body ]
    cold = coldBlocks cfg

    update :: CFG -> BlockId -> CFG
    update cfg node
        -- No successors, nothing to do.
        | null successors = cfg

        -- Mix of backedges and others:
        -- Always predict the backedges.
        -- This supersedes ohPredicts, wrongly for a rare retry: see
        -- Note [Static prediction of tag tests].
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

        -- Only backedges - no need to adjust. This case also keeps the weights
        -- of a back edge beside a heap or stack check's edge, which the mix
        -- case above excludes.
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
        -- backedges, but a retry the code generator adds, as cmpxchg_code's of
        -- an emulated atomic operation
        !backs = mapFindWithDefault setEmpty node backedges
        (m,not_m) = partition (\succ -> setMember (fst succ) backs
                                        && transitionSource (snd succ)
                                           /= AsmCodeGenRetry) successors

        -- Heuristics return nothing if they don't say anything about this branch
        -- or Just (prob_s1) where prob_s1 is the likelihood for s1 to be the
        -- taken branch. s1 is the successor with the smaller label, as
        -- getSuccessorEdges orders them, and need not be the true branch: a
        -- heuristic that needs the true branch looks it up, as ohPredicts does.

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

        -- Comparing to a constant is unlikely to be equal, whichever way the
        -- comparison is spelt: the control-flow optimisation may have turned
        -- an equality into an inequality (Note [Invert Cmm conditionals]).
        -- Wrong for a tag test where the likely constructor is declared first,
        -- so an inequality on bits under a mask, a tag's or a bitmap's, is left
        -- out, as is one on a constructor tag read from an info table, an
        -- unboxed sum's tag, a register holding a tag or one less than such a
        -- register: see Note [Static prediction of tag tests].
        -- TODO: the exception goes by an operand's form, not by what decides
        -- the test: a parity test, its mask inside arithmetic, is predicted,
        -- and an evaluation test, (x & 7) != 0, is left out with the tag tests.
        -- Predicting a pointer-tag test spelt with MO_Ne, such as (x & 7) != 0,
        -- as its MO_Eq spelling is predicted (the evaluated side likely) was
        -- tried on a development branch and showed no gain.
        -- BUG TODO: a constructor-tag test spelt as an equality is
        -- predicted like any other comparison, its equal side unlikely,
        -- though which side is likely depends on the order of the
        -- type's constructors: wrong for the trees of Data.Map and Data.Set
        -- (Note [Static prediction of tag tests]). This
        -- costs only speed, on x86-64 under -fcmm-static-pred.
        -- BUG: s1 is taken for the true side only where it is ltrue, the label
        -- the branch was made with, which an edge no longer ends at once the
        -- linear allocator's fixup block (addNodesBetween) or shortcutting
        -- (shortcutWeightMap) has redirected it. Where the redirected true
        -- edge's new target has the smaller label of the two, each side gets
        -- the other's prediction. This costs only speed, on x86-64 under
        -- -fcmm-static-pred.
        ohPredicts (s1,_s2)
            | CmmSource { trans_cmmNode = src1 } <- getTransitionSource node (fst s1) cfg
            , CmmCondBranch cond ltrue _lfalse likely <- src1
            , likely == Nothing
            , CmmMachOp mop args <- cond
            , Just trueProb <- case mop of
                                 MO_Eq {} -> Just 0.3
                                 MO_Ne {} | any isLit args
                                          , not (any isConTag args) -> Just 0.7
                                 _        -> Nothing
            , not (null [x | x@CmmLit{} <- args])
            = if fst s1 == ltrue then Just trueProb else Just (1 - trueProb)

            | otherwise
            = Nothing
        isConTag = isTagExpr (`S.member` tags)

        -- TODO: These are all the other heuristics from the paper.
        -- Not all will apply, for now we just stub them out as Nothing.
        phPredicts = const Nothing
        ghPredicts = const Nothing
        lhhPredicts = const Nothing
        chPredicts = const Nothing
        shPredicts = const Nothing
        rhPredicts = const Nothing

-- | Bits under a mask, a tag's or a bitmap's.
isMask :: CmmExpr -> Bool
isMask (CmmMachOp (MO_And _) _) = True
isMask _ = False

-- | A constructor tag read from an info table, a half word just below the info
-- pointer (Note [Tagging big families] in GHC.StgToCmm.Expr), widened or not;
-- or an unboxed sum's tag, narrowed to a byte from the register it is returned
-- in.
isConTagExpr :: CmmExpr -> Bool
isConTagExpr (CmmMachOp (MO_UU_Conv _ _) [e]) = isInfoTag e
isConTagExpr (CmmMachOp (MO_XX_Conv _ W8) [CmmReg _]) = True
isConTagExpr e = isInfoTag e

isInfoTag :: CmmExpr -> Bool
isInfoTag (CmmLoad addr ty _)
  | w <- typeWidth ty, w == W32 || w == W16
  = offsetOf addr == Just (negate (toInteger (widthInBytes w)))
  where
    offsetOf (CmmMachOp (MO_Add _) [_, CmmLit (CmmInt n _)]) = Just n
    offsetOf (CmmMachOp (MO_Sub _) [_, CmmLit (CmmInt n _)]) = Just (negate n)
    offsetOf (CmmRegOff _ n) = Just (toInteger n)
    offsetOf _ = Nothing
isInfoTag _ = False

-- | The local registers every assignment to which is a constructor tag:
-- a pointer's bits under a mask ('isMask'), one less than a tag
-- register (dataToTag#'s pointer side), 'isConTagExpr', or a copy of a tag
-- register.  A switch on a tag that is not a register gets one of these
-- (GHC.Cmm.Switch.Implement), as does dataToTag#'s join.
tagRegisters :: CmmGraph -> S.Set LocalReg
tagRegisters graph = M.keysSet (foldl' discard cands1 lost)
  where
    assigns =
      [ (r, e) | b <- toBlockList graph
               , let (_, mid, _) = blockSplit b
               , CmmAssign (CmmLocal r) e <- blockToList mid ]
    -- the registers some assignment to which is a tag if every register holds
    -- one, to those assignments' right-hand sides, and those of them assigned
    -- an expression that is not
    cands = M.fromListWith (++)
      [ (r, [e]) | (r, e) <- assigns, isTagExpr (const True) e ]
    untagged = [ r | (r, e) <- assigns, r `M.member` cands
                   , not (isTagExpr (const True) e) ]
    -- the results of unsafe foreign calls, the one other node that assigns
    -- a local register here: a safe call is lowered to unsafe ones before
    -- (lowerSafeForeignCall in GHC.Cmm.LayoutStack)
    called
      | M.null cands = []
      | otherwise    = [ r | b <- toBlockList graph
                           , let (_, mid, _) = blockSplit b
                           , CmmUnsafeForeignCall _ rs _ <- blockToList mid
                           , r <- rs ]
    cands1 = foldl' (flip M.delete) cands (untagged ++ called)
    -- the candidates assigned a copy of a register that is none, or one less;
    -- each is dropped, and with it the candidates assigned from it in turn,
    -- every candidate at most once: a pass over all of them for each one
    -- dropped took time quadratic in the length of a chain of copies
    lost = [ r | (r, es) <- M.toList cands1
               , not (all (isTagExpr (`M.member` cands1)) es) ]
    discard cs r
      | r `M.member` cs
      = foldl' discard (M.delete r cs) (M.findWithDefault [] r from)
      | otherwise
      = cs
    from = M.fromListWith (++)
      [ (s, [r]) | (r, es) <- M.toList cands1, e <- es
                 , Just s <- [tagSource e] ]

-- | A constructor tag in one of the forms 'tagRegisters' lists, given which
-- local registers hold one.
isTagExpr :: (LocalReg -> Bool) -> CmmExpr -> Bool
isTagExpr isTag e
  = isMask e || maybe (isConTagExpr e) isTag (tagSource e)

-- | The local register that an expression copies or is one less than.
tagSource :: CmmExpr -> Maybe LocalReg
tagSource e = case e of
  CmmRegOff (CmmLocal r) (-1) -> Just r
  CmmMachOp (MO_Sub _) [CmmReg (CmmLocal r), CmmLit (CmmInt 1 _)] -> Just r
  CmmReg (CmmLocal r) -> Just r
  _ -> Nothing

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
--
-- TODO: nothing checks the result, as sanityCheckCfg checks the CFG against
-- the blocks. A check could compare each block's frequency with the sum of the
-- frequencies on its in-edges; the propagation below departs from that sum
-- at some blocks, among them an irreducible block, a loop head whose cycle
-- probability is capped, and the entry block, which it gives 1 whatever its
-- in-edges.
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
          forM_ (successors block) $ \bi -> do
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
                irreducible <- anyM (\bp -> do
                    !bp_visited <- visited bp
                    let bp_backedge = isBackEdge bp block
                    return (not bp_visited && not bp_backedge)) preds

                -- An irreducible block keeps the frequency it has, 1 where
                -- it heads one of loopInfo's loops (that loop's pass set it)
                -- and else 0, and passes it on to its out-edges below. TODO:
                -- irreducible blocks are not rare in Haskell code. Such a
                -- block's frequency, and those of the blocks reached only
                -- through it, can thus be far below or above what summing its
                -- inflow would give, and on x86-64 under -fcmm-static-pred
                -- the layout by default ranks their edges by numbers that do
                -- not add up. Summing the inflow was tried
                -- on a development branch, in a replacement of this function:
                -- nofib's mkhprog ran 0.53% more instructions at -O2, eliza
                -- 0.20% and megaparsec's many and skipMany 0.72% to 1.06% fewer
                -- at -O1.
                if irreducible
                then return ()
                else do
                    setFreq block 0
                    !cycleProb <- foldM (\ !acc pred -> do
                        if isBackEdge pred block
                            then
                                (acc +) <$> getBackProb pred block
                            else do
                                !f <- getFreq block
                                !prob <- getEdgeFreq pred block
                                setFreq block $! f + prob
                                return acc) 0 preds
                    -- traceM $ "cycleProb:" ++ show cycleProb
                    let limit = 1 - 1/512 -- Paper uses 1 - epsilon, but this works.
                                          -- determines how large likelyhoods in loops can grow.
                    !cycleProb <- return $ min cycleProb limit -- <- return $ if cycleProb > limit then limit else cycleProb
                    -- traceM $ "cycleProb:" ++ show cycleProb

                    !f <- getFreq block
                    setFreq block (f / (1.0 - cycleProb))

            setVisited block
            calcOutFreqs head block

    -- Loops, by nesting, inner to outer.
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

    -- each block to the blocks its back edges enter: as master's set of the
    -- edges' pairs, each lookup comparing pairs, they made the compiler run
    -- 0.098% more instructions compiling !16269's CmmDiamondChain at -O1 and
    -- -O2, allocating 0.028% less, and 0.043% more instructions compiling
    -- nofib's puzzle at -O2, and nothing more at -O0, where calcFreqs does not
    -- run, on GHC 10.1.20261108
    isBackEdge from to = maybe False (IS.member to) (IM.lookup from backEdgeSet)
    backEdgeSet = IM.fromListWith IS.union
      [ (from, IS.singleton to) | (from, to) <- backEdges ]

    revGraph :: IntMap IntSet
    revGraph = IM.foldlWithKey' (\m from toMap -> addEdges m from toMap) IM.empty graph
        where
            addEdges m0 from toMap = IM.foldlWithKey' (\m k _ -> addEdge m from k) m0 toMap
            addEdge m0 from to = IM.insertWith IS.union to (IS.singleton from) m0
