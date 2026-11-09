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

    --Construction/Misc
    , getCfg, getCfgProc, pprEdgeWeights, sanityCheckCfg

    --Find backedges and update their weight
    , optimizeCFG

    -- Block frequencies before register allocation
    , BlockFreqs, blockFrequencies, blockFreq, blockFreqAt, noBlockFreqs
    , blockIndex, blockCount, tagRegisters

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

import GHC.CmmToAsm.CFG.Weight

import qualified Data.Map as M
import qualified Data.Set as S

import GHC.Utils.Outputable
import GHC.Utils.Panic
-- DEBUGGING ONLY
--import GHC.Cmm.DebugBlock
--import GHC.Data.OrdList
--import GHC.Cmm.DebugBlock.Trace

import Data.List (sort, partition)
import Control.Monad.ST

import Data.Array.MArray
import Data.Array.ST
import Data.Array.IArray
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt)
import Data.Int (Int32)
import qualified GHC.Data.Word64Map.Strict as WM
import qualified GHC.Data.Word64Map.Internal as WMI
import Data.Array.Unboxed (UArray)
import Data.Word (Word64)

import Control.Monad
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
              , trans_info :: !BranchInfo }
  | AsmCodeGen
  -- | An edge the code generator adds for an operation's retry, as
  -- cmpxchg_code's: static prediction's rule that a back edge is more likely
  -- than any other edge leaves it out.
  | AsmCodeGenRetry
  deriving (Eq)

-- | What 'getCfg' records of an edge beside its node: whether the node is
-- a heap or stack check, and whether the edge is the one made for the true
-- target of a 'CmmCondBranch'. The edge's target does not tell which side it
-- is: a fixup block of the register allocator ('addNodesBetween') or
-- a shortcut ('shortcutWeightMap') moves the edge to another target, and the
-- node keeps the labels it was made with.
data BranchInfo = NoInfo         -- ^ Unknown, but not heap or stack check.
                | NoInfoTrue     -- ^ The same, a branch's true side.
                | HeapStackCheck -- ^ Heap or stack check
                | HeapStackCheckTrue
                                 -- ^ Heap or stack check, its true side.
    deriving Eq

instance Outputable BranchInfo where
    ppr NoInfo = text "regular"
    ppr NoInfoTrue = text "regular, true side"
    ppr HeapStackCheck = text "heap/stack"
    ppr HeapStackCheckTrue = text "heap/stack, true side"

-- | The info of the same branch's true side.
trueSide :: BranchInfo -> BranchInfo
trueSide NoInfo = NoInfoTrue
trueSide HeapStackCheck = HeapStackCheckTrue
trueSide info = info

isHeapOrStackCheck :: TransitionSource -> Bool
isHeapOrStackCheck (CmmSource { trans_info = HeapStackCheck}) = True
isHeapOrStackCheck (CmmSource { trans_info = HeapStackCheckTrue}) = True
isHeapOrStackCheck _ = False

-- | Whether the edge is the one 'getCfg' made for the true target of
-- a 'CmmCondBranch'.
isTrueSide :: TransitionSource -> Bool
isTrueSide (CmmSource { trans_info = NoInfoTrue}) = True
isTrueSide (CmmSource { trans_info = HeapStackCheckTrue}) = True
isTrueSide _ = False

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
coldBlocks cfg = coldBlocksOf (checkFailures cfg) cfg

-- | 'coldBlocks', given the CFG's 'checkFailures'.
coldBlocksOf :: LabelSet -> CFG -> LabelSet
coldBlocksOf gc cfg = setUnion gc $ setFromList
    [ s | b <- setElems gc, Just outs <- [mapLookup b cfg], s <- mapKeys outs
        , inserted s ]
  where
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
              [ mkEdge False f condBranchWeight
              , mkEdge True  t condBranchWeight ]
          | l == Just True ->
              [ mkEdge False f unlikelyCondWeight
              , mkEdge True  t likelyCondWeight ]
          | l == Just False ->
              [ mkEdge False f likelyCondWeight
              , mkEdge True  t unlikelyCondWeight ]
          where
            mkEdgeInfo side = -- pprTrace "Info" (ppr branchInfo <+> ppr cond)
              EdgeInfo (CmmSource branch
                          (if side then trueSide branchInfo else branchInfo))
              . fromIntegral
            mkEdge side target weight = ((bid,target), mkEdgeInfo side weight)
            -- BUG: this runs after fixStgRegisters, which turns a read of
            -- a global register with no machine register into a load at
            -- its offset from BaseReg. So on x86-64 a test of CurrentTSO
            -- or CurrentNursery, such as `if (v == CurrentTSO)` in
            -- rts/Updates.cmm, is tagged HeapStackCheck too. This costs only
            -- speed, on x86-64: under -fcmm-static-pred or -fblock-layout-cfg,
            -- and, for an annotated test such as that one, in the linear
            -- allocator's cold blocks at any optimisation level.
            !branchInfo =
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
-- They differ from the back edges of static prediction's loops on three kinds
-- of edge (Note [Back edges in freqEstimate]). The search takes a block's
-- successors in their labels' order, so in an irreducible region the labels
-- pick which edges are back edges; under -fobject-determinism a Haskell
-- module's labels are the same for the same inputs (Note [Norms of the NCG] in
-- GHC.CmmToAsm).
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

-- | The CFG with its final weights and its back edges: where static prediction
-- estimated the weights, those of the loops it found, with the other back edges
-- of its search, which only an irreducible region has ('staticPredCfg'), else
-- 'findBackEdges'' with none; given the registers holding tags
-- ('tagRegisters').
optimizeCFG :: Bool -> S.Set LocalReg -> Weights -> RawCmmDecl -> CFG
            -> (CFG, Edges, Edges)
optimizeCFG _ _ _ (CmmData {}) !cfg = (cfg, [], [])
optimizeCFG doStaticPred tags weights (CmmProc info _lab _live graph) cfg
  -- no bonus for back edges where static prediction follows: it starts each
  -- two-way branch from optHsPatterns' weights, so that the penalties become
  -- probabilities, and overwrites the weights of every branch with a back edge
  -- among its targets except a heap or stack check's and one whose targets are
  -- all back edges; the back edges are those of its loops: one into the root
  -- is findBackEdges' alone, and a branch with one starts from its weight with
  -- no bonus, which changed no code measured and saves optimizeCFG a search
  | doStaticPred
  -- each component evaluated, here and in the guard below: left lazy, the CFG
  -- made the compiler allocate 0.048% more compiling T9961 at -O0, where only
  -- the guard below runs, and 0.019% more at -O, on GHC 10.1.20261111
  , (!cfg', !backs, !searchBacks)
      <- staticPredCfg tags entry $ optHsPatterns [] weights info cfg
  = (cfg', backs, searchBacks)
  | !backs <- findBackEdges entry cfg
  , !cfg' <- optHsPatterns backs weights info cfg
  = (cfg', backs, [])
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

-- | Each block's estimated frequency: see 'blockFrequencies'. The labels'
-- uniques in ascending order, and each one's frequency at its index.
data BlockFreqs = BlockFreqs !(UArray Int Word64) !(UArray Int Double)

-- | The estimate holding no block.
noBlockFreqs :: BlockFreqs
noBlockFreqs = BlockFreqs (listArray (0, -1) []) (listArray (0, -1) [])

-- | A block's estimated frequency, 0 for one the estimate does not hold.
blockFreq :: BlockFreqs -> BlockId -> Double
blockFreq (BlockFreqs ks fs) b
  | i >= 0    = unsafeAt fs i
  | otherwise = 0
  where i = uniqueIndex ks (lblToUnique b)

-- | The frequency of the block at an index 'blockIndex' gave, not -1.
blockFreqAt :: BlockFreqs -> Int -> Double
blockFreqAt (BlockFreqs _ fs) i = unsafeAt fs i

-- | A block's index in the estimate, from 0 to 'blockCount' - 1, or -1 for
-- one the estimate does not hold.
blockIndex :: BlockFreqs -> BlockId -> Int
blockIndex (BlockFreqs ks _) b = uniqueIndex ks (lblToUnique b)

-- | The number of blocks the estimate holds.
blockCount :: BlockFreqs -> Int
blockCount (BlockFreqs ks _) = snd (bounds ks) + 1

-- | A unique's index in an ascending array of uniques, -1 where absent.
uniqueIndex :: UArray Int Word64 -> Word64 -> Int
uniqueIndex ks !k = go 0 (snd (bounds ks))
  where
    go !lo !hi
      | lo > hi   = -1
      | otherwise = case compare (unsafeAt ks mid) k of
          LT -> go (mid + 1) hi
          GT -> go lo (mid - 1)
          EQ -> mid
      where mid = (lo + hi) `quot` 2

-- INLINE: rdI and wrI both, which GHC inlines without it as well, the
-- compiler's code unchanged, on GHC 10.1.20261108.
rdI :: STUArray s Int Int32 -> Int -> ST s Int
rdI a i = fromIntegral <$> unsafeRead a i
{-# INLINE rdI #-}

-- INLINE: see rdI.
wrI :: STUArray s Int Int32 -> Int -> Int -> ST s ()
wrI a i x = unsafeWrite a i (fromIntegral x)
{-# INLINE wrI #-}

-- | A block's head flag in 'freqEstimate': its state, 1 for a back edge's
-- target, 2 for a loop's head and 3 for one counting for prediction; whether
-- the block heads a loop; what is counted above the state, and the unit it is
-- counted in.
hdState, hdAbove :: Int -> Int
hdState f = f .&. 3
hdAbove f = f `shiftR` 2

hdHead :: Int -> Bool
hdHead f = hdState f >= 2

hdUnit :: Int
hdUnit = 4

-- | Run the action on each element of a label map with its label's unique,
-- in their order, and its index counted from the given one; the index after
-- the last.
forMapST :: (Word64 -> a -> Int -> ST s ()) -> LabelMap a -> Int -> ST s Int
forMapST f (LM m0) = go m0
  where
    -- over the map's constructors, a workaround for an unidentified issue:
    -- foldM over WM.toList made the compiler allocate 0.16% more and run
    -- 0.074% more instructions compiling !16269's CmmDiamondChain at -O1, 0.16%
    -- and 0.070% more at -O2, and nothing more at -O0, where blockFrequencies
    -- does not run, on GHC 10.1.20261108
    go WMI.Nil          !i = return i
    go (WMI.Tip k x)    !i = f k x i >> return (i + 1)
    go (WMI.Bin _ _ l r) !i = go l i >>= go r
{-# INLINE forMapST #-}

-- | Each block's estimated frequency, how many times it runs each time the
-- procedure runs from the given block ('freqEstimate'), given the procedure's
-- 'tagRegisters' and the CFG's checkFailures: the linear allocator's, made
-- before it allocates from the CFG as instruction selection gives it.
blockFrequencies :: S.Set LocalReg -> LabelSet -> BlockId -> CFG -> BlockFreqs
blockFrequencies tags failed root cfg
  = freqEstimate False noBlockFreqs (\keys _ fs -> BlockFreqs keys fs)
                 tags failed root cfg

{- Note [Back edges in freqEstimate]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
freqEstimate takes an edge for a loop's back edge where its target dominates its
source, and never takes an edge into the root for a back edge. Hence:

* BUG #28083: an edge into the entry block is never a back edge, so a loop whose
  head is the entry block is not found. This costs only speed, on x86-64 under
  -fcmm-static-pred.

findBackEdges, which optHsPatterns uses for the back edge bonus where static
prediction does not follow, classifies the edges by a depth-first search alone.
It counts as back edges the edges into the entry block and the back edges of
irreducible cycles, and no self loop. TODO: the two notions of a back edge
disagree on these three kinds of edge.
-}

-- | Static prediction's estimate (Note [Static Branch Prediction]): each
-- block's frequency, how many times it runs each time the procedure runs
-- from the given block, and each edge's probability, given the procedure's
-- 'tagRegisters' and the CFG's checkFailures, in time linear in the CFG but
-- for its loops' nesting and in two arrays: run at -O0 as well when this was
-- written, it made the compiler allocate 4.4% more compiling T783 at -O0 over
-- LabelMaps, and 0.18% more in the arrays. Run only under -fcmm-static-pred
-- now, for the allocator it made the compiler run 0.40% more instructions
-- compiling !16269's CmmDiamondChain at -O1 and -O2, and allocate 0.070% and
-- 0.065% more compiling T12707 at -O and -O2, and nothing more at -O0, where
-- it does not run, on GHC 10.1.20261108. The fixpoint is Wu and Larus's, inner
-- loops first, the back edges into one block taken as one loop. A back edge
-- is an edge into a block still on the stack of a depth-first search from
-- the given block, but one into that block; it is a loop's where its target
-- dominates its source, and the loop's blocks are those reached backwards from
-- its back edges' sources without passing the head. Where the walk from a
-- source reaches a block before the head in the search's reverse postorder, the
-- head does not dominate the source, and its edge, in an irreducible region,
-- counts for no block's frequency. A loop counts for static prediction's loop
-- heads where one of its back edges leaves a block not in 'coldBlocks', and for
-- the loop levels of the blocks reached from those edges' sources. A block the
-- given one does not reach runs 0 times. The search takes a block's successors
-- in their labels' order, as 'findBackEdges' does, which picks the back edges
-- of an irreducible region and the order in which a block's inflows are summed:
-- see 'findBackEdges' on -fobject-determinism.
--
-- Given True first, a block entered by a back edge from a block it does not
-- dominate is skipped: where it heads a loop, it runs once each time the
-- head of a loop holding it, or the procedure, runs, and else never. Static
-- prediction gives True and the allocator False ('blockFrequencies').
--
-- The estimate inherits static prediction's errors on tag tests: a lookup's
-- loop down one of Data.Map's trees comes out as rarely repeated (the BUG TODO
-- at ohPredicts, Note [Static prediction of tag tests]).
--
-- Given no blocks, or a given block not among them, the result is the second
-- argument; else the third applied to the blocks' uniques in ascending order
-- and the two arrays, for n blocks and m edges: the Int32 array holding at 0
-- the offsets of each block's edges, at n + 1 their targets as encoded below,
-- at n + 1 + m the blocks' head flags and at 2 * n + 1 + m the edges' flags,
-- and the Double array each block's frequency at its index and each edge's
-- probability at 2 * n plus its.
freqEstimate :: forall r. Bool -> r
             -> (UArray Int Word64 -> UArray Int Int32 -> UArray Int Double
                 -> r)
             -> S.Set LocalReg -> LabelSet -> BlockId -> CFG -> r
freqEstimate skip none found tags failed root cfg@(LM cm)
  | n == 0    = none
  | otherwise = runST $ do
    ks <- newArray (0, n - 1) 0 :: ST s (STUArray s Int Word64)
    _ <- forMapST (\k _ i -> unsafeWrite ks i k) cfg 0
    keys <- unsafeFreeze ks
    let rootV = uniqueIndex keys (lblToUnique root)
    if rootV < 0 then return none else estimate keys rootV
  where
    n = WM.size cm
    m = WM.foldl' (\a (LM om) -> a + WM.size om) 0 cm
    cold = coldBlocksOf failed cfg
    estimate :: UArray Int Word64 -> Int -> ST s r
    estimate keys rootV = do
        let vtx = uniqueIndex keys
        -- the Int32 array ia: each block's successors, the elements off + i ..
        -- off + i + 1 - 1 of tgt for block i, in their labels' order, -2 - j
        -- for a back edge into block j, with
        -- each edge's flags in ef, 1 for a heap or stack check, 2 for a retry
        -- and 4 for a back edge whose target does not dominate its source;
        -- each block's predecessors that are reached, the elements
        -- pof + i .. pof + i + 1 - 1 of prd; rpo, each block's position in
        -- the search's reverse postorder, -1 for one not reached and -2 for one
        -- on its stack; ord, the blocks by position; stk, the search's stack
        -- and then the walk's tags; nxt, each stacked block's next edge, then
        -- the walk's blocks and then the loop's queue; hd, 1 for a back edge's
        -- target, 2 for a loop's head, 3 for one counting for prediction, plus
        -- 4 times its loop level and then its count of forward edges in a loop
        -- still to visit; off, tgt, hd and ef come first, for the caller; ef's
        -- offset, feEf n m, is written as oHd + n: computed by feEf, it made
        -- the compiler run 0.013% more instructions compiling T12707 at -O, on
        -- GHC 10.1.20261111
        let oOff = 0; oTgt = feTgt n; oHd = oTgt + m; oEf = oHd + n
            oPrd = oEf + m; oPof = oPrd + m; oRpo = oPof + n + 1
            oOrd = oRpo + n; oStk = oOrd + n; oNxt = oStk + n
        ia <- newArray (0, oNxt + n - 1) 0 :: ST s (STUArray s Int Int32)
        -- the Double array da: each block's frequency, which holds its inflow
        -- until the block is visited, and its multiplier, at its index, and
        -- each edge's weight and then probability at its
        let oF = 0; oMul = n; oPr = fePr n
        da <- newArray (0, oPr + m - 1) 0 :: ST s (STUArray s Int Double)
        _ <- forMapST (\_ outs v -> do
               s <- rdI ia (oOff + v)
               e' <- forMapST (\t info e -> do
                       -- a node: getCfg makes one of every block it
                       -- reaches, and every later writer adds an edge by
                       -- addEdge, which makes its target one, or retargets
                       -- one to a node (shortcutWeightMap)
                       let !x = vtx t
                       wrI ia (oTgt + e)
                           (assertPpr (x >= 0) (ppr (mkHooplLabel t)) x)
                       let src = transitionSource info
                       wrI ia (oEf + e) ((if isHeapOrStackCheck src then 1 else 0)
                                         + (if src == AsmCodeGenRetry then 2 else 0))
                       unsafeWrite da (oPr + e) (weightToDouble (edgeWeight info)))
                     outs s
               wrI ia (oOff + v + 1) e')
             cfg 0
        forM_ [0 .. n - 1] $ \i -> do
          wrI ia (oRpo + i) (-1)
          unsafeWrite da (oMul + i) 1
        let dec x = if x < -1 then -2 - x else x
            -- each edge out of block v: its index, its target, whether a back
            -- edge
            edges v f = do
              s <- rdI ia (oOff + v)
              t <- rdI ia (oOff + v + 1)
              let go !e | e == t    = return ()
                        | otherwise = do
                            x <- rdI ia (oTgt + e)
                            f e (dec x) (x < -1)
                            go (e + 1)
              go s
            push sp v = do
              wrI ia (oRpo + v) (-2)
              rdI ia (oOff + v) >>= wrI ia (oNxt + v)
              wrI ia (oStk + sp) v
            dfs !sp !k
              | sp == 0 = return k
              | otherwise = do
                  v <- rdI ia (oStk + sp - 1)
                  e <- rdI ia (oNxt + v)
                  end <- rdI ia (oOff + v + 1)
                  if e == end
                    then do wrI ia (oRpo + v) (k - 1)
                            wrI ia (oOrd + k - 1) v
                            dfs (sp - 1) (k - 1)
                    else do
                      wrI ia (oNxt + v) (e + 1)
                      t <- rdI ia (oTgt + e)
                      st <- if t == rootV then return 0
                                          else rdI ia (oRpo + t)
                      case st of
                        -2 -> wrI ia (oTgt + e) (-2 - t) >> dfs sp k
                        -1 -> push sp t >> dfs (sp + 1) k
                        _  -> dfs sp k
        push 0 rootV
        first <- dfs 1 n
        let reached f = let go !r | r == n    = return ()
                                  | otherwise = rdI ia (oOrd + r) >>= f >> go (r + 1)
                        in go first
            bump i d = rdI ia i >>= wrI ia i . (+ d)
        reached $ \v -> edges v $ \_ t b -> do
          bump (oPof + t) 1
          when b $ do h <- rdI ia (oHd + t)
                      when (h == 0) $ wrI ia (oHd + t) 1
        forM_ [1 .. n] $ \i -> rdI ia (oPof + i - 1) >>= bump (oPof + i)
        reached $ \v -> edges v $ \_ t _ -> do
          c <- subtract 1 <$> rdI ia (oPof + t)
          wrI ia (oPrd + c) v
          wrI ia (oPof + t) c
        -- the back edge from block p into block h: its index in tgt; and
        -- whether h dominates p, the edge not flagged 4 in ef
        let backInto p h = do
              s <- rdI ia (oOff + p)
              let go !e = do x <- rdI ia (oTgt + e)
                             if x == -2 - h then return e else go (e + 1)
              go s
            dominated h p = do
              e <- backInto p h
              (\fl -> fl .&. 4 == 0) <$> rdI ia (oEf + e)
            -- the walk from the sources of a head's back edges that the given
            -- test takes, marking each block it reaches with the tag in stk
            -- and listing it in nxt, the head first: the number of the loop's
            -- blocks, or -1 where it reaches a block before the head in reverse
            -- postorder
            walk h tag from = do
              wrI ia (oStk + h) tag
              wrI ia oNxt h
              rh <- rdI ia (oRpo + h)
              ps <- rdI ia (oPof + h)
              pe <- rdI ia (oPof + h + 1)
              let tails !i !len
                    | i == pe = return len
                    | otherwise = do
                        p <- rdI ia (oPrd + i)
                        rp <- rdI ia (oRpo + p)
                        sp <- rdI ia (oStk + p)
                        ok <- if rp >= rh && sp /= tag then from p
                                                       else return False
                        if ok
                          then do wrI ia (oStk + p) tag
                                  wrI ia (oNxt + len) p
                                  tails (i + 1) (len + 1)
                          else tails (i + 1) len
                  go !j !len
                    | j == len = return len
                    | otherwise = do
                        u <- rdI ia (oNxt + j)
                        us <- rdI ia (oPof + u)
                        ue <- rdI ia (oPof + u + 1)
                        let ins !i !l
                              | i == ue = go (j + 1) l
                              | otherwise = do
                                  p <- rdI ia (oPrd + i)
                                  rp <- rdI ia (oRpo + p)
                                  sp <- rdI ia (oStk + p)
                                  if rp < 0 || sp == tag then ins (i + 1) l
                                  else if rp < rh then return (-1)
                                  else do wrI ia (oStk + p) tag
                                          wrI ia (oNxt + l) p
                                          ins (i + 1) (l + 1)
                        ins us len
              len0 <- tails ps 1
              go 1 len0
        reached $ \h -> do
          f <- rdI ia (oHd + h)
          when (hdState f == 1) $ do
            ps <- rdI ia (oPof + h)
            pe <- rdI ia (oPof + h + 1)
            rh <- rdI ia (oRpo + h)
            len <- walk h (-1 - h) (\_ -> return True)
            -- in an irreducible region, each source is walked alone, and the
            -- edge from one the head does not dominate is flagged: it is no
            -- back edge of the head's loop
            when (len < 0) $ forM_ [ps .. pe - 1] $ \i -> do
              p <- rdI ia (oPrd + i)
              rp <- rdI ia (oRpo + p)
              when (rp >= rh) $ do
                l <- walk h (2 * n + i) (return . (== p))
                when (l < 0) $ backInto p h >>= \e -> bump (oEf + e) 4
            -- the loop's back edges, and those of them leaving a block not
            -- in 'coldBlocks', else the loop heads nothing and counts for no
            -- level: such a back edge closes the cycle a failed heap or stack
            -- check makes through the garbage collector, and lehPredicts says
            -- nothing about a branch into a loop head, so it would say nothing
            -- about the test that stays in a loop holding a check. A back edge
            -- leaving a continuation that a call returns to as well counts the
            -- same, so a loop closed through such a call loses its head
            -- too (the TODO at checkFailures). The check's own branch already
            -- gets no heuristic (below); this rule is for a branch into the
            -- cycle's head, which is the check's block where the collector's
            -- call returns to it or to a block that jumps back to it, and,
            -- where the call returns to a block that chooses among case
            -- alternatives, for that block's own test, of which lehPredicts
            -- would take the branch towards an alternative without such a check
            -- for a loop exit. This saves cycles, not instructions: with the
            -- stay side predicted, the layout can let it fall through, a taken
            -- jump fewer an iteration, which an instruction count does not
            -- show, as a conditional jump is one instruction whether taken or
            -- not.
            let isCold p = mkHooplLabel (unsafeAt keys p) `setMember` cold
                warm p = (&& not (isCold p)) <$> dominated h p
                tally !i !nd !nw
                  | i == pe = return (nd, nw)
                  | otherwise = do
                      p <- rdI ia (oPrd + i)
                      rp <- rdI ia (oRpo + p)
                      if rp < rh then tally (i + 1) nd nw else do
                        d <- dominated h p
                        let w = d && not (isCold p)
                        tally (i + 1) (if d then nd + 1 else nd)
                                      (if w then nw + 1 else nw)
            (nd, nw) <- tally ps (0 :: Int) (0 :: Int)
            if nd == 0 then wrI ia (oHd + h) (f - 1) else do
              wrI ia (oHd + h) (f + if nw > 0 then 2 else 1)
              -- the levels: the blocks reached from the sources of those
              -- leaving a block not in 'coldBlocks'
              when (nw > 0) $ do
                lw <- if len >= 0 && nw == nd then return len
                      else walk h (2 * n + m + h) warm
                forM_ [0 .. lw - 1] $ \j -> do
                  u <- rdI ia (oNxt + j)
                  bump (oHd + u) hdUnit
        -- each edge's weight made a probability: a negative one taken as 0,
        -- over its block's total, or an equal share where that is 0
        let normalise s e1 = do
              let total !e !a | e == e1   = return a
                              | otherwise = do w <- unsafeRead da (oPr + e)
                                               total (e + 1) (a + max 0 w)
              tw <- total s 0
              forM_ [s .. e1 - 1] $ \e -> do
                w <- unsafeRead da (oPr + e)
                unsafeWrite da (oPr + e)
                  (if tw == 0 then 1 / fromIntegral (e1 - s) else max 0 w / tw)
        reached $ \v -> do
          s <- rdI ia (oOff + v)
          e1 <- rdI ia (oOff + v + 1)
          let cnt = e1 - s
          if cnt == 1 then unsafeWrite da (oPr + s) 1 else when (cnt > 1) $ do
            normalise s e1
            -- the back edges into a loop's head, but a retry the code
            -- generator adds (cmpxchg_code's of an emulated atomic operation),
            -- and the heap or stack checks: a branch with back edges and other
            -- edges and no check gives the back edges pred_LBH between them,
            -- superseding every other heuristic, wrongly for a rare retry, as
            -- the BUG in Note [Static prediction of tag tests] says; a check
            -- "loops", but only once, and gets no heuristic
            let kept x fl = x < -1 && fl .&. 4 == 0 && fl .&. 2 == 0
                count !e !nb !hc
                  | e == e1 = return (nb, hc)
                  | otherwise = do
                      x <- rdI ia (oTgt + e)
                      fl <- rdI ia (oEf + e)
                      count (e + 1) (if kept x fl then nb + 1 else nb)
                                    (hc + fl .&. 1)
            (nb, hc) <- count s (0 :: Int) (0 :: Int)
            if nb > 0 && nb < cnt && hc == 0
              then forM_ [s .. e1 - 1] $ \e -> do
                     x <- rdI ia (oTgt + e)
                     fl <- rdI ia (oEf + e)
                     unsafeWrite da (oPr + e)
                       (if kept x fl
                          then weightToDouble pred_LBH / fromIntegral nb
                          else (1 - weightToDouble pred_LBH) / fromIntegral (cnt - nb))
              else when (cnt == 2 && nb == 0 && hc == 0) $
                case mapLookup (mkHooplLabel (unsafeAt keys v)) cfg of
                  -- the two edges off the map's constructors: a match on
                  -- mapToList, which builds the list it matches, made the
                  -- compiler allocate 0.021% and 0.020% more compiling !16269's
                  -- CmmDiamondChain at -O1 and -O2 and run 0.009% and 0.008%
                  -- more instructions compiling T783 at -O and -O2, and nothing
                  -- more at -O0, where blockFrequencies does not run, on GHC
                  -- 10.1.20261108
                  Just (LM (WMI.Bin _ _ (WMI.Tip _ i1) (WMI.Tip _ _))) -> do
                    n1 <- unsafeRead da (oPr + s)
                    n2 <- unsafeRead da (oPr + s + 1)
                    t1 <- dec <$> rdI ia (oTgt + s)
                    t2 <- dec <$> rdI ia (oTgt + s + 1)
                    let fact t = do f <- rdI ia (oHd + t)
                                    return (hdState f == 3, hdAbove f)
                    (h1, l1) <- fact t1
                    (h2, l2) <- fact t2
                    let (p1, p2) = predictBranch tags h1 l1 i1 (EdgeWeight n1)
                                     h2 l2 (EdgeWeight n2)
                    unsafeWrite da (oPr + s) (weightToDouble p1)
                    unsafeWrite da (oPr + s + 1) (weightToDouble p2)
                  _ -> return ()
        -- TODO: nothing checks the result, as sanityCheckCfg checks the CFG
        -- against the blocks. A check could compare each block's frequency
        -- with the sum of the frequencies on its in-edges; the propagation
        -- below departs from that sum at some blocks, among them an irreducible
        -- block, a loop head whose cycle probability is capped, and the entry
        -- block, which it gives 1 whatever its in-edges.
        --
        -- the fixpoint, inner loops first: a loop's head runs once, every
        -- other block of it as often as the forward edges into it lead there,
        -- times its multiplier where it heads a loop itself, and the head's
        -- multiplier is 1 / (1 - p), p the probability of returning to it,
        -- capped at limit: the paper uses 1 - epsilon, but this works, and it
        -- bounds how large the frequencies in a loop can grow. The head flags
        -- keep 2 for a loop's head and, where the first argument says so, 1 for
        -- a block entered by a back edge from a block it does not dominate
        forM_ [0 .. n - 1] $ \i -> do
          f <- rdI ia (oHd + i)
          wrI ia (oHd + i) (if hdHead f then 2 else 0)
        -- TODO: irreducible blocks are not rare in Haskell code. Such a block's
        -- frequency, and those of the blocks reached only through it, can be
        -- far below or above what summing its inflow would give, and on x86-64
        -- under -fcmm-static-pred the layout by default ranks their edges by
        -- numbers that do not add up. Summing the inflow was tried on a
        -- development branch: nofib's mkhprog ran 0.53% more instructions at
        -- -O2, eliza 0.20% and megaparsec's many and skipMany 0.72% to 1.06%
        -- fewer at -O1.
        when skip $ reached $ \v -> edges v $ \e t _ -> do
          fl <- rdI ia (oEf + e)
          when (fl .&. 4 /= 0) $ rdI ia (oHd + t) >>= wrI ia (oHd + t) . (.|. 1)
        let limit = 1 - 1 / 512
            fwd u f = do
              ru <- rdI ia (oRpo + u)
              edges u $ \e t _ -> do
                rt <- rdI ia (oRpo + t)
                when (rt > ru) $ f e t
            addAcc t x = unsafeRead da (oF + t) >>= unsafeWrite da (oF + t) . (+ x)
            inflow v = do
              hv <- rdI ia (oHd + v)
              if odd hv then return (if hdHead hv then 1 else 0) else do
                a <- unsafeRead da (oF + v)
                x <- unsafeRead da (oMul + v)
                return (a * x)
            loops !r
              | r < first = return ()
              | otherwise = do
                  h <- rdI ia (oOrd + r)
                  hf <- rdI ia (oHd + h)
                  when (hdHead hf) $ do
                    let tag = h + n
                    len <- walk h tag (dominated h)
                    forM_ [0 .. len - 1] $ \j -> do
                      u <- rdI ia (oNxt + j)
                      unsafeWrite da (oF + u) 0
                      fwd u $ \_ t -> do
                        st <- rdI ia (oStk + t)
                        when (st == tag) $ bump (oHd + t) hdUnit
                    let kahn !qh !qt !cyc
                          | qh == qt = return cyc
                          | otherwise = do
                              u <- rdI ia (oNxt + qh)
                              fu <- if u == h then return 1 else inflow u
                              unsafeWrite da (oF + u) fu
                              ru <- rdI ia (oRpo + u)
                              s <- rdI ia (oOff + u)
                              t' <- rdI ia (oOff + u + 1)
                              let out !e !qt' !c
                                    | e == t' = kahn (qh + 1) qt' c
                                    | otherwise = do
                                        t <- dec <$> rdI ia (oTgt + e)
                                        p <- unsafeRead da (oPr + e)
                                        if t == h
                                          then out (e + 1) qt' (c + fu * p)
                                          else do
                                          st <- rdI ia (oStk + t)
                                          rt <- rdI ia (oRpo + t)
                                          if st /= tag || rt <= ru
                                            then out (e + 1) qt' c
                                            else do
                                            addAcc t (fu * p)
                                            d <- subtract hdUnit
                                                   <$> rdI ia (oHd + t)
                                            wrI ia (oHd + t) d
                                            if hdAbove d == 0
                                              then wrI ia (oNxt + qt') t
                                                     >> out (e + 1) (qt' + 1) c
                                              else out (e + 1) qt' c
                              out s qt cyc
                    cyc <- kahn 0 1 0
                    unsafeWrite da (oMul + h) (1 / (1 - min cyc limit))
                  loops (r - 1)
        loops (n - 1)
        -- the whole procedure, in reverse postorder
        forM_ [0 .. n - 1] $ \i -> unsafeWrite da (oF + i) 0
        reached $ \v -> do
          fv <- if v == rootV then return 1 else inflow v
          unsafeWrite da (oF + v) fv
          fwd v $ \e t -> do
            p <- unsafeRead da (oPr + e)
            addAcc t (fv * p)
        fs <- unsafeFreeze da
        is <- unsafeFreeze ia
        return $! found keys is fs

-- | The offsets in 'freqEstimate''s arrays, for n blocks and m edges, of the
-- edges' targets, probabilities and flags.
feTgt, fePr :: Int -> Int
feTgt n = n + 1
fePr n = 2 * n
feEf :: Int -> Int -> Int
feEf n m = 2 * n + 1 + m

-- | The CFG with each edge's weight its estimated frequency, how many times
-- it is taken each time the procedure runs, its source's frequency times its
-- probability ('freqEstimate'); with the back edges of the estimate's loops,
-- and the other back edges of its search, those whose target does not dominate
-- their source, which only an irreducible region has.
{-# SCC staticPredCfg #-}
staticPredCfg :: S.Set LocalReg -> BlockId -> CFG -> (CFG, Edges, Edges)
staticPredCfg tags entry cfg@(LM cm) = (cfg', backs', searchBacks')
  where
    -- the triple built here, so that optimizeCFG returns one on every path and
    -- keeps its worker (CPR): returning freqEstimate's result, it allocated
    -- the triple its caller takes apart at once, at -O0 as well
    (cfg', backs', searchBacks')
      = freqEstimate True (cfg, [], []) weigh tags (checkFailures cfg) entry cfg
    weigh keys is fs = (LM (snd (WM.mapAccum block 0 cm)), backs, searchBacks)
      where
        n = snd (bounds keys) + 1
        at i = fromIntegral (unsafeAt is i) :: Int
        m = at n
        block !v (LM outs) =
          (v + 1, LM (snd (WM.mapAccum (edge (unsafeAt fs v)) (at v) outs)))
        edge !f !e info =
          (e + 1, info { edgeWeight = EdgeWeight (f * unsafeAt fs (oPr + e)) })
        oPr = fePr n
        !oEf = feEf n m
        label v = mkHooplLabel (unsafeAt keys v)
        (loops, others) = partition (\(_, _, e) -> at (oEf + e) .&. 4 == 0)
          [ (v, -2 - x, e) | v <- [0 .. n - 1], e <- [at v .. at (v + 1) - 1]
                           , let x = at (feTgt n + e), x < -1 ]
        backs = [ (label v, label j) | (v, j, _) <- loops ]
        searchBacks = [ (label v, label j) | (v, j, _) <- others ]

{- Note [Where a branch's likely side is known]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Which side of a conditional branch runs more often is known in one of three
places, and the code generator treats each case differently:

* Where the branch is made. GHC.StgToCmm annotates some branches with their
  likely side, and getCfg sets the weights of an annotated branch's edges from
  the annotation. One such branch is that of a heap or stack check to the
  garbage collector, which GHC.StgToCmm.Heap marks as unlikely (`Just False`).
  getCfg tags such a branch HeapStackCheck, and freqEstimate leaves alone the
  weights of any branch with an edge so tagged. getCfg recognises the
  branch by the registers its condition reads, not by the annotation, so other
  tests of the same registers are tagged too (a BUG at getCfg). Of the third
  case's heuristics, two reach any other annotated branch: lehPredicts combines
  its prediction with the weights the branch has by then, and the back edge rule
  replaces them where the branch has both a back edge and another edge (the BUG
  in Note [Static Branch Prediction]). ohPredicts skips an annotated
  branch. Stack layout reads the annotation too, in unlikelyOnlyBlocks and
  gcRetryCopies (GHC.Cmm.LayoutStack).

* Upstream, and not carried down. Core and STG know which constructor a tag
  test selects, but the test that chooses a case alternative carries no
  annotation: cgAlts in GHC.StgToCmm.Expr makes it a CmmSwitch, which
  GHC.Cmm.Switch.Implement turns into CmmCondBranches without one. So ohPredicts
  treats it like any comparison with a constant where it is spelt as an equality
  (the BUG TODO there), and leaves it out where it is spelt as an inequality
  (the TODO there).

* Nowhere. Under -fcmm-static-pred, freqEstimate applies static prediction's
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
* Stack layout copies a heap or stack check that a body split off a continuation
  also enters, for the garbage collector's retry, under the conditions of
  Note [Retries copy their checks] in GHC.Cmm.LayoutStack: the copies cost code,
  and the hot check loses the retry's way in.
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
(AsmCodeGenRetry). WIP: the paper's other heuristics are not implemented, so
a two-way branch to which none of the three applies keeps the CFG's weights,
normalised.

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
  - Requires refactoring `freqEstimate` to work on both, Cmm Graphs and the backend CFG.
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
-fcmm-static-pred and only after register allocation, from the CFG as
shortcutting and optHsPatterns leave it (staticPredCfg, Note [Static Branch
Prediction]); blockFrequencies makes the same estimate, freqEstimate's, but for
the skip of irreducible regions (its first argument), for the linear allocator
before it allocates, under the same flag, from the CFG as instruction selection
gives it, and linearRA_SCCs orders the SCCs by the estimate. So the code that
depends on how often a block runs uses stand-ins, at -O0 and elsewhere, each
marked by a TODO pointing here. TODO: once the estimate is computed in a Cmm
pass, as Note [Static Branch Prediction] proposes, these could use it instead,
and they are to be reinvestigated together:

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
   GHC.CmmToAsm.Reg.Linear takes the last block's; taking the hottest block's,
   by the allocator's estimate, moved benchmarks both ways.

 * checkFailures counts as cold a block the garbage collector returns to that a
   call returns to as well, which the hot path runs, and linearRA_SCCs in
   GHC.CmmToAsm.Reg.Linear, where it has no estimate, defers it; it also counts
   a failure target that another edge enters, and, for static prediction and the
   layout, the lighter side of an unannotated test of a register held off
   BaseReg, as of CurrentTSO, once favourFewerPreds has broken its tie. The
   estimate would give each block's frequency, and its loops would give
   linearRA_SCCs an order taken from the loops rather than the SCCs.
   GHC.Cmm.LayoutStack's unlikelyOnlyBlocks computes the candidate cold set: it
   takes the likelihood where a branch is made, from cml_likely and literal
   conditions, and leaves warm a block a warm edge also enters.

 * regClobbers in GHC.CmmToAsm.Reg.Linear counts every write, those in the
   blocks that follow a failed heap or stack check too; counting a write only
   where its block's estimate is at least half that of the block allocating
   the vreg cost more instructions than it saved.
-}



-- | Chance that loops are taken.
pred_LBH :: EdgeWeight
pred_LBH = 0.875

-- | The weights static prediction gives the two edges of a two-way branch that
-- is neither a heap or stack check nor has a back edge, given the edges'
-- probabilities: lehPredicts' and ohPredicts' predictions applied in turn.
-- Each target comes with whether it is a loop head and the number of loops
-- holding it.
predictBranch :: S.Set LocalReg -> Bool -> Int -> EdgeInfo -> EdgeWeight
              -> Bool -> Int -> EdgeWeight
              -> (EdgeWeight, EdgeWeight)
predictBranch tags h1 l1 s1_info !n1 h2 l2 !n2
  = applyHeuristic (applyHeuristic (n1, n2) lehPredicts) ohPredicts
  where
    -- Apply result of a heuristic. Argument is the likelihood
    -- predicted for s1.
    applyHeuristic :: (EdgeWeight, EdgeWeight) -> Maybe Prob
                   -> (EdgeWeight, EdgeWeight)
    applyHeuristic old Nothing = old
    applyHeuristic old@(s1_old, s2_old) (Just (s1_pred :: Double))
      | s1_old == 0 || s2_old == 0
      = old
      | otherwise =
        let -- Predictions from heuristic
            s1_prob = EdgeWeight s1_pred :: EdgeWeight
            s2_prob = 1.0 - s1_prob
            -- Update
            d = (s1_old * s1_prob) + (s2_old * s2_prob) :: EdgeWeight
            !s1_prob' = s1_old * s1_prob / d
            !s2_prob' = s2_old * s2_prob / d
        in  (s1_prob', s2_prob')

    -- Heuristics return nothing if they don't say anything about this branch
    -- or Just (prob_s1) where prob_s1 is the likelihood for s1 to be the taken
    -- branch. s1 is the successor with the smaller label and need not be the
    -- true branch: a heuristic that needs the true branch reads which one
    -- s1 is off its edge ('isTrueSide'), as ohPredicts does.
    -- The other heuristics from the paper, phPredicts, ghPredicts,
    -- lhhPredicts, chPredicts, shPredicts and rhPredicts, are not implemented:
    -- the WIP in Note [Static Branch Prediction].

    -- Loop exit heuristic.
    -- We are unlikely to leave a loop unless it's to enter another one.
    pred_LEH = 0.75
    -- If and only if no successor is a loopheader,
    -- then we will likely not exit the current loop body.
    lehPredicts :: Maybe Prob
    lehPredicts
      | h1 || h2
      = Nothing

      | otherwise
      = case compare l1 l2 of
            EQ -> Nothing
            LT -> Just (1-pred_LEH) --s1 exits to a shallower loop level (exits loop)
            GT -> Just (pred_LEH)   --s1 exits to a deeper loop level

    -- Comparing to a constant is unlikely to be equal, whichever way the
    -- comparison is spelt: the control-flow optimisation may have turned
    -- an equality into an inequality (Note [Invert Cmm conditionals]).
    -- Wrong for a tag test where the likely constructor is declared first,
    -- so an inequality on bits under a mask, a tag's or a bitmap's, is left
    -- out, as is one on a constructor tag read from an info table, an unboxed
    -- sum's tag, a register holding a tag or one less than such a register: see
    -- Note [Static prediction of tag tests].
    -- TODO: the exception goes by an operand's form, not by what decides the
    -- test: a parity test, its mask inside arithmetic, is predicted, and an
    -- evaluation test, (x & 7) != 0, is left out with the tag tests.
    -- Predicting a pointer-tag test spelt with MO_Ne, such as (x & 7) != 0, as
    -- its MO_Eq spelling is predicted (the evaluated side likely) was tried on
    -- a development branch and showed no gain.
    -- BUG TODO: a constructor-tag test spelt as an equality is predicted
    -- like any other comparison, its equal side unlikely, though which side
    -- is likely depends on the order of the type's constructors: wrong for
    -- the trees of Data.Map and Data.Set (Note [Static prediction of tag
    -- tests]). This costs only speed, on x86-64 under -fcmm-static-pred.
    ohPredicts
        | s1_src@CmmSource { trans_cmmNode = src1 } <- transitionSource s1_info
        , CmmCondBranch cond _ltrue _lfalse likely <- src1
        , likely == Nothing
        , CmmMachOp mop args <- cond
        , Just trueProb <- case mop of
                             MO_Eq {} | any isLit args -> Just 0.3
                             MO_Ne {} | any isLit args
                                      , not (any isConTag args) -> Just 0.7
                             _        -> Nothing
        = if isTrueSide s1_src then Just trueProb else Just (1 - trueProb)

        | otherwise
        = Nothing
    isConTag = isTagExpr (`S.member` tags)

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
