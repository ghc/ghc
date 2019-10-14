--
-- Copyright (c) 2018 Andreas Klebinger
--

{-# LANGUAGE TypeFamilies, ScopedTypeVariables, CPP #-}

{-# OPTIONS_GHC -fprof-auto #-}
--{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -ddump-cmm #-}

module BlockLayout
    ( sequenceTop )
where

#include "HsVersions.h"
import GhcPrelude

import Instruction
import NCGMonad
import CFG

import BlockId
import Cmm
import Hoopl.Collections
import Hoopl.Label
import Hoopl.Block

import DynFlags (gopt, GeneralFlag(..), DynFlags, backendMaintainsCfg)
import UniqFM
import Util
import Unique

import Digraph
import Outputable
import Maybes

-- DEBUGGING ONLY
--import Debug
--import Debug.Trace
import ListSetOps (removeDups)
import PprCmm ()

import OrdList
import Data.List
import Data.Foldable (toList)
import Hoopl.Graph

import qualified Data.Set as Set
import Control.Applicative

{-
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ~~~ Note [Chain based CFG serialization]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  For additional information also look at
  https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CodeLayout

  We have a CFG with edge weights based on which we try to place blocks next to
  each other.

  Edge weights not only represent likelyhood of control transfer between blocks
  but also how much a block would benefit from being placed sequentially after
  it's predecessor.
  For example blocks which are preceeded by an info table are more likely to end
  up in a different cache line than their predecessor. So there is less benefit
  in placing them sequentially.

  For example consider this example:

  A:  ...
      jmp cond D (weak successor)
      jmp B
  B:  ...
      jmp C
  C:  ...
      jmp X
  D:  ...
      jmp B (weak successor)

  We determine a block layout by building up chunks (calling them chains) of
  possible control flows for which blocks will be placed sequentially.

  Eg for our example we might end up with two chains like:
  [A->B->C->X],[D]. Blocks inside chains will always be placed sequentially.
  However there is no particular order in which chains are placed since
  (hopefully) the blocks for which sequentially is important have already
  been placed in the same chain.

  -----------------------------------------------------------------------------
      First try to create a lists of good chains.
  -----------------------------------------------------------------------------

  We do so by taking a block not yet placed in a chain and
  looking at these cases:

  *)  Check if the best predecessor of the block is at the end of a chain.
      If so add the current block to the end of that chain.

      Eg if we look at block C and already have the chain (A -> B)
      then we extend the chain to (A -> B -> C).

      Combined with the fact that we process blocks in reverse post order
      this means loop bodies and trivially sequential control flow already
      ends up as a single chain.

  *)  Otherwise we create a singleton chain from the block we are looking at.
      Eg if we have from the example above already constructed (A->B)
      and look at D we create the chain (D) resulting in the chains [A->B, D]

  -----------------------------------------------------------------------------
      We then try to fuse chains.
  -----------------------------------------------------------------------------

  There are edge cases which result in two chains being created which trivially
  represent linear control flow. For example we might have the chains
  [(A-B-C),(D-E)] with an cfg triangle:

      A----->C->D->E
       \->B-/

  We also get three independent chains if two branches end with a jump
  to a common successor.

  We take care of these cases by fusing chains which are connected by an
  edge.

  We do so by looking at the list of edges sorted by weight.
  Given the edge (C -> D) we try to find two chains such that:
      * C is at the end of chain one.
      * D is in front of chain two.
      * If two such chains exist we fuse them.
  We then remove the edge and repeat the process for the rest of the edges.

  -----------------------------------------------------------------------------
      Place indirect successors (neighbours) after each other
  -----------------------------------------------------------------------------

  We might have chains [A,B,C,X],[E] in a CFG of the sort:

    A ---> B ---> C --------> X(exit)
                   \- ->E- -/

  While E does not follow X it's still beneficial to place them near each other.
  This can be advantageous if eg C,X,E will end up in the same cache line.

  TODO: If we remove edges as we use them (eg if we build up A->B remove A->B
        from the list) we could save some more work in later phases.


  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ~~~ Note [Triangle Control Flow]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Checking if an argument is already evaluating leads to a somewhat
  special case  which looks like this:

    A:
        if (R1 & 7 != 0) goto Leval; else goto Lwork;
    Leval: // global
        call (I64[R1])(R1) returns to Lwork, args: 8, res: 8, upd: 8;
    Lwork: // global
        ...

        A
        |\
        | Leval
        |/ - (This edge can be missing because of optimizations)
        Lwork

  Once we hit the metal the call instruction is just 2-3 bytes large
  depending on the register used. So we lay out the assembly like this:

        movq %rbx,%rax
        andl $7,%eax
        cmpq $1,%rax
        jne Lwork
    Leval:
        jmp *(%rbx) # encoded in 2-3 bytes.
    <info table>
    Lwork:
        ...

  We could explicitly check for this control flow pattern.

  This is advantageous because:
  * It's optimal if the argument isn't evaluated.
  * If it's evaluated we only have the extra cost of jumping over
    the 2-3 bytes for the call.
  * Guarantees the smaller encoding for the conditional jump.

  However given that Lwork usually has an info table we
  penalize this edge. So Leval should get placed first
  either way and things work out for the best.

  Optimizing for the evaluated case instead would penalize
  the other code path. It adds an jump as we can't fall through
  to Lwork because of the info table.
  Assuming that Lwork is large the chance that the "call" ends up
  in the same cache line is also fairly small.

-}


-- | Look at X number of blocks in two chains to determine
--   if they are "neighbours".
neighbourOverlapp :: Int
neighbourOverlapp = 2

-- | Only edges heavier than this are considered
--   for fusing two chains into a single chain.
fuseEdgeThreshold :: EdgeWeight
fuseEdgeThreshold = 0


-- | A non empty ordered sequence of basic blocks.
--   It is suitable for serialization in this order.
data BlockChain
    = BlockChain
    { chainMembers :: !LabelSet
    , chainBlocks :: !BlockSequence
    }

instance Eq (BlockChain) where
    (BlockChain s1 _) == (BlockChain s2 _)
        = s1 == s2

instance Outputable (BlockChain) where
    ppr (BlockChain _ blks) =
        parens (text "Chain:" <+> ppr (seqToList $ blks) )

data WeightedEdge = WeightedEdge !BlockId !BlockId EdgeWeight deriving (Eq)

-- Useful for things like sets and debugging purposes, sorts by blocks
-- in the chain.
instance Ord (BlockChain) where
   (BlockChain lbls1 _) `compare` (BlockChain lbls2 _)
       = lbls1 `compare` lbls2

-- | Non deterministic! (Uniques) Sorts edges by weight and nodes.
instance Ord WeightedEdge where
  compare (WeightedEdge from1 to1 weight1)
          (WeightedEdge from2 to2 weight2)
    | weight1 < weight2 || weight1 == weight2 && from1 < from2 ||
      weight1 == weight2 && from1 == from2 && to1 < to2
    = LT
    | from1 == from2 && to1 == to2 && weight1 == weight2
    = EQ
    | otherwise
    = GT

instance Outputable WeightedEdge where
    ppr (WeightedEdge from to info) =
        ppr from <> text "->" <> ppr to <> brackets (ppr info)

type WeightedEdgeList = [WeightedEdge]

noDups :: [BlockChain] -> Bool
noDups chains =
    let chainBlocks = concatMap chainToBlocks chains :: [BlockId]
        (_blocks, dups) = removeDups compare chainBlocks
    in if null dups then True
        else pprTrace "Duplicates:" (ppr (map toList dups) $$ text "chains" <+> ppr chains ) False

inFront :: BlockId -> BlockChain -> Bool
inFront bid (BlockChain _ seq)
  = seqFront seq == bid

chainMember :: BlockId -> BlockChain -> Bool
chainMember bid chain
  = setMember bid . chainMembers $ chain

chainSingleton :: BlockId -> BlockChain
chainSingleton lbl
    = BlockChain (setSingleton lbl) (Singleton lbl)

chainSnoc :: BlockChain -> BlockId -> BlockChain
chainSnoc (BlockChain lbls blks) lbl
  = BlockChain (setInsert lbl lbls) (seqSnoc blks lbl)

chainConcat :: BlockChain -> BlockChain -> BlockChain
chainConcat (BlockChain lbls1 blks1) (BlockChain lbls2 blks2)
  = BlockChain (setUnion lbls1 lbls2) (blks1 `seqConcat` blks2)

chainToBlocks :: BlockChain -> [BlockId]
chainToBlocks (BlockChain _ blks) = seqToList blks

-- | Given the Chain A -> B -> C -> D and we break at C
--   we get the two Chains (A -> B, C -> D) as result.
breakChainAt :: BlockId -> BlockChain
             -> (BlockChain,BlockChain)
breakChainAt bid (BlockChain lbls blks)
    | not (setMember bid lbls)
    = panic "Block not in chain"
    | otherwise
    = let (lblks, rblks) = break (\lbl -> lbl == bid)
                                 (seqToList blks)
          --TODO: Remove old
          --lblSet :: [GenBasicBlock i] -> BlockChain
          --lblSet blks =
          --  setFromList
                --(map (\(BasicBlock lbl _) -> lbl) $ toList blks)
      in
      (BlockChain (setFromList lblks) (seqFromBids lblks),
       BlockChain (setFromList rblks) (seqFromBids rblks))

takeR :: Int -> BlockChain -> [BlockId]
takeR n (BlockChain _ blks) =
    take n . seqToRList $ blks


takeL :: Int -> BlockChain -> [BlockId]
takeL n (BlockChain _ blks) = --error "TODO: takeLn"
    take n . seqToList $ blks

-- | For a given list of chains try to fuse chains with strong
--   edges between them into a single chain.
--   Returns the list of fused chains together with a set of
--   used edges. The set of edges is indirectly encoded in the
--   chains so doesn't need to be considered for later passes.
fuseChains :: WeightedEdgeList -> LabelMap BlockChain
           -> (LabelMap BlockChain, Set.Set WeightedEdge)
fuseChains weights chains
    = let fronts = mapFromList $
                    map (\chain -> (head $ takeL 1 chain,chain)) $
                    mapElems chains :: LabelMap BlockChain
          (chains', used, _) = applyEdges weights chains fronts Set.empty
      in (chains', used)
    where
        applyEdges :: WeightedEdgeList -> LabelMap BlockChain
                   -> LabelMap BlockChain -> Set.Set WeightedEdge
                   -> (LabelMap BlockChain, Set.Set WeightedEdge, LabelMap BlockChain)
        applyEdges [] chainsEnd chainsFront used
            = (chainsEnd, used, chainsFront)
        applyEdges (edge@(WeightedEdge from to w):edges) chainsEnd chainsFront used
            --Since we order edges descending by weight we can stop here
            | w <= fuseEdgeThreshold
            = ( chainsEnd, used, chainsFront)
            --Fuse the two chains
            | Just c1 <- mapLookup from chainsEnd
            , Just c2 <- mapLookup to chainsFront
            , c1 /= c2
            = let newChain = chainConcat c1 c2
                  front = head $ takeL 1 newChain
                  end = head $ takeR 1 newChain
                  chainsFront' = mapInsert front newChain $
                                 mapDelete to chainsFront
                  chainsEnd'   = mapInsert end newChain $
                                 mapDelete from chainsEnd
              in applyEdges edges chainsEnd' chainsFront'
                            (Set.insert edge used)
            | otherwise
            --Check next edge
            = applyEdges edges chainsEnd chainsFront used


-- See also Note [Chain based CFG serialization]
-- We have the chains (A-B-C-D) and (E-F) and an Edge C->E.
--
-- While placing the later after the former doesn't result in sequential
-- control flow it is still be benefical since block C and E might end
-- up in the same cache line.
--
-- So we place these chains next to each other even if we can't fuse them.
--
--   A -> B -> C -> D
--             v
--             - -> E -> F ...
--
-- Simple heuristic to chose which chains we want to combine:
--   * Process edges in descending priority.
--   * Check if there is a edge near the end of one chain which goes
--     to a block near the start of another edge.
--
-- While we could take into account the space between the two blocks which
-- share an edge this blows up compile times quite a bit. It requires
-- us to find all edges between two chains, check the distance for all edges,
-- rank them based on the distance and and only then we can select two chains
-- to combine. Which would add a lot of complexity for little gain.

-- | For a given list of chains and edges try to combine chains with strong
--   edges between them.
combineNeighbourhood :: WeightedEdgeList -> [BlockChain]
                     -> [BlockChain]
combineNeighbourhood edges chains
    = -- pprTraceIt "Neigbours" $
      applyEdges edges endFrontier startFrontier
    where
        --Build maps from chain ends to chains
        endFrontier, startFrontier :: FrontierMap
        endFrontier =
            mapFromList $ concatMap (\chain ->
                                let ends = getEnds chain
                                    entry = (ends,chain)
                                in map (\x -> (x,entry)) ends ) chains
        startFrontier =
            mapFromList $ concatMap (\chain ->
                                let front = getFronts chain
                                    entry = (front,chain)
                                in map (\x -> (x,entry)) front) chains
        applyEdges :: WeightedEdgeList -> FrontierMap -> FrontierMap
                   -> [BlockChain]
        applyEdges [] chainEnds _chainFronts =
            ordNub $ map snd $ mapElems chainEnds
        applyEdges ((WeightedEdge from to _w):edges) chainEnds chainFronts
            | Just (c1_e,c1) <- mapLookup from chainEnds
            , Just (c2_f,c2) <- mapLookup to chainFronts
            , c1 /= c2 -- Avoid trying to concat a short chain with itself.
            = let newChain = chainConcat c1 c2
                  newChainFrontier = getFronts newChain
                  newChainEnds = getEnds newChain
                  newFronts :: FrontierMap
                  newFronts =
                    let withoutOld =
                            foldl' (\m b -> mapDelete b m :: FrontierMap) chainFronts (c2_f ++ getFronts c1)
                        entry =
                            (newChainFrontier,newChain) --let bound to ensure sharing
                    in foldl' (\m x -> mapInsert x entry m)
                              withoutOld newChainFrontier

                  newEnds =
                    let withoutOld = foldl' (\m b -> mapDelete b m) chainEnds (c1_e ++ getEnds c2)
                        entry = (newChainEnds,newChain) --let bound to ensure sharing
                    in foldl' (\m x -> mapInsert x entry m)
                              withoutOld newChainEnds
              in
                -- pprTrace "ApplyEdges"
                --  (text "before" $$
                --   text "fronts" <+> ppr chainFronts $$
                --   text "ends" <+> ppr chainEnds $$

                --   text "various" $$
                --   text "newChain" <+> ppr newChain $$
                --   text "newChainFrontier" <+> ppr newChainFrontier $$
                --   text "newChainEnds" <+> ppr newChainEnds $$
                --   text "drop" <+> ppr ((c2_f ++ getFronts c1) ++ (c1_e ++ getEnds c2)) $$

                --   text "after" $$
                --   text "fronts" <+> ppr newFronts $$
                --   text "ends" <+> ppr newEnds
                --   )
                 applyEdges edges newEnds newFronts
            | otherwise
            = --pprTrace "noNeigbours" (ppr ()) $
              applyEdges edges chainEnds chainFronts
         where

        getFronts chain = takeL neighbourOverlapp chain
        getEnds chain = takeR neighbourOverlapp chain



-- See [Chain based CFG serialization]
buildChains :: CFG -> [BlockId]
            -> ( LabelMap BlockChain  -- Resulting chains.
               , Set.Set (BlockId, BlockId)) --List of fused edges.
buildChains succWeights blocks
  = let (_, fusedEdges, chains) = buildNext setEmpty mapEmpty blocks Set.empty
    in (chains, fusedEdges)
  where
    -- We keep a map from the last block in a chain to the chain itself.
    -- This we we can easily check if an block should be appened to an
    -- existing chain!
    buildNext :: LabelSet
              -> LabelMap BlockChain -- Map from last element to chain.
              -> [BlockId] -- Blocks to place
              -> Set.Set (BlockId, BlockId)
              -> ( [BlockChain]  -- Placed Blocks
                 , Set.Set (BlockId, BlockId) --List of fused edges
                 , LabelMap BlockChain
                 )
    buildNext _placed chains [] linked =
        ([], linked, chains)
    buildNext placed chains (block:todo) linked
        | setMember block placed
        = buildNext placed chains todo linked
        | otherwise
        = buildNext placed' chains' todo linked'
      where
        placed' = (foldl' (flip setInsert) placed placedBlocks)
        linked' = Set.union linked linkedEdges
        (placedBlocks, chains', linkedEdges) = findChain block

        --Add the block to a existing or new chain
        --Returns placed blocks, list of resulting chains
        --and fused edges
        findChain :: BlockId
                -> ([BlockId],LabelMap BlockChain, Set.Set (BlockId, BlockId))
        findChain block
        -- B) place block at end of existing chain if
        -- there is no better block to append.
          | (pred:_) <- preds
          , alreadyPlaced pred
          , Just predChain <- mapLookup pred chains
          , (best:_) <- filter (not . alreadyPlaced) $ getSuccs pred
          , best == lbl
          = --pprTrace "B.2)" (ppr (pred,lbl)) $
            let newChain = chainSnoc predChain block
                chainMap = mapInsert lbl newChain $ mapDelete pred chains
            in  ( [lbl]
                , chainMap
                , Set.singleton (pred,lbl) )

          | otherwise
          = --pprTrace "single" (ppr lbl)
            ( [lbl]
            , mapInsert lbl (chainSingleton lbl) chains
            , Set.empty)
            where
              alreadyPlaced blkId = (setMember blkId placed)
              lbl = block
              getSuccs = map fst . getSuccEdgesSorted succWeights
              preds = map fst $ getSuccEdgesSorted predWeights lbl
    --For efficiency we also create the map to look up predecessors here
    predWeights = reverseEdges succWeights



-- We make the CFG a Hoopl Graph, so we can reuse revPostOrder.
newtype BlockNode e x = BN (BlockId,[BlockId])
instance NonLocal (BlockNode) where
  entryLabel (BN (lbl,_))   = lbl
  successors (BN (_,succs)) = succs

fromNode :: BlockNode C C -> BlockId
fromNode (BN x) = fst x

sequenceChain :: forall a i. (Instruction i, Outputable i) => LabelMap a -> CFG
            -> [GenBasicBlock i] -> [GenBasicBlock i]
sequenceChain _info _weights    [] = []
sequenceChain _info _weights    [x] = [x]
sequenceChain  info weights'     blocks@((BasicBlock entry _):_) =
    --Optimization, delete edges of weight <= 0.
    --This significantly improves performance whenever
    --we iterate over all edges, which is a few times!
    let weights :: CFG
        weights
            = filterEdges (\_f _t edgeInfo -> edgeWeight edgeInfo > 0) weights'
        blockMap :: LabelMap (GenBasicBlock i)
        blockMap
            = foldl' (\m blk@(BasicBlock lbl _ins) ->
                        mapInsert lbl blk m)
                     mapEmpty blocks

        toNode :: BlockId -> BlockNode C C
        toNode bid =
            -- sorted such that heavier successors come first.
            BN (bid,map fst . getSuccEdgesSorted weights' $ bid)

        orderedBlocks :: [BlockId]
        orderedBlocks
            = map fromNode $
              revPostorderFrom (fmap (toNode . blockId) blockMap) entry

        (builtChains, builtEdges)
            = {-# SCC "buildChains" #-}
              --pprTraceIt "generatedChains" $
              --pprTrace "orderedBlocks" (ppr orderedBlocks) $
              buildChains weights orderedBlocks

        rankedEdges :: WeightedEdgeList
        -- Sort edges descending, remove fused eges
        rankedEdges =
            map (\(from, to, weight) -> WeightedEdge from to weight) .
            filter (\(from, to, _)
                        -> not (Set.member (from,to) builtEdges)) .
            sortWith (\(_,_,w) -> - w) $ weightedEdgeList weights

        (fusedChains, fusedEdges)
            = ASSERT(noDups $ mapElems builtChains)
              {-# SCC "fuseChains" #-}
              --(pprTrace "RankedEdges" $ ppr rankedEdges) $
              --pprTraceIt "FusedChains" $
              fuseChains rankedEdges builtChains

        rankedEdges' =
            filter (\edge -> not $ Set.member edge fusedEdges) $ rankedEdges

        neighbourChains
            = ASSERT(noDups $ mapElems fusedChains)
              {-# SCC "groupNeighbourChains" #-}
              --pprTraceIt "ResultChains" $
              combineNeighbourhood rankedEdges' (mapElems fusedChains)

        --Make sure the first block stays first
        ([entryChain],chains')
            = ASSERT(noDups $ neighbourChains)
              partition (chainMember entry) neighbourChains
        (entryChain':entryRest)
            | inFront entry entryChain = [entryChain]
            | (rest,entry) <- breakChainAt entry entryChain
            = [entry,rest]
            | otherwise = pprPanic "Entry point eliminated" $
                            ppr ([entryChain],chains')

        prepedChains
            = entryChain':(entryRest++chains') :: [BlockChain]
        blockList
            -- = (concatMap chainToBlocks prepedChains)
            = (concatMap seqToList $ map chainBlocks prepedChains)

        --chainPlaced = setFromList $ map blockId blockList :: LabelSet
        chainPlaced = setFromList $ blockList :: LabelSet
        unplaced =
            let blocks = mapKeys blockMap
                isPlaced b = setMember (b) chainPlaced
            in filter (\block -> not (isPlaced block)) blocks

        placedBlocks =
            --pprTraceIt "placedBlocks" $
            blockList ++ unplaced
        getBlock bid = expectJust "Block placment" $ mapLookup bid blockMap
    in
        --Assert we placed all blocks given as input
        ASSERT(all (\bid -> mapMember bid blockMap) placedBlocks)
        dropJumps info $ map getBlock placedBlocks

dropJumps :: forall a i. Instruction i => LabelMap a -> [GenBasicBlock i]
          -> [GenBasicBlock i]
dropJumps _    [] = []
dropJumps info ((BasicBlock lbl ins):todo)
    | not . null $ ins --This can happen because of shortcutting
    , [dest] <- jumpDestsOfInstr (last ins)
    , ((BasicBlock nextLbl _) : _) <- todo
    , not (mapMember dest info)
    , nextLbl == dest
    = BasicBlock lbl (init ins) : dropJumps info todo
    | otherwise
    = BasicBlock lbl ins : dropJumps info todo


-- -----------------------------------------------------------------------------
-- Sequencing the basic blocks

-- Cmm BasicBlocks are self-contained entities: they always end in a
-- jump, either non-local or to another basic block in the same proc.
-- In this phase, we attempt to place the basic blocks in a sequence
-- such that as many of the local jumps as possible turn into
-- fallthroughs.

sequenceTop
    :: (Instruction instr, Outputable instr)
    => DynFlags -- Determine which layout algo to use
    -> NcgImpl statics instr jumpDest
    -> Maybe CFG -- ^ CFG if we have one.
    -> NatCmmDecl statics instr -- ^ Function to serialize
    -> NatCmmDecl statics instr

sequenceTop _     _       _           top@(CmmData _ _) = top
sequenceTop dflags ncgImpl edgeWeights
            (CmmProc info lbl live (ListGraph blocks))
  | (gopt Opt_CfgBlocklayout dflags) && backendMaintainsCfg dflags
  --Use chain based algorithm
  , Just cfg <- edgeWeights
  = CmmProc info lbl live ( ListGraph $ ncgMakeFarBranches ncgImpl info $
                            {-# SCC layoutBlocks #-}
                            sequenceChain info cfg blocks )
  | otherwise
  --Use old algorithm
  = let cfg = if dontUseCfg then Nothing else edgeWeights
    in  CmmProc info lbl live ( ListGraph $ ncgMakeFarBranches ncgImpl info $
                                {-# SCC layoutBlocks #-}
                                sequenceBlocks cfg info blocks)
  where
    dontUseCfg = gopt Opt_WeightlessBlocklayout dflags ||
                 (not $ backendMaintainsCfg dflags)


-- The old algorithm:
-- It is very simple (and stupid): We make a graph out of
-- the blocks where there is an edge from one block to another iff the
-- first block ends by jumping to the second.  Then we topologically
-- sort this graph.  Then traverse the list: for each block, we first
-- output the block, then if it has an out edge, we move the
-- destination of the out edge to the front of the list, and continue.

-- FYI, the classic layout for basic blocks uses postorder DFS; this
-- algorithm is implemented in Hoopl.

sequenceBlocks :: Instruction inst => Maybe CFG -> LabelMap a
               -> [GenBasicBlock inst] -> [GenBasicBlock inst]
sequenceBlocks _edgeWeight _ [] = []
sequenceBlocks edgeWeights infos (entry:blocks) =
    let entryNode = mkNode edgeWeights entry
        bodyNodes = reverse
                    (flattenSCCs (sccBlocks edgeWeights blocks))
    in dropJumps infos . seqBlocks infos $ ( entryNode : bodyNodes)
  -- the first block is the entry point ==> it must remain at the start.

sccBlocks
        :: Instruction instr
        => Maybe CFG -> [NatBasicBlock instr]
        -> [SCC (Node BlockId (NatBasicBlock instr))]
sccBlocks edgeWeights blocks =
    stronglyConnCompFromEdgedVerticesUniqR
        (map (mkNode edgeWeights) blocks)

mkNode :: (Instruction t)
       => Maybe CFG -> GenBasicBlock t
       -> Node BlockId (GenBasicBlock t)
mkNode edgeWeights block@(BasicBlock id instrs) =
    DigraphNode block id outEdges
  where
    outEdges :: [BlockId]
    outEdges
      --Select the heaviest successor, ignore weights <= zero
      = successor
      where
        successor
          | Just successors <- fmap (`getSuccEdgesSorted` id)
                                    edgeWeights -- :: Maybe [(Label, EdgeInfo)]
          = case successors of
            [] -> []
            ((target,info):_)
              | length successors > 2 || edgeWeight info <= 0 -> []
              | otherwise -> [target]
          | otherwise
          = case jumpDestsOfInstr (last instrs) of
                [one] -> [one]
                _many -> []


seqBlocks :: LabelMap i -> [Node BlockId (GenBasicBlock t1)]
                        -> [GenBasicBlock t1]
seqBlocks infos blocks = placeNext pullable0 todo0
  where
    -- pullable: Blocks that are not yet placed
    -- todo:     Original order of blocks, to be followed if we have no good
    --           reason not to;
    --           may include blocks that have already been placed, but then
    --           these are not in pullable
    pullable0 = listToUFM [ (i,(b,n)) | DigraphNode b i n <- blocks ]
    todo0     = map node_key blocks

    placeNext _ [] = []
    placeNext pullable (i:rest)
        | Just (block, pullable') <- lookupDeleteUFM pullable i
        = place pullable' rest block
        | otherwise
        -- We already placed this block, so ignore
        = placeNext pullable rest

    place pullable todo (block,[])
                          = block : placeNext pullable todo
    place pullable todo (block@(BasicBlock id instrs),[next])
        | mapMember next infos
        = block : placeNext pullable todo
        | Just (nextBlock, pullable') <- lookupDeleteUFM pullable next
        = BasicBlock id instrs : place pullable' todo nextBlock
        | otherwise
        = block : placeNext pullable todo
    place _ _ (_,tooManyNextNodes)
        = pprPanic "seqBlocks" (ppr tooManyNextNodes)


lookupDeleteUFM :: Uniquable key => UniqFM elt -> key
                -> Maybe (elt, UniqFM elt)
lookupDeleteUFM m k = do -- Maybe monad
    v <- lookupUFM m k
    return (v, delFromUFM m k)

-- -------------------------------------------------------------------
-- Some specialized data structures to speed things up:
--  * BlockSequence: A specialized version of Data.Sequence.
--    Better at indexing at the front/end but lacks ability
--    to do lookup by position.

type FrontierMap = LabelMap ([BlockId],BlockChain)

-- | A "reverse zipper" of sorts.
-- We store a list of blocks in two parts, the initial part from left to right
-- and the remaining part stored in reverse order. This makes it easy to look
-- the last/first element and append on both sides.
data BlockSequence
  = Singleton !BlockId
  | Pair (OrdList BlockId) (OrdList BlockId)
    -- ^ For a non empty pair there is at least one element in the left part.
  | Empty

seqFront :: BlockSequence -> BlockId
seqFront Empty = panic "Empty sequence"
seqFront (Singleton bid) = bid
seqFront (Pair lefts rights) = expectJust "Seq invariant" $
    listToMaybe (fromOL lefts) <|> listToMaybe (fromOL $ reverseOL rights)

-- seqEnd :: BlockSequence -> BlockId
-- seqEnd Empty = panic "Empty sequence"
-- seqEnd (Singleton bid) = bid
-- seqEnd (Pair lefts rights) = expectJust "Seq invariant" $
--     listToMaybe (fromOL rights) <|> listToMaybe (fromOL $ reverseOL lefts)

seqToList :: BlockSequence -> [BlockId]
seqToList Empty = []
seqToList (Singleton bid) = [bid]
seqToList (Pair lefts rights) = fromOL $ lefts `appOL` reverseOL rights


seqToRList :: BlockSequence -> [BlockId]
seqToRList Empty = []
seqToRList (Singleton bid) = [bid]
seqToRList (Pair lefts rights) = fromOL $ rights `appOL` reverseOL lefts

seqSnoc :: BlockSequence -> BlockId -> BlockSequence
seqSnoc (Empty) bid = Singleton bid
seqSnoc (Singleton s) bid= Pair (unitOL s) (unitOL bid)
seqSnoc (Pair lefts rights) bid = Pair lefts (bid `consOL` rights)

seqConcat :: BlockSequence -> BlockSequence -> BlockSequence
seqConcat (Empty) x2 = x2
seqConcat (Singleton b1) (Singleton b2) = Pair (unitOL b1) (unitOL b2)
seqConcat x1 (Empty) = x1
seqConcat (Singleton b1) (Pair lefts rights) = Pair (b1 `consOL` lefts) rights
seqConcat (Pair lefts rights) (Singleton b2) = Pair lefts (b2 `consOL` rights)
seqConcat (Pair lefts1 rights1) (Pair lefts2 rights2) =
    Pair (lefts1 `appOL` (reverseOL rights1) `appOL` lefts2) rights2

seqFromBids :: [BlockId] -> BlockSequence
seqFromBids [] = Empty
seqFromBids [b1] = Singleton b1
seqFromBids [b1,b2] = Pair (unitOL b1) (unitOL b2)
seqFromBids [b1,b2,b3] = Pair (consOL b1 $ unitOL b2) (unitOL b3)
seqFromBids (b1:b2:b3:bs) = Pair (toOL [b1,b2,b3]) (toOL bs)
