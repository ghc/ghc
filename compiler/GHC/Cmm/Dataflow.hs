{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

--
-- Copyright (c) 2010, João Dias, Simon Marlow, Simon Peyton Jones,
-- and Norman Ramsey
--
-- Modifications copyright (c) The University of Glasgow 2012
--
-- This module is a specialised and optimised version of
-- Compiler.Hoopl.Dataflow in the hoopl package.  In particular it is
-- specialised to the UniqDSM monad.
--

module GHC.Cmm.Dataflow
  ( C, O, Block
  , lastNode, entryLabel
  , foldNodesBwdOO
  , foldRewriteNodesBwdOO
  , DataflowLattice(..), OldFact(..), NewFact(..), JoinedFact(..)
  , TransferFun, RewriteFun
  , Fact, FactBase
  , getFact, mkFactBase
  , analyzeCmmFwd, analyzeCmmBwd
  , rewriteCmmBwd
  , changedIf
  , joinOutFacts
  , joinFacts
  )
where

import GHC.Prelude

import GHC.Cmm
import GHC.Types.Unique.DSM

import Data.Array
import Data.Maybe
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Kind (Type)

import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import qualified GHC.Cmm.Dataflow.Label as Det

type family   Fact (x :: Extensibility) f :: Type
type instance Fact C f = FactBase f
type instance Fact O f = f

newtype OldFact a = OldFact a

newtype NewFact a = NewFact a

-- | The result of joining OldFact and NewFact.
data JoinedFact a
    = Changed !a     -- ^ Result is different than OldFact.
    | NotChanged !a  -- ^ Result is the same as OldFact.

getJoined :: JoinedFact a -> a
getJoined (Changed a) = a
getJoined (NotChanged a) = a

changedIf :: Bool -> a -> JoinedFact a
changedIf True = Changed
changedIf False = NotChanged

type JoinFun a = OldFact a -> NewFact a -> JoinedFact a

data DataflowLattice a = DataflowLattice
    { fact_bot :: a
    , fact_join :: JoinFun a
    }

data Direction = Fwd | Bwd

type TransferFun f = CmmBlock -> FactBase f -> FactBase f

-- | `TransferFun` abstracted over `n` (the node type)
type TransferFun' (n :: Extensibility -> Extensibility -> Type) f =
    Block n C C -> FactBase f -> FactBase f


-- | Function for rewriting and analysis combined. To be used with
-- @rewriteCmm@.
--
-- Currently set to work with @UniqDSM@ monad, but we could probably abstract
-- that away (if we do that, we might want to specialize the fixpoint algorithms
-- to the particular monads through SPECIALIZE).
type RewriteFun f = CmmBlock -> FactBase f -> UniqDSM (CmmBlock, FactBase f)

-- | `RewriteFun` abstracted over `n` (the node type)
type RewriteFun' (n :: Extensibility -> Extensibility -> Type) f =
    Block n C C -> FactBase f -> UniqDSM (Block n C C, FactBase f)

analyzeCmmBwd, analyzeCmmFwd
    :: (NonLocal node)
    => DataflowLattice f
    -> TransferFun' node f
    -> GenCmmGraph node
    -> FactBase f
    -> FactBase f
analyzeCmmBwd = analyzeCmm Bwd
analyzeCmmFwd = analyzeCmm Fwd

analyzeCmm
    :: (NonLocal node)
    => Direction
    -> DataflowLattice f
    -> TransferFun' node f
    -> GenCmmGraph node
    -> FactBase f
    -> FactBase f
analyzeCmm dir lattice transfer cmmGraph initFact =
    {-# SCC analyzeCmm #-}
    let entry = g_entry cmmGraph
        hooplGraph = g_graph cmmGraph
        blockMap =
            case hooplGraph of
                GMany NothingO bm NothingO -> bm
    in fixpointAnalysis dir lattice transfer entry blockMap initFact

-- Fixpoint algorithm.
fixpointAnalysis
    :: forall f node.
       (NonLocal node)
    => Direction
    -> DataflowLattice f
    -> TransferFun' node f
    -> Label
    -> LabelMap (Block node C C)
    -> FactBase f
    -> FactBase f
fixpointAnalysis direction lattice do_block entry blockmap = loop start
  where
    -- Sorting the blocks helps to minimize the number of times we need to
    -- process blocks. For instance, for forward analysis we want to look at
    -- blocks in reverse postorder. Also, see comments for sortBlocks.
    blocks     = sortBlocks direction entry blockmap
    num_blocks = length blocks
    block_arr  = {-# SCC "block_arr" #-} listArray (0, num_blocks - 1) blocks
    start      = {-# SCC "start" #-} IntSet.fromDistinctAscList
      [0 .. num_blocks - 1]
    dep_blocks = {-# SCC "dep_blocks" #-} mkDepBlocks direction blocks
    join       = fact_join lattice

    loop
        :: IntHeap     -- Worklist, i.e., blocks to process
        -> FactBase f  -- Current result (increases monotonically)
        -> FactBase f
    loop todo !fbase1 | Just (index, todo1) <- IntSet.minView todo =
        let block = block_arr ! index
            out_facts = {-# SCC "do_block" #-} do_block block fbase1
            -- For each of the outgoing edges, we join it with the current
            -- information in fbase1 and (if something changed) we update it
            -- and add the affected blocks to the worklist.
            (todo2, fbase2) = {-# SCC "mapFoldWithKey" #-}
                Det.mapFoldlWithKey
                    (updateFact join dep_blocks) (todo1, fbase1) out_facts
        in loop todo2 fbase2
    loop _ !fbase1 = fbase1

rewriteCmmBwd
    :: (NonLocal node)
    => DataflowLattice f
    -> RewriteFun' node f
    -> GenCmmGraph node
    -> FactBase f
    -> UniqDSM (GenCmmGraph node, FactBase f)
rewriteCmmBwd = rewriteCmm Bwd

rewriteCmm
    :: (NonLocal node)
    => Direction
    -> DataflowLattice f
    -> RewriteFun' node f
    -> GenCmmGraph node
    -> FactBase f
    -> UniqDSM (GenCmmGraph node, FactBase f)
rewriteCmm dir lattice rwFun cmmGraph initFact = {-# SCC rewriteCmm #-} do
    let entry = g_entry cmmGraph
        hooplGraph = g_graph cmmGraph
        blockMap1 =
            case hooplGraph of
                GMany NothingO bm NothingO -> bm
    (blockMap2, facts) <-
        fixpointRewrite dir lattice rwFun entry blockMap1 initFact
    return (cmmGraph {g_graph = GMany NothingO blockMap2 NothingO}, facts)

fixpointRewrite
    :: forall f node.
       NonLocal node
    => Direction
    -> DataflowLattice f
    -> RewriteFun' node f
    -> Label
    -> LabelMap (Block node C C)
    -> FactBase f
    -> UniqDSM (LabelMap (Block node C C), FactBase f)
fixpointRewrite dir lattice do_block entry blockmap = loop start blockmap
  where
    -- Sorting the blocks helps to minimize the number of times we need to
    -- process blocks. For instance, for forward analysis we want to look at
    -- blocks in reverse postorder. Also, see comments for sortBlocks.
    blocks     = sortBlocks dir entry blockmap
    num_blocks = length blocks
    block_arr  = {-# SCC "block_arr_rewrite" #-}
                 listArray (0, num_blocks - 1) blocks
    start      = {-# SCC "start_rewrite" #-}
                 IntSet.fromDistinctAscList [0 .. num_blocks - 1]
    dep_blocks = {-# SCC "dep_blocks_rewrite" #-} mkDepBlocks dir blocks
    join       = fact_join lattice

    loop
        :: IntHeap                    -- Worklist, i.e., blocks to process
        -> LabelMap (Block node C C)  -- Rewritten blocks.
        -> FactBase f                 -- Current facts.
        -> UniqDSM (LabelMap (Block node C C), FactBase f)
    loop todo !blocks1 !fbase1
      | Just (index, todo1) <- IntSet.minView todo = do
        -- Note that we use the *original* block here. This is important.
        -- We're optimistically rewriting blocks even before reaching the fixed
        -- point, which means that the rewrite might be incorrect. So if the
        -- facts change, we need to rewrite the original block again (taking
        -- into account the new facts).
        let block = block_arr ! index
        (new_block, out_facts) <- {-# SCC "do_block_rewrite" #-}
            do_block block fbase1
        let blocks2 = Det.mapInsert (entryLabel new_block) new_block blocks1
            (todo2, fbase2) = {-# SCC "mapFoldWithKey_rewrite" #-}
                Det.mapFoldlWithKey
                    (updateFact join dep_blocks) (todo1, fbase1) out_facts
        loop todo2 blocks2 fbase2
    loop _ !blocks1 !fbase1 = return (blocks1, fbase1)


{-
Note [Unreachable blocks]
~~~~~~~~~~~~~~~~~~~~~~~~~
A block that is not in the domain of tfb_fbase is "currently unreachable".
A currently-unreachable block is not even analyzed.  Reason: consider
constant prop and this graph, with entry point L1:
  L1: x:=3; goto L4
  L2: x:=4; goto L4
  L4: if x>3 goto L2 else goto L5
Here L2 is actually unreachable, but if we process it with bottom input fact,
we'll propagate (x=4) to L4, and nuke the otherwise-good rewriting of L4.

* If a currently-unreachable block is not analyzed, then its rewritten
  graph will not be accumulated in tfb_rg.  And that is good:
  unreachable blocks simply do not appear in the output.

* Note that clients must be careful to provide a fact (even if bottom)
  for each entry point. Otherwise useful blocks may be garbage collected.

* Note that updateFact must set the change-flag if a label goes from
  not-in-fbase to in-fbase, even if its fact is bottom.  In effect the
  real fact lattice is
       UNR
       bottom
       the points above bottom

* Even if the fact is going from UNR to bottom, we still call the
  client's fact_join function because it might give the client
  some useful debugging information.

* All of this only applies for *forward* ixpoints.  For the backward
  case we must treat every block as reachable; it might finish with a
  'return', and therefore have no successors, for example.
-}


-----------------------------------------------------------------------------
--  Pieces that are shared by fixpoint and fixpoint_anal
-----------------------------------------------------------------------------

-- | Sort the blocks into the right order for analysis. This means reverse
-- postorder for a forward analysis. For the backward one, we simply reverse
-- that (see Note [Backward vs forward analysis]).
sortBlocks
    :: NonLocal n
    => Direction -> Label -> LabelMap (Block n C C) -> [Block n C C]
sortBlocks direction entry blockmap =
    case direction of
        Fwd -> fwd
        Bwd -> reverse fwd
  where
    fwd = revPostorderFrom blockmap entry

-- Note [Backward vs forward analysis]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The forward and backward cases are not dual.  In the forward case, the entry
-- points are known, and one simply traverses the body blocks from those points.
-- In the backward case, something is known about the exit points, but a
-- backward analysis must also include reachable blocks that don't reach the
-- exit, as in a procedure that loops forever and has side effects.)
-- For instance, let E be the entry and X the exit blocks (arrows indicate
-- control flow)
--   E -> X
--   E -> B
--   B -> C
--   C -> B
-- We do need to include B and C even though they're unreachable in the
-- *reverse* graph (that we could use for backward analysis):
--   E <- X
--   E <- B
--   B <- C
--   C <- B
-- So when sorting the blocks for the backward analysis, we simply take the
-- reverse of what is used for the forward one.


-- | Construct a mapping from a @Label@ to the block indexes that should be
-- re-analyzed if the facts at that @Label@ change.
--
-- Note that we're considering here the entry point of the block, so if the
-- facts change at the entry:
-- * for a backward analysis we need to re-analyze all the predecessors, but
-- * for a forward analysis, we only need to re-analyze the current block
--   (and that will in turn propagate facts into its successors).
mkDepBlocks :: NonLocal node => Direction -> [Block node C C] -> LabelMap IntSet
mkDepBlocks Fwd blocks = go blocks 0 Det.mapEmpty
  where
    go []     !_ !dep_map = dep_map
    go (b:bs) !n !dep_map =
        go bs (n + 1) $ Det.mapInsert (entryLabel b) (IntSet.singleton n) dep_map
mkDepBlocks Bwd blocks = go blocks 0 Det.mapEmpty
  where
    go []     !_ !dep_map = dep_map
    go (b:bs) !n !dep_map =
        let insert m l = Det.mapInsertWith IntSet.union l (IntSet.singleton n) m
        in go bs (n + 1) $ foldl' insert dep_map (successors b)

-- | After some new facts have been generated by analysing a block, we
-- fold this function over them to generate (a) a list of block
-- indices to (re-)analyse, and (b) the new FactBase.
updateFact
    :: JoinFun f
    -> LabelMap IntSet
    -> (IntHeap, FactBase f)
    -> Label
    -> f -- out fact
    -> (IntHeap, FactBase f)
updateFact fact_join dep_blocks (todo, fbase) lbl new_fact
  = case lookupFact lbl fbase of
      Nothing ->
          -- See Note [No old fact]
          let !z = mapInsert lbl new_fact fbase in (changed, z)
      Just old_fact ->
          case fact_join (OldFact old_fact) (NewFact new_fact) of
              (NotChanged _) -> (todo, fbase)
              (Changed f) -> let !z = mapInsert lbl f fbase in (changed, z)
  where
    changed = todo `IntSet.union`
              Det.mapFindWithDefault IntSet.empty lbl dep_blocks

{-
Note [No old fact]
~~~~~~~~~~~~~~~~~~
We know that the new_fact is >= _|_, so we don't need to join.  However,
if the new fact is also _|_, and we have already analysed its block,
we don't need to record a change.  So there's a tradeoff here.  It turns
out that always recording a change is faster.
-}

----------------------------------------------------------------
--       Utilities
----------------------------------------------------------------

-- Fact lookup: the fact `orelse` bottom
getFact  :: DataflowLattice f -> Label -> FactBase f -> f
getFact lat l fb = case lookupFact l fb of Just  f -> f
                                           Nothing -> fact_bot lat

-- | Returns the result of joining the facts from all the successors of the
-- provided node or block.
joinOutFacts :: (NonLocal n) => DataflowLattice f -> n e C -> FactBase f -> f
joinOutFacts lattice nonLocal fact_base = foldl' join (fact_bot lattice) facts
  where
    join new old = getJoined $ fact_join lattice (OldFact old) (NewFact new)
    facts =
        [ fromJust fact
        | s <- successors nonLocal
        , let fact = lookupFact s fact_base
        , isJust fact
        ]

joinFacts :: DataflowLattice f -> [f] -> f
joinFacts lattice facts  = foldl' join (fact_bot lattice) facts
  where
    join new old = getJoined $ fact_join lattice (OldFact old) (NewFact new)

-- | Returns the joined facts for each label.
mkFactBase :: DataflowLattice f -> [(Label, f)] -> FactBase f
mkFactBase lattice = foldl' add mapEmpty
  where
    join = fact_join lattice

    add result (l, f1) =
        let !newFact =
                case mapLookup l result of
                    Nothing -> f1
                    Just f2 -> getJoined $ join (OldFact f1) (NewFact f2)
        in mapInsert l newFact result

-- | Folds backward over all nodes of an open-open block.
-- Strict in the accumulator.
foldNodesBwdOO :: (node O O -> f -> f) -> Block node O O -> f -> f
foldNodesBwdOO funOO = go
  where
    go (BCat b1 b2) f = go b1 $! go b2 f
    go (BSnoc h n) f = go h $! funOO n f
    go (BCons n t) f = funOO n $! go t f
    go (BMiddle n) f = funOO n f
    go BNil f = f
{-# INLINABLE foldNodesBwdOO #-}

-- | Folds backward over all the nodes of an open-open block and allows
-- rewriting them. The accumulator is both the block of nodes and @f@ (usually
-- dataflow facts).
-- Strict in both accumulated parts.
foldRewriteNodesBwdOO
    :: forall f node.
       (node O O -> f -> UniqDSM (Block node O O, f))
    -> Block node O O
    -> f
    -> UniqDSM (Block node O O, f)
foldRewriteNodesBwdOO rewriteOO initBlock initFacts = go initBlock initFacts
  where
    go (BCons node1 block1) !fact1 = (rewriteOO node1 `comp` go block1) fact1
    go (BSnoc block1 node1) !fact1 = (go block1 `comp` rewriteOO node1) fact1
    go (BCat blockA1 blockB1) !fact1 = (go blockA1 `comp` go blockB1) fact1
    go (BMiddle node) !fact1 = rewriteOO node fact1
    go BNil !fact = return (BNil, fact)

    comp rew1 rew2 = \f1 -> do
        (b, f2) <- rew2 f1
        (a, !f3) <- rew1 f2
        let !c = joinBlocksOO a b
        return (c, f3)
    {-# INLINE comp #-}
{-# INLINABLE foldRewriteNodesBwdOO #-}

joinBlocksOO :: Block n O O -> Block n O O -> Block n O O
joinBlocksOO BNil b = b
joinBlocksOO b BNil = b
joinBlocksOO (BMiddle n) b = blockCons n b
joinBlocksOO b (BMiddle n) = blockSnoc b n
joinBlocksOO b1 b2 = BCat b1 b2

type IntHeap = IntSet
