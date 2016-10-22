{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fprof-auto-top #-}

--
-- Copyright (c) 2010, João Dias, Simon Marlow, Simon Peyton Jones,
-- and Norman Ramsey
--
-- Modifications copyright (c) The University of Glasgow 2012
--
-- This module is a specialised and optimised version of
-- Compiler.Hoopl.Dataflow in the hoopl package.  In particular it is
-- specialised to the UniqSM monad.
--

module Hoopl.Dataflow
  ( DataflowLattice(..), OldFact(..), NewFact(..), Fact, mkFactBase
  , ChangeFlag(..)
  , FwdPass(..), FwdTransfer, mkFTransfer, mkFTransfer3, getFTransfer3

  , BwdPass(..), BwdTransfer, mkBTransfer, mkBTransfer3, getBTransfer3

  , noBwdRewrite, noFwdRewrite

  , analyzeFwd, analyzeFwdBlocks, analyzeBwd
  )
where

import UniqSupply

import Data.Array

import Compiler.Hoopl hiding (noFwdRewrite, noBwdRewrite)

noRewrite :: a -> b -> UniqSM (Maybe c)
noRewrite _ _ = return Nothing

noFwdRewrite :: FwdRewrite UniqSM n f
noFwdRewrite = FwdRewrite3 (noRewrite, noRewrite, noRewrite)

noBwdRewrite :: BwdRewrite UniqSM n f
noBwdRewrite = BwdRewrite3 (noRewrite, noRewrite, noRewrite)

----------------------------------------------------------------
--       Forward Analysis only
----------------------------------------------------------------

-- | if the graph being analyzed is open at the entry, there must
--   be no other entry point, or all goes horribly wrong...
analyzeFwd
   :: forall n f e .  NonLocal n =>
      FwdPass UniqSM n f
   -> MaybeC e [Label]
   -> Graph n e C -> Fact e f
   -> FactBase f
analyzeFwd FwdPass { fp_lattice = lattice,
                     fp_transfer = FwdTransfer3 (ftr, mtr, ltr) }
  entries g in_fact = graph g in_fact
  where
    graph :: Graph n e C -> Fact e f -> FactBase f
    graph (GMany entry blockmap NothingO)
      = case (entries, entry) of
         (NothingC, JustO entry)   -> block entry `cat` body (successors entry)
         (JustC entries, NothingO) -> body entries
     where
       body  :: [Label] -> Fact C f -> Fact C f
       body entries f
         = fixpointAnal Fwd lattice do_block entries blockmap f
         where
           do_block :: forall x . Block n C x -> FactBase f -> Fact x f
           do_block b fb = block b entryFact
             where entryFact = getFact lattice (entryLabel b) fb

    -- NB. eta-expand block, GHC can't do this by itself.  See #5809.
    block :: forall e x . Block n e x -> f -> Fact x f
    block BNil            f = f
    block (BlockCO n b)   f = (ftr n `cat`  block b) f
    block (BlockCC l b n) f = (ftr l `cat` (block b `cat` ltr n)) f
    block (BlockOC   b n) f =              (block b `cat` ltr n) f

    block (BMiddle n)     f = mtr n f
    block (BCat b1 b2)    f = (block b1 `cat` block b2) f
    block (BSnoc h n)     f = (block h  `cat` mtr n) f
    block (BCons n t)     f = (mtr  n   `cat` block t) f

    {-# INLINE cat #-}
    cat :: forall f1 f2 f3 . (f1 -> f2) -> (f2 -> f3) -> (f1 -> f3)
    cat ft1 ft2 = \f -> ft2 $! ft1 f

-- | if the graph being analyzed is open at the entry, there must
--   be no other entry point, or all goes horribly wrong...
analyzeFwdBlocks
   :: forall n f e .  NonLocal n =>
      FwdPass UniqSM n f
   -> MaybeC e [Label]
   -> Graph n e C -> Fact e f
   -> FactBase f
analyzeFwdBlocks FwdPass { fp_lattice = lattice,
                           fp_transfer = FwdTransfer3 (ftr, _, ltr) }
  entries g in_fact = graph g in_fact
  where
    graph :: Graph n e C -> Fact e f -> FactBase f
    graph (GMany entry blockmap NothingO)
      = case (entries, entry) of
         (NothingC, JustO entry)   -> block entry `cat` body (successors entry)
         (JustC entries, NothingO) -> body entries
     where
       body  :: [Label] -> Fact C f -> Fact C f
       body entries f
         = fixpointAnal Fwd lattice do_block entries blockmap f
         where
           do_block :: forall x . Block n C x -> FactBase f -> Fact x f
           do_block b fb = block b entryFact
             where entryFact = getFact lattice (entryLabel b) fb

    -- NB. eta-expand block, GHC can't do this by itself.  See #5809.
    block :: forall e x . Block n e x -> f -> Fact x f
    block BNil            f = f
    block (BlockCO n _)   f = ftr n f
    block (BlockCC l _ n) f = (ftr l `cat` ltr n) f
    block (BlockOC   _ n) f = ltr n f
    block _               _ = error "analyzeFwdBlocks"

    {-# INLINE cat #-}
    cat :: forall f1 f2 f3 . (f1 -> f2) -> (f2 -> f3) -> (f1 -> f3)
    cat ft1 ft2 = \f -> ft2 $! ft1 f

----------------------------------------------------------------
--       Backward Analysis only
----------------------------------------------------------------

-- | if the graph being analyzed is open at the entry, there must
--   be no other entry point, or all goes horribly wrong...
analyzeBwd
   :: forall n f e .  NonLocal n =>
      BwdPass UniqSM n f
   -> MaybeC e [Label]
   -> Graph n e C -> Fact C f
   -> FactBase f
analyzeBwd BwdPass { bp_lattice = lattice,
                     bp_transfer = BwdTransfer3 (ftr, mtr, ltr) }
   entries g in_fact = graph g in_fact
  where
    graph :: Graph n e C -> Fact C f -> FactBase f
    graph (GMany entry blockmap NothingO)
      = case (entries, entry) of
         (NothingC, JustO entry)   -> body (successors entry)
         (JustC entries, NothingO) -> body entries
     where
       body  :: [Label] -> Fact C f -> Fact C f
       body entries f
         = fixpointAnal Bwd lattice do_block entries blockmap f
         where
           do_block :: forall x . Block n C x -> Fact x f -> FactBase f
           do_block b fb = mapSingleton (entryLabel b) (block b fb)

    -- NB. eta-expand block, GHC can't do this by itself.  See #5809.
    block :: forall e x . Block n e x -> Fact x f -> f
    block BNil            f = f
    block (BlockCO n b)   f = (ftr n `cat`  block b) f
    block (BlockCC l b n) f = ((ftr l `cat` block b) `cat` ltr n) f
    block (BlockOC   b n) f =              (block b `cat` ltr n) f

    block (BMiddle n)     f = mtr n f
    block (BCat b1 b2)    f = (block b1 `cat` block b2) f
    block (BSnoc h n)     f = (block h  `cat` mtr n) f
    block (BCons n t)     f = (mtr  n   `cat` block t) f

    {-# INLINE cat #-}
    cat :: forall f1 f2 f3 . (f2 -> f3) -> (f1 -> f2) -> (f1 -> f3)
    cat ft1 ft2 = \f -> ft1 $! ft2 f


-----------------------------------------------------------------------------
--      fixpoint
-----------------------------------------------------------------------------

data Direction = Fwd | Bwd

-- | fixpointing for analysis-only
--
fixpointAnal :: forall n f. NonLocal n
 => Direction
 -> DataflowLattice f
 -> (Block n C C -> Fact C f -> Fact C f)
 -> [Label]
 -> LabelMap (Block n C C)
 -> Fact C f -> FactBase f

fixpointAnal direction DataflowLattice{ fact_bot = _, fact_join = join }
              do_block entries blockmap init_fbase
  = loop start init_fbase
  where
    blocks     = sortBlocks direction entries blockmap
    n          = length blocks
    block_arr  = {-# SCC "block_arr" #-} listArray (0,n-1) blocks
    start      = {-# SCC "start" #-} [0..n-1]
    dep_blocks = {-# SCC "dep_blocks" #-} mkDepBlocks direction blocks

    loop
       :: IntHeap      -- blocks still to analyse
       -> FactBase f  -- current factbase (increases monotonically)
       -> FactBase f

    loop []        fbase = fbase
    loop (ix:todo) fbase =
           let
               blk = block_arr ! ix

               out_facts = {-# SCC "do_block" #-} do_block blk fbase

               !(todo', fbase') = {-# SCC "mapFoldWithKey" #-}
                     mapFoldWithKey (updateFact join dep_blocks)
                                    (todo,fbase) out_facts
           in
           -- trace ("analysing: " ++ show (entryLabel blk)) $
           -- trace ("fbase': " ++ show (mapKeys fbase')) $ return ()
           -- trace ("changed: " ++ show changed) $ return ()
           -- trace ("to analyse: " ++ show to_analyse) $ return ()

           loop todo' fbase'


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
--
-- Note: We're using Hoopl's confusingly named `postorder_dfs_from` but AFAICS
-- it returns the *reverse* postorder of the blocks (it visits blocks in the
-- postorder and uses (:) to collect them, which gives the reverse of the
-- visitation order).
sortBlocks
    :: NonLocal n
    => Direction -> [Label] -> LabelMap (Block n C C) -> [Block n C C]
sortBlocks direction entries blockmap =
    case direction of
        Fwd -> fwd
        Bwd -> reverse fwd
  where
    fwd = postorder_dfs_from blockmap entries

-- Note [Backward vs forward analysis]
--
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


-- | construct a mapping from L -> block indices.  If the fact for L
-- changes, re-analyse the given blocks.
mkDepBlocks :: NonLocal n => Direction -> [Block n C C] -> LabelMap [Int]
mkDepBlocks Fwd blocks = go blocks 0 mapEmpty
  where go []     !_  m = m
        go (b:bs) !n m = go bs (n+1) $! mapInsert (entryLabel b) [n] m
mkDepBlocks Bwd blocks = go blocks 0 mapEmpty
  where go []     !_ m = m
        go (b:bs) !n m = go bs (n+1) $! go' (successors b) m
            where go' [] m = m
                  go' (l:ls) m = go' ls (mapInsertWith (++) l [n] m)


-- | After some new facts have been generated by analysing a block, we
-- fold this function over them to generate (a) a list of block
-- indices to (re-)analyse, and (b) the new FactBase.
--
updateFact :: JoinFun f -> LabelMap [Int]
           -> Label -> f       -- out fact
           -> (IntHeap, FactBase f)
           -> (IntHeap, FactBase f)

updateFact fact_join dep_blocks lbl new_fact (todo, fbase)
  = case lookupFact lbl fbase of
      Nothing       -> let !z = mapInsert lbl new_fact fbase in (changed, z)
                           -- Note [no old fact]
      Just old_fact ->
        case fact_join lbl (OldFact old_fact) (NewFact new_fact) of
          (NoChange, _) -> (todo, fbase)
          (_,        f) -> let !z = mapInsert lbl f fbase in (changed, z)
  where
     changed = foldr insertIntHeap todo $
                 mapFindWithDefault [] lbl dep_blocks

{-
Note [no old fact]

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

-- -----------------------------------------------------------------------------
-- a Heap of Int

-- We should really use a proper Heap here, but my attempts to make
-- one have not succeeded in beating the simple ordered list.  Another
-- alternative is IntSet (using deleteFindMin), but that was also
-- slower than the ordered list in my experiments --SDM 25/1/2012

type IntHeap = [Int] -- ordered

insertIntHeap :: Int -> [Int] -> [Int]
insertIntHeap x [] = [x]
insertIntHeap x (y:ys)
  | x < y     = x : y : ys
  | x == y    = x : ys
  | otherwise = y : insertIntHeap x ys
