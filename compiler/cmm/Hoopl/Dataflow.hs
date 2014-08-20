{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
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
  -- * Respecting Fuel

  -- $fuel
  , FwdRewrite,  mkFRewrite,  mkFRewrite3,  getFRewrite3, noFwdRewrite
  , wrapFR, wrapFR2
  , BwdPass(..), BwdTransfer, mkBTransfer, mkBTransfer3, getBTransfer3
  , wrapBR, wrapBR2
  , BwdRewrite,  mkBRewrite,  mkBRewrite3,  getBRewrite3, noBwdRewrite
  , analyzeAndRewriteFwd,  analyzeAndRewriteBwd
  , analyzeFwd, analyzeFwdBlocks, analyzeBwd
  )
where

import UniqSupply

import Data.Maybe
import Data.Array

import Compiler.Hoopl hiding
   ( mkBRewrite3, mkFRewrite3, noFwdRewrite, noBwdRewrite
   , analyzeAndRewriteBwd, analyzeAndRewriteFwd
   )
import Compiler.Hoopl.Internals
  ( wrapFR, wrapFR2
  , wrapBR, wrapBR2
  , splice
  )


-- -----------------------------------------------------------------------------

noRewrite :: a -> b -> UniqSM (Maybe c)
noRewrite _ _ = return Nothing

noFwdRewrite :: FwdRewrite UniqSM n f
noFwdRewrite = FwdRewrite3 (noRewrite, noRewrite, noRewrite)

-- | Functions passed to 'mkFRewrite3' should not be aware of the fuel supply.
-- The result returned by 'mkFRewrite3' respects fuel.
mkFRewrite3 :: forall n f.
               (n C O -> f -> UniqSM (Maybe (Graph n C O)))
            -> (n O O -> f -> UniqSM (Maybe (Graph n O O)))
            -> (n O C -> f -> UniqSM (Maybe (Graph n O C)))
            -> FwdRewrite UniqSM n f
mkFRewrite3 f m l = FwdRewrite3 (lift f, lift m, lift l)
  where lift :: forall t t1 a. (t -> t1 -> UniqSM (Maybe a))
                             -> t -> t1 -> UniqSM (Maybe (a, FwdRewrite UniqSM n f))
        {-# INLINE lift #-}
        lift rw node fact = do
             a <- rw node fact
             case a of
               Nothing -> return Nothing
               Just a  -> return (Just (a,noFwdRewrite))

noBwdRewrite :: BwdRewrite UniqSM n f
noBwdRewrite = BwdRewrite3 (noRewrite, noRewrite, noRewrite)

mkBRewrite3 :: forall n f.
               (n C O -> f          -> UniqSM (Maybe (Graph n C O)))
            -> (n O O -> f          -> UniqSM (Maybe (Graph n O O)))
            -> (n O C -> FactBase f -> UniqSM (Maybe (Graph n O C)))
            -> BwdRewrite UniqSM n f
mkBRewrite3 f m l = BwdRewrite3 (lift f, lift m, lift l)
  where lift :: forall t t1 a. (t -> t1 -> UniqSM (Maybe a))
                             -> t -> t1 -> UniqSM (Maybe (a, BwdRewrite UniqSM n f))
        {-# INLINE lift #-}
        lift rw node fact = do
             a <- rw node fact
             case a of
               Nothing -> return Nothing
               Just a  -> return (Just (a,noBwdRewrite))

-----------------------------------------------------------------------------
--              Analyze and rewrite forward: the interface
-----------------------------------------------------------------------------

-- | if the graph being analyzed is open at the entry, there must
--   be no other entry point, or all goes horribly wrong...
analyzeAndRewriteFwd
   :: forall n f e x .  NonLocal n =>
      FwdPass UniqSM n f
   -> MaybeC e [Label]
   -> Graph n  e x -> Fact e f
   -> UniqSM (Graph n e x, FactBase f, MaybeO x f)
analyzeAndRewriteFwd pass entries g f =
  do (rg, fout) <- arfGraph pass (fmap targetLabels entries) g f
     let (g', fb) = normalizeGraph rg
     return (g', fb, distinguishedExitFact g' fout)

distinguishedExitFact :: forall n e x f . Graph n e x -> Fact x f -> MaybeO x f
distinguishedExitFact g f = maybe g
    where maybe :: Graph n e x -> MaybeO x f
          maybe GNil       = JustO f
          maybe (GUnit {}) = JustO f
          maybe (GMany _ _ x) = case x of NothingO -> NothingO
                                          JustO _  -> JustO f

----------------------------------------------------------------
--       Forward Implementation
----------------------------------------------------------------

type Entries e = MaybeC e [Label]

arfGraph :: forall n f e x .  NonLocal n =>
            FwdPass UniqSM n f ->
            Entries e -> Graph n e x -> Fact e f -> UniqSM (DG f n e x, Fact x f)
arfGraph pass@FwdPass { fp_lattice = lattice,
                        fp_transfer = transfer,
                        fp_rewrite  = rewrite } entries g in_fact = graph g in_fact
  where
    {- nested type synonyms would be so lovely here 
    type ARF  thing = forall e x . thing e x -> f        -> m (DG f n e x, Fact x f)
    type ARFX thing = forall e x . thing e x -> Fact e f -> m (DG f n e x, Fact x f)
    -}
    graph ::              Graph n e x -> Fact e f -> UniqSM (DG f n e x, Fact x f)
    block :: forall e x .
             Block n e x -> f -> UniqSM (DG f n e x, Fact x f)

    body  :: [Label] -> LabelMap (Block n C C)
          -> Fact C f -> UniqSM (DG f n C C, Fact C f)
                    -- Outgoing factbase is restricted to Labels *not* in
                    -- in the Body; the facts for Labels *in*
                    -- the Body are in the 'DG f n C C'

    cat :: forall e a x f1 f2 f3.
           (f1 -> UniqSM (DG f n e a, f2))
        -> (f2 -> UniqSM (DG f n a x, f3))
        -> (f1 -> UniqSM (DG f n e x, f3))

    graph GNil            f = return (dgnil, f)
    graph (GUnit blk)     f = block blk f
    graph (GMany e bdy x) f = ((e `ebcat` bdy) `cat` exit x) f
     where
      ebcat :: MaybeO e (Block n O C) -> Body n -> Fact e f -> UniqSM (DG f n e C, Fact C f)
      exit  :: MaybeO x (Block n C O)           -> Fact C f -> UniqSM (DG f n C x, Fact x f)
      exit (JustO blk) f = arfx block blk f
      exit NothingO    f = return (dgnilC, f)
      ebcat entry bdy f = c entries entry f
       where c :: MaybeC e [Label] -> MaybeO e (Block n O C)
                -> Fact e f -> UniqSM (DG f n e C, Fact C f)
             c NothingC (JustO entry)   f = (block entry `cat` body (successors entry) bdy) f
             c (JustC entries) NothingO f = body entries bdy f
             c _ _ _ = error "bogus GADT pattern match failure"

    -- Lift from nodes to blocks
    block BNil            f = return (dgnil, f)
    block (BlockCO n b)   f = (node n `cat` block b) f
    block (BlockCC l b n) f = (node l `cat` block b `cat` node n) f
    block (BlockOC   b n) f =              (block b `cat` node n) f

    block (BMiddle n)     f = node n f
    block (BCat b1 b2)    f = (block b1 `cat` block b2) f
    block (BSnoc h n)     f = (block h  `cat` node n) f
    block (BCons n t)     f = (node  n  `cat` block t) f

    {-# INLINE node #-}
    node :: forall e x . (ShapeLifter e x)
         => n e x -> f -> UniqSM (DG f n e x, Fact x f)
    node n f
     = do { grw <- frewrite rewrite n f
          ; case grw of
              Nothing -> return ( singletonDG f n
                                , ftransfer transfer n f )
              Just (g, rw) ->
                  let pass' = pass { fp_rewrite = rw }
                      f'    = fwdEntryFact n f
                  in  arfGraph pass' (fwdEntryLabel n) g f' }

    -- | Compose fact transformers and concatenate the resulting
    -- rewritten graphs.
    {-# INLINE cat #-} 
    cat ft1 ft2 f = do { (g1,f1) <- ft1 f
                       ; (g2,f2) <- ft2 f1
                       ; let !g = g1 `dgSplice` g2
                       ; return (g, f2) }

    arfx :: forall x .
            (Block n C x ->        f -> UniqSM (DG f n C x, Fact x f))
         -> (Block n C x -> Fact C f -> UniqSM (DG f n C x, Fact x f))
    arfx arf thing fb = 
      arf thing $ fromJust $ lookupFact (entryLabel thing) $ joinInFacts lattice fb
     -- joinInFacts adds debugging information


                    -- Outgoing factbase is restricted to Labels *not* in
                    -- in the Body; the facts for Labels *in*
                    -- the Body are in the 'DG f n C C'
    body entries blockmap init_fbase
      = fixpoint Fwd lattice do_block entries blockmap init_fbase
      where
        lattice = fp_lattice pass
        do_block :: forall x . Block n C x -> FactBase f
                 -> UniqSM (DG f n C x, Fact x f)
        do_block b fb = block b entryFact
          where entryFact = getFact lattice (entryLabel b) fb


-- Join all the incoming facts with bottom.
-- We know the results _shouldn't change_, but the transfer
-- functions might, for example, generate some debugging traces.
joinInFacts :: DataflowLattice f -> FactBase f -> FactBase f
joinInFacts (lattice @ DataflowLattice {fact_bot = bot, fact_join = fj}) fb =
  mkFactBase lattice $ map botJoin $ mapToList fb
    where botJoin (l, f) = (l, snd $ fj l (OldFact bot) (NewFact f))

forwardBlockList :: (NonLocal n)
                 => [Label] -> Body n -> [Block n C C]
-- This produces a list of blocks in order suitable for forward analysis,
-- along with the list of Labels it may depend on for facts.
forwardBlockList entries blks = postorder_dfs_from blks entries

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
         _ -> error "bogus GADT pattern match failure"
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
         _ -> error "bogus GADT pattern match failure"
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
         _ -> error "bogus GADT pattern match failure"
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
--              Backward analysis and rewriting: the interface
-----------------------------------------------------------------------------


-- | if the graph being analyzed is open at the exit, I don't
--   quite understand the implications of possible other exits
analyzeAndRewriteBwd
   :: NonLocal n
   => BwdPass UniqSM n f
   -> MaybeC e [Label] -> Graph n e x -> Fact x f
   -> UniqSM (Graph n e x, FactBase f, MaybeO e f)
analyzeAndRewriteBwd pass entries g f =
  do (rg, fout) <- arbGraph pass (fmap targetLabels entries) g f
     let (g', fb) = normalizeGraph rg
     return (g', fb, distinguishedEntryFact g' fout)

distinguishedEntryFact :: forall n e x f . Graph n e x -> Fact e f -> MaybeO e f
distinguishedEntryFact g f = maybe g
    where maybe :: Graph n e x -> MaybeO e f
          maybe GNil       = JustO f
          maybe (GUnit {}) = JustO f
          maybe (GMany e _ _) = case e of NothingO -> NothingO
                                          JustO _  -> JustO f


-----------------------------------------------------------------------------
--              Backward implementation
-----------------------------------------------------------------------------

arbGraph :: forall n f e x .
            NonLocal n =>
            BwdPass UniqSM n f ->
            Entries e -> Graph n e x -> Fact x f -> UniqSM (DG f n e x, Fact e f)
arbGraph pass@BwdPass { bp_lattice  = lattice,
                        bp_transfer = transfer,
                        bp_rewrite  = rewrite } entries g in_fact = graph g in_fact
  where
    {- nested type synonyms would be so lovely here 
    type ARB  thing = forall e x . thing e x -> Fact x f -> m (DG f n e x, f)
    type ARBX thing = forall e x . thing e x -> Fact x f -> m (DG f n e x, Fact e f)
    -}
    graph ::              Graph n e x -> Fact x f -> UniqSM (DG f n e x, Fact e f)
    block :: forall e x . Block n e x -> Fact x f -> UniqSM (DG f n e x, f)
    body  :: [Label] -> Body n -> Fact C f -> UniqSM (DG f n C C, Fact C f)
    node  :: forall e x . (ShapeLifter e x) 
             => n e x       -> Fact x f -> UniqSM (DG f n e x, f)
    cat :: forall e a x info info' info''.
           (info' -> UniqSM (DG f n e a, info''))
        -> (info  -> UniqSM (DG f n a x, info'))
        -> (info  -> UniqSM (DG f n e x, info''))

    graph GNil            f = return (dgnil, f)
    graph (GUnit blk)     f = block blk f
    graph (GMany e bdy x) f = ((e `ebcat` bdy) `cat` exit x) f
     where
      ebcat :: MaybeO e (Block n O C) -> Body n -> Fact C f -> UniqSM (DG f n e C, Fact e f)
      exit  :: MaybeO x (Block n C O)           -> Fact x f -> UniqSM (DG f n C x, Fact C f)
      exit (JustO blk) f = arbx block blk f
      exit NothingO    f = return (dgnilC, f)
      ebcat entry bdy f = c entries entry f
       where c :: MaybeC e [Label] -> MaybeO e (Block n O C)
                -> Fact C f -> UniqSM (DG f n e C, Fact e f)
             c NothingC (JustO entry)   f = (block entry `cat` body (successors entry) bdy) f
             c (JustC entries) NothingO f = body entries bdy f
             c _ _ _ = error "bogus GADT pattern match failure"

    -- Lift from nodes to blocks
    block BNil            f = return (dgnil, f)
    block (BlockCO n b)   f = (node n `cat` block b) f
    block (BlockCC l b n) f = (node l `cat` block b `cat` node n) f
    block (BlockOC   b n) f =              (block b `cat` node n) f

    block (BMiddle n)     f = node n f
    block (BCat b1 b2)    f = (block b1 `cat` block b2) f
    block (BSnoc h n)     f = (block h  `cat` node n) f
    block (BCons n t)     f = (node  n  `cat` block t) f

    {-# INLINE node #-}
    node n f
      = do { bwdres <- brewrite rewrite n f
           ; case bwdres of
               Nothing -> return (singletonDG entry_f n, entry_f)
                            where entry_f = btransfer transfer n f
               Just (g, rw) ->
                          do { let pass' = pass { bp_rewrite = rw }
                             ; (g, f) <- arbGraph pass' (fwdEntryLabel n) g f
                             ; return (g, bwdEntryFact lattice n f)} }

    -- | Compose fact transformers and concatenate the resulting
    -- rewritten graphs.
    {-# INLINE cat #-} 
    cat ft1 ft2 f = do { (g2,f2) <- ft2 f
                       ; (g1,f1) <- ft1 f2
                       ; let !g = g1 `dgSplice` g2
                       ; return (g, f1) }

    arbx :: forall x .
            (Block n C x -> Fact x f -> UniqSM (DG f n C x, f))
         -> (Block n C x -> Fact x f -> UniqSM (DG f n C x, Fact C f))

    arbx arb thing f = do { (rg, f) <- arb thing f
                          ; let fb = joinInFacts (bp_lattice pass) $
                                     mapSingleton (entryLabel thing) f
                          ; return (rg, fb) }
     -- joinInFacts adds debugging information

                    -- Outgoing factbase is restricted to Labels *not* in
                    -- in the Body; the facts for Labels *in*
                    -- the Body are in the 'DG f n C C'
    body entries blockmap init_fbase
      = fixpoint Bwd (bp_lattice pass) do_block entries blockmap init_fbase
      where
        do_block :: forall x. Block n C x -> Fact x f -> UniqSM (DG f n C x, LabelMap f)
        do_block b f = do (g, f) <- block b f
                          return (g, mapSingleton (entryLabel b) f)


{-

The forward and backward cases are not dual.  In the forward case, the
entry points are known, and one simply traverses the body blocks from
those points.  In the backward case, something is known about the exit
points, but this information is essentially useless, because we don't
actually have a dual graph (that is, one with edges reversed) to
compute with.  (Even if we did have a dual graph, it would not avail
us---a backward analysis must include reachable blocks that don't
reach the exit, as in a procedure that loops forever and has side
effects.)

-}

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


-- | fixpointing for combined analysis/rewriting
--
fixpoint :: forall n f. NonLocal n
 => Direction
 -> DataflowLattice f
 -> (Block n C C -> Fact C f -> UniqSM (DG f n C C, Fact C f))
 -> [Label]
 -> LabelMap (Block n C C)
 -> (Fact C f -> UniqSM (DG f n C C, Fact C f))

fixpoint direction DataflowLattice{ fact_bot = _, fact_join = join }
         do_block entries blockmap init_fbase
  = do
        -- trace ("fixpoint: " ++ show (case direction of Fwd -> True; Bwd -> False) ++ " " ++ show (mapKeys blockmap) ++ show entries ++ " " ++ show (mapKeys init_fbase)) $ return()
        (fbase, newblocks) <- loop start init_fbase mapEmpty
        -- trace ("fixpoint DONE: " ++ show (mapKeys fbase) ++ show (mapKeys newblocks)) $ return()
        return (GMany NothingO newblocks NothingO,
                mapDeleteList (mapKeys blockmap) fbase)
    -- The successors of the Graph are the the Labels
    -- for which we have facts and which are *not* in
    -- the blocks of the graph
  where
    blocks     = sortBlocks direction entries blockmap
    n          = length blocks
    block_arr  = {-# SCC "block_arr" #-} listArray (0,n-1) blocks
    start      = {-# SCC "start" #-} [0..n-1]
    dep_blocks = {-# SCC "dep_blocks" #-} mkDepBlocks direction blocks

    loop
       :: IntHeap
       -> FactBase f  -- current factbase (increases monotonically)
       -> LabelMap (DBlock f n C C)  -- transformed graph
       -> UniqSM (FactBase f, LabelMap (DBlock f n C C))

    loop [] fbase newblocks = return (fbase, newblocks)
    loop (ix:todo) fbase !newblocks = do
           let blk = block_arr ! ix

           -- trace ("analysing: " ++ show (entryLabel blk)) $ return ()
           (rg, out_facts) <- do_block blk fbase
           let !(todo', fbase') =
                  mapFoldWithKey (updateFact join dep_blocks)
                                 (todo,fbase) out_facts

           -- trace ("fbase': " ++ show (mapKeys fbase')) $ return ()
           -- trace ("changed: " ++ show changed) $ return ()
           -- trace ("to analyse: " ++ show to_analyse) $ return ()

           let newblocks' = case rg of
                              GMany _ blks _ -> mapUnion blks newblocks
     
           loop todo' fbase' newblocks'


{-  Note [TxFactBase invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The TxFactBase is used only during a fixpoint iteration (or "sweep"),
and accumulates facts (and the transformed code) during the fixpoint
iteration.

* tfb_fbase increases monotonically, across all sweeps

* At the beginning of each sweep
      tfb_cha  = NoChange
      tfb_lbls = {}

* During each sweep we process each block in turn.  Processing a block
  is done thus:
    1.  Read from tfb_fbase the facts for its entry label (forward)
        or successors labels (backward)
    2.  Transform those facts into new facts for its successors (forward)
        or entry label (backward)
    3.  Augment tfb_fbase with that info
  We call the labels read in step (1) the "in-labels" of the sweep

* The field tfb_lbls is the set of in-labels of all blocks that have
  been processed so far this sweep, including the block that is
  currently being processed.  tfb_lbls is initialised to {}.  It is a
  subset of the Labels of the *original* (not transformed) blocks.

* The tfb_cha field is set to SomeChange iff we decide we need to
  perform another iteration of the fixpoint loop. It is initialsed to NoChange.

  Specifically, we set tfb_cha to SomeChange in step (3) iff
    (a) The fact in tfb_fbase for a block L changes
    (b) L is in tfb_lbls
  Reason: until a label enters the in-labels its accumuated fact in tfb_fbase
  has not been read, hence cannot affect the outcome

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

-- | Sort the blocks into the right order for analysis.
sortBlocks :: NonLocal n => Direction -> [Label] -> LabelMap (Block n C C)
           -> [Block n C C]
sortBlocks direction entries blockmap
   = case direction of Fwd -> fwd
                       Bwd -> reverse fwd
  where fwd = forwardBlockList entries blockmap

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

-----------------------------------------------------------------------------
--      DG: an internal data type for 'decorated graphs'
--          TOTALLY internal to Hoopl; each block is decorated with a fact
-----------------------------------------------------------------------------

type DG f  = Graph' (DBlock f)
data DBlock f n e x = DBlock f (Block n e x) -- ^ block decorated with fact

instance NonLocal n => NonLocal (DBlock f n) where
  entryLabel (DBlock _ b) = entryLabel b
  successors (DBlock _ b) = successors b

--- constructors

dgnil  :: DG f n O O
dgnilC :: DG f n C C
dgSplice  :: NonLocal n => DG f n e a -> DG f n a x -> DG f n e x

---- observers

normalizeGraph :: forall n f e x .
                  NonLocal n => DG f n e x
               -> (Graph n e x, FactBase f)
                 -- A Graph together with the facts for that graph
                 -- The domains of the two maps should be identical

normalizeGraph g = (mapGraphBlocks dropFact g, facts g)
    where dropFact :: DBlock t t1 t2 t3 -> Block t1 t2 t3
          dropFact (DBlock _ b) = b
          facts :: DG f n e x -> FactBase f
          facts GNil = noFacts
          facts (GUnit _) = noFacts
          facts (GMany _ body exit) = bodyFacts body `mapUnion` exitFacts exit
          exitFacts :: MaybeO x (DBlock f n C O) -> FactBase f
          exitFacts NothingO = noFacts
          exitFacts (JustO (DBlock f b)) = mapSingleton (entryLabel b) f
          bodyFacts :: LabelMap (DBlock f n C C) -> FactBase f
          bodyFacts body = mapFoldWithKey f noFacts body
            where f :: forall t a x. (NonLocal t) => Label -> DBlock a t C x -> LabelMap a -> LabelMap a
                  f lbl (DBlock f _) fb = mapInsert lbl f fb

--- implementation of the constructors (boring)

dgnil  = GNil
dgnilC = GMany NothingO emptyBody NothingO

dgSplice = splice fzCat
  where fzCat :: DBlock f n e O -> DBlock t n O x -> DBlock f n e x
        fzCat (DBlock f b1) (DBlock _ b2) = DBlock f $! b1 `blockAppend` b2
        -- NB. strictness, this function is hammered.

----------------------------------------------------------------
--       Utilities
----------------------------------------------------------------

-- Lifting based on shape:
--  - from nodes to blocks
--  - from facts to fact-like things
-- Lowering back:
--  - from fact-like things to facts
-- Note that the latter two functions depend only on the entry shape.
class ShapeLifter e x where
 singletonDG   :: f -> n e x -> DG f n e x
 fwdEntryFact  :: NonLocal n => n e x -> f -> Fact e f
 fwdEntryLabel :: NonLocal n => n e x -> MaybeC e [Label]
 ftransfer :: FwdTransfer n f -> n e x -> f -> Fact x f
 frewrite  :: FwdRewrite m n f -> n e x
           -> f -> m (Maybe (Graph n e x, FwdRewrite m n f))
-- @ end node.tex
 bwdEntryFact :: NonLocal n => DataflowLattice f -> n e x -> Fact e f -> f
 btransfer    :: BwdTransfer n f -> n e x -> Fact x f -> f
 brewrite     :: BwdRewrite m n f -> n e x
              -> Fact x f -> m (Maybe (Graph n e x, BwdRewrite m n f))

instance ShapeLifter C O where
  singletonDG f n = gUnitCO (DBlock f (BlockCO n BNil))
  fwdEntryFact     n f  = mapSingleton (entryLabel n) f
  bwdEntryFact lat n fb = getFact lat (entryLabel n) fb
  ftransfer (FwdTransfer3 (ft, _, _)) n f = ft n f
  btransfer (BwdTransfer3 (bt, _, _)) n f = bt n f
  frewrite  (FwdRewrite3  (fr, _, _)) n f = fr n f
  brewrite  (BwdRewrite3  (br, _, _)) n f = br n f
  fwdEntryLabel n = JustC [entryLabel n]

instance ShapeLifter O O where
  singletonDG f = gUnitOO . DBlock f . BMiddle
  fwdEntryFact   _ f = f
  bwdEntryFact _ _ f = f
  ftransfer (FwdTransfer3 (_, ft, _)) n f = ft n f
  btransfer (BwdTransfer3 (_, bt, _)) n f = bt n f
  frewrite  (FwdRewrite3  (_, fr, _)) n f = fr n f
  brewrite  (BwdRewrite3  (_, br, _)) n f = br n f
  fwdEntryLabel _ = NothingC

instance ShapeLifter O C where
  singletonDG f n = gUnitOC (DBlock f (BlockOC BNil n))
  fwdEntryFact   _ f = f
  bwdEntryFact _ _ f = f
  ftransfer (FwdTransfer3 (_, _, ft)) n f = ft n f
  btransfer (BwdTransfer3 (_, _, bt)) n f = bt n f
  frewrite  (FwdRewrite3  (_, _, fr)) n f = fr n f
  brewrite  (BwdRewrite3  (_, _, br)) n f = br n f
  fwdEntryLabel _ = NothingC

{-
class ShapeLifter e x where
  singletonDG   :: f -> n e x -> DG f n e x

instance ShapeLifter C O where
  singletonDG f n = gUnitCO (DBlock f (BlockCO n BNil))

instance ShapeLifter O O where
  singletonDG f = gUnitOO . DBlock f . BMiddle

instance ShapeLifter O C where
  singletonDG f n = gUnitOC (DBlock f (BlockOC BNil n))
-}

-- Fact lookup: the fact `orelse` bottom
getFact  :: DataflowLattice f -> Label -> FactBase f -> f
getFact lat l fb = case lookupFact l fb of Just  f -> f
                                           Nothing -> fact_bot lat



{-  Note [Respects fuel]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}
-- $fuel
-- A value of type 'FwdRewrite' or 'BwdRewrite' /respects fuel/ if 
-- any function contained within the value satisfies the following properties:
--
--   * When fuel is exhausted, it always returns 'Nothing'.
--
--   * When it returns @Just g rw@, it consumes /exactly/ one unit
--     of fuel, and new rewrite 'rw' also respects fuel.
--
-- Provided that functions passed to 'mkFRewrite', 'mkFRewrite3', 
-- 'mkBRewrite', and 'mkBRewrite3' are not aware of the fuel supply,
-- the results respect fuel.
--
-- It is an /unchecked/ run-time error for the argument passed to 'wrapFR',
-- 'wrapFR2', 'wrapBR', or 'warpBR2' to return a function that does not respect fuel.

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
