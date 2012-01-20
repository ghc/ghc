{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 703
{-# OPTIONS_GHC -fprof-auto-top #-}
#endif
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

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
  , analyzeFwd, analyzeBwd
  )
where

import OptimizationFuel

import Control.Monad
import Data.Maybe
import Data.Array
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Compiler.Hoopl.Collections
import Compiler.Hoopl.Fuel
import Compiler.Hoopl.Graph hiding (Graph) -- hiding so we can redefine
                                           -- and include definition in paper
import qualified Compiler.Hoopl.GraphUtil as U
import Compiler.Hoopl.Label
import Compiler.Hoopl.Util
import Compiler.Hoopl.Dataflow (JoinFun)

import Compiler.Hoopl.Dataflow (
    DataflowLattice(..), OldFact(..), NewFact(..), Fact
  , ChangeFlag(..), mkFactBase
  , FwdPass(..), FwdRewrite(..), FwdTransfer(..), mkFRewrite,  getFRewrite3, mkFTransfer, mkFTransfer3
  , wrapFR, wrapFR2
  , BwdPass(..), BwdRewrite(..),  BwdTransfer(..), mkBTransfer, mkBTransfer3, getBTransfer3
  , wrapBR, wrapBR2
  , mkBRewrite,  getBRewrite3
  )

import Debug.Trace

noRewrite :: a -> b -> FuelUniqSM (Maybe c)
noRewrite _ _ = return Nothing

noFwdRewrite :: FwdRewrite FuelUniqSM n f
noFwdRewrite = FwdRewrite3 (noRewrite, noRewrite, noRewrite)

-- | Functions passed to 'mkFRewrite3' should not be aware of the fuel supply.
-- The result returned by 'mkFRewrite3' respects fuel.
mkFRewrite3 :: forall n f.
               (n C O -> f -> FuelUniqSM (Maybe (Graph n C O)))
            -> (n O O -> f -> FuelUniqSM (Maybe (Graph n O O)))
            -> (n O C -> f -> FuelUniqSM (Maybe (Graph n O C)))
            -> FwdRewrite FuelUniqSM n f
mkFRewrite3 f m l = FwdRewrite3 (lift f, lift m, lift l)
  where lift :: forall t t1 a. (t -> t1 -> FuelUniqSM (Maybe a))
                             -> t -> t1 -> FuelUniqSM (Maybe (a, FwdRewrite FuelUniqSM n f))
        {-# INLINE lift #-}
        lift rw node fact = do
             a <- rw node fact
             case a of
               Nothing -> return Nothing
               Just a  -> do f <- getFuel
                             if f == 0
                                then return Nothing
                                else setFuel (f-1) >> return (Just (a,noFwdRewrite))

noBwdRewrite :: BwdRewrite FuelUniqSM n f
noBwdRewrite = BwdRewrite3 (noRewrite, noRewrite, noRewrite)

mkBRewrite3 :: forall n f.
               (n C O -> f          -> FuelUniqSM (Maybe (Graph n C O)))
            -> (n O O -> f          -> FuelUniqSM (Maybe (Graph n O O)))
            -> (n O C -> FactBase f -> FuelUniqSM (Maybe (Graph n O C)))
            -> BwdRewrite FuelUniqSM n f
mkBRewrite3 f m l = BwdRewrite3 (lift f, lift m, lift l)
  where lift :: forall t t1 a. (t -> t1 -> FuelUniqSM (Maybe a))
                             -> t -> t1 -> FuelUniqSM (Maybe (a, BwdRewrite FuelUniqSM n f))
        {-# INLINE lift #-}
        lift rw node fact = do
             a <- rw node fact
             case a of
               Nothing -> return Nothing
               Just a  -> do f <- getFuel
                             if f == 0
                                then return Nothing
                                else setFuel (f-1) >> return (Just (a,noBwdRewrite))

-----------------------------------------------------------------------------
--              Analyze and rewrite forward: the interface
-----------------------------------------------------------------------------

-- | if the graph being analyzed is open at the entry, there must
--   be no other entry point, or all goes horribly wrong...
analyzeAndRewriteFwd
   :: forall n f e x .  NonLocal n =>
      FwdPass FuelUniqSM n f
   -> MaybeC e [Label]
   -> Graph n  e x -> Fact e f
   -> FuelUniqSM (Graph n e x, FactBase f, MaybeO x f)
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
            FwdPass FuelUniqSM n f ->
            Entries e -> Graph n e x -> Fact e f -> FuelUniqSM (DG f n e x, Fact x f)
arfGraph pass@FwdPass { fp_lattice = lattice,
                        fp_transfer = transfer,
                        fp_rewrite  = rewrite } entries = graph
  where
    {- nested type synonyms would be so lovely here 
    type ARF  thing = forall e x . thing e x -> f        -> m (DG f n e x, Fact x f)
    type ARFX thing = forall e x . thing e x -> Fact e f -> m (DG f n e x, Fact x f)
    -}
    graph ::              Graph n e x -> Fact e f -> FuelUniqSM (DG f n e x, Fact x f)
    block :: forall e x .
             Block n e x -> f -> FuelUniqSM (DG f n e x, Fact x f)

    body  :: [Label] -> LabelMap (Block n C C)
          -> Fact C f -> FuelUniqSM (DG f n C C, Fact C f)
                    -- Outgoing factbase is restricted to Labels *not* in
                    -- in the Body; the facts for Labels *in*
                    -- the Body are in the 'DG f n C C'

    cat :: forall e a x f1 f2 f3.
           (f1 -> FuelUniqSM (DG f n e a, f2))
        -> (f2 -> FuelUniqSM (DG f n a x, f3))
        -> (f1 -> FuelUniqSM (DG f n e x, f3))

    graph GNil            = \f -> return (dgnil, f)
    graph (GUnit blk)     = block blk
    graph (GMany e bdy x) = (e `ebcat` bdy) `cat` exit x
     where
      ebcat :: MaybeO e (Block n O C) -> Body n -> Fact e f -> FuelUniqSM (DG f n e C, Fact C f)
      exit  :: MaybeO x (Block n C O)           -> Fact C f -> FuelUniqSM (DG f n C x, Fact x f)
      exit (JustO blk) = arfx block blk
      exit NothingO    = \fb -> return (dgnilC, fb)
      ebcat entry bdy = c entries entry
       where c :: MaybeC e [Label] -> MaybeO e (Block n O C)
                -> Fact e f -> FuelUniqSM (DG f n e C, Fact C f)
             c NothingC (JustO entry)   = block entry `cat` body (successors entry) bdy
             c (JustC entries) NothingO = body entries bdy
             c _ _ = error "bogus GADT pattern match failure"

    -- Lift from nodes to blocks
    block BNil          = \f -> return (dgnil, f)
    block (BlockCO n b) = node n `cat` block b
    block (BlockCC l b n) = node l `cat` block b `cat` node n
    block (BlockOC   b n) =              block b `cat` node n

    block (BMiddle n)  = node n
    block (BCat b1 b2) = block b1 `cat` block b2
    block (BHead h n)  = block h  `cat` node n
    block (BTail n t)  = node  n  `cat` block t

    {-# INLINE node #-}
    node :: forall e x . (ShapeLifter e x)
         => n e x -> f -> FuelUniqSM (DG f n e x, Fact x f)
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
                       ; return (g1 `dgSplice` g2, f2) }

    arfx :: forall x .
            (Block n C x ->        f -> FuelUniqSM (DG f n C x, Fact x f))
         -> (Block n C x -> Fact C f -> FuelUniqSM (DG f n C x, Fact x f))
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
                 -> FuelUniqSM (DG f n C x, Fact x f)
        do_block b fb = block b entryFact
          where entryFact = getFact lattice (entryLabel b) fb


-- Join all the incoming facts with bottom.
-- We know the results _shouldn't change_, but the transfer
-- functions might, for example, generate some debugging traces.
joinInFacts :: DataflowLattice f -> FactBase f -> FactBase f
joinInFacts (lattice @ DataflowLattice {fact_bot = bot, fact_join = fj}) fb =
  mkFactBase lattice $ map botJoin $ mapToList fb
    where botJoin (l, f) = (l, snd $ fj l (OldFact bot) (NewFact f))

forwardBlockList :: (NonLocal n, LabelsPtr entry)
                 => entry -> Body n -> [Block n C C]
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
      FwdPass FuelUniqSM n f
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
         = fixpoint_anal Fwd lattice do_block entries blockmap f
         where
           do_block :: forall x . Block n C x -> FactBase f -> Fact x f
           do_block b fb = block b entryFact
             where entryFact = getFact lattice (entryLabel b) fb

    block :: forall e x . Block n e x -> f -> Fact x f
    block BNil            = id
    block (BlockCO n b)   = ftr n `cat` block b
    block (BlockCC l b n) = ftr l `cat` block b `cat` ltr n
    block (BlockOC   b n) =             block b `cat` ltr n

    block (BMiddle n)     = mtr n
    block (BCat b1 b2)    = block b1 `cat` block b2
    block (BHead h n)     = block h  `cat` mtr n
    block (BTail n t)     = mtr  n   `cat` block t

    {-# INLINE cat #-}
    cat ft1 ft2 f = ft2 (ft1 f)

----------------------------------------------------------------
--       Backward Analysis only
----------------------------------------------------------------

-- | if the graph being analyzed is open at the entry, there must
--   be no other entry point, or all goes horribly wrong...
analyzeBwd
   :: forall n f e .  NonLocal n =>
      BwdPass FuelUniqSM n f
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
         = fixpoint_anal Bwd lattice do_block entries blockmap f
         where
           do_block :: forall x . Block n C x -> Fact x f -> FactBase f
           do_block b fb = mapSingleton (entryLabel b) (block b fb)

    block :: forall e x . Block n e x -> Fact x f -> f
    block BNil            = id
    block (BlockCO n b)   = ftr n `cat` block b
    block (BlockCC l b n) = ftr l `cat` block b `cat` ltr n
    block (BlockOC   b n) =             block b `cat` ltr n

    block (BMiddle n)     = mtr n
    block (BCat b1 b2)    = block b1 `cat` block b2
    block (BHead h n)     = block h  `cat` mtr n
    block (BTail n t)     = mtr  n   `cat` block t

    {-# INLINE cat #-}
    cat ft1 ft2 f = ft1 (ft2 f)

-----------------------------------------------------------------------------
--              Backward analysis and rewriting: the interface
-----------------------------------------------------------------------------


-- | if the graph being analyzed is open at the exit, I don't
--   quite understand the implications of possible other exits
analyzeAndRewriteBwd
   :: NonLocal n
   => BwdPass FuelUniqSM n f
   -> MaybeC e [Label] -> Graph n e x -> Fact x f
   -> FuelUniqSM (Graph n e x, FactBase f, MaybeO e f)
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
            BwdPass FuelUniqSM n f ->
            Entries e -> Graph n e x -> Fact x f -> FuelUniqSM (DG f n e x, Fact e f)
arbGraph pass@BwdPass { bp_lattice  = lattice,
                        bp_transfer = transfer,
                        bp_rewrite  = rewrite } entries = graph
  where
    {- nested type synonyms would be so lovely here 
    type ARB  thing = forall e x . thing e x -> Fact x f -> m (DG f n e x, f)
    type ARBX thing = forall e x . thing e x -> Fact x f -> m (DG f n e x, Fact e f)
    -}
    graph ::              Graph n e x -> Fact x f -> FuelUniqSM (DG f n e x, Fact e f)
    block :: forall e x . Block n e x -> Fact x f -> FuelUniqSM (DG f n e x, f)
    body  :: [Label] -> Body n -> Fact C f -> FuelUniqSM (DG f n C C, Fact C f)
    node  :: forall e x . (ShapeLifter e x) 
             => n e x       -> Fact x f -> FuelUniqSM (DG f n e x, f)
    cat :: forall e a x info info' info''.
           (info' -> FuelUniqSM (DG f n e a, info''))
        -> (info  -> FuelUniqSM (DG f n a x, info'))
        -> (info  -> FuelUniqSM (DG f n e x, info''))

    graph GNil            = \f -> return (dgnil, f)
    graph (GUnit blk)     = block blk
    graph (GMany e bdy x) = (e `ebcat` bdy) `cat` exit x
     where
      ebcat :: MaybeO e (Block n O C) -> Body n -> Fact C f -> FuelUniqSM (DG f n e C, Fact e f)
      exit  :: MaybeO x (Block n C O)           -> Fact x f -> FuelUniqSM (DG f n C x, Fact C f)
      exit (JustO blk) = arbx block blk
      exit NothingO    = \fb -> return (dgnilC, fb)
      ebcat entry bdy = c entries entry
       where c :: MaybeC e [Label] -> MaybeO e (Block n O C)
                -> Fact C f -> FuelUniqSM (DG f n e C, Fact e f)
             c NothingC (JustO entry)   = block entry `cat` body (successors entry) bdy
             c (JustC entries) NothingO = body entries bdy
             c _ _ = error "bogus GADT pattern match failure"

    -- Lift from nodes to blocks
    block BNil            = \f -> return (dgnil, f)
    block (BlockCO l b)   = node l `cat` block b
    block (BlockCC l b n) = node l `cat` block b `cat` node n
    block (BlockOC   b n) =              block b `cat` node n
    block (BMiddle n)    = node n
    block (BCat b1 b2)   = block b1 `cat` block b2
    block (BHead h n)    = block h  `cat` node n
    block (BTail n t)    = node  n  `cat` block t

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
                       ; return (g1 `dgSplice` g2, f1) }

    arbx :: forall x .
            (Block n C x -> Fact x f -> FuelUniqSM (DG f n C x, f))
         -> (Block n C x -> Fact x f -> FuelUniqSM (DG f n C x, Fact C f))

    arbx arb thing f = do { (rg, f) <- arb thing f
                          ; let fb = joinInFacts (bp_lattice pass) $
                                     mapSingleton (entryLabel thing) f
                          ; return (rg, fb) }
     -- joinInFacts adds debugging information

                    -- Outgoing factbase is restricted to Labels *not* in
                    -- in the Body; the facts for Labels *in*
                    -- the Body are in the 'DG f n C C'
    body entries blockmap init_fbase
      = fixpoint Bwd (bp_lattice pass) do_block (map entryLabel (backwardBlockList entries blockmap)) blockmap init_fbase
      where
        do_block :: forall x. Block n C x -> Fact x f -> FuelUniqSM (DG f n C x, LabelMap f)
        do_block b f = do (g, f) <- block b f
                          return (g, mapSingleton (entryLabel b) f)


backwardBlockList :: NonLocal n => [Label] -> Body n -> [Block n C C]
-- This produces a list of blocks in order suitable for backward analysis,
-- along with the list of Labels it may depend on for facts.
backwardBlockList entries body = reverse $ forwardBlockList entries body

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
--      fixpoint (analysis only)
-----------------------------------------------------------------------------

-- Note [newblocks]
-- For a block whose input is *in* the initial fact base, and is
-- reached by another block, but the join gives NoChange, we must
-- still process it at least once to get its out facts.

updateFact_anal :: f -> JoinFun f -> Bool
           -> LabelSet         -- Note [newblocks]
           -> Label -> f       -- out fact
           -> ([Label], FactBase f)
           -> ([Label], FactBase f)
-- See Note [TxFactBase change flag]
updateFact_anal bot fact_join is_bwd newblocks lbl new_fact (cha, fbase)
  = case lookupFact lbl fbase of
      Nothing       -> (lbl:cha, mapInsert lbl new_fact fbase)
      Just old_fact ->
        case fact_join lbl (OldFact old_fact) (NewFact new_fact) of
           (NoChange, _) | can_say_no_change -> (cha, fbase)
           (_,        f)                     -> (lbl:cha, mapInsert lbl f fbase)
  where
    can_say_no_change = is_bwd || lbl `setMember` newblocks

{-
-- this doesn't work because it can't be implemented
class Monad m => FixpointMonad m where
  observeChangedFactBase :: m (Maybe (FactBase f)) -> Maybe (FactBase f)
-}

data Direction = Fwd | Bwd
fixpoint_anal :: forall n f. NonLocal n
 => Direction
 -> DataflowLattice f
 -> (Block n C C -> Fact C f -> Fact C f)
 -> [Label]
 -> LabelMap (Block n C C)
 -> Fact C f -> FactBase f

fixpoint_anal direction DataflowLattice{ fact_bot = bot, fact_join = join }
              do_block entries blockmap init_fbase
  = loop start init_fbase setEmpty
  where
    blocks               = forwardBlockList entries blockmap
    ordered_blocks       = case direction of
                             Fwd -> blocks
                             Bwd -> reverse blocks
    block_arr            = listArray (0,length blocks - 1) ordered_blocks

    start | Fwd <- direction
          = IS.fromList (concatMap (\l -> mapFindWithDefault [] l dep_blocks) entries)
          | otherwise        = IS.fromList [0 .. length blocks - 1]

    -- mapping from L -> blocks.  If the fact for L changes, re-analyse blocks.
    dep_blocks :: LabelMap [Int]
    dep_blocks = mapFromListWith (++)
                        [ (l, [ix])
                        | (b,ix) <- zip ordered_blocks [0..]
                        , l <- case direction of
                                 Fwd -> [entryLabel b]
                                 Bwd -> successors b
                        ]

    is_bwd = case direction of Bwd -> True; Fwd -> False

    loop
       :: IntSet      -- blocks still to analyse
       -> FactBase f  -- current factbase (increases monotonically)
       -> LabelSet
       -> FactBase f

    loop !todo fbase !newblocks
      | IS.null todo = fbase
      | (ix,todo') <- IS.deleteFindMin todo =
           let blk = block_arr ! ix
               lbl = entryLabel blk
           in
           -- trace ("analysing: " ++ show lbl) $
           let out_facts = do_block blk fbase

               (changed, fbase') = mapFoldWithKey
                                     (updateFact_anal bot join is_bwd newblocks)
                                     ([],fbase) out_facts
           in
           -- trace ("fbase': " ++ show (mapKeys fbase')) $ return ()
           -- trace ("changed: " ++ show changed) $ return ()
     
           let to_analyse
                 = concatMap (\l -> mapFindWithDefault [] l dep_blocks) changed
           in

           -- trace ("to analyse: " ++ show to_analyse) $ return ()

           let newblocks' | is_bwd    = newblocks
                          | otherwise = setInsert lbl newblocks
           in

           loop (foldr IS.insert todo' to_analyse) fbase' newblocks'

-----------------------------------------------------------------------------
--      fixpoint: finding fixed points
-----------------------------------------------------------------------------

     -- See Note [TxFactBase invariants]

updateFact :: f -> JoinFun f -> Bool
           -> LabelMap (DBlock f n C C)
           -> Label -> f       -- out fact
           -> ([Label], FactBase f)
           -> ([Label], FactBase f)
-- See Note [TxFactBase change flag]
updateFact bot fact_join is_bwd newblocks lbl new_fact (cha, fbase)
  = case lookupFact lbl fbase of
      Nothing       -> (lbl:cha, mapInsert lbl new_fact fbase)
                            -- Note [no old fact]
      Just old_fact ->
        case fact_join lbl (OldFact old_fact) (NewFact new_fact) of
           (NoChange, _) | can_say_no_change -> (cha, fbase)
           (_,        f)                     -> (lbl:cha, mapInsert lbl f fbase)
  where
    can_say_no_change = is_bwd || lbl `mapMember` newblocks

{-
Note [no old fact]

We know that the new_fact is >= _|_, so we don't need to join.  However,
if the new fact is also _|_, and we have already analysed its block,
we don't need to record a change.  So there's a tradeoff here.  It turns
out that always recording a change is faster.
-}

{-
-- this doesn't work because it can't be implemented
class Monad m => FixpointMonad m where
  observeChangedFactBase :: m (Maybe (FactBase f)) -> Maybe (FactBase f)
-}

fixpoint :: forall n f. NonLocal n
 => Direction
 -> DataflowLattice f
 -> (Block n C C -> Fact C f -> FuelUniqSM (DG f n C C, Fact C f))
 -> [Label]
 -> LabelMap (Block n C C)
 -> (Fact C f -> FuelUniqSM (DG f n C C, Fact C f))

fixpoint direction DataflowLattice{ fact_bot = bot, fact_join = join }
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
    blocks               = forwardBlockList entries blockmap
    ordered_blocks       = case direction of
                             Fwd -> blocks
                             Bwd -> reverse blocks
    block_arr            = listArray (0,length blocks - 1) ordered_blocks

    start | Fwd <- direction
          = IS.fromList (concatMap (\l -> mapFindWithDefault [] l dep_blocks) entries)
          | otherwise        = IS.fromList [0 .. length blocks - 1]

    -- mapping from L -> blocks.  If the fact for L changes, re-analyse blocks.
    dep_blocks :: LabelMap [Int]
    dep_blocks = mapFromListWith (++)
                        [ (l, [ix])
                        | (b,ix) <- zip ordered_blocks [0..]
                        , l <- case direction of
                                 Fwd -> [entryLabel b]
                                 Bwd -> successors b
                        ]

    is_bwd = case direction of Bwd -> True; Fwd -> False

    loop
       :: IntSet
       -> FactBase f  -- current factbase (increases monotonically)
       -> LabelMap (DBlock f n C C)  -- transformed graph
       -> FuelUniqSM (FactBase f, LabelMap (DBlock f n C C))

    loop !todo fbase !newblocks
      | IS.null todo = return (fbase, newblocks)
      | (ix,todo') <- IS.deleteFindMin todo = do
           let blk = block_arr ! ix
               lbl = entryLabel blk

           -- trace ("analysing: " ++ show lbl) $ return ()
           (rg, out_facts) <- do_block blk fbase
           let (changed, fbase') = mapFoldWithKey
                                     (updateFact bot join is_bwd newblocks)
                                     ([],fbase) out_facts
           -- trace ("fbase': " ++ show (mapKeys fbase')) $ return ()
           -- trace ("changed: " ++ show changed) $ return ()
     
           let to_analyse
                 = concatMap (\l -> mapFindWithDefault [] l dep_blocks) changed

           -- trace ("to analyse: " ++ show to_analyse) $ return ()

           let newblocks' = case rg of
                              GMany _ blks _ -> mapUnion blks newblocks
     
           loop (foldr IS.insert todo' to_analyse) fbase' newblocks'


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
--      DG: an internal data type for 'decorated graphs'
--          TOTALLY internal to Hoopl; each block is decorated with a fact
-----------------------------------------------------------------------------

type Graph = Graph' Block
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

normalizeGraph g = (graphMapBlocks dropFact g, facts g)
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

dgSplice = U.splice fzCat
  where fzCat :: DBlock f n e O -> DBlock t n O x -> DBlock f n e x
        fzCat (DBlock f b1) (DBlock _ b2) = DBlock f $! b1 `U.cat` b2
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
