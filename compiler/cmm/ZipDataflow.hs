{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS -fglasgow-exts #-}
-- -fglagow-exts for kind signatures

module ZipDataflow
    ( DebugNodes(), RewritingDepth(..), LastOutFacts(..)
    , zdfSolveFrom, zdfRewriteFrom
    , zdfSolveFromL
    , ForwardTransfers(..), BackwardTransfers(..)
    , ForwardRewrites(..),  BackwardRewrites(..) 
    , ForwardFixedPoint, BackwardFixedPoint
    , zdfFpFacts
    , zdfFpOutputFact
    , zdfGraphChanged
    , zdfDecoratedGraph -- not yet implemented
    , zdfFpContents
    , zdfFpLastOuts
    , zdfBRewriteFromL, zdfFRewriteFromL 
    )
where

import BlockId
import CmmTx
import DFMonad
import OptimizationFuel as F
import MkZipCfg
import ZipCfg
import qualified ZipCfg as G

import Maybes
import Outputable

import Control.Monad

{- 

This module implements two useful tools:

  1. An iterative solver for dataflow problems

  2. The combined dataflow-analysis-and-transformation framework
     described by Lerner, Grove, and Chambers in their excellent
     2002 POPL paper (http://tinyurl.com/3zycbr or 
     http://tinyurl.com/3pnscd).

Each tool comes in two flavors: one for forward dataflow problems
and one for backward dataflow problems.

We quote the paper above:

  Dataflow analyses can have mutually beneficial interactions.
  Previous efforts to exploit these interactions have either
  (1) iteratively performed each individual analysis until no
  further improvements are discovered or (2) developed "super-
  analyses" that manually combine conceptually separate anal-
  yses. We have devised a new approach that allows anal-
  yses to be defined independently while still enabling them
  to be combined automatically and profitably. Our approach
  avoids the loss of precision associated with iterating indi-
  vidual analyses and the implementation difficulties of man-
  ually writing a super-analysis.    

The key idea is to provide at each CFG node not only a dataflow
transfer function but also a rewriting function that has the option to
replace the node with a new (possibly empty) graph.  The rewriting
function takes a dataflow fact as input, and the fact is used to
justify any rewriting.  For example, in a backward problem, the fact
that variable x is dead can be used to justify rewriting node
  x := e
to the empty graph.  In a forward problem, the fact that x == 7 can
be used to justify rewriting node
  y := x + 1
to 
  y := 8
which in turn will be analyzed and produce a new fact:
x == 7 and y == 8.

In its most general form, this module takes as input graph, transfer
equations, rewrites, and an initial set of dataflow facts, and
iteratively computes a new graph and a new set of dataflow facts such
that
  * The set of facts is a fixed point of the transfer equations
  * The graph has been rewritten as much as is consistent with
    the given facts and requested rewriting depth (see below)
N.B. 'A set of facts' is shorthand for 'A finite map from CFG label to fact'.

The types of transfer equations, rewrites, and fixed points are
different for forward and backward problems.  To avoid cluttering the
name space with two versions of every name, other names such as
zdfSolveFrom are overloaded to work in both forward or backward
directions.  This design decision is based on experience with the
predecessor module, which has been mercifully deleted.


This module is deliberately very abstract.  It is a completely general
framework and well-nigh impossible to understand in isolation.  The
cautious reader will begin with some concrete examples in the form of
clients.  NR recommends

  CmmLiveZ             A simple liveness analysis

  CmmSpillReload.removeDeadAssignmentsAndReloads
                       A piece of spaghetti to pull on, which leads to
                         - a two-part liveness analysis that tracks
                           variables live in registers and live on the stack
                         - elimination of assignments to dead variables
                         - elimination of redundant reloads

Even hearty souls should avoid the CmmProcPointZ client, at least for
the time being.

-}   


{- ============ TRANSFER FUNCTIONS AND REWRITES =========== -}

-- | For a backward transfer, you're given the fact on a node's 
-- outedge and you compute the fact on the inedge.  Facts have type 'a'.
-- A last node may have multiple outedges, each pointing to a labelled
-- block, so instead of a fact it is given a mapping from BlockId to fact.

data BackwardTransfers middle last a = BackwardTransfers
    { bt_first_in  :: BlockId -> a              -> a
    , bt_middle_in :: middle  -> a              -> a
    , bt_last_in   :: last    -> (BlockId -> a) -> a
    } 

-- | For a forward transfer, you're given the fact on a node's 
-- inedge and you compute the fact on the outedge. Because a last node
-- may have multiple outedges, each pointing to a labelled
-- block, so instead of a fact it produces a list of (BlockId, fact) pairs.

data ForwardTransfers middle last a = ForwardTransfers
    { ft_first_out  :: BlockId -> a -> a
    , ft_middle_out :: middle  -> a -> a
    , ft_last_outs  :: last    -> a -> LastOutFacts a
    , ft_exit_out   ::            a -> a
    } 

newtype LastOutFacts a = LastOutFacts [(BlockId, a)] 
  -- ^ These are facts flowing out of a last node to the node's successors.
  -- They are either to be set (if they pertain to the graph currently
  -- under analysis) or propagated out of a sub-analysis


-- | A backward rewrite takes the same inputs as a backward transfer,
-- but instead of producing a fact, it produces a replacement graph or Nothing.

data BackwardRewrites middle last a = BackwardRewrites
    { br_first  :: BlockId -> a              -> Maybe (AGraph middle last)
    , br_middle :: middle  -> a              -> Maybe (AGraph middle last)
    , br_last   :: last    -> (BlockId -> a) -> Maybe (AGraph middle last)
    , br_exit   ::                              Maybe (AGraph middle last)
    } 

-- | A forward rewrite takes the same inputs as a forward transfer,
-- but instead of producing a fact, it produces a replacement graph or Nothing.

data ForwardRewrites middle last a = ForwardRewrites
    { fr_first  :: BlockId -> a -> Maybe (AGraph middle last)
    , fr_middle :: middle  -> a -> Maybe (AGraph middle last)
    , fr_last   :: last    -> a -> Maybe (AGraph middle last)
    , fr_exit   ::            a -> Maybe (AGraph middle last)
    } 

{- ===================== FIXED POINTS =================== -}

-- | The result of combined analysis and transformation is a 
-- solution to the set of dataflow equations together with a 'contained value'.
-- This solution is a member of type class 'FixedPoint', which is parameterized by
--   * middle and last nodes 'm' and 'l'
--   * data flow fact 'fact'
--   * the type 'a' of the contained value
--
-- In practice, the contained value 'zdfFpContents' is either a
-- rewritten graph, when rewriting, or (), when solving without
-- rewriting.  A function 'zdfFpMap' allows a client to change 
-- the contents without changing other values.
--
-- To save space, we provide the solution 'zdfFpFacts' as a mapping
-- from BlockId to fact; if necessary, facts on edges can be
-- reconstructed using the transfer functions; this functionality is
-- intended to be included as the 'zdfDecoratedGraph', but the code
-- has not yet been implemented.
--
-- The solution may also includes a fact 'zdfFpOuputFact', which is
-- not associated with any label:
--   * for a backward problem, this is the fact at entry
--   * for a forward problem, this is the fact at the distinguished exit node,
--     if such a node is present
--
-- For a forward problem only, the solution includes 'zdfFpLastOuts',
-- which is the set of facts on edges leaving the graph.
--
-- The flag 'zdfGraphChanged' tells whether the engine did any rewriting.

class FixedPoint fp where
    zdfFpContents     :: fp m l fact a -> a
    zdfFpFacts        :: fp m l fact a -> BlockEnv fact
    zdfFpOutputFact   :: fp m l fact a -> fact  -- entry for backward; exit for forward
    zdfDecoratedGraph :: fp m l fact a -> Graph (fact, m) (fact, l)
    zdfGraphChanged   :: fp m l fact a -> ChangeFlag
    zdfFpMap          :: (a -> b) -> (fp m l fact a -> fp m l fact b)

-- | The class 'FixedPoint' has two instances: one for forward problems and
-- one for backward problems.  The 'CommonFixedPoint' defines all fields 
-- common to both.  (The instance declarations are uninteresting and appear below.)

data CommonFixedPoint m l fact a = FP
    { fp_facts     :: BlockEnv fact
    , fp_out       :: fact  -- entry for backward; exit for forward
    , fp_changed   :: ChangeFlag
    , fp_dec_graph :: Graph (fact, m) (fact, l)
    , fp_contents  :: a
    }

-- | The common fixed point is sufficient for a backward problem.
type BackwardFixedPoint = CommonFixedPoint

-- | A forward problem needs the common fields, plus the facts on the outedges.
data ForwardFixedPoint m l fact a = FFP
    { ffp_common    :: CommonFixedPoint m l fact a
    , zdfFpLastOuts :: LastOutFacts fact
    }


{- ============== SOLVING AND REWRITING ============== -}

type PassName = String

-- | 'zdfSolveFrom' is an overloaded name that resolves to a pure
-- analysis with no rewriting.  It has only two instances: forward and
-- backward.  Since it needs no rewrites, the type parameters of the
-- class are transfer functions and the fixed point.
--
--
-- An iterative solver normally starts with the bottom fact at every
-- node, but it can be useful in other contexts as well.  For this
-- reason the initial set of facts (at labelled blocks only) is a
-- parameter to the solver.  
--
-- The constraints on the type signature exist purely for debugging;
-- they make it possible to prettyprint nodes and facts.  The parameter of
-- type 'PassName' is also used just for debugging.
--
-- Note that the result is a fixed point with no contents, that is,
-- the contents have type ().
-- 
-- The intent of the rest of the type signature should be obvious.
-- If not, place a skype call to norman-ramsey or complain bitterly
-- to <norman-ramsey@acm.org>.

class DataflowSolverDirection transfers fixedpt where
  zdfSolveFrom   :: (DebugNodes m l, Outputable a)
                 => BlockEnv a        -- ^ Initial facts (unbound == bottom)
                 -> PassName
                 -> DataflowLattice a -- ^ Lattice
                 -> transfers m l a   -- ^ Dataflow transfer functions
                 -> a                 -- ^ Fact flowing in (at entry or exit)
                 -> Graph m l         -- ^ Graph to be analyzed
                 -> FuelMonad (fixedpt m l a ())  -- ^ Answers
  zdfSolveFromL  :: (DebugNodes m l, Outputable a)
                 => BlockEnv a        -- Initial facts (unbound == bottom)
                 -> PassName
                 -> DataflowLattice a -- Lattice
                 -> transfers m l a   -- Dataflow transfer functions
                 -> a                 -- Fact flowing in (at entry or exit)
                 -> LGraph m l         -- Graph to be analyzed
                 -> FuelMonad (fixedpt m l a ())  -- Answers
  zdfSolveFromL b p l t a g = zdfSolveFrom b p l t a $ quickGraph g

-- There are exactly two instances: forward and backward
instance DataflowSolverDirection ForwardTransfers ForwardFixedPoint
  where zdfSolveFrom = solve_f

instance DataflowSolverDirection BackwardTransfers BackwardFixedPoint
  where zdfSolveFrom = solve_b


-- | zdfRewriteFrom is an overloaded name that resolves to an
-- interleaved analysis and transformation.  It too is instantiated in
-- forward and backward directions.
-- 
-- The type parameters of the class include not only transfer
-- functions and the fixed point but also rewrites.
--
-- The type signature of 'zdfRewriteFrom' is that of 'zdfSolveFrom'
-- with the rewrites and a rewriting depth as additional parameters,
-- as well as a different result, which contains a rewritten graph.

class DataflowSolverDirection transfers fixedpt =>
      DataflowDirection transfers fixedpt rewrites where
  zdfRewriteFrom :: (DebugNodes m l, Outputable a)
                 => RewritingDepth      -- whether to rewrite a rewritten graph
                 -> BlockEnv a          -- initial facts (unbound == bottom)
                 -> PassName
                 -> DataflowLattice a
                 -> transfers m l a
                 -> rewrites m l a
                 -> a                   -- fact flowing in (at entry or exit)
                 -> Graph m l
                 -> FuelMonad (fixedpt m l a (Graph m l))

-- Temporarily lifting from Graph to LGraph -- an experiment to see how we
-- can eliminate some hysteresis between Graph and LGraph.
-- Perhaps Graph should be confined to dataflow code.
-- Trading space for time
quickGraph :: LastNode l => LGraph m l -> Graph m l
quickGraph g = Graph (ZLast $ mkBranchNode $ lg_entry g) $ lg_blocks g

quickLGraph :: LastNode l => Graph m l -> FuelMonad (LGraph m l)
quickLGraph (Graph (ZLast (LastOther l)) blockenv)
    | isBranchNode l = return $ LGraph (branchNodeTarget l) blockenv
quickLGraph g = F.lGraphOfGraph g

fixptWithLGraph :: LastNode l => CommonFixedPoint m l fact (Graph m l) ->
                                 FuelMonad (CommonFixedPoint m l fact (LGraph m l))
fixptWithLGraph cfp =
  do fp_c <- quickLGraph $ fp_contents cfp
     return $ cfp {fp_contents = fp_c}

ffixptWithLGraph :: LastNode l => ForwardFixedPoint m l fact (Graph m l) ->
                                  FuelMonad (ForwardFixedPoint m l fact (LGraph m l))
ffixptWithLGraph fp =
  do common <- fixptWithLGraph $ ffp_common fp
     return $ fp {ffp_common = common}

zdfFRewriteFromL :: (DebugNodes m l, Outputable a)
               => RewritingDepth      -- whether to rewrite a rewritten graph
               -> BlockEnv a          -- initial facts (unbound == bottom)
               -> PassName
               -> DataflowLattice a
               -> ForwardTransfers m l a
               -> ForwardRewrites m l a
               -> a                   -- fact flowing in (at entry or exit)
               -> LGraph m l
               -> FuelMonad (ForwardFixedPoint m l a (LGraph m l))
zdfFRewriteFromL d b p l t r a g@(LGraph _ _) =
  do fp <- zdfRewriteFrom d b p l t r a $ quickGraph g
     ffixptWithLGraph fp

zdfBRewriteFromL :: (DebugNodes m l, Outputable a)
               => RewritingDepth      -- whether to rewrite a rewritten graph
               -> BlockEnv a          -- initial facts (unbound == bottom)
               -> PassName
               -> DataflowLattice a
               -> BackwardTransfers m l a
               -> BackwardRewrites m l a
               -> a                   -- fact flowing in (at entry or exit)
               -> LGraph m l
               -> FuelMonad (BackwardFixedPoint m l a (LGraph m l))
zdfBRewriteFromL d b p l t r a g@(LGraph _ _) =
  do fp <- zdfRewriteFrom d b p l t r a $ quickGraph g
     fixptWithLGraph fp


data RewritingDepth = RewriteShallow | RewriteDeep
-- When a transformation proposes to rewrite a node, 
-- you can either ask the system to
--  * "shallow": accept the new graph, analyse it without further rewriting
--  * "deep": recursively analyse-and-rewrite the new graph


-- There are currently four instances, but there could be more
--	forward, backward (instantiates transfers, fixedpt, rewrites)
--	Graph, AGraph     (instantiates graph)

instance DataflowDirection ForwardTransfers ForwardFixedPoint ForwardRewrites
  where zdfRewriteFrom = rewrite_f_agraph

instance DataflowDirection BackwardTransfers BackwardFixedPoint BackwardRewrites
  where zdfRewriteFrom = rewrite_b_agraph


{- =================== IMPLEMENTATIONS ===================== -}


-----------------------------------------------------------
--	solve_f: forward, pure 

solve_f         :: (DebugNodes m l, Outputable a)
                => BlockEnv a        -- initial facts (unbound == bottom)
                -> PassName
                -> DataflowLattice a -- lattice
                -> ForwardTransfers m l a   -- dataflow transfer functions
                -> a
                -> Graph m l         -- graph to be analyzed
                -> FuelMonad (ForwardFixedPoint m l a ())  -- answers
solve_f env name lattice transfers in_fact g =
   runDFM lattice $ fwd_pure_anal name env transfers in_fact g
    
rewrite_f_agraph :: (DebugNodes m l, Outputable a)
                 => RewritingDepth
                 -> BlockEnv a
                 -> PassName
                 -> DataflowLattice a
                 -> ForwardTransfers m l a
                 -> ForwardRewrites  m l a
                 -> a                 -- fact flowing in (at entry or exit)
                 -> Graph m l
                 -> FuelMonad (ForwardFixedPoint m l a (Graph m l))
rewrite_f_agraph depth start_facts name lattice transfers rewrites in_fact g =
    runDFM lattice $
    do fuel <- fuelRemaining
       (fp, fuel') <- forward_rew maybeRewriteWithFuel depth start_facts name
                      transfers rewrites in_fact g fuel
       fuelDecrement name fuel fuel'
       return fp

areturn :: AGraph m l -> DFM a (Graph m l)
areturn g = liftToDFM $ liftUniq $ graphOfAGraph g

-- | Here we prefer not simply to slap on 'goto eid' because this
-- introduces an unnecessary basic block at each rewrite, and we don't
-- want to stress out the finite map more than necessary
lgraphToGraph :: LastNode l => LGraph m l -> Graph m l
lgraphToGraph (LGraph eid blocks) =
    if flip any (eltsBlockEnv blocks) $ \block -> any (== eid) (succs block) then
        Graph (ZLast (mkBranchNode eid)) blocks
    else -- common case: entry is not a branch target
        let Block _ entry = lookupBlockEnv blocks eid `orElse` panic "missing entry!"
        in  Graph entry (delFromBlockEnv blocks eid)
    

class (Outputable m, Outputable l, LastNode l, Outputable (LGraph m l)) => DebugNodes m l

fwd_pure_anal :: (DebugNodes m l, LastNode l, Outputable a)
             => PassName
             -> BlockEnv a
             -> ForwardTransfers m l a
             -> a
             -> Graph m l
             -> DFM a (ForwardFixedPoint m l a ())

fwd_pure_anal name env transfers in_fact g =
    do (fp, _) <- anal_f name env transfers panic_rewrites in_fact g panic_fuel
       return fp
  where -- definitely a case of "I love lazy evaluation"
    anal_f = forward_sol (\_ _ -> Nothing) panic_depth
    panic_rewrites = panic "pure analysis asked for a rewrite function"
    panic_fuel     = panic "pure analysis asked for fuel"
    panic_depth    = panic "pure analysis asked for a rewrite depth"

-----------------------------------------------------------------------
--
--	Here beginneth the super-general functions
--
--  Think of them as (typechecked) macros
--   *  They are not exported
--
--   *  They are called by the specialised wrappers
--	above, and always inlined into their callers
--
-- There are four functions, one for each combination of:
--	Forward, Backward
--	Solver, Rewriter
--
-- A "solver" produces a (DFM f (f, Fuel)), 
--	where f is the fact at entry(Bwd)/exit(Fwd)
--	and from the DFM you can extract 
--		the BlockId->f
--		the change-flag
--		and more besides
--
-- A "rewriter" produces a rewritten *Graph* as well
--
-- Both constrain their rewrites by 
--	a) Fuel
--	b) RewritingDepth: shallow/deep

-----------------------------------------------------------------------

type Fuel = OptimizationFuel

forward_sol
        :: forall m l a . 
           (DebugNodes m l, LastNode l, Outputable a)
        => (forall a . Fuel -> Maybe a -> Maybe a)
		-- Squashes proposed rewrites if there is
		-- no more fuel; OR if we are doing a pure
		-- analysis, so totally ignore the rewrite
		-- ie. For pure-analysis the fn is (\_ _ -> Nothing)
        -> RewritingDepth	-- Shallow/deep
        -> PassName
        -> BlockEnv a		-- Initial set of facts
        -> ForwardTransfers m l a
        -> ForwardRewrites m l a
        -> a			-- Entry fact
        -> Graph m l
        -> Fuel
        -> DFM a (ForwardFixedPoint m l a (), Fuel)
forward_sol check_maybe = forw
 where
  forw :: RewritingDepth
       -> PassName
       -> BlockEnv a
       -> ForwardTransfers m l a
       -> ForwardRewrites m l a
       -> a
       -> Graph m l
       -> Fuel
       -> DFM a (ForwardFixedPoint m l a (), Fuel)
  forw rewrite name start_facts transfers rewrites =
   let anal_f :: DFM a b -> a -> Graph m l -> DFM a b
       anal_f finish in' g =
           do { _ <- fwd_pure_anal name emptyBlockEnv transfers in' g; finish }

       solve :: DFM a b -> a -> Graph m l -> Fuel -> DFM a (b, Fuel)
       solve finish in_fact (Graph entry blockenv) fuel =
         let blocks = G.postorder_dfs_from blockenv entry
             set_or_save = mk_set_or_save (isJust . lookupBlockEnv blockenv)
             set_successor_facts (Block id tail) fuel =
               do { idfact <- getFact id
                  ; (last_outs, fuel) <- rec_rewrite (fr_first rewrites id idfact)
                                                (ft_first_out transfers id idfact)
                                                getExitFact (solve_tail tail)
                                                (solve_tail tail) idfact fuel
                  ; set_or_save last_outs
                  ; return fuel }
         in do { (last_outs, fuel) <- solve_tail entry in_fact fuel
                   -- last_outs contains a mix of internal facts, which
                   -- are inputs to 'run', and external facts, which
                   -- are going to be forgotten by 'run'
               ; set_or_save last_outs
               ; fuel <- run "forward" name set_successor_facts blocks fuel
               ; set_or_save last_outs
                   -- Re-set facts that may have been forgotten by run
               ; b <-  finish
               ; return (b, fuel)
               }

       -- The need for both k1 and k2 suggests that maybe there's an opportunity
       -- for improvement here -- in most cases, they're the same...
       rec_rewrite :: forall t bI bW.
                      Maybe (AGraph m l) -> t -> DFM a bW
                   -> (t -> Fuel -> DFM a bI)
                   -> (bW -> Fuel -> DFM a bI)
                   -> a -> Fuel -> DFM a bI
       rec_rewrite rewritten analyzed finish k1 k2 in' fuel =
         case check_maybe fuel rewritten of -- fr_first rewrites id idfact of
           Nothing -> k1 analyzed fuel
           Just g -> do g <- areturn g
                        (a, fuel) <- subAnalysis' $
                          case rewrite of
                            RewriteDeep -> solve finish in' g (oneLessFuel fuel)
                            RewriteShallow -> do { a <- anal_f finish in' g
                                                 ; return (a, oneLessFuel fuel) }
                        k2 a fuel
       solve_tail (G.ZTail m t) in' fuel =
         rec_rewrite (fr_middle rewrites m in') (ft_middle_out transfers m in')
                     getExitFact (solve_tail t) (solve_tail t) in' fuel
       solve_tail (G.ZLast (LastOther l)) in' fuel = 
         rec_rewrite (fr_last rewrites l in') (ft_last_outs transfers l in')
                     lastOutFacts k k in' fuel
           where k a b = return (a, b)
       solve_tail (G.ZLast LastExit) in' fuel =
         rec_rewrite (fr_exit rewrites in') (ft_exit_out transfers in')
                     lastOutFacts k (\a b -> return (a, b)) in' fuel
           where k a fuel = do { setExitFact a ; return (LastOutFacts [], fuel) }

       fixed_point in_fact g fuel =
         do { setAllFacts start_facts
            ; (a, fuel) <- solve getExitFact in_fact g fuel
            ; facts <- getAllFacts
            ; last_outs <- lastOutFacts
            ; let cfp = FP facts a NoChange (panic "no decoration?!") ()
            ; let fp = FFP cfp last_outs
            ; return (fp, fuel)
            }
   in fixed_point




mk_set_or_save :: (DataflowAnalysis df, Monad (df a), Outputable a) =>
                  (BlockId -> Bool) -> LastOutFacts a -> df a ()
mk_set_or_save is_local (LastOutFacts l) = mapM_ set_or_save_one l
    where set_or_save_one (id, a) =
              if is_local id then setFact id a else addLastOutFact (id, a)



forward_rew
        :: forall m l a . 
           (DebugNodes m l, LastNode l, Outputable a)
        => (forall a . Fuel -> Maybe a -> Maybe a)
        -> RewritingDepth
        -> BlockEnv a
        -> PassName
        -> ForwardTransfers m l a
        -> ForwardRewrites m l a
        -> a
        -> Graph m l
        -> Fuel
        -> DFM a (ForwardFixedPoint m l a (Graph m l), Fuel)
forward_rew check_maybe = forw
  where
    forw :: RewritingDepth
         -> BlockEnv a
         -> PassName
         -> ForwardTransfers m l a
         -> ForwardRewrites m l a
         -> a
         -> Graph m l
         -> Fuel
         -> DFM a (ForwardFixedPoint m l a (Graph m l), Fuel)
    forw depth xstart_facts name transfers rewrites in_factx gx fuelx =
      let rewrite :: BlockEnv a -> DFM a b
                  -> a -> Graph m l -> Fuel
                  -> DFM a (b, Graph m l, Fuel)
          rewrite start finish in_fact g fuel =
           in_fact `seq` g `seq`
            let Graph entry blockenv = g
                blocks = G.postorder_dfs_from blockenv entry
            in do { _ <- forward_sol check_maybe depth name start 
                                     transfers rewrites in_fact g fuel
                  ; eid <- freshBlockId "temporary entry id"
                  ; (rewritten, fuel) <-
                      rew_tail (ZFirst eid) in_fact entry emptyBlockEnv fuel
                  ; (rewritten, fuel) <- rewrite_blocks blocks rewritten fuel
                  ; a <- finish
                  ; return (a, lgraphToGraph (LGraph eid rewritten), fuel)
                  }

          don't_rewrite :: forall t.
                           BlockEnv a -> DFM a t -> a
                        -> Graph m l -> Fuel
                        -> DFM a (t, Graph m l, Fuel)
          don't_rewrite facts finish in_fact g fuel =
              do  { _ <- forward_sol check_maybe depth name facts 
                                     transfers rewrites in_fact g fuel
                  ; a <- finish
                  ; return (a, g, fuel)
                  }

          inner_rew :: DFM a f -> a -> Graph m l -> Fuel -> DFM a (f, Graph m l, Fuel)
          inner_rew f i g fu = getAllFacts >>= \facts -> inner_rew' facts f i g fu
              where inner_rew' = case depth of RewriteShallow -> don't_rewrite
                                               RewriteDeep    -> rewrite
          fixed_pt_and_fuel =
              do { (a, g, fuel) <- rewrite xstart_facts getExitFact in_factx gx fuelx
                 ; facts <- getAllFacts
                 ; changed <- graphWasRewritten
                 ; last_outs <- lastOutFacts
                 ; let cfp = FP facts a changed (panic "no decoration?!") g
                 ; let fp = FFP cfp last_outs
                 ; return (fp, fuel)
                 }

-- JD: WHY AREN'T WE TAKING ANY FUEL HERE?
          rewrite_blocks :: [Block m l] -> (BlockEnv (Block m l))
                         -> Fuel -> DFM a (BlockEnv (Block m l), Fuel)
          rewrite_blocks [] rewritten fuel = return (rewritten, fuel)
          rewrite_blocks (G.Block id t : bs) rewritten fuel =
            do let h = ZFirst id
               a <- getFact id
               case check_maybe fuel $ fr_first rewrites id a of
                 Nothing -> do { (rewritten, fuel) <-
                                    rew_tail h (ft_first_out transfers id a)
                                             t rewritten fuel
                               ; rewrite_blocks bs rewritten fuel }
                 Just g  -> do { markGraphRewritten
                               ; g <- areturn g
                               ; (outfact, g, fuel) <- inner_rew getExitFact a g fuel
                               ; let (blocks, h) = splice_head' h g
                               ; (rewritten, fuel) <-
                                 rew_tail h outfact t (blocks `plusBlockEnv` rewritten) fuel
                               ; rewrite_blocks bs rewritten fuel }

          rew_tail head in' (G.ZTail m t) rewritten fuel =
           in' `seq` rewritten `seq`
            my_trace "Rewriting middle node" (ppr m) $
            case check_maybe fuel $ fr_middle rewrites m in' of
              Nothing -> rew_tail (G.ZHead head m) (ft_middle_out transfers m in') t
                                  rewritten fuel
              Just g -> do { markGraphRewritten
                           ; g <- areturn g
                           ; (a, g, fuel) <- inner_rew getExitFact in' g fuel
                           ; let (blocks, h) = G.splice_head' head g
                           ; rew_tail h a t (blocks `plusBlockEnv` rewritten) fuel
                           }
          rew_tail h in' (G.ZLast l) rewritten fuel = 
           in' `seq` rewritten `seq`
            my_trace "Rewriting last node" (ppr l) $
            case check_maybe fuel $ either_last rewrites in' l of
              Nothing -> do check_facts in' l
                            return (insertBlock (zipht h (G.ZLast l)) rewritten, fuel)
              Just g ->  do { markGraphRewritten
                           ; g <- areturn g
                           ; ((), g, fuel) <-
                               my_trace "Just" (ppr g) $ inner_rew (return ()) in' g fuel
                           ; let g' = G.splice_head_only' h g
                           ; return (G.lg_blocks g' `plusBlockEnv` rewritten, fuel)
                           }
          either_last rewrites in' (LastExit) = fr_exit rewrites in'
          either_last rewrites in' (LastOther l) = fr_last rewrites l in'
          check_facts in' (LastOther l) =
            let LastOutFacts last_outs = ft_last_outs transfers l in'
            in mapM_ (uncurry checkFactMatch) last_outs
          check_facts _ LastExit = return ()
      in  fixed_pt_and_fuel

lastOutFacts :: DFM f (LastOutFacts f)
lastOutFacts = bareLastOutFacts >>= return . LastOutFacts

{- ================================================================ -}

solve_b         :: (DebugNodes m l, Outputable a)
                => BlockEnv a        -- initial facts (unbound == bottom)
                -> PassName
                -> DataflowLattice a -- lattice
                -> BackwardTransfers m l a   -- dataflow transfer functions
                -> a                 -- exit fact
                -> Graph m l         -- graph to be analyzed
                -> FuelMonad (BackwardFixedPoint m l a ())  -- answers
solve_b env name lattice transfers exit_fact g =
   runDFM lattice $ bwd_pure_anal name env transfers g exit_fact
    

rewrite_b_agraph :: (DebugNodes m l, Outputable a)
                 => RewritingDepth
                 -> BlockEnv a
                 -> PassName
                 -> DataflowLattice a
                 -> BackwardTransfers m l a
                 -> BackwardRewrites m l a
                 -> a                 -- fact flowing in at exit
                 -> Graph m l
                 -> FuelMonad (BackwardFixedPoint m l a (Graph m l))
rewrite_b_agraph depth start_facts name lattice transfers rewrites exit_fact g =
    runDFM lattice $
    do fuel <- fuelRemaining
       (fp, fuel') <- backward_rew maybeRewriteWithFuel depth start_facts name
                      transfers rewrites g exit_fact fuel
       fuelDecrement name fuel fuel'
       return fp



backward_sol
        :: forall m l a . 
           (DebugNodes m l, LastNode l, Outputable a)
        => (forall a . Fuel -> Maybe a -> Maybe a)
        -> RewritingDepth
        -> PassName
        -> BlockEnv a
        -> BackwardTransfers m l a
        -> BackwardRewrites m l a
        -> Graph m l
        -> a
        -> Fuel
        -> DFM a (BackwardFixedPoint m l a (), Fuel)
backward_sol check_maybe = back
 where
  back :: RewritingDepth
       -> PassName
       -> BlockEnv a
       -> BackwardTransfers m l a
       -> BackwardRewrites m l a
       -> Graph m l
       -> a
       -> Fuel
       -> DFM a (BackwardFixedPoint m l a (), Fuel)
  back rewrite name start_facts transfers rewrites =
   let anal_b :: Graph m l -> a -> DFM a a
       anal_b g out =
           do { fp <- bwd_pure_anal name emptyBlockEnv transfers g out
              ; return $ zdfFpOutputFact fp }

       subsolve :: AGraph m l -> a -> Fuel -> DFM a (a, Fuel)
       subsolve =
         case rewrite of
           RewriteDeep    -> \g a fuel ->
               subAnalysis' $ do { g <- areturn g; solve g a (oneLessFuel fuel) }
           RewriteShallow -> \g a fuel ->
               subAnalysis' $ do { g <- areturn g; a <- anal_b g a
                                 ; return (a, oneLessFuel fuel) }

       solve :: Graph m l -> a -> Fuel -> DFM a (a, Fuel)
       solve (Graph entry blockenv) exit_fact fuel =
         let blocks = reverse $ G.postorder_dfs_from blockenv entry
             last_in  _env (LastExit)    = exit_fact
             last_in   env (LastOther l) = bt_last_in transfers l env
             last_rew _env (LastExit)    = br_exit rewrites 
             last_rew  env (LastOther l) = br_last rewrites l env
             set_block_fact block fuel =
                 let (h, l) = G.goto_end (G.unzip block) in
                 do { env <- factsEnv
                    ; (a, fuel) <-
                      case check_maybe fuel $ last_rew env l of
                        Nothing -> return (last_in env l, fuel)
                        Just g -> do g' <- areturn g
                                     my_trace "analysis rewrites last node"
                                      (ppr l <+> pprGraph g') $
                                      subsolve g exit_fact fuel
                    ; _ <- set_head_fact h a fuel
                    ; return fuel }

         in do { fuel <- run "backward" name set_block_fact blocks fuel
               ; eid <- freshBlockId "temporary entry id"
               ; fuel <- set_block_fact (Block eid entry) fuel
               ; a <- getFact eid
               ; forgetFact eid
               ; return (a, fuel)
               }

       set_head_fact (G.ZFirst id) a fuel =
         case check_maybe fuel $ br_first rewrites id a of
           Nothing -> do { my_trace "set_head_fact" (ppr id <+> text "=" <+>
                                                     ppr (bt_first_in transfers id a)) $
                           setFact id $ bt_first_in transfers id a
                         ; return fuel }
           Just g  -> do { g' <- areturn g
                         ; (a, fuel) <- my_trace "analysis rewrites first node"
                                      (ppr id <+> pprGraph g') $
                                      subsolve g a fuel
                         ; setFact id $ bt_first_in transfers id a
                         ; return fuel
                         }
       set_head_fact (G.ZHead h m) a fuel =
         case check_maybe fuel $ br_middle rewrites m a of
           Nothing -> set_head_fact h (bt_middle_in transfers m a) fuel
           Just g -> do { g' <- areturn g
                        ; (a, fuel) <- my_trace "analysis rewrites middle node"
                                      (ppr m <+> pprGraph g') $
                                      subsolve g a fuel
                        ; set_head_fact h a fuel }

       fixed_point g exit_fact fuel =
         do { setAllFacts start_facts
            ; (a, fuel) <- solve g exit_fact fuel
            ; facts <- getAllFacts
            ; let cfp = FP facts a NoChange (panic "no decoration?!") ()
            ; return (cfp, fuel)
            }
   in fixed_point

bwd_pure_anal :: (DebugNodes m l, LastNode l, Outputable a)
             => PassName
             -> BlockEnv a
             -> BackwardTransfers m l a
             -> Graph m l
             -> a
             -> DFM a (BackwardFixedPoint m l a ())

bwd_pure_anal name env transfers g exit_fact =
    do (fp, _) <- anal_b name env transfers panic_rewrites g exit_fact panic_fuel
       return fp
  where -- another case of "I love lazy evaluation"
    anal_b = backward_sol (\_ _ -> Nothing) panic_depth
    panic_rewrites = panic "pure analysis asked for a rewrite function"
    panic_fuel     = panic "pure analysis asked for fuel"
    panic_depth    = panic "pure analysis asked for a rewrite depth"


{- ================================================================ -}

backward_rew
        :: forall m l a . 
           (DebugNodes m l, LastNode l, Outputable a)
        => (forall a . Fuel -> Maybe a -> Maybe a)
        -> RewritingDepth
        -> BlockEnv a
        -> PassName
        -> BackwardTransfers m l a
        -> BackwardRewrites m l a
        -> Graph m l
        -> a
        -> Fuel
        -> DFM a (BackwardFixedPoint m l a (Graph m l), Fuel)
backward_rew check_maybe = back
  where
    solve = backward_sol check_maybe
    back :: RewritingDepth
         -> BlockEnv a
         -> PassName
         -> BackwardTransfers m l a
         -> BackwardRewrites m l a
         -> Graph m l
         -> a
         -> Fuel
         -> DFM a (BackwardFixedPoint m l a (Graph m l), Fuel)
    back depth xstart_facts name transfers rewrites gx exit_fact fuelx =
      let rewrite :: BlockEnv a
                  -> Graph m l -> a -> Fuel
                  -> DFM a (a, Graph m l, Fuel)
          rewrite start g exit_fact fuel =
           let Graph entry blockenv = g
               blocks = reverse $ G.postorder_dfs_from blockenv entry
           in do { (FP _ in_fact _ _ _, _) <-    -- don't drop the entry fact!
                     solve depth name start transfers rewrites g exit_fact fuel
                 --; env <- getAllFacts
                 -- ; my_trace "facts after solving" (ppr env) $ return ()
                 ; eid <- freshBlockId "temporary entry id"
                 ; (rewritten, fuel) <- rewrite_blocks True blocks emptyBlockEnv fuel
                 -- We can't have the fact check fail on the bogus entry, which _may_ change
                 ; (rewritten, fuel) <-
                     rewrite_blocks False [Block eid entry] rewritten fuel
                 ; my_trace "eid" (ppr eid) $ return ()
                 ; my_trace "exit_fact" (ppr exit_fact) $ return ()
                 ; my_trace "in_fact" (ppr in_fact) $ return ()
                 ; return (in_fact, lgraphToGraph (LGraph eid rewritten), fuel)
                 } -- Remember: the entry fact computed by @solve@ accounts for rewriting
          don't_rewrite facts g exit_fact fuel =
            do { (fp, _) <-
                     solve depth name facts transfers rewrites g exit_fact fuel
               ; return (zdfFpOutputFact fp, g, fuel) }
          inner_rew :: Graph m l -> a -> Fuel -> DFM a (a, Graph m l, Fuel)
          inner_rew g a f = getAllFacts >>= \facts -> inner_rew' facts g a f
              where inner_rew' = case depth of RewriteShallow -> don't_rewrite
                                               RewriteDeep    -> rewrite
          fixed_pt_and_fuel =
              do { (a, g, fuel) <- rewrite xstart_facts gx exit_fact fuelx
                 ; facts <- getAllFacts
                 ; changed <- graphWasRewritten
                 ; let fp = FP facts a changed (panic "no decoration?!") g
                 ; return (fp, fuel)
                 }
          rewrite_blocks :: Bool -> [Block m l] -> (BlockEnv (Block m l))
                         -> Fuel -> DFM a (BlockEnv (Block m l), Fuel)
          rewrite_blocks check bs rewritten fuel =
              do { env <- factsEnv
                 ; let rew [] r f = return (r, f)
                       rew (b : bs) r f =
                           do { (r, f) <- rewrite_block check env b r f; rew bs r f }
                 ; rew bs rewritten fuel }
          rewrite_block check env b rewritten fuel =
            let (h, l) = G.goto_end (G.unzip b) in
            case maybeRewriteWithFuel fuel $ either_last env l of
              Nothing -> propagate check fuel h (last_in env l) (ZLast l) rewritten
              Just g ->
                do { markGraphRewritten
                   ; g <- areturn g
                   ; (a, g, fuel) <- inner_rew g exit_fact fuel
                   ; let G.Graph t new_blocks = g
                   ; let rewritten' = new_blocks `plusBlockEnv` rewritten
                   ; propagate check fuel h a t rewritten' -- continue at entry of g
                   } 
          either_last _env (LastExit)    = br_exit rewrites 
          either_last  env (LastOther l) = br_last rewrites l env
          last_in _env (LastExit)    = exit_fact
          last_in  env (LastOther l) = bt_last_in transfers l env
          propagate check fuel (ZHead h m) a tail rewritten =
            case maybeRewriteWithFuel fuel $ br_middle rewrites m a of
              Nothing ->
                propagate check fuel h (bt_middle_in transfers m a) (ZTail m tail) rewritten
              Just g  ->
                do { markGraphRewritten
                   ; g <- areturn g
                   ; my_trace "With Facts" (ppr a) $ return ()
                   ; my_trace "  Rewrote middle node"
                                             (f4sep [ppr m, text "to", pprGraph g]) $
                     return ()
                   ; (a, g, fuel) <- inner_rew g a fuel
                   ; let Graph t newblocks = G.splice_tail g tail
                   ; my_trace "propagating facts" (ppr a) $
                     propagate check fuel h a t (newblocks `plusBlockEnv` rewritten) }
          propagate check fuel (ZFirst id) a tail rewritten =
            case maybeRewriteWithFuel fuel $ br_first rewrites id a of
              Nothing -> do { if check then
                                checkFactMatch id $ bt_first_in transfers id a
                              else return ()
                            ; return (insertBlock (Block id tail) rewritten, fuel) }
              Just g ->
                do { markGraphRewritten
                   ; g <- areturn g
                   ; my_trace "Rewrote first node"
                     (f4sep [ppr id <> colon, text "to", pprGraph g]) $ return ()
                   ; (a, g, fuel) <- inner_rew g a fuel
                   ; if check then checkFactMatch id (bt_first_in transfers id a)
                     else return ()
                   ; let Graph t newblocks = G.splice_tail g tail
                   ; let r = insertBlock (Block id t) (newblocks `plusBlockEnv` rewritten)
                   ; return (r, fuel) }
      in  fixed_pt_and_fuel

{- ================================================================ -}

instance FixedPoint CommonFixedPoint where
    zdfFpFacts        = fp_facts
    zdfFpOutputFact   = fp_out
    zdfGraphChanged   = fp_changed
    zdfDecoratedGraph = fp_dec_graph
    zdfFpContents     = fp_contents
    zdfFpMap f (FP fs out ch dg a) = FP fs out ch dg (f a)

instance FixedPoint ForwardFixedPoint where
    zdfFpFacts        = fp_facts     . ffp_common
    zdfFpOutputFact   = fp_out       . ffp_common
    zdfGraphChanged   = fp_changed   . ffp_common
    zdfDecoratedGraph = fp_dec_graph . ffp_common
    zdfFpContents     = fp_contents  . ffp_common
    zdfFpMap f (FFP fp los) = FFP (zdfFpMap f fp) los


dump_things :: Bool
dump_things = False

my_trace :: String -> SDoc -> a -> a
my_trace = if dump_things then pprTrace else \_ _ a -> a


-- | Here's a function to run an action on blocks until we reach a fixed point.
run :: (Outputable a, DebugNodes m l) =>
       String -> String -> (Block m l -> b -> DFM a b) -> [Block m l] -> b -> DFM a b
run dir name do_block blocks b =
   do { show_blocks $ iterate (1::Int) }
   where
     -- N.B. Each iteration starts with the same transaction limit;
     -- only the rewrites in the final iteration actually count
     trace_block (b, cnt) block =
         do b' <- my_trace "about to do" (text name <+> text "on" <+>
                     ppr (blockId block) <+> ppr cnt) $
                    do_block block b
            return (b', cnt + 1)
     iterate n = 
         do { forgetLastOutFacts
            ; markFactsUnchanged
            ; (b, _) <- foldM trace_block (b, 0 :: Int) blocks
            ; changed <- factsStatus
            ; facts <- getAllFacts
            ; let depth = 0 -- was nesting depth
            ; ppIter depth n $
              case changed of
                NoChange -> unchanged depth $ return b
                SomeChange ->
                    pprFacts depth n facts $ 
                    if n < 1000 then iterate (n+1)
                    else panic $ msg n
            }
     msg n = concat [name, " didn't converge in ", show n, " " , dir,
                     " iterations"]
     my_nest depth sdoc = my_trace "" $ nest (3*depth) sdoc
     ppIter depth n = my_nest depth (empty $$ text "*************** iteration" <+> pp_i n)
     pp_i n = int n <+> text "of" <+> text name <+> text "on" <+> graphId
     unchanged depth =
       my_nest depth (text "facts for" <+> graphId <+> text "are unchanged")

     graphId = case blocks of { Block id _ : _ -> ppr id ; [] -> text "<empty>" }
     show_blocks = my_trace "Blocks:" (vcat (map pprBlock blocks))
     pprBlock (Block id t) = nest 2 (pprFact (id, t))
     pprFacts depth n env =
         my_nest depth (text "facts for iteration" <+> pp_i n <+> text "are:" $$
                        (nest 2 $ vcat $ map pprFact $ blockEnvToList env))

pprFact :: (Outputable a, Outputable b) => (a,b) -> SDoc
pprFact (id, a) = hang (ppr id <> colon) 4 (ppr a)

f4sep :: [SDoc] -> SDoc
f4sep [] = fsep []
f4sep (d:ds) = fsep (d : map (nest 4) ds)


subAnalysis' :: (Monad (m f), DataflowAnalysis m, Outputable f) =>
                m f a -> m f a
subAnalysis' m =
    do { a <- subAnalysis $
               do { a <- m; -- facts <- getAllFacts
                  ; -- my_trace "after sub-analysis facts are" (pprFacts facts) $
                    return a }
       -- ; facts <- getAllFacts
       ; -- my_trace "in parent analysis facts are" (pprFacts facts) $
         return a }
  -- where pprFacts env = nest 2 $ vcat $ map pprFact $ blockEnvToList env
        -- pprFact (id, a) = hang (ppr id <> colon) 4 (ppr a)
