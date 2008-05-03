{-# LANGUAGE MultiParamTypeClasses #-}
module ZipDataflow0
  ( Answer(..)
  , BComputation(..), BAnalysis, BTransformation, BFunctionalTransformation
        , BPass, BUnlimitedPass
  , FComputation(..), FAnalysis, FTransformation, FFunctionalTransformation
        , FPass, FUnlimitedPass
  , LastOutFacts(..)
  , DebugNodes
  , anal_b, a_t_b, a_ft_b, a_ft_b_unlimited, ignore_transactions_b
  , anal_f, a_t_f 
  , null_f_ft, null_b_ft
  , run_b_anal, run_f_anal
  , refine_f_anal, refine_b_anal, fold_edge_facts_b, fold_edge_facts_with_nodes_b
  , b_rewrite, f_rewrite, b_shallow_rewrite, f_shallow_rewrite
  , solve_graph_b, solve_graph_f
  )
where

import CmmTx
import DFMonad
import ZipCfg
import qualified ZipCfg as G

import Outputable
import Panic
import UniqFM
import UniqSupply

import Control.Monad
import Maybe

#include "HsVersions.h"

{-

\section{A very polymorphic infrastructure for dataflow problems}

This module presents a framework for solving iterative dataflow
problems. 
There are two major submodules: one for forward problems and another
for backward problems.
Both modules incorporate the composition framework developed by
Lerner, Grove, and Chambers.
They also support a \emph{transaction limit}, which enables the
binary-search debugging technique developed by Whalley and Davidson
under the name \emph{vpoiso}.
Transactions may either be known to the individual dataflow solvers or
may be managed by the framework.
-}

-- | In the composition framework, a pass either produces a dataflow
-- fact or proposes to rewrite the graph.  To make life easy for the
-- clients, the rewrite is given in unlabelled form, but we use
-- labelled form internally throughout, because it greatly simplifies
-- the implementation not to have the first block be a special case
-- edverywhere.

data Answer m l a = Dataflow a | Rewrite (Graph m l)


{-

============== Descriptions of dataflow passes} ================

------ Passes for backward dataflow problemsa

The computation of a fact is the basis of a dataflow pass.
A computation takes *four* type parameters:

  * 'middle' and 'last' are the types of the middle
    and last nodes of the graph over which the dataflow
    solution is being computed

  * 'input' is an input, from which it should be possible to
     derive a dataflow fact of interest.  For example, 'input' might
     be equal to a fact, or it might be a tuple of which one element
     is a fact.

  * 'output' is an output, or possibly a function from 'fuel' to an
    output

A computation is interesting for any pair of 'middle' and 'last' type
parameters that can form a reasonable graph.  But it is not useful to
instantiate 'input' and 'output' arbitrarily.  Rather, only certain
combinations of instances are likely to be useful, such as those shown
below.

Backward analyses compute *in* facts (facts on inedges). 
-}

-- A dataflow pass requires a name and a transfer function for each of
-- four kinds of nodes: 
--	first (the BlockId), 
--	middle
--	last 
--	LastExit  

-- A 'BComputation' describes a complete backward dataflow pass, as a
-- record of transfer functions.  Because the analysis works
-- back-to-front, we write the exit node at the beginning.
-- 
-- So there is
--	an 'input' for each out-edge of the node
--		(hence (BlockId -> input) for bc_last_in)
--	an 'output' for the in-edge of the node

data BComputation middle last input output = BComp
   { bc_name      :: String
   , bc_exit_in   ::                                  output
   , bc_last_in   :: (BlockId -> input) -> last    -> output
   , bc_middle_in :: input              -> middle  -> output
   , bc_first_in  :: input              -> BlockId -> output
   } 

-- | From these elements we build several kinds of passes:
--     * A pure analysis computes a fact, using that fact as input and output.
--     * A pure transformation computes no facts but only changes the graph.
--     * A fully general pass both computes a fact and rewrites the graph,
--       respecting the current transaction limit.
--
type BAnalysis                 m l a = BComputation m l a a
type BTransformation           m l a = BComputation m l a (Maybe (UniqSM (Graph m l)))
type BFunctionalTransformation m l a = BComputation m l a (Maybe         (Graph m l))
	-- ToDo: consider replacing UniqSM (Graph l m) with (AGraph m l)

type BPass          m l a = BComputation m l a (OptimizationFuel -> DFM a (Answer m l a))
type BUnlimitedPass m l a = BComputation m l a (                    DFM a (Answer m l a))

	-- (DFM a t) maintains the (BlockId -> a) map
	-- ToDo: overlap with bc_last_in??

{-
\paragraph{Passes for forward dataflow problems}

A forward dataflow pass has a similar structure, but the details are
different.  In particular, the output fact from a [[last]] node has a
higher-order representation: it takes a function that mutates a
[[uid]] to account for the new fact, then performs the necessary
mutation on every successor of the last node.  We therefore have two
kinds of type parameter for outputs: output from a [[middle]] node
is~[[outmid]], and output from a [[last]] node is~[[outlast]].
-}

data FComputation middle last input outmid outlast = FComp
 { fc_name       :: String 
 , fc_first_out  :: input -> BlockId   -> outmid
 , fc_middle_out :: input -> middle    -> outmid
 , fc_last_outs  :: input -> last      -> outlast
 , fc_exit_out   :: input              -> outmid
 } 

-- | The notions of analysis, pass, and transformation are analogous to the
-- backward case.

newtype LastOutFacts a = LastOutFacts [(BlockId, a)] 
  -- ^ These are facts flowing out of a last node to the node's successors.
  -- They are either to be set (if they pertain to the graph currently
  -- under analysis) or propagated out of a sub-analysis

type FAnalysis m l a       = FComputation m l a a (LastOutFacts a)
type FTransformation m l a = FComputation m l a (Maybe (UniqSM (Graph m l)))
                                                (Maybe (UniqSM (Graph m l)))
type FFunctionalTransformation m l a =
                             FComputation m l a (Maybe (Graph m l))
                                                (Maybe (Graph m l))
	-- ToDo: consider replacing UniqSM (Graph l m) with (AGraph m l)

type FPass m l a           = FComputation m l a
                                (OptimizationFuel -> DFM a (Answer m l a))
                                (OptimizationFuel -> DFM a (Answer m l (LastOutFacts a)))

type FUnlimitedPass m l a  = FComputation m l a
                                (DFM a (Answer m l a))
                                (DFM a (Answer m l (LastOutFacts a)))

{-
\paragraph{Composing passes}

Both forward and backward engines share a handful of functions for
composing analyses, transformations, and passes.

We can make an analysis pass, or we can 
combine a related analysis and transformation into a full pass.
-}

null_b_ft :: BFunctionalTransformation m l a
null_f_ft :: FFunctionalTransformation m l a

anal_b :: BAnalysis m l a -> BPass m l a
a_t_b  :: BAnalysis m l a -> BTransformation           m l a -> BPass m l a
a_ft_b :: BAnalysis m l a -> BFunctionalTransformation m l a -> BPass m l a
a_ft_b_unlimited
       :: BAnalysis m l a -> BFunctionalTransformation m l a -> BPass m l a
  -- ^ Ignores transaction limits.  Could produce a BUnlimitedPass statically,
  -- but that would cost too much code in the implementation for a
  -- static distinction that is not worth so much. 
ignore_transactions_b :: BUnlimitedPass m l a -> BPass m l a



anal_f :: FAnalysis m l a -> FPass m l a
a_t_f  :: FAnalysis m l a -> FTransformation m l a -> FPass m l a


{-
\paragraph {Running the dataflow engine}

Every function for running analyses has two forms, because for a
forward analysis, we supply an entry fact, whereas for a backward
analysis, we don't need to supply an exit fact (because a graph for a
procedure doesn't have an exit node).
It's possible we could make these things more regular.
-}

-- | The analysis functions set properties on unique IDs.

run_b_anal :: (DebugNodes m l, LastNode l, Outputable a) =>
              BAnalysis m l a ->      LGraph m l -> DFA a ()
run_f_anal :: (DebugNodes m l, LastNode l, Outputable a) =>
              FAnalysis m l a -> a -> LGraph m l -> DFA a ()
                              -- ^ extra parameter is the entry fact

-- | Rematerialize results of analysis for use elsewhere.  Simply applies a
-- fold function to every edge fact, in reverse postorder dfs.  The facts
-- should already have been computed into the monady by run_b_anal or b_rewrite.
fold_edge_facts_b
    :: LastNode l =>
       (a -> b -> b) -> BAnalysis m l a -> LGraph m l -> (BlockId -> a) -> b -> b

fold_edge_facts_with_nodes_b :: LastNode l
                             => (l -> a -> b -> b)  -- ^ inedge to last node
                             -> (m -> a -> b -> b)  -- ^ inedge to middle node
                             -> (BlockId -> a -> b -> b) -- ^ fact at label
                             -> BAnalysis m l a          -- ^ backwards analysis
                             -> LGraph m l               -- ^ graph
                             -> (BlockId -> a)           -- ^ solution to bwd anal
                             -> b -> b


-- | It can be useful to refine the results of an existing analysis,
-- or for example to use the outcome of a forward analsysis in a
-- backward analysis.  These functions can also be used to compute a
-- fixed point iteratively starting from somewhere other than bottom
-- (as in the reachability analysis done for proc points).

class (Outputable m, Outputable l, LastNode l) => DebugNodes m l

refine_f_anal :: (DebugNodes m l, LastNode l, Outputable a) =>
        FAnalysis m l a -> LGraph m l -> DFA a () -> DFA a ()

refine_b_anal :: (DebugNodes m l, LastNode l, Outputable a) =>
        BAnalysis m l a -> LGraph m l -> DFA a () -> DFA a ()

b_rewrite :: (DebugNodes m l, Outputable a) =>
             BPass m l a ->      LGraph m l -> DFM a (LGraph m l)
f_rewrite :: (DebugNodes m l, LastNode l, Outputable m, Outputable a) =>
             FPass m l a -> a -> LGraph m l -> DFM a (LGraph m l)
                    -- ^ extra parameter is the entry fact

b_shallow_rewrite
    :: (DebugNodes m l, Outputable a)
    => BAnalysis m l a -> BFunctionalTransformation m l a ->
       Graph m l -> DFM a (Graph m l)

b_shallow_rewrite = error "unimp"

f_shallow_rewrite
    :: (DebugNodes m l, Outputable a)
    => FAnalysis m l a -> FFunctionalTransformation m l a ->
       a -> Graph m l -> DFM a (Graph m l)


-- | If the solution to a problem is already sitting in a monad, we
-- should be able to take a short cut and just rewrite it in one pass.
-- But not yet implemented.

{-
f_rewrite_solved :: (LastNode l, Outputable m, Outputable a) =>
                    FPass m l a -> DFM a () -> LGraph m l -> DFM a (LGraph m l)
b_rewrite_solved :: (LastNode l, Outputable m, Outputable a) =>
                    BPass m l a -> DFM a () -> LGraph m l -> DFM a (LGraph m l)
-}

-- ===================== IMPLEMENTATION ======================--

-- | Here's a function to run an action on blocks until we reach a fixed point.
run :: (DataflowAnalysis anal, Monad (anal a), Outputable a, DebugNodes m l) =>
       String -> String -> anal a () -> (b -> Block m l -> anal a b) ->
       b -> [Block m l] -> anal a b
run dir name set_entry do_block b blocks =
   do { set_entry; show_blocks $ iterate (1::Int) }
   where
     -- N.B. Each iteration starts with the same transaction limit;
     -- only the rewrites in the final iteration actually count
     trace_block b block = my_trace "about to do" (text name <+> text "on" <+> ppr (blockId block)) $
                           do_block b block
     iterate n = 
         do { markFactsUnchanged
            ; b <- foldM trace_block b blocks
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
     unchanged depth = my_nest depth (text "facts are unchanged")

     pprFacts depth n env =
         my_nest depth (text "facts for iteration" <+> pp_i n <+> text "are:" $$
                        (nest 2 $ vcat $ map pprFact $ ufmToList env))
     pprFact (id, a) = hang (ppr id <> colon) 4 (ppr a)
     graphId = case blocks of { Block id _ : _ -> ppr id ; [] -> text "<empty>" }
     show_blocks = my_trace "Blocks:" (vcat (map pprBlock blocks))
     pprBlock (Block id t) = nest 2 (pprFact (id, t))

{-
\subsection{Backward problems}

In a backward problem, we compute \emph{in} facts from \emph{out}
facts.  The analysis gives us [[exit_in]], [[last_in]], [[middle_in]],
and [[first_in]], each of which computes an \emph{in} fact for one
kind of node.  We provide [[head_in]], which computes the \emph{in}
fact for a first node followed by zero or more middle nodes.

We don't compute and return the \emph{in} fact for block; instead, we
use [[setFact]] to attach that fact to the block's unique~ID.
We iterate until no more facts have changed.
-}
run_b_anal comp graph =
  refine_b_anal comp graph (return ()) 
      -- for a backward analysis, everything is initially bottom

refine_b_anal comp graph initial =
      run "backward" (bc_name comp) initial set_block_fact () blocks
  where
    blocks = reverse (postorder_dfs graph)
    set_block_fact () b@(G.Block id _) =              
      let (h, l) = G.goto_end (G.unzip b) in
      do  env <- factsEnv
          setFact id $ head_in h (last_in comp env l) -- 'in' fact for the block
    head_in (G.ZHead h m) out = head_in h (bc_middle_in comp out m)
    head_in (G.ZFirst id) out = bc_first_in comp out id

last_in :: BComputation m l i o -> (BlockId -> i) -> G.ZLast l -> o
last_in comp env (G.LastOther l) = bc_last_in comp env l
last_in comp _   (G.LastExit)    = bc_exit_in comp 

------ we can now pass those facts elsewhere
fold_edge_facts_b f comp graph env z =
    foldl fold_block_facts z (postorder_dfs graph)
  where
    fold_block_facts z b =              
      let (h, l) = G.goto_end (G.unzip b) 
      in head_fold h (last_in comp env l) z
    head_fold (G.ZHead h m) out z = head_fold h (bc_middle_in comp out m) (f out z)
    head_fold (G.ZFirst id) out z = f (bc_first_in comp out id) (f out z)

fold_edge_facts_with_nodes_b fl fm ff comp graph env z =
    foldl fold_block_facts z (postorder_dfs graph)
  where
    fold_block_facts z b =
      let (h, l) = G.goto_end (G.unzip b)
          in' = last_in comp env l
          z' = case l of { G.LastExit -> z ; G.LastOther l -> fl l in' z }
      in head_fold h in' z'
    head_fold (G.ZHead h m) out z =
      let a  = bc_middle_in comp out m
          z' = fm m a z
      in  head_fold h a z'
    head_fold (G.ZFirst id) out z = 
      let a  = bc_first_in comp out id
          z' = ff id a z
      in  z'


-- | In the general case we solve a graph in the context of a larger subgraph.
-- To do this, we need a locally modified computation that allows an
-- ``exit fact'' to flow into the exit node.

comp_with_exit_b :: BComputation m l i (OptimizationFuel -> DFM f (Answer m l o)) -> o ->
                    BComputation m l i (OptimizationFuel -> DFM f (Answer m l o))
comp_with_exit_b comp exit_fact =
    comp { bc_exit_in = \_fuel -> return $ Dataflow $ exit_fact }

-- | Given this function, we can now solve a graph simply by doing a
-- backward analysis on the modified computation.  Note we have to be
-- very careful with 'Rewrite'.  Either a rewrite is going to
-- participate, in which case we mark the graph rerewritten, or we're
-- going to analysis the proposed rewrite and then throw away
-- everything but the answer, in which case it's a 'subAnalysis'.  A
-- Rewrite should always use exactly one of these monadic operations.

solve_graph_b ::
    (DebugNodes m l, Outputable a) =>
    BPass m l a -> OptimizationFuel -> G.LGraph m l -> a -> DFM a (OptimizationFuel, a)
solve_graph_b comp fuel graph exit_fact =
    general_backward (comp_with_exit_b comp exit_fact) fuel graph
  where
    -- general_backward :: BPass m l a -> OptimizationFuel -> G.LGraph m l -> DFM a (OptimizationFuel, a)
    general_backward comp fuel graph = 
      let -- set_block_fact :: OptimizationFuel -> G.Block m l -> DFM a OptimizationFuel
          set_block_fact fuel b =
              do { (fuel, block_in) <-
                        let (h, l) = G.goto_end (G.unzip b) in
                            factsEnv >>= \env -> last_in comp env l fuel >>= \x ->
                              case x of
                                Dataflow a -> head_in fuel h a
                                Rewrite g ->
                                  do { bot <- botFact
                                     ; (fuel, a) <- subAnalysis' $
                                         solve_graph_b_g comp (oneLessFuel fuel) g bot
                                     ; head_in fuel h a }
                 ; my_trace "result of" (text (bc_name comp) <+>
                   text "on" <+> ppr (G.blockId b) <+> text "is" <+> ppr block_in) $
                   setFact (G.blockId b) block_in
                 ; return fuel
                 }
          head_in fuel (G.ZHead h m) out = 
              bc_middle_in comp out m fuel >>= \x -> case x of
                Dataflow a -> head_in fuel h a
                Rewrite g ->
                  do { (fuel, a) <- subAnalysis' $ solve_graph_b_g comp (oneLessFuel fuel) g out 
                     ; my_trace "Rewrote middle node"
                                    (f4sep [ppr m, text "to", pprGraph g]) $
                       head_in fuel h a }
          head_in fuel (G.ZFirst id) out =
              bc_first_in comp out id fuel >>= \x -> case x of
                Dataflow a -> return (fuel, a)
                Rewrite g -> do { subAnalysis' $ solve_graph_b_g comp (oneLessFuel fuel) g out }

      in do { fuel <-
                  run "backward" (bc_name comp) (return ()) set_block_fact fuel blocks
            ; a <- getFact (G.lg_entry graph)
            ; facts <- getAllFacts
            ; my_trace "Solution to graph after pass 1 is" (pprFacts graph facts a) $
              return (fuel, a) }
               
    blocks = reverse (G.postorder_dfs graph)
    pprFacts g env a = (ppr a <+> text "with") $$ vcat (pprLgraph g : map pprFact (ufmToList env))
    pprFact (id, a) = hang (ppr id <> colon) 4 (ppr a)

solve_graph_b_g ::
    (DebugNodes m l, Outputable a) =>
    BPass m l a -> OptimizationFuel -> G.Graph m l -> a -> DFM a (OptimizationFuel, a)
solve_graph_b_g comp fuel graph exit_fact =
  do { g <- lgraphOfGraph graph ; solve_graph_b comp fuel g exit_fact }


lgraphOfGraph :: G.Graph m l -> DFM f (G.LGraph m l)
lgraphOfGraph g =
    do id <- freshBlockId "temporary id for dataflow analysis"
       return $ labelGraph id g

labelGraph :: BlockId -> G.Graph m l -> G.LGraph m l
labelGraph id (Graph tail blocks) = LGraph id (insertBlock (Block id tail) blocks)

-- | We can remove the entry label of an LGraph and remove
-- it, leaving a Graph.  Notice that this operation is NOT SAFE if a 
-- block within the LGraph branches to the entry point.  It should
-- be used only to complement 'lgraphOfGraph' above.

remove_entry_label :: LGraph m l -> Graph m l
remove_entry_label g =
    let FGraph e (ZBlock (ZFirst id) tail) others = entry g
    in  ASSERT (id == e) Graph tail others

{-
We solve and rewrite in two passes: the first pass iterates to a fixed
point to reach a dataflow solution, and the second pass uses that
solution to rewrite the graph.

The
key job is done by [[propagate]], which propagates a fact of type~[[a]]
between a head and tail.
The tail is in final form; the head is still to be rewritten.
-}

solve_and_rewrite_b ::
  (DebugNodes m l, Outputable a) =>
  BPass m l a -> OptimizationFuel -> LGraph m l -> a -> DFM a (OptimizationFuel, a, LGraph m l)
solve_and_rewrite_b_graph ::
  (DebugNodes m l, Outputable a) =>
  BPass m l a -> OptimizationFuel -> Graph m l -> a -> DFM a (OptimizationFuel, a, Graph m l)


solve_and_rewrite_b comp fuel graph exit_fact =
  do { (_, a) <- solve_graph_b comp fuel graph exit_fact -- pass 1
     ; facts <- getAllFacts
     ; (fuel, g) <-                                           -- pass 2
       my_trace "Solution to graph after pass 1 is" (pprFacts graph facts) $
           backward_rewrite (comp_with_exit_b comp exit_fact) fuel graph 
     ; facts <- getAllFacts
     ; my_trace "Rewritten graph after pass 2 is" (pprFacts g facts) $
       return (fuel, a, g) }
  where
    pprFacts g env = vcat (pprLgraph g : map pprFact (ufmToList env))
    pprFact (id, a) = hang (ppr id <> colon) 4 (ppr a)
    eid = G.lg_entry graph
    backward_rewrite comp fuel graph =
      rewrite_blocks comp fuel emptyBlockEnv $ reverse (G.postorder_dfs graph)
    -- rewrite_blocks ::
    --   BPass m l a -> OptimizationFuel ->
    --   BlockEnv (Block m l) -> [Block m l] -> DFM a (OptimizationFuel,G.LGraph m l)
    rewrite_blocks _comp fuel rewritten [] = return (fuel, G.LGraph eid rewritten)
    rewrite_blocks  comp fuel rewritten (b:bs) =
      let rewrite_next_block fuel =
            let (h, l) = G.goto_end (G.unzip b) in
            factsEnv >>= \env -> last_in comp env l fuel >>= \x -> case x of
              Dataflow a -> propagate fuel h a (G.ZLast l) rewritten
              Rewrite g ->
                do { markGraphRewritten
                   ; bot <- botFact
                   ; (fuel, a, g') <- solve_and_rewrite_b_graph comp (oneLessFuel fuel) g bot
                   ; let G.Graph t new_blocks = g'
                   ; let rewritten' = new_blocks `plusUFM` rewritten
                   ; propagate fuel h a t rewritten' -- continue at entry of g'
                   } 
          -- propagate :: OptimizationFuel -- Number of rewrites permitted
          --           -> G.ZHead m        -- Part of current block yet to be rewritten
          --           -> a                -- Fact on edge between head and tail
          --           -> G.ZTail m l      -- Part of current block already rewritten
          --           -> BlockEnv (Block m l)  -- Blocks already rewritten
          --           -> DFM a (OptimizationFuel, G.LGraph m l)
          propagate fuel (G.ZHead h m) out tail rewritten =
              bc_middle_in comp out m fuel >>= \x -> case x of
                Dataflow a -> propagate fuel h a (G.ZTail m tail) rewritten
                Rewrite g ->
                  do { markGraphRewritten
                     ; (fuel, a, g') <- solve_and_rewrite_b_graph comp (oneLessFuel fuel) g out
                     ; let G.Graph t newblocks = G.splice_tail g' tail
                     ; my_trace "Rewrote middle node"
                                             (f4sep [ppr m, text "to", pprGraph g']) $
                       propagate fuel h a t (newblocks `plusUFM` rewritten) }
          propagate fuel h@(G.ZFirst id) out tail rewritten =
              bc_first_in comp out id fuel >>= \x -> case x of
                Dataflow a ->
                  let b = G.Block id tail in
                  do { checkFactMatch id a
                     ; rewrite_blocks comp fuel (extendBlockEnv rewritten id b) bs }
                Rewrite g ->
                  do { markGraphRewritten
                     ; (fuel, a, g') <- solve_and_rewrite_b_graph comp (oneLessFuel fuel) g out
                     ; let G.Graph t newblocks = G.splice_tail g' tail 
                     ; my_trace "Rewrote label " (f4sep [ppr id,text "to",pprGraph g])$
                       propagate fuel h a t (newblocks `plusUFM` rewritten) }
      in rewrite_next_block fuel 

{- Note [Rewriting labelled LGraphs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's hugely annoying that we get in an LGraph and in order to solve it
we have to slap on a new label which we then immediately strip off.
But the alternative is to have all the iterative solvers work on
Graphs, and then suddenly instead of a single case (ZBlock) every
solver has to deal with two cases (ZBlock and ZTail).  So until
somebody comes along who is smart enough to do this and still leave
the code understandable for mortals, it stays as it is.

(One part of the solution will be postorder_dfs_from_except.)
-}

solve_and_rewrite_b_graph comp fuel graph exit_fact =
    do g <- lgraphOfGraph graph
       (fuel, a, g') <- solve_and_rewrite_b comp fuel g exit_fact
       return (fuel, a, remove_entry_label g')

b_rewrite comp g = 
  do { fuel <- fuelRemaining
     ; bot <- botFact
     ; (fuel', _, gc) <- solve_and_rewrite_b comp fuel g bot
     ; fuelDecrement (bc_name comp) fuel fuel'
     ; return gc
     }

{-
This debugging stuff is left over from imperative-land.
It might be useful one day if I learn how to cheat the IO monad!

debug_b :: (Outputable m, Outputable l, Outputable a) => BPass m l a -> BPass m l a

let debug s (f, comp) =
  let pr = Printf.eprintf in
  let fact dir node a = pr "%s %s for %s = %s\n" f.fact_name dir node (s a) in
  let rewr node g = pr "%s rewrites %s to <not-shown>\n" comp.name node in
  let wrap f nodestring node fuel =
    let answer = f node fuel in
    let () = match answer with
    | Dataflow a -> fact "in " (nodestring node) a
    | Rewrite g  -> rewr (nodestring node) g in
    answer in
  let wrapout f nodestring out node fuel =
    fact "out" (nodestring node) out;
    wrap (f out) nodestring node fuel in
  let last_in = wrap comp.last_in (RS.rtl << G.last_instr) in
  let middle_in = wrapout comp.middle_in (RS.rtl << G.mid_instr) in
  let first_in  =
    let first = function G.Entry -> "<entry>" | G.Label ((u, l), _, _) -> l in
    wrapout comp.first_in first in
  f, { comp with last_in = last_in; middle_in = middle_in; first_in = first_in; }
-}

anal_b comp = comp { bc_last_in   = wrap2 $ bc_last_in   comp
                   , bc_exit_in   = wrap0 $ bc_exit_in   comp
                   , bc_middle_in = wrap2 $ bc_middle_in comp
                   , bc_first_in  = wrap2 $ bc_first_in  comp }
  where wrap2 f out node _fuel = return $ Dataflow (f out node)
        wrap0 fact       _fuel = return $ Dataflow fact

ignore_transactions_b comp =
    comp { bc_last_in   = wrap2 $ bc_last_in   comp
         , bc_exit_in   = wrap0 $ bc_exit_in   comp
         , bc_middle_in = wrap2 $ bc_middle_in comp
         , bc_first_in  = wrap2 $ bc_first_in  comp }
  where wrap2 f out node _fuel = f out node
        wrap0 fact       _fuel = fact

answer' :: (b -> DFM f (Graph m l)) -> OptimizationFuel -> Maybe b -> a -> DFM f (Answer m l a)
answer' lift fuel r a = 
    case r of Just gc | canRewriteWithFuel fuel
                          -> do { g <- lift gc; return $ Rewrite g }
              _ -> return $ Dataflow a

unlimited_answer'
    :: (b -> DFM f (Graph m l)) -> OptimizationFuel -> Maybe b -> a -> DFM f (Answer m l a)
unlimited_answer' lift _fuel r a =
    case r of Just gc -> do { g <- lift gc; return $ Rewrite g }
              _ -> return $ Dataflow a

combine_a_t_with :: (OptimizationFuel -> Maybe b -> a -> DFM a (Answer m l a)) ->
                    BAnalysis m l a -> BComputation m l a (Maybe b) ->
                    BPass m l a
combine_a_t_with answer anal tx =
 let last_in env l fuel =
       answer fuel (bc_last_in tx env l) (bc_last_in anal env l)
     exit_in fuel = answer fuel (bc_exit_in tx) (bc_exit_in anal)
     middle_in out m fuel =
       answer fuel (bc_middle_in tx out m) (bc_middle_in anal out m) 
     first_in out f fuel =
       answer fuel (bc_first_in tx out f) (bc_first_in anal out f) 
 in BComp { bc_name = concat [bc_name anal, " and ", bc_name tx]
          , bc_last_in = last_in, bc_middle_in = middle_in
          , bc_first_in = first_in, bc_exit_in = exit_in }

a_t_b            = combine_a_t_with (answer' liftUSM)
a_ft_b           = combine_a_t_with (answer' return)
a_ft_b_unlimited = combine_a_t_with (unlimited_answer' return)


-- =============== FORWARD ================

-- | We don't compute and return the \emph{in} fact for block; instead, we
-- use [[P.set]] to attach that fact to the block's unique~ID.
-- We iterate until no more facts have changed.

dump_things :: Bool
dump_things = False

my_trace :: String -> SDoc -> a -> a
my_trace = if dump_things then pprTrace else \_ _ a -> a

run_f_anal comp entry_fact graph = refine_f_anal comp graph set_entry
  where set_entry = setFact (G.lg_entry graph) entry_fact

refine_f_anal comp graph initial =
    run "forward" (fc_name comp) initial set_successor_facts () blocks
  where blocks = G.postorder_dfs graph
        set_successor_facts () (G.Block id t) =
          let forward in' (G.ZTail m t) = forward (fc_middle_out comp in' m) t
              forward in' (G.ZLast l)   = last_outs setEdgeFacts comp in' l
              _blockname = if id == G.lg_entry graph then "<entry>" else show id
          in  getFact id >>= \a -> forward (fc_first_out comp a id) t
        setEdgeFacts (LastOutFacts fs) = mapM_ setEdgeFact fs
        setEdgeFact (id, a) = setFact id a

last_outs :: (DataflowAnalysis df, Outputable a) => (LastOutFacts a -> df a ()) -> FComputation m l i a (LastOutFacts a) -> i -> G.ZLast l -> df a ()
last_outs _do_last_outs comp i (G.LastExit) = setExitFact (fc_exit_out comp i)
last_outs  do_last_outs comp i (G.LastOther l) = do_last_outs $ fc_last_outs comp i l

last_rewrite :: FComputation m l i a a -> i -> G.ZLast l -> a
last_rewrite comp i (G.LastExit)    = fc_exit_out  comp i
last_rewrite comp i (G.LastOther l) = fc_last_outs comp i l


-- | Given [[comp_with_exit_f]], we can now solve a graph simply by doing a
-- forward analysis on the modified computation.
solve_graph_f ::
    (DebugNodes m l, Outputable a) =>
    FPass m l a -> OptimizationFuel -> G.LGraph m l -> a ->
    DFM a (OptimizationFuel, a, LastOutFacts a)
solve_graph_f comp fuel g in_fact =
  do { fuel <- general_forward fuel in_fact g
     ; a <- getExitFact
     ; outs <- lastOutFacts
     ; return (fuel, a, outs) }
  where
    -- general_forward :: FPass m l a -> OptimizationFuel -> a -> G.LGraph m l -> DFM a OptimizationFuel
    general_forward fuel entry_fact graph =
      let blocks = G.postorder_dfs g
          is_local id = isJust $ lookupBlockEnv (G.lg_blocks g) id
          -- set_or_save :: LastOutFacts a -> DFM a ()
          set_or_save (LastOutFacts l) = mapM_ set_or_save_one l
          set_or_save_one (id, a) =
            if is_local id then setFact id a else addLastOutFact (id, a)
          set_entry = setFact (G.lg_entry graph) entry_fact

          set_successor_facts fuel b =
            let set_tail_facts fuel in' (G.ZTail m t) =
                  my_trace "Solving middle node" (ppr m) $
                  fc_middle_out comp in' m fuel >>= \ x -> case x of
                    Dataflow a -> set_tail_facts fuel a t
                    Rewrite g -> 
                      do (fuel, out, last_outs) <-
                             subAnalysis' $ solve_graph_f_g comp (oneLessFuel fuel) g in'
                         set_or_save last_outs
                         set_tail_facts fuel out t
                set_tail_facts fuel in' (G.ZLast LastExit) =
                  fc_exit_out comp in' fuel >>= \x -> case x of
                    Dataflow a -> do { setExitFact a; return fuel }
                    Rewrite _g -> error "rewriting exit node not implemented"
                set_tail_facts fuel in' (G.ZLast (G.LastOther l)) =
                  fc_last_outs comp in' l fuel >>= \x -> case x of
                    Dataflow outs -> do { set_or_save outs; return fuel }
                    Rewrite g ->
                      do (fuel, _, last_outs) <-
                             subAnalysis' $ solve_graph_f_g comp (oneLessFuel fuel) g in'
                         set_or_save last_outs
                         return fuel
                G.Block id t = b
            in  do idfact <- getFact id
                   infact <- fc_first_out comp idfact id fuel
                   case infact of Dataflow a -> set_tail_facts fuel a t
                                  Rewrite g ->
                                    do (fuel, out, last_outs) <- subAnalysis' $
                                           solve_graph_f_g comp (oneLessFuel fuel) g idfact
                                       set_or_save last_outs
                                       set_tail_facts fuel out t
      in run "forward" (fc_name comp) set_entry set_successor_facts fuel blocks

solve_graph_f_g ::
    (DebugNodes m l, Outputable a) =>
    FPass m l a -> OptimizationFuel -> G.Graph m l -> a -> 
    DFM a (OptimizationFuel, a, LastOutFacts a)
solve_graph_f_g comp fuel graph in_fact =
  do { g <- lgraphOfGraph graph ; solve_graph_f comp fuel g in_fact }


{-
We solve and rewrite in two passes: the first pass iterates to a fixed
point to reach a dataflow solution, and the second pass uses that
solution to rewrite the graph.

The key job is done by [[propagate]], which propagates a fact of type~[[a]]
between a head and tail.
The tail is in final form; the head is still to be rewritten.
-}
solve_and_rewrite_f ::
  (DebugNodes m l, Outputable a) =>
  FPass m l a -> OptimizationFuel -> LGraph m l -> a ->
  DFM a (OptimizationFuel, a, LGraph m l)
solve_and_rewrite_f comp fuel graph in_fact =
  do solve_graph_f comp fuel graph in_fact                   -- pass 1
     (fuel, g) <- forward_rewrite comp fuel graph in_fact
     exit_fact  <- getExitFact   --- XXX should drop this; it's in the monad
     return (fuel, exit_fact, g)

f_shallow_rewrite anal ftx in_fact g =
    do { fuel <- fuelRemaining
       ; solve_shallow_graph_f (return ()) anal ftx in_fact g fuel
       ; id <- freshBlockId "temporary entry id"
       ; (blocks, fuel') <-
           forward_rewrite_gen don't_rewrite anal ftx (ZFirst id) in_fact g fuel
       ; fuelDecrement (fc_name ftx) fuel fuel'
       ; return (remove_entry_label (LGraph id blocks))
       }
  where don't_rewrite finish g fuel = finish >>= \b -> return (b, g, fuel)


shallow_tail_solve_f
    :: (DebugNodes m l, Outputable a)
    => DFM a b   -- final action and result after solving this tail
    -> FAnalysis m l a -> FFunctionalTransformation m l a
    -> (BlockId -> Bool) -- local blocks
    -> a -> ZTail m l -> OptimizationFuel -> DFM a (b, OptimizationFuel)
shallow_tail_solve_f finish anal ftx is_local in' (G.ZTail m t) fuel =
  my_trace "Solving middle node" (ppr m) $
  case maybeRewriteWithFuel fuel $ fc_middle_out ftx in' m of
    Just g -> do out <- subAnalysis' $ liftAnal $ 
                        anal_f_general getExitFact anal in' g
                 shallow_tail_solve_f finish anal ftx is_local out t (oneLessFuel fuel)
    Nothing -> shallow_tail_solve_f finish anal ftx is_local
               (fc_middle_out anal in' m) t fuel
shallow_tail_solve_f finish anal ftx is_local in' (G.ZLast (G.LastOther l)) fuel =
  case maybeRewriteWithFuel fuel $ fc_last_outs ftx in' l of
    Just g  -> do { last_outs <-
                       subAnalysis' $ liftAnal $ anal_f_general lastOutFacts anal in' g
                  ; set_or_save last_outs
                  ; b <- finish
                  ; return (b, oneLessFuel fuel) }
    Nothing -> do { set_or_save (fc_last_outs anal in' l)
                  ; b <- finish
                  ; return (b, fuel) }
  where set_or_save = mk_set_or_save is_local
shallow_tail_solve_f finish anal ftx _is_local in' (G.ZLast LastExit) fuel =
  case maybeRewriteWithFuel fuel $ fc_exit_out ftx in' of
    Just g  -> do { a <-
                       subAnalysis' $ liftAnal $ anal_f_general getExitFact anal in' g
                  ; setExitFact a
                  ; b <- finish
                  ; return (b, oneLessFuel fuel) }
    Nothing -> do { setExitFact $ fc_exit_out anal in'
                  ; b <- finish
                  ; return (b, fuel) }

anal_f_general :: (DebugNodes m l, Outputable a)
               => DFA a b -> FAnalysis m l a -> a -> Graph m l -> DFA a b
anal_f_general finish anal in_fact (Graph entry blockenv) =
    general_forward in_fact
  where
    is_local id = isJust $ lookupBlockEnv blockenv id
    set_or_save = mk_set_or_save is_local
    anal_tail = gen_tail_anal_f set_or_save anal
    blocks = G.postorder_dfs_from blockenv entry
    general_forward in_fact =
      do { let setup = anal_tail in_fact entry -- sufficient to do once
         ; let set_successor_facts () (Block id tail) =
                do { idfact <- getFact id
                   ; anal_tail (fc_first_out anal idfact id) tail }
         ; run "forward" (fc_name anal) setup set_successor_facts () blocks 
         ; finish
         }

gen_tail_anal_f :: (Outputable a) =>
    (LastOutFacts a -> DFA a ()) -> FAnalysis m l a -> a -> ZTail m l -> DFA a ()
gen_tail_anal_f do_last_outs anal a tail = propagate a tail
    where propagate a (ZTail m t) = propagate (fc_middle_out anal a m) t
          propagate a (ZLast LastExit) = setExitFact (fc_exit_out anal a)
          propagate a (ZLast (LastOther l)) = do_last_outs $ fc_last_outs anal a l


solve_shallow_graph_f ::
    (DebugNodes m l, Outputable a) =>
    DFM a b -> 
    FAnalysis m l a -> FFunctionalTransformation m l a -> a -> G.Graph m l
                    -> OptimizationFuel -> DFM a (b, OptimizationFuel)
solve_shallow_graph_f finish anal ftx in_fact (Graph entry blockenv) fuel =
  do { fuel <- general_forward in_fact fuel
     ; b <- finish
     ; return (b, fuel) }
  where
    is_local id = isJust $ lookupBlockEnv blockenv id
    set_or_save = mk_set_or_save is_local
    solve_tail = shallow_tail_solve_f lastOutFacts anal ftx is_local
    blocks = G.postorder_dfs_from blockenv entry
    name = concat [fc_name anal, " and ", fc_name ftx]
    general_forward in_fact fuel =
      do { (last_outs, fuel) <- solve_tail in_fact entry fuel
         ; set_or_save last_outs                                    
         ; let set_successor_facts fuel (Block id tail) =
                do { idfact <- getFact id
                   ; (last_outs, fuel) <-
                       case maybeRewriteWithFuel fuel $ fc_first_out ftx idfact id of
                         Nothing -> solve_tail idfact tail fuel
                         Just g ->
                           do outfact <-
                                  subAnalysis' $ liftAnal $
                                               anal_f_general getExitFact anal idfact g
                              solve_tail outfact tail (oneLessFuel fuel)
                   ; set_or_save last_outs
                   ; return fuel }
        ; run "forward" name (return ()) set_successor_facts fuel blocks }

mk_set_or_save :: (DataflowAnalysis df, Monad (df a), Outputable a) =>
                  (BlockId -> Bool) -> LastOutFacts a -> df a ()
mk_set_or_save is_local (LastOutFacts l) = mapM_ set_or_save_one l
    where set_or_save_one (id, a) =
              if is_local id then setFact id a else addLastOutFact (id, a)

lastOutFacts :: (DataflowAnalysis m, Monad (m f)) => m f (LastOutFacts f)
lastOutFacts = bareLastOutFacts >>= return . LastOutFacts


fwd_rew_tail_gen :: (DebugNodes m l, Outputable a) =>
    (forall b . DFM a b -> Graph m l -> OptimizationFuel -> DFM a (b, Graph m l, OptimizationFuel)) ->
    FAnalysis m l a -> FFunctionalTransformation m l a -> ZHead m -> a -> ZTail m l
     -> BlockEnv (Block m l)
     -> OptimizationFuel -> DFM a (BlockEnv (Block m l), OptimizationFuel)
fwd_rew_tail_gen recursive_rewrite anal ftx head in_fact tail rewritten fuel =
    propagate head in_fact tail rewritten fuel
   where
    propagate h in' (G.ZTail m t) rewritten fuel = 
      my_trace "Rewriting middle node" (ppr m) $
      case maybeRewriteWithFuel fuel $ fc_middle_out ftx in' m of
        Nothing -> propagate (G.ZHead h m) (fc_middle_out anal in' m) t rewritten fuel
        Just g -> do markGraphRewritten
                     (a, g, fuel) <- recursive_rewrite getExitFact g fuel
                     let (blocks, h') = G.splice_head' h g
                     propagate h' a t (blocks `plusUFM` rewritten) fuel
    propagate h in' (G.ZLast l) rewritten fuel = 
      case maybeRewriteWithFuel fuel $ last_rewrite ftx in' l of
        Nothing -> -- can throw away facts because this is the rewriting phase
                   return (insertBlock (zipht h (G.ZLast l)) rewritten, fuel)
        Just g -> do markGraphRewritten
                     ((), g, fuel) <- recursive_rewrite (return ()) g fuel
                     let g' = G.splice_head_only' h g
                     return (G.lg_blocks g' `plusUFM` rewritten, fuel)

forward_rewrite_gen ::
    (DebugNodes m l, Outputable a) =>
    (forall b . DFM a b -> Graph m l -> OptimizationFuel -> DFM a (b, Graph m l, OptimizationFuel)) ->
    FAnalysis m l a -> FFunctionalTransformation m l a -> ZHead m -> a -> Graph m l
                    -> OptimizationFuel -> DFM a (BlockEnv (Block m l), OptimizationFuel)
forward_rewrite_gen recursive_rewrite anal ftx head a (Graph entry blockenv) fuel =
  do (rewritten, fuel) <- rewrite_tail head a entry emptyBlockEnv fuel
     rewrite_blocks (G.postorder_dfs_from blockenv entry) rewritten fuel
  where
    -- need to build in some checking for consistency of facts
    rewrite_tail = fwd_rew_tail_gen recursive_rewrite anal ftx
    rewrite_blocks [] rewritten fuel = return (rewritten, fuel)
    rewrite_blocks (G.Block id t : bs) rewritten fuel =
        do id_fact <- getFact id
           case maybeRewriteWithFuel fuel $ fc_first_out ftx id_fact id of
             Nothing -> do { (rewritten, fuel) <-
                                 rewrite_tail (ZFirst id) id_fact t rewritten fuel
                           ; rewrite_blocks bs rewritten fuel }
             Just g  -> do { (outfact, g, fuel) <- recursive_rewrite getExitFact g fuel
                           ; let (blocks, h) = splice_head' (ZFirst id) g
                           ; (rewritten, fuel) <-
                             rewrite_tail h outfact t (blocks `plusUFM` rewritten) fuel
                           ; rewrite_blocks bs rewritten fuel }






solve_and_rewrite_f_graph ::
  (DebugNodes m l, Outputable a) =>
  FPass m l a -> OptimizationFuel -> Graph m l -> a ->
  DFM a (OptimizationFuel, a, Graph m l)
solve_and_rewrite_f_graph comp fuel graph in_fact =
    do g <- lgraphOfGraph graph
       (fuel, a, g') <- solve_and_rewrite_f comp fuel g in_fact
       return (fuel, a, remove_entry_label g')

forward_rewrite ::
  (DebugNodes m l, Outputable a) =>
  FPass m l a -> OptimizationFuel -> G.LGraph m l -> a ->
  DFM a (OptimizationFuel, G.LGraph m l)
forward_rewrite comp fuel graph entry_fact =
  do setFact eid entry_fact
     rewrite_blocks fuel emptyBlockEnv (G.postorder_dfs graph) 
  where
    eid = G.lg_entry graph
    is_local id = isJust $ lookupBlockEnv (G.lg_blocks graph) id
    -- set_or_save :: LastOutFacts a -> DFM a ()
    set_or_save (LastOutFacts l) = mapM_ set_or_save_one l
    set_or_save_one (id, a) =
        if is_local id then checkFactMatch id a
        else panic "set fact outside graph during rewriting pass?!"

    -- rewrite_blocks ::
    --   OptimizationFuel -> BlockEnv (Block m l) -> [Block m l] -> DFM a (OptimizationFuel, LGraph m l)
    rewrite_blocks fuel rewritten [] = return (fuel, G.LGraph eid rewritten)
    rewrite_blocks fuel rewritten (G.Block id t : bs) = 
        do id_fact <- getFact id
           first_out <- fc_first_out comp id_fact id fuel
           case first_out of
             Dataflow a -> propagate fuel (G.ZFirst id) a t rewritten bs
             Rewrite g  -> do { markGraphRewritten
                              ; rewrite_blocks (oneLessFuel fuel) rewritten
                                (G.postorder_dfs (labelGraph id g) ++ bs) }
    -- propagate :: OptimizationFuel -> G.ZHead m -> a -> G.ZTail m l -> BlockEnv (G.Block m l) ->
    --             [G.Block m l] -> DFM a (OptimizationFuel, G.LGraph m l)
    propagate fuel h in' (G.ZTail m t) rewritten bs = 
        my_trace "Rewriting middle node" (ppr m) $
        do fc_middle_out comp in' m fuel >>= \x -> case x of
             Dataflow a -> propagate fuel (G.ZHead h m) a t rewritten bs
             Rewrite g ->
               do markGraphRewritten
                  (fuel, a, g) <- solve_and_rewrite_f_graph comp (oneLessFuel fuel) g in' 
                  let (blocks, h') = G.splice_head' h g
                  propagate fuel h' a t (blocks `plusUFM` rewritten) bs
    propagate fuel h in' t@(G.ZLast G.LastExit) rewritten bs = 
        do fc_exit_out comp in' fuel >>= \x -> case x of
             Dataflow a ->
               do setExitFact a
                  let b = G.zipht h t
                  rewrite_blocks fuel (G.insertBlock b rewritten) bs
             Rewrite g ->
                do markGraphRewritten
                   (fuel, _, g) <- solve_and_rewrite_f_graph comp (oneLessFuel fuel) g in' 
                   let g' = G.splice_head_only' h g
                   rewrite_blocks fuel (G.lg_blocks g' `plusUFM` rewritten) bs
    propagate fuel h in' t@(G.ZLast (G.LastOther l)) rewritten bs = 
        do fc_last_outs comp in' l fuel >>= \x -> case x of
             Dataflow outs ->
               do set_or_save outs
                  let b = G.zipht h t
                  rewrite_blocks fuel (G.insertBlock b rewritten) bs
             Rewrite g ->
                do markGraphRewritten
                   (fuel, _, g) <- solve_and_rewrite_f_graph comp (oneLessFuel fuel) g in' 
                   let g' = G.splice_head_only' h g
                   rewrite_blocks fuel (G.lg_blocks g' `plusUFM` rewritten) bs

f_rewrite comp entry_fact g =
  do { fuel <- fuelRemaining
     ; (fuel', _, gc) <- solve_and_rewrite_f comp fuel g entry_fact
     ; fuelDecrement (fc_name comp) fuel fuel'
     ; return gc
     }


{-
debug_f :: (Outputable m, Outputable l, Outputable a) => FPass m l a -> FPass m l a

let debug s (f, comp) =
  let pr = Printf.eprintf in
  let fact dir node a = pr "%s %s for %s = %s\n" f.fact_name dir node (s a) in
  let setter dir node run_sets set =
    run_sets (fun u a -> pr "%s %s for %s = %s\n" f.fact_name dir node (s a); set u a) in
  let rewr node g = pr "%s rewrites %s to <not-shown>\n" comp.name node in
  let wrap f nodestring wrap_answer in' node fuel =
    fact "in " (nodestring node) in';
    wrap_answer (nodestring node) (f in' node fuel)
  and wrap_fact n answer =
    let () = match answer with
    | Dataflow a -> fact "out" n a
    | Rewrite g  -> rewr n g in
    answer
  and wrap_setter n answer =
    match answer with
    | Dataflow set -> Dataflow (setter "out" n set)
    | Rewrite g  -> (rewr n g; Rewrite g) in
  let middle_out = wrap comp.middle_out (RS.rtl << G.mid_instr) wrap_fact in
  let last_outs = wrap comp.last_outs (RS.rtl << G.last_instr) wrap_setter in
  f, { comp with last_outs = last_outs; middle_out = middle_out; }
-}

anal_f comp = comp { fc_first_out  = wrap2 $ fc_first_out  comp 
                   , fc_middle_out = wrap2 $ fc_middle_out comp
                   , fc_last_outs  = wrap2 $ fc_last_outs  comp
                   , fc_exit_out   = wrap1 $ fc_exit_out   comp
                   }
  where wrap2 f out node _fuel = return $ Dataflow (f out node)
        wrap1 f fact     _fuel = return $ Dataflow (f fact)


a_t_f anal tx =
 let answer = answer' liftUSM
     first_out in' id fuel =
         answer fuel (fc_first_out tx in' id) (fc_first_out anal in' id)
     middle_out in' m fuel =
         answer fuel (fc_middle_out tx in' m) (fc_middle_out anal in' m)
     last_outs in' l fuel = 
         answer fuel (fc_last_outs tx in' l) (fc_last_outs anal in' l)
     exit_out in' fuel = undefined
         answer fuel (fc_exit_out tx in') (fc_exit_out anal in')
 in  FComp { fc_name = concat [fc_name anal, " and ", fc_name tx]
           , fc_last_outs = last_outs, fc_middle_out = middle_out
           , fc_first_out = first_out, fc_exit_out = exit_out }


f4sep :: [SDoc] -> SDoc
f4sep [] = fsep []
f4sep (d:ds) = fsep (d : map (nest 4) ds)

subAnalysis' :: (Monad (m f), DataflowAnalysis m, Outputable f) =>
                m f a -> m f a
subAnalysis' m =
    do { a <- subAnalysis $
               do { a <- m; facts <- getAllFacts
                  ; my_trace "after sub-analysis facts are" (pprFacts facts) $
                    return a }
       ; facts <- getAllFacts
       ; my_trace "in parent analysis facts are" (pprFacts facts) $
         return a }
  where pprFacts env = nest 2 $ vcat $ map pprFact $ ufmToList env
        pprFact (id, a) = hang (ppr id <> colon) 4 (ppr a)

null_b_ft = BComp "do nothing" Nothing no2 no2 no2
    where no2 _ _ = Nothing

null_f_ft = FComp "do nothing" no2 no2 no2 (\_ -> Nothing)
    where no2 _ _ = Nothing

