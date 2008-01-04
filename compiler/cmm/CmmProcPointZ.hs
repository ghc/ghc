
module CmmProcPointZ
    ( minimalProcPointSet
    , addProcPointProtocols
    )
where

import Prelude hiding (zip, unzip)

import ClosureInfo
import Cmm hiding (blockId)
import CmmExpr
import CmmContFlowOpt
import CmmLiveZ
import CmmTx
import DFMonad
import ForeignCall -- used in protocol for the entry point
import MachOp (MachHint(NoHint))
import Maybes
import Outputable
import Panic
import UniqFM
import UniqSet
import ZipCfg
import ZipCfgCmmRep
import ZipDataflow0

-- Compute a minimal set of proc points for a control-flow graph.

-- Determine a protocol for each proc point (which live variables will
-- be passed as arguments and which will be on the stack). 

{-
A proc point is a basic block that, after CPS transformation, will
start a new function.  The entry block of the original function is a
proc point, as is the continuation of each function call.
A third kind of proc point arises if we want to avoid copying code.
Suppose we have code like the following:

  f() {
    if (...) { ..1..; call foo(); ..2..}
    else     { ..3..; call bar(); ..4..}
    x = y + z;
    return x;
  }

The statement 'x = y + z' can be reached from two different proc
points: the continuations of foo() and bar().  We would prefer not to
put a copy in each continuation; instead we would like 'x = y + z' to
be the start of a new procedure to which the continuations can jump:

  f_cps () {
    if (...) { ..1..; push k_foo; jump foo_cps(); }
    else     { ..3..; push k_bar; jump bar_cps(); }
  }
  k_foo() { ..2..; jump k_join(y, z); }
  k_bar() { ..4..; jump k_join(y, z); }
  k_join(y, z) { x = y + z; return x; }

You might think then that a criterion to make a node a proc point is
that it is directly reached by two distinct proc points.  (Note
[Direct reachability].)  But this criterion is a bit two simple; for
example, 'return x' is also reached by two proc points, yet there is
no point in pulling it out of k_join.  A good criterion would be to
say that a node should be made a proc point if it is reached by a set
of proc points that is different than its immediate dominator.  NR
believes this criterion can be shown to produce a minimum set of proc
points, and given a dominator tree, the proc points can be chosen in
time linear in the number of blocks.  Lacking a dominator analysis,
however, we turn instead to an iterative solution, starting with no
proc points and adding them according to these rules:

  1. The entry block is a proc point.
  2. The continuation of a call is a proc point.
  3. A node is a proc point if it is directly reached by more proc
     points than one of its predecessors.

Because we don't understand the problem very well, we apply rule 3 at
most once per iteration, then recompute the reachability information.
(See Note [No simple dataflow].)  The choice of the new proc point is
arbitrary, and I don't know if the choice affects the final solution,
so I don't know if the number of proc points chosen is the
minimum---but the set will be minimal.
-}

type ProcPointSet = BlockSet

data Status
  = ReachedBy ProcPointSet  -- set of proc points that directly reach the block
  | ProcPoint               -- this block is itself a proc point

instance Outputable Status where
  ppr (ReachedBy ps)
      | isEmptyUniqSet ps = text "<not-reached>"
      | otherwise = text "reached by" <+>
                    (hsep $ punctuate comma $ map ppr $ uniqSetToList ps)
  ppr ProcPoint = text "<procpt>"


lattice :: DataflowLattice Status
lattice = DataflowLattice "direct proc-point reachability" unreached add_to False
    where unreached = ReachedBy emptyBlockSet
          add_to _ ProcPoint = noTx ProcPoint
          add_to ProcPoint _ = aTx ProcPoint -- aTx because of previous case again
          add_to (ReachedBy p) (ReachedBy p') =
              let union = unionUniqSets p p'
              in  if sizeUniqSet union > sizeUniqSet p' then
                      aTx (ReachedBy union)
                  else
                      noTx (ReachedBy p')
--------------------------------------------------
-- transfer equations

forward :: FAnalysis Middle Last Status
forward = FComp "proc-point reachability" first middle last exit
    where first ProcPoint id = ReachedBy $ unitUniqSet id
          first  x _ = x
          middle x _ = x
          last _ (LastCall _ (Just id)) = LastOutFacts [(id, ProcPoint)]
          last x l = LastOutFacts $ map (\id -> (id, x)) (succs l)
          exit x   = x
                
minimalProcPointSet :: CmmGraph -> ProcPointSet
minimalProcPointSet g = extendPPSet g (postorder_dfs g) entryPoint
    where entryPoint = unitUniqSet (lg_entry g)

extendPPSet :: CmmGraph -> [CmmBlock] -> ProcPointSet -> ProcPointSet
extendPPSet g blocks procPoints =
    case newPoint of Just id ->
                       if elemBlockSet id procPoints' then panic "added old proc pt"
                       else extendPPSet g blocks (extendBlockSet procPoints' id)
                     Nothing -> procPoints'
    where env = runDFA lattice $
                do refine_f_anal forward g set_init_points
                   allFacts
          set_init_points = mapM_ (\id -> setFact id ProcPoint)
                            (uniqSetToList procPoints)
          procPoints' = fold_blocks add emptyBlockSet g
          add block pps = let id = blockId block
                          in  case lookupBlockEnv env id of
                                Just ProcPoint -> extendBlockSet pps id
                                _ -> pps
                                     
          newPoint = listToMaybe (mapMaybe ppSuccessor blocks)
          ppSuccessor b@(Block id _) =
              let nreached id = case lookupBlockEnv env id `orElse` panic "no ppt" of
                                  ProcPoint -> 1
                                  ReachedBy ps -> sizeUniqSet ps
                  my_nreached = nreached id
                  -- | Looking for a successor of b that is reached by
                  -- more proc points than b and is not already a proc
                  -- point.  If found, it can become a proc point.
                  newId succ_id = not (elemBlockSet succ_id procPoints') &&
                                  nreached succ_id > my_nreached
              in  listToMaybe $ filter newId $ succs b
                                    

------------------------------------------------------------------------
--                    Computing Proc-Point Protocols                  --
------------------------------------------------------------------------

{-

There is one major trick, discovered by Michael Adams, which is that
we want to choose protocols in a way that enables us to optimize away
some continuations.  The optimization is very much like branch-chain
elimination, except that it involves passing results as well as
control.  The idea is that if a call's continuation k does nothing but
CopyIn its results and then goto proc point P, the call's continuation
may be changed to P, *provided* P's protocol is identical to the
protocol for the CopyIn.  We choose protocols to make this so.

Here's an explanatory example; we begin with the source code (lines
separate basic blocks):

  ..1..;
  x, y = g();
  goto P;
  -------
  P: ..2..;

Zipperization converts this code as follows:

  ..1..;
  call g() returns to k;
  -------
  k: CopyIn(x, y);
     goto P;
  -------
  P: ..2..;

What we'd like to do is assign P the same CopyIn protocol as k, so we
can eliminate k:

  ..1..;
  call g() returns to P;
  -------
  P: CopyIn(x, y); ..2..;

Of course, P may be the target of more than one continuation, and
different continuations may have different protocols.  Michael Adams
implemented a voting mechanism, but he thinks a simple greedy
algorithm would be just as good, so that's what we do.

-}

data Protocol = Protocol Convention CmmFormals
  deriving Eq

-- | Function 'optimize_calls' chooses protocols only for those proc
-- points that are relevant to the optimization explained above.
-- The others are assigned by 'add_unassigned', which is not yet clever.

addProcPointProtocols :: ProcPointSet -> CmmFormalsWithoutKinds -> CmmGraph -> CmmGraph
addProcPointProtocols procPoints formals g =
       snd $ add_unassigned procPoints $ optimize_calls g
    where optimize_calls g =  -- see Note [Separate Adams optimization]
              let (protos, blocks') =
                      fold_blocks maybe_add_call (init_protocols, emptyBlockEnv) g
                  g' = LGraph (lg_entry g) (add_CopyIns protos blocks')
              in  (protos, runTx removeUnreachableBlocksZ g')
          maybe_add_call :: CmmBlock -> (BlockEnv Protocol, BlockEnv CmmBlock)
                         -> (BlockEnv Protocol, BlockEnv CmmBlock)
          -- ^ If the block is a call whose continuation goes to a proc point
          -- whose protocol either matches the continuation's or is not yet set,
          -- redirect the call (cf 'newblock') and set the protocol if necessary
          maybe_add_call block (protos, blocks) =
              case goto_end $ unzip block of
                (h, LastOther (LastCall tgt (Just k)))
                    | Just proto <- lookupBlockEnv protos k,
                      Just pee <- jumpsToProcPoint k
                    -> let newblock =
                               zipht h (tailOfLast (LastCall tgt (Just pee)))
                           changed_blocks   = insertBlock newblock blocks
                           unchanged_blocks = insertBlock block    blocks
                       in case lookupBlockEnv protos pee of
                            Nothing -> (extendBlockEnv protos pee proto,changed_blocks)
                            Just proto' ->
                              if proto == proto' then (protos, changed_blocks)
                              else (protos, unchanged_blocks)
                _ -> (protos, insertBlock block blocks)

          jumpsToProcPoint :: BlockId -> Maybe BlockId
          -- ^ Tells whether the named block is just a jump to a proc point
          jumpsToProcPoint id =
              let (Block _ t) = lookupBlockEnv (lg_blocks g) id `orElse`
                                panic "jump out of graph"
              in case t of
                   ZTail (CopyIn {}) (ZLast (LastOther (LastBranch pee)))
                       | elemBlockSet pee procPoints -> Just pee
                   _ -> Nothing
          init_protocols = fold_blocks maybe_add_proto emptyBlockEnv g
          maybe_add_proto :: CmmBlock -> BlockEnv Protocol -> BlockEnv Protocol
          maybe_add_proto (Block id (ZTail (CopyIn c fs _srt) _)) env =
              extendBlockEnv env id (Protocol c fs)
          maybe_add_proto (Block id _) env | id == lg_entry g =
              extendBlockEnv env id (Protocol stdArgConvention hinted_formals)
          maybe_add_proto _ env = env
          hinted_formals = map (\x -> CmmHinted x NoHint) formals
          stdArgConvention = ConventionStandard CmmCallConv Arguments

-- | For now, following a suggestion by Ben Lippmeier, we pass all
-- live variables as arguments, hoping that a clever register
-- allocator might help.

add_unassigned
    :: ProcPointSet -> (BlockEnv Protocol, CmmGraph) -> (BlockEnv Protocol, CmmGraph) 
add_unassigned = pass_live_vars_as_args

pass_live_vars_as_args
    :: ProcPointSet -> (BlockEnv Protocol, CmmGraph) -> (BlockEnv Protocol, CmmGraph) 
pass_live_vars_as_args procPoints (protos, g) = (protos', g')
  where liveness = cmmLivenessZ g
        protos' = foldUniqSet addLiveVars protos procPoints
        addLiveVars :: BlockId -> BlockEnv Protocol -> BlockEnv Protocol
        addLiveVars id protos =
            case lookupBlockEnv protos id of
              Just _ -> protos
              Nothing -> let live = lookupBlockEnv liveness id `orElse`
                                    emptyRegSet -- XXX there's a bug lurking!
                                    -- panic ("no liveness at block " ++ show id)
                             formals = map (\x -> CmmHinted x NoHint) $ uniqSetToList live
                         in  extendBlockEnv protos id (Protocol ConventionPrivate formals)
        g' = g { lg_blocks = add_CopyIns protos' (lg_blocks g) }


-- | Add a CopyIn node to each block that has a protocol but lacks the
-- appropriate CopyIn node.

add_CopyIns :: BlockEnv Protocol -> BlockEnv CmmBlock -> BlockEnv CmmBlock
add_CopyIns protos = mapUFM (maybe_insert_CopyIn protos)
    where maybe_insert_CopyIn :: BlockEnv Protocol -> CmmBlock -> CmmBlock
          maybe_insert_CopyIn protos b@(Block id t) =
            case lookupBlockEnv protos id of
              Nothing -> b
              Just (Protocol c fs) ->
                  case t of
                    ZTail (CopyIn c' fs' _) _ ->
                      if c == c' && fs == fs' then b
                      else panic ("mismatched protocols for block " ++ show id)
                    _ -> Block id (ZTail (CopyIn c fs NoC_SRT) t)

-- XXX also need to add the relevant CopyOut nodes!!!

----------------------------------------------------------------

{-
Note [Direct reachability]

Block B is directly reachable from proc point P iff control can flow
from P to B without passing through an intervening proc point.
-}

----------------------------------------------------------------

{-
Note [No simple dataflow]

Sadly, it seems impossible to compute the proc points using a single
dataflow pass.  One might attempt to use this simple lattice:

  data Location = Unknown
                | InProc BlockId -- node is in procedure headed by the named proc point
                | ProcPoint      -- node is itself a proc point   

At a join, a node in two different blocks becomes a proc point.  
The difficulty is that the change of information during iterative
computation may promote a node prematurely.  Here's a program that
illustrates the difficulty:

  f () {
  entry:
    ....
  L1:
    if (...) { ... }
    else { ... }

  L2: if (...) { g(); goto L1; }
      return x + y;
  }

The only proc-point needed (besides the entry) is L1.  But in an
iterative analysis, consider what happens to L2.  On the first pass
through, it rises from Unknown to 'InProc entry', but when L1 is
promoted to a proc point (because it's the successor of g()), L1's
successors will be promoted to 'InProc L1'.  The problem hits when the
new fact 'InProc L1' flows into L2 which is already bound to 'InProc entry'.
The join operation makes it a proc point when in fact it needn't be,
because its immediate dominator L1 is already a proc point and there
are no other proc points that directly reach L2.
-}



{- Note [Separate Adams optimization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It may be worthwhile to attempt the Adams optimization by rewriting
the graph before the assignment of proc-point protocols.  Here are a
couple of rules:
                                                                  
  g() returns to k;                    g() returns to L;          
  k: CopyIn c ress; goto L:             
   ...                        ==>        ...                       
  L: // no CopyIn node here            L: CopyIn c ress; 

                                                                  
And when c == c' and ress == ress', this also:

  g() returns to k;                    g() returns to L;          
  k: CopyIn c ress; goto L:             
   ...                        ==>        ...                       
  L: CopyIn c' ress'                   L: CopyIn c' ress' ; 

In both cases the goal is to eliminate k.
-}
