{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}

module GHC.Cmm.ProcPoint
    ( ProcPointSet, Status(..)
    , callProcPoints, minimalProcPointSet
    , splitAtProcPoints, procPointAnalysis
    , attachContInfoTables
    )
where

import GHC.Prelude hiding (last, unzip, succ, zip)

import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.Info
import GHC.Cmm.Liveness
import GHC.Cmm.Switch
import Data.List (sortBy)
import GHC.Data.Maybe
import Control.Monad
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform
import GHC.Types.Unique.DSM
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Graph
import qualified GHC.Cmm.Dataflow.Label as Det
import GHC.Cmm.Dataflow.Label (Label)
import qualified GHC.Cmm.Dataflow.Label.NonDet as NonDet

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
[Direct reachability].)  But this criterion is a bit too simple; for
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



Note [Proc-point analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a specified set of proc-points (a set of block-ids), "proc-point
analysis" figures out, for every block, which proc-point it belongs to.
All the blocks belonging to proc-point P will constitute a single
top-level C procedure.

A non-proc-point block B "belongs to" a proc-point P iff B is
reachable from P without going through another proc-point.

Invariant: a block B should belong to at most one proc-point; if it
belongs to two, that's a bug.

Note [Non-existing proc-points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On some architectures it might happen that the list of proc-points
computed before stack layout pass will be invalidated by the stack
layout. This will happen if stack layout removes from the graph
blocks that were determined to be proc-points. Later on in the pipeline
we use list of proc-points to perform [Proc-point analysis], but
if a proc-point does not exist anymore then we will get compiler panic.
See #8205.
-}

type ProcPointSet = NonDet.LabelSet

data Status
  = ReachedBy ProcPointSet  -- set of proc points that directly reach the block
  | ProcPoint               -- this block is itself a proc point

instance Outputable Status where
  ppr (ReachedBy ps)
      | NonDet.setNull ps = text "<not-reached>"
      | otherwise = text "reached by" <+>
                    (hsep $ punctuate comma $ map ppr $ NonDet.nonDetSetElems ps)
  ppr ProcPoint = text "<procpt>"

--------------------------------------------------
-- Proc point analysis

-- Once you know what the proc-points are, figure out
-- what proc-points each block is reachable from
-- See Note [Proc-point analysis]
procPointAnalysis :: ProcPointSet -> CmmGraph -> NonDet.LabelMap Status
procPointAnalysis procPoints cmmGraph@(CmmGraph {g_graph = graph}) =
    analyzeCmmFwd procPointLattice procPointTransfer cmmGraph initProcPoints
  where
    initProcPoints =
        mkFactBase
            procPointLattice
            [ (id, ProcPoint)
            | id <- NonDet.nonDetSetElems procPoints
            -- See Note [Non-existing proc-points]
            , id `NonDet.setMember` labelsInGraph
            ]
    labelsInGraph = labelsDefined graph

procPointTransfer :: TransferFun Status
procPointTransfer block facts =
    let label = entryLabel block
        !fact = case getFact procPointLattice label facts of
            ProcPoint -> ReachedBy $! NonDet.setSingleton label
            f -> f
        result = map (\id -> (id, fact)) (successors block)
    in mkFactBase procPointLattice result

procPointLattice :: DataflowLattice Status
procPointLattice = DataflowLattice unreached add_to
  where
    unreached = ReachedBy NonDet.setEmpty
    add_to (OldFact ProcPoint) _ = NotChanged ProcPoint
    add_to _ (NewFact ProcPoint) = Changed ProcPoint -- because of previous case
    add_to (OldFact (ReachedBy p)) (NewFact (ReachedBy p'))
        | NonDet.setSize union > NonDet.setSize p = Changed (ReachedBy union)
        | otherwise = NotChanged (ReachedBy p)
      where
        union = NonDet.setUnion p' p

----------------------------------------------------------------------

-- It is worth distinguishing two sets of proc points: those that are
-- induced by calls in the original graph and those that are
-- introduced because they're reachable from multiple proc points.
--
-- Extract the set of Continuation BlockIds, see Note [Continuation BlockIds].
callProcPoints      :: CmmGraph -> ProcPointSet
callProcPoints g = foldlGraphBlocks add (NonDet.setSingleton (g_entry g)) g
  where add :: NonDet.LabelSet -> CmmBlock -> NonDet.LabelSet
        add set b = case lastNode b of
                      CmmCall {cml_cont = Just k} -> NonDet.setInsert k set
                      CmmForeignCall {succ=k}     -> NonDet.setInsert k set
                      _ -> set

minimalProcPointSet :: Platform -> ProcPointSet -> CmmGraph
                    -> UniqDSM ProcPointSet
-- Given the set of successors of calls (which must be proc-points)
-- figure out the minimal set of necessary proc-points
minimalProcPointSet platform callProcPoints g
  = extendPPSet platform g (revPostorder g) callProcPoints

extendPPSet
    :: Platform -> CmmGraph -> [CmmBlock] -> ProcPointSet -> UniqDSM ProcPointSet
extendPPSet platform g blocks procPoints =
    let env = procPointAnalysis procPoints g
        add pps block = let id = entryLabel block
                        in  case NonDet.mapLookup id env of
                              Just ProcPoint -> NonDet.setInsert id pps
                              _ -> pps
        procPoints' = foldlGraphBlocks add NonDet.setEmpty g
        newPoints = mapMaybe ppSuccessor blocks
        newPoint  = listToMaybe newPoints
        ppSuccessor b =
            let nreached id = case NonDet.mapLookup id env `orElse`
                                    pprPanic "no ppt" (ppr id <+> pdoc platform b) of
                                ProcPoint -> 1
                                ReachedBy ps -> NonDet.setSize ps
                block_procpoints = nreached (entryLabel b)
                -- Looking for a successor of b that is reached by
                -- more proc points than b and is not already a proc
                -- point.  If found, it can become a proc point.
                newId succ_id = not (NonDet.setMember succ_id procPoints') &&
                                nreached succ_id > block_procpoints
            in  listToMaybe $ filter newId $ successors b

    in case newPoint of
         Just id ->
             if NonDet.setMember id procPoints'
                then panic "added old proc pt"
                else extendPPSet platform g blocks (NonDet.setInsert id procPoints')
         Nothing -> return procPoints'


-- At this point, we have found a set of procpoints, each of which should be
-- the entry point of a procedure.
-- Now, we create the procedure for each proc point,
-- which requires that we:
-- 1. build a map from proc points to the blocks reachable from the proc point
-- 2. turn each branch to a proc point into a jump
-- 3. turn calls and returns into jumps
-- 4. build info tables for the procedures -- and update the info table for
--    the SRTs in the entry procedure as well.
-- Input invariant: A block should only be reachable from a single ProcPoint.
-- ToDo: use the _ret naming convention that the old code generator
-- used. -- EZY
splitAtProcPoints :: Platform -> CLabel -> ProcPointSet-> ProcPointSet -> NonDet.LabelMap Status -> CmmDecl
                  -> UniqDSM [CmmDecl]
splitAtProcPoints _ _ _ _ _ t@(CmmData _ _) = return [t]
splitAtProcPoints platform entry_label callPPs procPoints procMap cmmProc = do
  -- Build a map from procpoints to the blocks they reach
  let (CmmProc (TopInfo {info_tbls = info_tbls}) top_l _ g@(CmmGraph {g_entry=entry})) = cmmProc

  let add graphEnv procId bid b = NonDet.mapInsert procId graph' graphEnv
        where
          graph' = Det.mapInsert bid b graph
          graph  = NonDet.mapLookup procId graphEnv `orElse` Det.mapEmpty

  let add_block :: NonDet.LabelMap (Det.LabelMap CmmBlock) -> CmmBlock -> NonDet.LabelMap (Det.LabelMap CmmBlock)
      add_block graphEnv b =
        case NonDet.mapLookup bid procMap of
          Just ProcPoint -> add graphEnv bid bid b
          Just (ReachedBy set) ->
            case NonDet.nonDetSetElems set of
              []   -> graphEnv
              [id] -> add graphEnv id bid b
              _    -> panic "Each block should be reachable from only one ProcPoint"
          Nothing -> graphEnv
        where
          bid = entryLabel b


  let liveness = cmmGlobalLiveness platform g
  let ppLiveness pp = filter isArgReg $ regSetToList $
                        expectJust "ppLiveness" $ NonDet.mapLookup pp liveness
  graphEnv <- return $ foldlGraphBlocks add_block NonDet.mapEmpty g

  -- Build a map from proc point BlockId to pairs of:
  --  * Labels for their new procedures
  --  * Labels for the info tables of their new procedures (only if
  --    the proc point is a callPP)
  -- Due to common blockification, we may overestimate the set of procpoints.
  let add_label map pp = NonDet.mapInsert pp lbls map
        where lbls | pp == entry = (entry_label, fmap cit_lbl (Det.mapLookup entry info_tbls))
                   | otherwise   = (block_lbl, guard (NonDet.setMember pp callPPs) >>
                                                 Just info_table_lbl)
                   where block_lbl      = blockLbl pp
                         info_table_lbl = infoTblLbl pp

      procLabels :: NonDet.LabelMap (CLabel, Maybe CLabel)
      procLabels = foldl' add_label NonDet.mapEmpty
                          (filter (flip Det.mapMember (toBlockMap g)) (NonDet.nonDetSetElems procPoints))

  -- In each new graph, add blocks jumping off to the new procedures,
  -- and replace branches to procpoints with branches to the jump-off blocks
  let add_jump_block :: (NonDet.LabelMap Label, [CmmBlock])
                     -> (Label, CLabel)
                     -> UniqDSM (NonDet.LabelMap Label, [CmmBlock])
      add_jump_block (env, bs) (pp, l) = do
        bid <- liftM mkBlockId getUniqueDSM
        let b    = blockJoin (CmmEntry bid GlobalScope) emptyBlock jump
            live = ppLiveness pp
            jump = CmmCall (CmmLit (CmmLabel l)) Nothing live 0 0 0
        return (NonDet.mapInsert pp bid env, b : bs)

  -- when jumping to a PP that has an info table, if
  -- tablesNextToCode is off we must jump to the entry
  -- label instead.
  let tablesNextToCode = platformTablesNextToCode platform

  let jump_label (Just info_lbl) _
                 | tablesNextToCode = info_lbl
                 | otherwise        = toEntryLbl platform info_lbl
      jump_label Nothing  block_lbl = block_lbl

  let add_if_pp id rst =
        case NonDet.mapLookup id procLabels of
          Just (lbl, mb_info_lbl) -> (id, jump_label mb_info_lbl lbl) : rst
          Nothing                 -> rst

  let add_if_branch_to_pp :: CmmBlock -> [(BlockId, CLabel)] -> [(BlockId, CLabel)]
      add_if_branch_to_pp block rst =
        case lastNode block of
          CmmBranch id            -> add_if_pp id rst
          CmmCondBranch _ ti fi _ -> add_if_pp ti (add_if_pp fi rst)
          CmmSwitch _ ids         -> foldr add_if_pp rst $ switchTargetsToList ids
          _                       -> rst

  let add_jumps :: NonDet.LabelMap CmmGraph -> (Label, Det.LabelMap CmmBlock) -> UniqDSM (NonDet.LabelMap CmmGraph)
      add_jumps newGraphEnv (ppId, blockEnv) = do
        -- find which procpoints we currently branch to
        let needed_jumps = Det.mapFoldr add_if_branch_to_pp [] blockEnv

        (jumpEnv, jumpBlocks) <-
           foldM add_jump_block (NonDet.mapEmpty, []) needed_jumps
            -- update the entry block
        let b = expectJust "block in env" $ Det.mapLookup ppId blockEnv
            blockEnv' = Det.mapInsert ppId b blockEnv
            -- replace branches to procpoints with branches to jumps
            blockEnv'' = toBlockMap $ replaceBranches jumpEnv $ ofBlockMap ppId blockEnv'
            -- add the jump blocks to the graph
            blockEnv''' = foldl' (flip addBlock) blockEnv'' jumpBlocks
        let g' = ofBlockMap ppId blockEnv'''
        -- pprTrace "g' pre jumps" (ppr g') $ do
        return (NonDet.mapInsert ppId g' newGraphEnv)

  graphEnv <- foldM add_jumps NonDet.mapEmpty $ NonDet.nonDetMapToList graphEnv

  let to_proc (bid, g)
          | bid == entry
          =  CmmProc (TopInfo {info_tbls  = info_tbls,
                               stack_info = stack_info})
                     top_l live g'
          | otherwise
          = case expectJust "pp label" $ NonDet.mapLookup bid procLabels of
              (lbl, Just info_lbl)
                 -> CmmProc (TopInfo { info_tbls = Det.mapSingleton (g_entry g) (mkEmptyContInfoTable info_lbl)
                                     , stack_info=stack_info})
                            lbl live g'
              (lbl, Nothing)
                 -> CmmProc (TopInfo {info_tbls = Det.mapEmpty, stack_info=stack_info})
                            lbl live g'
             where
              g' = replacePPIds g
              live = ppLiveness (g_entry g')
              stack_info = StackInfo { arg_space = 0
                                     , do_layout = True }
                            -- cannot use panic, this is printed by -ddump-cmm

      -- References to procpoint IDs can now be replaced with the
      -- infotable's label
      replacePPIds g = {-# SCC "replacePPIds" #-}
                       mapGraphNodes (id, mapExp repl, mapExp repl) g
        where repl e@(CmmLit (CmmBlock bid)) =
                case NonDet.mapLookup bid procLabels of
                  Just (_, Just info_lbl)  -> CmmLit (CmmLabel info_lbl)
                  _ -> e
              repl e = e

  -- The C back end expects to see return continuations before the
  -- call sites.  Here, we sort them in reverse order -- it gets
  -- reversed later.
  let add_block_num (i, map) block =
        (i + 1, NonDet.mapInsert (entryLabel block) i map)
  let (_, block_order) =
          foldl' add_block_num (0::Int, NonDet.mapEmpty :: NonDet.LabelMap Int)
                (revPostorder g)
  let sort_fn (bid, _) (bid', _) =
        compare (expectJust "block_order" $ NonDet.mapLookup bid  block_order)
                (expectJust "block_order" $ NonDet.mapLookup bid' block_order)

  return $ map to_proc $ sortBy sort_fn $ NonDet.nonDetMapToList graphEnv

-- Only called from GHC.Cmm.ProcPoint.splitAtProcPoints. NB. does a
-- recursive lookup, see comment below.
replaceBranches :: NonDet.LabelMap BlockId -> CmmGraph -> CmmGraph
replaceBranches env cmmg
  = {-# SCC "replaceBranches" #-}
    ofBlockMap (g_entry cmmg) $ Det.mapMap f $ toBlockMap cmmg
  where
    f block = replaceLastNode block $ last (lastNode block)

    last :: CmmNode O C -> CmmNode O C
    last (CmmBranch id)          = CmmBranch (lookup id)
    last (CmmCondBranch e ti fi l) = CmmCondBranch e (lookup ti) (lookup fi) l
    last (CmmSwitch e ids)       = CmmSwitch e (mapSwitchTargets lookup ids)
    last l@(CmmCall {})          = l { cml_cont = Nothing }
            -- NB. remove the continuation of a CmmCall, since this
            -- label will now be in a different CmmProc.  Not only
            -- is this tidier, it stops CmmLint from complaining.
    last l@(CmmForeignCall {})   = l
    lookup id = fmap lookup (NonDet.mapLookup id env) `orElse` id
            -- XXX: this is a recursive lookup, it follows chains
            -- until the lookup returns Nothing, at which point we
            -- return the last BlockId

-- --------------------------------------------------------------
-- Not splitting proc points: add info tables for continuations

attachContInfoTables :: ProcPointSet -> CmmDecl -> CmmDecl
attachContInfoTables call_proc_points (CmmProc top_info top_l live g)
 = CmmProc top_info{info_tbls = info_tbls'} top_l live g
 where
   info_tbls' = Det.mapUnion (info_tbls top_info) $
                Det.mapFromList [ (l, mkEmptyContInfoTable (infoTblLbl l))
                            | l <- NonDet.nonDetSetElems call_proc_points
                            , l /= g_entry g ]
attachContInfoTables _ other_decl
 = other_decl

----------------------------------------------------------------

{-
Note [Direct reachability]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Block B is directly reachable from proc point P iff control can flow
from P to B without passing through an intervening proc point.
-}

----------------------------------------------------------------

{-
Note [No simple dataflow]
~~~~~~~~~~~~~~~~~~~~~~~~~
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
