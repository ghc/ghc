{-# LANGUAGE GADTs, DisambiguateRecordFields #-}

module CmmProcPoint
    ( ProcPointSet, Status(..)
    , callProcPoints, minimalProcPointSet
    , splitAtProcPoints, procPointAnalysis
    , attachContInfoTables
    )
where

import Prelude hiding (last, unzip, succ, zip)

import DynFlags
import BlockId
import CLabel
import Cmm
import PprCmm ()
import CmmUtils
import CmmInfo
import CmmLive (cmmGlobalLiveness)
import Data.List (sortBy)
import Maybes
import Control.Monad
import Outputable
import Platform
import UniqSupply

import Hoopl

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
-}

type ProcPointSet = BlockSet

data Status
  = ReachedBy ProcPointSet  -- set of proc points that directly reach the block
  | ProcPoint               -- this block is itself a proc point

instance Outputable Status where
  ppr (ReachedBy ps)
      | setNull ps = text "<not-reached>"
      | otherwise = text "reached by" <+>
                    (hsep $ punctuate comma $ map ppr $ setElems ps)
  ppr ProcPoint = text "<procpt>"

--------------------------------------------------
-- Proc point analysis

procPointAnalysis :: ProcPointSet -> CmmGraph -> UniqSM (BlockEnv Status)
-- Once you know what the proc-points are, figure out
-- what proc-points each block is reachable from
procPointAnalysis procPoints g =
  -- pprTrace "procPointAnalysis" (ppr procPoints) $
  dataflowAnalFwdBlocks g initProcPoints $ analFwd lattice forward
  where initProcPoints = [(id, ProcPoint) | id <- setElems procPoints]

-- transfer equations

forward :: FwdTransfer CmmNode Status
forward = mkFTransfer3 first middle last
    where
      first :: CmmNode C O -> Status -> Status
      first (CmmEntry id) ProcPoint = ReachedBy $ setSingleton id
      first  _ x = x

      middle _ x = x

      last :: CmmNode O C -> Status -> FactBase Status
      last l x = mkFactBase lattice $ map (\id -> (id, x)) (successors l)

lattice :: DataflowLattice Status
lattice = DataflowLattice "direct proc-point reachability" unreached add_to
    where unreached = ReachedBy setEmpty
          add_to _ (OldFact ProcPoint) _ = (NoChange, ProcPoint)
          add_to _ _ (NewFact ProcPoint) = (SomeChange, ProcPoint)
                       -- because of previous case
          add_to _ (OldFact (ReachedBy p)) (NewFact (ReachedBy p'))
             | setSize union > setSize p = (SomeChange, ReachedBy union)
             | otherwise                 = (NoChange, ReachedBy p)
           where
             union = setUnion p' p

----------------------------------------------------------------------

-- It is worth distinguishing two sets of proc points: those that are
-- induced by calls in the original graph and those that are
-- introduced because they're reachable from multiple proc points.
--
-- Extract the set of Continuation BlockIds, see Note [Continuation BlockIds].
callProcPoints      :: CmmGraph -> ProcPointSet
callProcPoints g = foldGraphBlocks add (setSingleton (g_entry g)) g
  where add :: CmmBlock -> BlockSet -> BlockSet
        add b set = case lastNode b of
                      CmmCall {cml_cont = Just k} -> setInsert k set
                      CmmForeignCall {succ=k}     -> setInsert k set
                      _ -> set

minimalProcPointSet :: Platform -> ProcPointSet -> CmmGraph
                    -> UniqSM ProcPointSet
-- Given the set of successors of calls (which must be proc-points)
-- figure out the minimal set of necessary proc-points
minimalProcPointSet platform callProcPoints g
  = extendPPSet platform g (postorderDfs g) callProcPoints

extendPPSet :: Platform -> CmmGraph -> [CmmBlock] -> ProcPointSet -> UniqSM ProcPointSet
extendPPSet platform g blocks procPoints =
    do env <- procPointAnalysis procPoints g
       -- pprTrace "extensPPSet" (ppr env) $ return ()
       let add block pps = let id = entryLabel block
                           in  case mapLookup id env of
                                 Just ProcPoint -> setInsert id pps
                                 _ -> pps
           procPoints' = foldGraphBlocks add setEmpty g
           newPoints = mapMaybe ppSuccessor blocks
           newPoint  = listToMaybe newPoints
           ppSuccessor b =
               let nreached id = case mapLookup id env `orElse`
                                       pprPanic "no ppt" (ppr id <+> ppr b) of
                                   ProcPoint -> 1
                                   ReachedBy ps -> setSize ps
                   block_procpoints = nreached (entryLabel b)
                   -- | Looking for a successor of b that is reached by
                   -- more proc points than b and is not already a proc
                   -- point.  If found, it can become a proc point.
                   newId succ_id = not (setMember succ_id procPoints') &&
                                   nreached succ_id > block_procpoints
               in  listToMaybe $ filter newId $ successors b
{-
       case newPoints of
           []  -> return procPoints'
           pps -> extendPPSet g blocks
                    (foldl extendBlockSet procPoints' pps)
-}
       case newPoint of
         Just id ->
             if setMember id procPoints'
                then panic "added old proc pt"
                else extendPPSet platform g blocks (setInsert id procPoints')
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
splitAtProcPoints :: DynFlags -> CLabel -> ProcPointSet-> ProcPointSet -> BlockEnv Status ->
                     CmmDecl -> UniqSM [CmmDecl]
splitAtProcPoints dflags entry_label callPPs procPoints procMap
                  (CmmProc (TopInfo {info_tbls = info_tbls})
                           top_l _ g@(CmmGraph {g_entry=entry})) =
  do -- Build a map from procpoints to the blocks they reach
     let addBlock b graphEnv =
           case mapLookup bid procMap of
             Just ProcPoint -> add graphEnv bid bid b
             Just (ReachedBy set) ->
               case setElems set of
                 []   -> graphEnv
                 [id] -> add graphEnv id bid b 
                 _    -> panic "Each block should be reachable from only one ProcPoint"
             Nothing -> graphEnv
           where bid = entryLabel b
         add graphEnv procId bid b = mapInsert procId graph' graphEnv
               where graph  = mapLookup procId graphEnv `orElse` mapEmpty
                     graph' = mapInsert bid b graph

     let liveness = cmmGlobalLiveness dflags g
     let ppLiveness pp = filter isArgReg $
                         regSetToList $
                         expectJust "ppLiveness" $ mapLookup pp liveness

     graphEnv <- return $ foldGraphBlocks addBlock emptyBlockMap g

     -- Build a map from proc point BlockId to pairs of:
     --  * Labels for their new procedures
     --  * Labels for the info tables of their new procedures (only if
     --    the proc point is a callPP)
     -- Due to common blockification, we may overestimate the set of procpoints.
     let add_label map pp = mapInsert pp lbls map
           where lbls | pp == entry = (entry_label, fmap cit_lbl (mapLookup entry info_tbls))
                      | otherwise   = (block_lbl, guard (setMember pp callPPs) >>
                                                    Just (toInfoLbl block_lbl))
                      where block_lbl = blockLbl pp

         procLabels :: LabelMap (CLabel, Maybe CLabel)
         procLabels = foldl add_label mapEmpty
                            (filter (flip mapMember (toBlockMap g)) (setElems procPoints))

     -- In each new graph, add blocks jumping off to the new procedures,
     -- and replace branches to procpoints with branches to the jump-off blocks
     let add_jump_block (env, bs) (pp, l) =
           do bid <- liftM mkBlockId getUniqueM
              let b = blockJoin (CmmEntry bid) emptyBlock jump
                  live = ppLiveness pp
                  jump = CmmCall (CmmLit (CmmLabel l)) Nothing live 0 0 0
              return (mapInsert pp bid env, b : bs)

         add_jumps newGraphEnv (ppId, blockEnv) =
           do let needed_jumps = -- find which procpoints we currently branch to
                    mapFold add_if_branch_to_pp [] blockEnv
                  add_if_branch_to_pp :: CmmBlock -> [(BlockId, CLabel)] -> [(BlockId, CLabel)]
                  add_if_branch_to_pp block rst =
                    case lastNode block of
                      CmmBranch id          -> add_if_pp id rst
                      CmmCondBranch _ ti fi -> add_if_pp ti (add_if_pp fi rst)
                      CmmSwitch _ tbl       -> foldr add_if_pp rst (catMaybes tbl)
                      _                     -> rst

                  -- when jumping to a PP that has an info table, if
                  -- tablesNextToCode is off we must jump to the entry
                  -- label instead.
                  jump_label (Just info_lbl) _
                             | tablesNextToCode dflags = info_lbl
                             | otherwise               = toEntryLbl info_lbl
                  jump_label Nothing         block_lbl = block_lbl

                  add_if_pp id rst = case mapLookup id procLabels of
                                       Just (lbl, mb_info_lbl) -> (id, jump_label mb_info_lbl lbl) : rst
                                       Nothing                 -> rst
              (jumpEnv, jumpBlocks) <-
                 foldM add_jump_block (mapEmpty, []) needed_jumps
                  -- update the entry block
              let b = expectJust "block in env" $ mapLookup ppId blockEnv
                  blockEnv' = mapInsert ppId b blockEnv
                  -- replace branches to procpoints with branches to jumps
                  blockEnv'' = toBlockMap $ replaceBranches jumpEnv $ ofBlockMap ppId blockEnv'
                  -- add the jump blocks to the graph
                  blockEnv''' = foldl (flip insertBlock) blockEnv'' jumpBlocks
              let g' = ofBlockMap ppId blockEnv'''
              -- pprTrace "g' pre jumps" (ppr g') $ do
              return (mapInsert ppId g' newGraphEnv)

     graphEnv <- foldM add_jumps emptyBlockMap $ mapToList graphEnv

     let to_proc (bid, g)
             | bid == entry
             =  CmmProc (TopInfo {info_tbls  = info_tbls,
                                  stack_info = stack_info})
                        top_l live g'
             | otherwise
             = case expectJust "pp label" $ mapLookup bid procLabels of
                 (lbl, Just info_lbl)
                    -> CmmProc (TopInfo { info_tbls = mapSingleton (g_entry g) (mkEmptyContInfoTable info_lbl)
                                        , stack_info=stack_info})
                               lbl live g'
                 (lbl, Nothing)
                    -> CmmProc (TopInfo {info_tbls = mapEmpty, stack_info=stack_info})
                               lbl live g'
                where
                 g' = replacePPIds g
                 live = ppLiveness (g_entry g')
                 stack_info = StackInfo { arg_space = 0
                                        , updfr_space =  Nothing
                                        , do_layout = True }
                               -- cannot use panic, this is printed by -ddump-cmm

         -- References to procpoint IDs can now be replaced with the
         -- infotable's label
         replacePPIds g = {-# SCC "replacePPIds" #-}
                          mapGraphNodes (id, mapExp repl, mapExp repl) g
           where repl e@(CmmLit (CmmBlock bid)) =
                   case mapLookup bid procLabels of
                     Just (_, Just info_lbl)  -> CmmLit (CmmLabel info_lbl)
                     _ -> e
                 repl e = e

     -- The C back end expects to see return continuations before the
     -- call sites.  Here, we sort them in reverse order -- it gets
     -- reversed later.
     let (_, block_order) = foldl add_block_num (0::Int, emptyBlockMap) (postorderDfs g)
         add_block_num (i, map) block = (i+1, mapInsert (entryLabel block) i map)
         sort_fn (bid, _) (bid', _) =
           compare (expectJust "block_order" $ mapLookup bid  block_order)
                   (expectJust "block_order" $ mapLookup bid' block_order)
     procs <- return $ map to_proc $ sortBy sort_fn $ mapToList graphEnv
     return -- pprTrace "procLabels" (ppr procLabels)
            -- pprTrace "splitting graphs" (ppr procs)
            procs
splitAtProcPoints _ _ _ _ _ t@(CmmData _ _) = return [t]

-- Only called from CmmProcPoint.splitAtProcPoints. NB. does a
-- recursive lookup, see comment below.
replaceBranches :: BlockEnv BlockId -> CmmGraph -> CmmGraph
replaceBranches env cmmg
  = {-# SCC "replaceBranches" #-}
    ofBlockMap (g_entry cmmg) $ mapMap f $ toBlockMap cmmg
  where
    f block = replaceLastNode block $ last (lastNode block)

    last :: CmmNode O C -> CmmNode O C
    last (CmmBranch id)          = CmmBranch (lookup id)
    last (CmmCondBranch e ti fi) = CmmCondBranch e (lookup ti) (lookup fi)
    last (CmmSwitch e tbl)       = CmmSwitch e (map (fmap lookup) tbl)
    last l@(CmmCall {})          = l { cml_cont = Nothing }
            -- NB. remove the continuation of a CmmCall, since this
            -- label will now be in a different CmmProc.  Not only
            -- is this tidier, it stops CmmLint from complaining.
    last l@(CmmForeignCall {})   = l
    lookup id = fmap lookup (mapLookup id env) `orElse` id
            -- XXX: this is a recursive lookup, it follows chains
            -- until the lookup returns Nothing, at which point we
            -- return the last BlockId

-- --------------------------------------------------------------
-- Not splitting proc points: add info tables for continuations

attachContInfoTables :: ProcPointSet -> CmmDecl -> CmmDecl
attachContInfoTables call_proc_points (CmmProc top_info top_l live g)
 = CmmProc top_info{info_tbls = info_tbls'} top_l live g
 where
   info_tbls' = mapUnion (info_tbls top_info) $
                mapFromList [ (l, mkEmptyContInfoTable (infoTblLbl l))
                            | l <- setElems call_proc_points
                            , l /= g_entry g ]
attachContInfoTables _ other_decl
 = other_decl

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
