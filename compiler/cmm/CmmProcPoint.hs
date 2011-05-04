{-# LANGUAGE GADTs, DisambiguateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module CmmProcPoint
    ( ProcPointSet, Status(..)
    , callProcPoints, minimalProcPointSet
    , addProcPointProtocols, splitAtProcPoints, procPointAnalysis
    )
where

import Prelude hiding (last, unzip, succ, zip)

import BlockId
import CLabel
import Cmm
import CmmDecl
import CmmExpr
import CmmContFlowOpt
import CmmInfo
import CmmLive
import Constants
import Data.List (sortBy)
import Maybes
import MkGraph
import Control.Monad
import OptimizationFuel
import Outputable
import UniqSet
import UniqSupply

import Compiler.Hoopl

import qualified Data.Map as Map

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

lattice :: DataflowLattice Status
lattice = DataflowLattice "direct proc-point reachability" unreached add_to
    where unreached = ReachedBy setEmpty
          add_to _ (OldFact ProcPoint) _ = (NoChange, ProcPoint)
          add_to _ _ (NewFact ProcPoint) = (SomeChange, ProcPoint) -- because of previous case
          add_to _ (OldFact (ReachedBy p)) (NewFact (ReachedBy p')) =
              let union = setUnion p' p
              in  if setSize union > setSize p then (SomeChange, ReachedBy union)
                                               else (NoChange, ReachedBy p)
--------------------------------------------------
-- transfer equations

forward :: FwdTransfer CmmNode Status
forward = mkFTransfer3 first middle ((mkFactBase lattice . ) . last)
    where first :: CmmNode C O -> Status -> Status
          first (CmmEntry id) ProcPoint = ReachedBy $ setSingleton id
          first  _ x = x

          middle _ x = x

          last :: CmmNode O C -> Status -> [(Label, Status)]
          last (CmmCall {cml_cont = Just k}) _ = [(k, ProcPoint)]
          last (CmmForeignCall {succ = k})   _ = [(k, ProcPoint)]
          last l x = map (\id -> (id, x)) (successors l)

-- It is worth distinguishing two sets of proc points:
-- those that are induced by calls in the original graph
-- and those that are introduced because they're reachable from multiple proc points.
callProcPoints      :: CmmGraph -> ProcPointSet
callProcPoints g = foldGraphBlocks add (setSingleton (g_entry g)) g
  where add :: CmmBlock -> BlockSet -> BlockSet
        add b set = case lastNode b of
                      CmmCall {cml_cont = Just k} -> setInsert k set
                      CmmForeignCall {succ=k}     -> setInsert k set
                      _ -> set

minimalProcPointSet :: ProcPointSet -> CmmGraph -> FuelUniqSM ProcPointSet
-- Given the set of successors of calls (which must be proc-points)
-- figure out the minimal set of necessary proc-points
minimalProcPointSet callProcPoints g = extendPPSet g (postorderDfs g) callProcPoints

procPointAnalysis :: ProcPointSet -> CmmGraph -> FuelUniqSM (BlockEnv Status)
-- Once you know what the proc-points are, figure out
-- what proc-points each block is reachable from
procPointAnalysis procPoints g =
  liftM snd $ dataflowPassFwd g initProcPoints $ analFwd lattice forward
  where initProcPoints = [(id, ProcPoint) | id <- setElems procPoints]

extendPPSet :: CmmGraph -> [CmmBlock] -> ProcPointSet -> FuelUniqSM ProcPointSet
extendPPSet g blocks procPoints =
    do env <- procPointAnalysis procPoints g
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
       case newPoint of Just id ->
                          if setMember id procPoints' then panic "added old proc pt"
                          else extendPPSet g blocks (setInsert id procPoints')
                        Nothing -> return procPoints'


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

data Protocol = Protocol Convention CmmFormals Area
  deriving Eq
instance Outputable Protocol where
  ppr (Protocol c fs a) = text "Protocol" <+> ppr c <+> ppr fs <+> ppr a

-- | Function 'optimize_calls' chooses protocols only for those proc
-- points that are relevant to the optimization explained above.
-- The others are assigned by 'add_unassigned', which is not yet clever.

addProcPointProtocols :: ProcPointSet -> ProcPointSet -> CmmGraph -> FuelUniqSM CmmGraph
addProcPointProtocols callPPs procPoints g =
  do liveness <- cmmLiveness g
     (protos, g') <- optimize_calls liveness g
     blocks'' <- add_CopyOuts protos procPoints g'
     return $ ofBlockMap (g_entry g) blocks''
    where optimize_calls liveness g =  -- see Note [Separate Adams optimization]
            do let (protos, blocks') =
                       foldGraphBlocks maybe_add_call (mapEmpty, mapEmpty) g
                   protos' = add_unassigned liveness procPoints protos
               let g' = ofBlockMap (g_entry g) (add_CopyIns callPPs protos' blocks')
               return (protos', removeUnreachableBlocks g')
          maybe_add_call :: CmmBlock -> (BlockEnv Protocol, BlockEnv CmmBlock)
                         -> (BlockEnv Protocol, BlockEnv CmmBlock)
          -- ^ If the block is a call whose continuation goes to a proc point
          -- whose protocol either matches the continuation's or is not yet set,
          -- redirect the call (cf 'newblock') and set the protocol if necessary
          maybe_add_call block (protos, blocks) =
              case lastNode block of
                CmmCall tgt (Just k) args res s
                    | Just proto <- mapLookup k protos,
                      Just pee   <- branchesToProcPoint k
                    -> let newblock = replaceLastNode block (CmmCall tgt (Just pee)
                                                                     args res s)
                           changed_blocks   = insertBlock newblock blocks
                           unchanged_blocks = insertBlock block    blocks
                       in case mapLookup pee protos of
                            Nothing -> (mapInsert pee proto protos, changed_blocks)
                            Just proto' ->
                              if proto == proto' then (protos, changed_blocks)
                              else (protos, unchanged_blocks)
                _ -> (protos, insertBlock block blocks)

          branchesToProcPoint :: BlockId -> Maybe BlockId
          -- ^ Tells whether the named block is just a branch to a proc point
          branchesToProcPoint id =
              let block = mapLookup id (toBlockMap g) `orElse`
                                    panic "branch out of graph"
              in case blockToNodeList block of
-- MS: There is an ugly bug in ghc-6.10, which rejects following valid code.
-- After trying several tricks, the NOINLINE on getItOut worked. Uffff.
#if __GLASGOW_HASKELL__ >= 612
                   (_, [], JustC (CmmBranch pee)) | setMember pee procPoints -> Just pee
                   _                                                         -> Nothing
#else
                   (_, [], exit) | CmmBranch pee <- getItOut exit
                                 , setMember pee procPoints      -> Just pee
                   _                                             -> Nothing
              where {-# NOINLINE getItOut #-}
                    getItOut :: MaybeC C a -> a
                    getItOut (JustC a) = a
#endif

-- | For now, following a suggestion by Ben Lippmeier, we pass all
-- live variables as arguments, hoping that a clever register
-- allocator might help.

add_unassigned :: BlockEnv CmmLive -> ProcPointSet -> BlockEnv Protocol ->
                  BlockEnv Protocol
add_unassigned = pass_live_vars_as_args

pass_live_vars_as_args :: BlockEnv CmmLive -> ProcPointSet ->
                          BlockEnv Protocol -> BlockEnv Protocol
pass_live_vars_as_args _liveness procPoints protos = protos'
  where protos' = setFold addLiveVars protos procPoints
        addLiveVars :: BlockId -> BlockEnv Protocol -> BlockEnv Protocol
        addLiveVars id protos =
            case mapLookup id protos of
              Just _  -> protos
              Nothing -> let live = emptyRegSet
                                    --lookupBlockEnv _liveness id `orElse`
                                    --panic ("no liveness at block " ++ show id)
                             formals = uniqSetToList live
                             prot = Protocol Private formals $ CallArea $ Young id
                         in  mapInsert id prot protos


-- | Add copy-in instructions to each proc point that did not arise from a call
-- instruction. (Proc-points that arise from calls already have their copy-in instructions.)

add_CopyIns :: ProcPointSet -> BlockEnv Protocol -> BlockEnv CmmBlock -> BlockEnv CmmBlock
add_CopyIns callPPs protos blocks = mapFold maybe_insert_CopyIns mapEmpty blocks
    where maybe_insert_CopyIns block blocks
             | not $ setMember bid callPPs
             , Just (Protocol c fs _area) <- mapLookup bid protos
             = let nodes     = copyInSlot c fs
                   (h, m, l) = blockToNodeList block
               in insertBlock (blockOfNodeList (h, nodes ++ m, l)) blocks
             | otherwise = insertBlock block blocks
           where bid = entryLabel block


-- | Add a CopyOut node before each procpoint.
-- If the predecessor is a call, then the copy outs should already be done by the callee.
-- Note: If we need to add copy-out instructions, they may require stack space,
-- so we accumulate a map from the successors to the necessary stack space,
-- then update the successors after we have finished inserting the copy-outs.

add_CopyOuts :: BlockEnv Protocol -> ProcPointSet -> CmmGraph ->
                FuelUniqSM (BlockEnv CmmBlock)
add_CopyOuts protos procPoints g = foldGraphBlocks mb_copy_out (return mapEmpty) g
    where mb_copy_out :: CmmBlock -> FuelUniqSM (BlockEnv CmmBlock) ->
                                     FuelUniqSM (BlockEnv CmmBlock)
          mb_copy_out b z | entryLabel b == g_entry g = skip b z
          mb_copy_out b z =
            case lastNode b of
              CmmCall {}        -> skip b z -- copy out done by callee
              CmmForeignCall {} -> skip b z -- copy out done by callee
              _ -> copy_out b z
          copy_out b z = foldr trySucc init (successors b) >>= finish
            where init = (\bmap -> (b, bmap)) `liftM` z
                  trySucc succId z =
                    if setMember succId procPoints then
                      case mapLookup succId protos of
                        Nothing -> z
                        Just (Protocol c fs _area) -> insert z succId $ copyOutSlot c fs
                    else z
                  insert z succId m =
                    do (b, bmap) <- z
                       (b, bs)   <- insertBetween b m succId
                       -- pprTrace "insert for succ" (ppr succId <> ppr m) $ do
                       return $ (b, foldl (flip insertBlock) bmap bs)
                  finish (b, bmap) = return $ insertBlock b bmap
          skip b bs = insertBlock b `liftM` bs

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
splitAtProcPoints :: CLabel -> ProcPointSet-> ProcPointSet -> BlockEnv Status ->
                     CmmTop -> FuelUniqSM [CmmTop]
splitAtProcPoints entry_label callPPs procPoints procMap
                  (CmmProc (TopInfo {info_tbl=info_tbl, stack_info=stack_info})
                           top_l g@(CmmGraph {g_entry=entry})) =
  do -- Build a map from procpoints to the blocks they reach
     let addBlock b graphEnv =
           case mapLookup bid procMap of
             Just ProcPoint -> add graphEnv bid bid b
             Just (ReachedBy set) ->
               case setElems set of
                 []   -> graphEnv
                 [id] -> add graphEnv id bid b 
                 _    -> panic "Each block should be reachable from only one ProcPoint"
             Nothing -> pprPanic "block not reached by a proc point?" (ppr bid)
           where bid = entryLabel b
         add graphEnv procId bid b = mapInsert procId graph' graphEnv
               where graph  = mapLookup procId graphEnv `orElse` mapEmpty
                     graph' = mapInsert bid b graph
     graphEnv <- return $ foldGraphBlocks addBlock emptyBlockMap g
     -- Build a map from proc point BlockId to labels for their new procedures
     -- Due to common blockification, we may overestimate the set of procpoints.
     let add_label map pp = return $ Map.insert pp lbl map
           where lbl = if pp == entry then entry_label else blockLbl pp
     procLabels <- foldM add_label Map.empty
                         (filter (flip mapMember (toBlockMap g)) (setElems procPoints))
     -- For each procpoint, we need to know the SP offset on entry.
     -- If the procpoint is:
     --  - continuation of a call, the SP offset is in the call
     --  - otherwise, 0 (and left out of the spEntryMap)
     let add_sp_off :: CmmBlock -> BlockEnv CmmStackInfo -> BlockEnv CmmStackInfo
         add_sp_off b env =
           case lastNode b of
             CmmCall {cml_cont = Just succ, cml_ret_args = off, cml_ret_off = updfr_off} ->
               mapInsert succ (StackInfo { arg_space = off, updfr_space = Just updfr_off}) env
             CmmForeignCall {succ = succ, updfr = updfr_off} ->
               mapInsert succ (StackInfo { arg_space = wORD_SIZE, updfr_space = Just updfr_off}) env
             _ -> env
         spEntryMap = foldGraphBlocks add_sp_off (mapInsert entry stack_info emptyBlockMap) g
         getStackInfo id = mapLookup id spEntryMap `orElse` StackInfo {arg_space = 0, updfr_space = Nothing}
     -- In each new graph, add blocks jumping off to the new procedures,
     -- and replace branches to procpoints with branches to the jump-off blocks
     let add_jump_block (env, bs) (pp, l) =
           do bid <- liftM mkBlockId getUniqueM
              let b = blockOfNodeList (JustC (CmmEntry bid), [], JustC jump)
                  StackInfo {arg_space = argSpace, updfr_space = off} = getStackInfo pp
                  jump = CmmCall (CmmLit (CmmLabel l')) Nothing argSpace 0
                                 (off `orElse` 0) -- Jump's shouldn't need the offset...
                  l' = if setMember pp callPPs then entryLblToInfoLbl l else l
              return (mapInsert pp bid env, b : bs)
         add_jumps (newGraphEnv) (ppId, blockEnv) =
           do let needed_jumps = -- find which procpoints we currently branch to
                    mapFold add_if_branch_to_pp [] blockEnv
                  add_if_branch_to_pp :: CmmBlock -> [(BlockId, CLabel)] -> [(BlockId, CLabel)]
                  add_if_branch_to_pp block rst =
                    case lastNode block of
                      CmmBranch id          -> add_if_pp id rst
                      CmmCondBranch _ ti fi -> add_if_pp ti (add_if_pp fi rst)
                      CmmSwitch _ tbl       -> foldr add_if_pp rst (catMaybes tbl)
                      _                     -> rst
                  add_if_pp id rst = case Map.lookup id procLabels of
                                       Just x -> (id, x) : rst
                                       Nothing -> rst
              (jumpEnv, jumpBlocks) <-
                 foldM add_jump_block (mapEmpty, []) needed_jumps
                  -- update the entry block
              let b = expectJust "block in env" $ mapLookup ppId blockEnv
                  off = getStackInfo ppId
                  blockEnv' = mapInsert ppId b blockEnv
                  -- replace branches to procpoints with branches to jumps
                  blockEnv'' = toBlockMap $ replaceBranches jumpEnv $ ofBlockMap ppId blockEnv'
                  -- add the jump blocks to the graph
                  blockEnv''' = foldl (flip insertBlock) blockEnv'' jumpBlocks
              let g' = (off, ofBlockMap ppId blockEnv''')
              -- pprTrace "g' pre jumps" (ppr g') $ do
              return (mapInsert ppId g' newGraphEnv)
     graphEnv <- foldM add_jumps emptyBlockMap $ mapToList graphEnv
     let to_proc (bid, (stack_info, g)) | setMember bid callPPs =
           if bid == entry then
             CmmProc (TopInfo {info_tbl=info_tbl, stack_info=stack_info})
                     top_l (replacePPIds g)
           else
             CmmProc (TopInfo {info_tbl=emptyContInfoTable, stack_info=stack_info})
                     lbl (replacePPIds g)
           where lbl = expectJust "pp label" $ Map.lookup bid procLabels
         to_proc (bid, (stack_info, g)) =
           CmmProc (TopInfo {info_tbl=CmmNonInfoTable, stack_info=stack_info})
                   lbl (replacePPIds g)
             where lbl = expectJust "pp label" $ Map.lookup bid procLabels
         -- References to procpoint IDs can now be replaced with the infotable's label
         replacePPIds g = mapGraphNodes (id, mapExp repl, mapExp repl) g
           where repl e@(CmmLit (CmmBlock bid)) =
                   case Map.lookup bid procLabels of
                     Just l  -> CmmLit (CmmLabel (entryLblToInfoLbl l))
                     Nothing -> e
                 repl e = e
     -- The C back end expects to see return continuations before the call sites.
     -- Here, we sort them in reverse order -- it gets reversed later.
     let (_, block_order) = foldl add_block_num (0::Int, emptyBlockMap) (postorderDfs g)
         add_block_num (i, map) block = (i+1, mapInsert (entryLabel block) i map)
         sort_fn (bid, _) (bid', _) =
           compare (expectJust "block_order" $ mapLookup bid  block_order)
                   (expectJust "block_order" $ mapLookup bid' block_order)
     procs <- return $ map to_proc $ sortBy sort_fn $ mapToList graphEnv
     return -- pprTrace "procLabels" (ppr procLabels)
            -- pprTrace "splitting graphs" (ppr procs)
            procs
splitAtProcPoints _ _ _ _ t@(CmmData _ _) = return [t]

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
