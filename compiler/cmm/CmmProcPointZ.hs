module CmmProcPointZ
    ( ProcPointSet, Status(..)
    , callProcPoints, minimalProcPointSet
    , addProcPointProtocols, splitAtProcPoints, procPointAnalysis
    )
where

import Prelude hiding (zip, unzip, last)

import BlockId
import CLabel
import Cmm hiding (blockId)
import CmmContFlowOpt
import CmmInfo
import CmmLiveZ
import CmmTx
import DFMonad
import Data.List (sortBy)
import Maybes
import MkZipCfg
import MkZipCfgCmm hiding (CmmBlock, CmmGraph, CmmTopZ)
import Control.Monad
import Outputable
import UniqSet
import UniqSupply
import ZipCfg
import ZipCfgCmmRep
import ZipDataflow

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
      | isEmptyBlockSet ps = text "<not-reached>"
      | otherwise = text "reached by" <+>
                    (hsep $ punctuate comma $ map ppr $ blockSetToList ps)
  ppr ProcPoint = text "<procpt>"


lattice :: DataflowLattice Status
lattice = DataflowLattice "direct proc-point reachability" unreached add_to False
    where unreached = ReachedBy emptyBlockSet
          add_to _ ProcPoint = noTx ProcPoint
          add_to ProcPoint _ = aTx ProcPoint -- aTx because of previous case again
          add_to (ReachedBy p) (ReachedBy p') =
              let union = unionBlockSets p p'
              in  if sizeBlockSet union > sizeBlockSet p' then
                      aTx (ReachedBy union)
                  else
                      noTx (ReachedBy p')
--------------------------------------------------
-- transfer equations

forward :: ForwardTransfers Middle Last Status
forward = ForwardTransfers first middle last exit
    where first id ProcPoint = ReachedBy $ unitBlockSet id
          first  _ x = x
          middle _ x = x
          last (LastCall _ (Just id) _ _ _) _ = LastOutFacts [(id, ProcPoint)]
          last l x = LastOutFacts $ map (\id -> (id, x)) (succs l)
          exit x   = x
                
-- It is worth distinguishing two sets of proc points:
-- those that are induced by calls in the original graph
-- and those that are introduced because they're reachable from multiple proc points.
callProcPoints      :: CmmGraph -> ProcPointSet
callProcPoints g = fold_blocks add (unitBlockSet (lg_entry g)) g
  where add b set = case last $ unzip b of
                      LastOther (LastCall _ (Just k) _ _ _) -> extendBlockSet set k
                      _ -> set

minimalProcPointSet :: ProcPointSet -> CmmGraph -> FuelMonad ProcPointSet
-- Given the set of successors of calls (which must be proc-points)
-- figure ou the minimal set of necessary proc-points
minimalProcPointSet callProcPoints g = extendPPSet g (postorder_dfs g) callProcPoints

type PPFix = FuelMonad (ForwardFixedPoint Middle Last Status ())

procPointAnalysis :: ProcPointSet -> CmmGraph -> FuelMonad (BlockEnv Status)
-- Once you know what the proc-points are, figure out
-- what proc-points each block is reachable from
procPointAnalysis procPoints g =
  let addPP env id = extendBlockEnv env id ProcPoint
      initProcPoints = foldl addPP emptyBlockEnv (blockSetToList procPoints)
  in liftM zdfFpFacts $
        (zdfSolveFrom initProcPoints "proc-point reachability" lattice
                              forward (fact_bot lattice) $ graphOfLGraph g :: PPFix)

extendPPSet :: CmmGraph -> [CmmBlock] -> ProcPointSet -> FuelMonad ProcPointSet
extendPPSet g blocks procPoints =
    do env <- procPointAnalysis procPoints g
       let add block pps = let id = blockId block
                           in  case lookupBlockEnv env id of
                                 Just ProcPoint -> extendBlockSet pps id
                                 _ -> pps
           procPoints' = fold_blocks add emptyBlockSet g
           newPoints = mapMaybe ppSuccessor blocks
           newPoint  = listToMaybe newPoints 
           ppSuccessor b@(Block bid _) =
               let nreached id = case lookupBlockEnv env id `orElse`
                                       pprPanic "no ppt" (ppr id <+> ppr b) of
                                   ProcPoint -> 1
                                   ReachedBy ps -> sizeBlockSet ps
                   block_procpoints = nreached bid
                   -- | Looking for a successor of b that is reached by
                   -- more proc points than b and is not already a proc
                   -- point.  If found, it can become a proc point.
                   newId succ_id = not (elemBlockSet succ_id procPoints') &&
                                   nreached succ_id > block_procpoints
               in  listToMaybe $ filter newId $ succs b
{-
       case newPoints of
           []  -> return procPoints'
           pps -> extendPPSet g blocks
                    (foldl extendBlockSet procPoints' pps)
-}
       case newPoint of Just id ->
                          if elemBlockSet id procPoints' then panic "added old proc pt"
                          else extendPPSet g blocks (extendBlockSet procPoints' id)
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

addProcPointProtocols :: ProcPointSet -> ProcPointSet -> CmmGraph -> FuelMonad CmmGraph
addProcPointProtocols callPPs procPoints g =
  do liveness <- cmmLivenessZ g
     (protos, g') <- optimize_calls liveness g
     blocks'' <- add_CopyOuts protos procPoints g'
     return $ LGraph (lg_entry g) blocks''
    where optimize_calls liveness g =  -- see Note [Separate Adams optimization]
            do let (protos, blocks') =
                       fold_blocks maybe_add_call (init_protocols, emptyBlockEnv) g
                   protos' = add_unassigned liveness procPoints protos
               blocks <- add_CopyIns callPPs protos' blocks'
               let g' = LGraph (lg_entry g) (mkBlockEnv (map withKey (concat blocks)))
                   withKey b@(Block bid _) = (bid, b)
               return (protos', runTx removeUnreachableBlocksZ g')
          maybe_add_call :: CmmBlock -> (BlockEnv Protocol, BlockEnv CmmBlock)
                         -> (BlockEnv Protocol, BlockEnv CmmBlock)
          -- ^ If the block is a call whose continuation goes to a proc point
          -- whose protocol either matches the continuation's or is not yet set,
          -- redirect the call (cf 'newblock') and set the protocol if necessary
          maybe_add_call block (protos, blocks) =
              case goto_end $ unzip block of
                (h, LastOther (LastCall tgt (Just k) args res s))
                    | Just proto <- lookupBlockEnv protos k,
                      Just pee   <- branchesToProcPoint k
                    -> let newblock = zipht h (tailOfLast (LastCall tgt (Just pee)
                                                                    args res s))
                           changed_blocks   = insertBlock newblock blocks
                           unchanged_blocks = insertBlock block    blocks
                       in case lookupBlockEnv protos pee of
                            Nothing -> (extendBlockEnv protos pee proto,changed_blocks)
                            Just proto' ->
                              if proto == proto' then (protos, changed_blocks)
                              else (protos, unchanged_blocks)
                _ -> (protos, insertBlock block blocks)

          branchesToProcPoint :: BlockId -> Maybe BlockId
          -- ^ Tells whether the named block is just a branch to a proc point
          branchesToProcPoint id =
              let (Block _ t) = lookupBlockEnv (lg_blocks g) id `orElse`
                                    panic "branch out of graph"
              in case t of
                   ZLast (LastOther (LastBranch pee))
                       | elemBlockSet pee procPoints -> Just pee
                   _ -> Nothing
          init_protocols = fold_blocks maybe_add_proto emptyBlockEnv g
          maybe_add_proto :: CmmBlock -> BlockEnv Protocol -> BlockEnv Protocol
          --maybe_add_proto (Block id (ZTail (CopyIn c _ fs _srt) _)) env =
          --    extendBlockEnv env id (Protocol c fs $ toArea id fs)
          maybe_add_proto _ env = env
          -- JD: Is this proto stuff even necessary, now that we have
          -- common blockification?

-- | For now, following a suggestion by Ben Lippmeier, we pass all
-- live variables as arguments, hoping that a clever register
-- allocator might help.

add_unassigned :: BlockEnv CmmLive -> ProcPointSet -> BlockEnv Protocol ->
                  BlockEnv Protocol
add_unassigned = pass_live_vars_as_args

pass_live_vars_as_args :: BlockEnv CmmLive -> ProcPointSet ->
                          BlockEnv Protocol -> BlockEnv Protocol
pass_live_vars_as_args _liveness procPoints protos = protos'
  where protos' = foldBlockSet addLiveVars protos procPoints
        addLiveVars :: BlockId -> BlockEnv Protocol -> BlockEnv Protocol
        addLiveVars id protos =
            case lookupBlockEnv protos id of
              Just _  -> protos
              Nothing -> let live = emptyRegSet
                                    --lookupBlockEnv _liveness id `orElse`
                                    --panic ("no liveness at block " ++ show id)
                             formals = uniqSetToList live
                             prot = Protocol Private formals $ CallArea $ Young id
                         in  extendBlockEnv protos id prot


-- | Add copy-in instructions to each proc point that did not arise from a call
-- instruction. (Proc-points that arise from calls already have their copy-in instructions.)

add_CopyIns :: ProcPointSet -> BlockEnv Protocol -> BlockEnv CmmBlock ->
               FuelMonad [[CmmBlock]]
add_CopyIns callPPs protos blocks =
  liftUniq $ mapM maybe_insert_CopyIns (blockEnvToList blocks)
    where maybe_insert_CopyIns (_, b@(Block id t))
           | not $ elemBlockSet id callPPs
           = case lookupBlockEnv protos id of
               Just (Protocol c fs _area) ->
                 do LGraph _ blocks <-
                      lgraphOfAGraph (mkLabel id <*> copyInSlot c fs <*> mkZTail t)
                    return (map snd $ blockEnvToList blocks)
               Nothing -> return [b]
           | otherwise = return [b]

-- | Add a CopyOut node before each procpoint.
-- If the predecessor is a call, then the copy outs should already be done by the callee.
-- Note: If we need to add copy-out instructions, they may require stack space,
-- so we accumulate a map from the successors to the necessary stack space,
-- then update the successors after we have finished inserting the copy-outs.

add_CopyOuts :: BlockEnv Protocol -> ProcPointSet -> CmmGraph ->
                FuelMonad (BlockEnv CmmBlock)
add_CopyOuts protos procPoints g = fold_blocks mb_copy_out (return emptyBlockEnv) g
    where mb_copy_out :: CmmBlock -> FuelMonad (BlockEnv CmmBlock) ->
                                     FuelMonad (BlockEnv CmmBlock)
          mb_copy_out b@(Block bid _) z | bid == lg_entry g = skip b z 
          mb_copy_out b z =
            case last $ unzip b of
              LastOther (LastCall _ _ _ _ _) -> skip b z -- copy out done by callee
              _ -> copy_out b z
          copy_out b z = fold_succs trySucc b init >>= finish
            where init = z >>= (\bmap -> return (b, bmap))
                  trySucc succId z =
                    if elemBlockSet succId procPoints then
                      case lookupBlockEnv protos succId of
                        Nothing -> z
                        Just (Protocol c fs _area) -> insert z succId $ copyOutSlot c fs
                    else z
                  insert z succId m =
                    do (b, bmap) <- z
                       (b, bs)   <- insertBetween b m succId
                       -- pprTrace "insert for succ" (ppr succId <> ppr m) $ do
                       return $ (b, foldl (flip insertBlock) bmap bs)
                  finish (b@(Block bid _), bmap) =
                    return $ (extendBlockEnv bmap bid b)
          skip b@(Block bid _) bs =
            bs >>= (\bmap -> return (extendBlockEnv bmap bid b))

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
splitAtProcPoints :: CLabel -> ProcPointSet-> ProcPointSet -> BlockEnv Status ->
                     CmmTopZ -> FuelMonad [CmmTopZ]
splitAtProcPoints entry_label callPPs procPoints procMap
                  (CmmProc (CmmInfo gc upd_fr info_tbl) top_l top_args
                           (stackInfo, g@(LGraph entry blocks))) =
  do -- Build a map from procpoints to the blocks they reach
     let addBlock b@(Block bid _) graphEnv =
           case lookupBlockEnv procMap bid of
             Just ProcPoint -> add graphEnv bid bid b
             Just (ReachedBy set) ->
               case blockSetToList set of
                 []   -> graphEnv
                 [id] -> add graphEnv id bid b 
                 _    -> panic "Each block should be reachable from only one ProcPoint"
             Nothing -> pprPanic "block not reached by a proc point?" (ppr bid)
         add graphEnv procId bid b = extendBlockEnv graphEnv procId graph'
               where graph  = lookupBlockEnv graphEnv procId `orElse` emptyBlockEnv
                     graph' = extendBlockEnv graph bid b
     graphEnv <- return $ fold_blocks addBlock emptyBlockEnv g
     -- Build a map from proc point BlockId to labels for their new procedures
     -- Due to common blockification, we may overestimate the set of procpoints.
     let add_label map pp = return $ Map.insert pp lbl map
           where lbl = if pp == entry then entry_label else blockLbl pp
     procLabels <- foldM add_label Map.empty
                         (filter (elemBlockEnv blocks) (blockSetToList procPoints))
     -- For each procpoint, we need to know the SP offset on entry.
     -- If the procpoint is:
     --  - continuation of a call, the SP offset is in the call
     --  - otherwise, 0 -- no overflow for passing those variables
     let add_sp_off b env =
           case last (unzip b) of
             LastOther (LastCall {cml_cont = Just succ, cml_ret_args = off,
                                  cml_ret_off = updfr_off}) ->
               extendBlockEnv env succ (off, updfr_off)
             _ -> env
         spEntryMap = fold_blocks add_sp_off (mkBlockEnv [(entry, stackInfo)]) g
         getStackInfo id = lookupBlockEnv spEntryMap id `orElse` (0, Nothing)
     -- In each new graph, add blocks jumping off to the new procedures,
     -- and replace branches to procpoints with branches to the jump-off blocks
     let add_jump_block (env, bs) (pp, l) =
           do bid <- liftM mkBlockId getUniqueM
              let b = Block bid (ZLast (LastOther jump))
                  (argSpace, _) = getStackInfo pp
                  jump = LastCall (CmmLit (CmmLabel l')) Nothing argSpace 0 Nothing
                  l' = if elemBlockSet pp callPPs then entryLblToInfoLbl l else l
              return (extendBlockEnv env pp bid, b : bs)
         add_jumps (newGraphEnv) (ppId, blockEnv) =
           do let needed_jumps = -- find which procpoints we currently branch to
                    foldBlockEnv' add_if_branch_to_pp [] blockEnv
                  add_if_branch_to_pp block rst =
                    case last (unzip block) of
                      LastOther (LastBranch id) -> add_if_pp id rst
                      LastOther (LastCondBranch _ ti fi) ->
                        add_if_pp ti (add_if_pp fi rst)
                      LastOther (LastSwitch _ tbl) -> foldr add_if_pp rst (catMaybes tbl)
                      _ -> rst
                  add_if_pp id rst = case Map.lookup id procLabels of
                                       Just x -> (id, x) : rst
                                       Nothing -> rst
              (jumpEnv, jumpBlocks) <-
                 foldM add_jump_block (emptyBlockEnv, []) needed_jumps
                  -- update the entry block
              let b = expectJust "block in env" $ lookupBlockEnv blockEnv ppId
                  off = getStackInfo ppId
                  blockEnv' = extendBlockEnv blockEnv ppId b
                  -- replace branches to procpoints with branches to jumps
                  LGraph _ blockEnv'' = replaceBranches jumpEnv $ LGraph ppId blockEnv'
                  -- add the jump blocks to the graph
                  blockEnv''' = foldl (flip insertBlock) blockEnv'' jumpBlocks
              let g' = (off, LGraph ppId blockEnv''')
              -- pprTrace "g' pre jumps" (ppr g') $ do
              return (extendBlockEnv newGraphEnv ppId g')
     graphEnv <- foldM add_jumps emptyBlockEnv $ blockEnvToList graphEnv
     let to_proc (bid, g) | elemBlockSet bid callPPs =
           if bid == entry then 
             CmmProc (CmmInfo gc upd_fr info_tbl) top_l top_args (replacePPIds g)
           else
             CmmProc emptyContInfoTable lbl [] (replacePPIds g)
           where lbl = expectJust "pp label" $ Map.lookup bid procLabels
         to_proc (bid, g) =
           CmmProc (CmmInfo Nothing Nothing CmmNonInfoTable) lbl [] (replacePPIds g)
             where lbl = expectJust "pp label" $ Map.lookup bid procLabels
         -- References to procpoint IDs can now be replaced with the infotable's label
         replacePPIds (x, g) = (x, map_nodes id (mapExpMiddle repl) (mapExpLast repl) g)
           where repl e@(CmmLit (CmmBlock bid)) =
                   case Map.lookup bid procLabels of
                     Just l  -> CmmLit (CmmLabel (entryLblToInfoLbl l))
                     Nothing -> e
                 repl e = e
     -- The C back end expects to see return continuations before the call sites.
     -- Here, we sort them in reverse order -- it gets reversed later.
     let (_, block_order) = foldl add_block_num (0::Int, emptyBlockEnv) (postorder_dfs g)
         add_block_num (i, map) (Block bid _) = (i+1, extendBlockEnv map bid i)
         sort_fn (bid, _) (bid', _) =
           compare (expectJust "block_order" $ lookupBlockEnv block_order bid)
                   (expectJust "block_order" $ lookupBlockEnv block_order bid')
     procs <- return $ map to_proc $ sortBy sort_fn $ blockEnvToList graphEnv
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
