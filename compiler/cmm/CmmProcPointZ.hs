
module CmmProcPointZ
    ( callProcPoints, minimalProcPointSet
    , addProcPointProtocols
    , splitAtProcPoints
    )
where

import Prelude hiding (zip, unzip, last)

import BlockId
import CLabel
--import ClosureInfo
import Cmm hiding (blockId)
import CmmExpr
import CmmContFlowOpt
import CmmLiveZ
import CmmTx
import DFMonad
import FiniteMap
import MachOp (MachHint(NoHint))
import Maybes
import MkZipCfgCmm hiding (CmmBlock, CmmGraph)
import Monad
import Name
import Outputable
import Panic
import UniqFM
import UniqSet
import UniqSupply
import ZipCfg
import ZipCfgCmmRep
import ZipDataflow

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

forward :: ForwardTransfers Middle Last Status
forward = ForwardTransfers first middle last exit
    where first ProcPoint id = ReachedBy $ unitUniqSet id
          first  x _ = x
          middle x _ = x
          last _ (LastCall _ (Just id)) = LastOutFacts [(id, ProcPoint)]
          last x l = LastOutFacts $ map (\id -> (id, x)) (succs l)
          exit x   = x
                
-- It is worth distinguishing two sets of proc points:
-- those that are induced by calls in the original graph
-- and those that are introduced because they're reachable from multiple proc points.
callProcPoints      :: CmmGraph -> ProcPointSet
minimalProcPointSet :: ProcPointSet -> CmmGraph -> FuelMonad ProcPointSet

callProcPoints g = fold_blocks add entryPoint g
  where entryPoint = unitUniqSet (lg_entry g)
        add b set = case last $ unzip b of
                      LastOther (LastCall _ (Just k)) -> extendBlockSet set k
                      _ -> set

minimalProcPointSet callProcPoints g = extendPPSet g (postorder_dfs g) callProcPoints

type PPFix = FuelMonad (ForwardFixedPoint Middle Last Status ())

procPointAnalysis :: ProcPointSet -> CmmGraph -> FuelMonad PPFix
procPointAnalysis procPoints g =
  let addPP env id = extendBlockEnv env id ProcPoint
      initProcPoints = foldl addPP emptyBlockEnv (uniqSetToList procPoints)
  in runDFM lattice $ -- init with old facts and solve
       return $ (zdfSolveFrom initProcPoints "proc-point reachability" lattice
                              forward (fact_bot lattice) $ graphOfLGraph g :: PPFix)

extendPPSet :: CmmGraph -> [CmmBlock] -> ProcPointSet -> FuelMonad ProcPointSet
extendPPSet g blocks procPoints =
    do res <- procPointAnalysis procPoints g
       env <- liftM zdfFpFacts res
       let add block pps = let id = blockId block
                           in  case lookupBlockEnv env id of
                                 Just ProcPoint -> extendBlockSet pps id
                                 _ -> pps
           procPoints' = fold_blocks add emptyBlockSet g
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
     (protos, g') <- return $ optimize_calls liveness g
     blocks'' <- add_CopyOuts protos procPoints g'
     return $ LGraph (lg_entry g) blocks''
    where optimize_calls liveness g =  -- see Note [Separate Adams optimization]
              let (protos, blocks') =
                      fold_blocks maybe_add_call (init_protocols, emptyBlockEnv) g
                  protos' = add_unassigned liveness procPoints protos
                  g'  = LGraph (lg_entry g) $ add_CopyIns callPPs protos' blocks'
              in  (protos', runTx removeUnreachableBlocksZ g')
          maybe_add_call :: CmmBlock -> (BlockEnv Protocol, BlockEnv CmmBlock)
                         -> (BlockEnv Protocol, BlockEnv CmmBlock)
          -- ^ If the block is a call whose continuation goes to a proc point
          -- whose protocol either matches the continuation's or is not yet set,
          -- redirect the call (cf 'newblock') and set the protocol if necessary
          maybe_add_call block (protos, blocks) =
              case goto_end $ unzip block of
                (h, LastOther (LastCall tgt (Just k)))
                    | Just proto <- lookupBlockEnv protos k,
                      Just pee   <- jumpsToProcPoint k
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
              extendBlockEnv env id (Protocol c fs $ toArea id fs)
          maybe_add_proto _ env = env
          toArea id fs = mkCallArea id fs $ Just fs

-- | For now, following a suggestion by Ben Lippmeier, we pass all
-- live variables as arguments, hoping that a clever register
-- allocator might help.

add_unassigned :: BlockEnv CmmLive -> ProcPointSet -> BlockEnv Protocol ->
                  BlockEnv Protocol
add_unassigned = pass_live_vars_as_args

pass_live_vars_as_args :: BlockEnv CmmLive -> ProcPointSet ->
                          BlockEnv Protocol -> BlockEnv Protocol
pass_live_vars_as_args liveness procPoints protos = protos'
  where protos' = foldUniqSet addLiveVars protos procPoints
        addLiveVars :: BlockId -> BlockEnv Protocol -> BlockEnv Protocol
        addLiveVars id protos =
            case lookupBlockEnv protos id of
              Just _  -> protos
              Nothing -> let live = lookupBlockEnv liveness id `orElse`
                                    panic ("no liveness at block " ++ show id)
                             formals = map (\x -> CmmKinded x NoHint) $ uniqSetToList live
                             prot = Protocol ConventionPrivate formals $
                                             mkCallArea id formals $ Just formals
                         in  extendBlockEnv protos id prot


-- | Add copy-in instructions to each proc point that did not arise from a call
-- instruction. (Proc-points that arise from calls already have their copy-in instructions.)

add_CopyIns :: ProcPointSet -> BlockEnv Protocol -> BlockEnv CmmBlock -> BlockEnv CmmBlock
add_CopyIns callPPs protos = mapUFM maybe_insert_CopyIns
    where maybe_insert_CopyIns :: CmmBlock -> CmmBlock
          maybe_insert_CopyIns b@(Block id t) | not $ elementOfUniqSet id callPPs =
            case lookupBlockEnv protos id of
              Nothing -> b
              Just (Protocol c fs area) ->
                  case t of
                    --ZTail (CopyIn c' fs' _) _ ->
                    --  if c == c' && fs == fs' then b
                    --  else panic ("mismatched protocols for block " ++ show id)
                    _ -> Block id -- (ZTail (CopyIn c fs NoC_SRT) t)
                           $ foldr ZTail t (copyIn c area fs)
          maybe_insert_CopyIns b = b

-- | Add a CopyOut node before each procpoint.
-- If the predecessor is a call, then the CopyOut should already exist (in the callee).

add_CopyOuts :: BlockEnv Protocol -> ProcPointSet -> CmmGraph ->
                FuelMonad (BlockEnv CmmBlock)
add_CopyOuts protos procPoints g = fold_blocks maybe_insert_CopyOut (return emptyBlockEnv) g
    where maybe_insert_CopyOut :: CmmBlock -> FuelMonad (BlockEnv CmmBlock) ->
                                  FuelMonad (BlockEnv CmmBlock)
          maybe_insert_CopyOut b@(Block bid _) blocks | bid == lg_entry g = skip b blocks 
          maybe_insert_CopyOut b blocks =
            case last $ unzip b of
              LastOther (LastCall _ _) -> skip b blocks -- copy out done by callee
              _ -> maybe_insert_CopyOut' b blocks
          maybe_insert_CopyOut' b blocks = fold_succs trySucc b init >>= finish
            where init = blocks >>= (\bmap -> return (b, bmap))
                  trySucc succId z =
                    if elemBlockSet succId procPoints then
                      case lookupBlockEnv protos succId of
                        Nothing -> z
                        Just (Protocol c fs area) ->
                          insert z succId $ copyOut c area $ map fetch fs
                          -- CopyOut c $ map fetch fs
                    else z
                  fetch k = k {kindlessCmm = CmmReg $ CmmLocal $ kindlessCmm k}
                  insert z succId m =
                    do (b, bmap) <- z
                       (b, bs)   <- insertBetween b m succId
                       return $ (b, foldl (flip insertBlock) bmap bs)
                  finish (b@(Block bid _), bmap) = return $ extendBlockEnv bmap bid b
          skip b@(Block bid _) bs = bs >>= (\bmap -> return $ extendBlockEnv bmap bid b)



-- Input invariant: A block should only be reachable from a single ProcPoint.
-- If you want to duplicate blocks, do it before this gets called.
splitAtProcPoints :: CmmFormalsWithoutKinds -> CLabel -> ProcPointSet ->
                     CmmGraph -> FuelMonad [CmmGraph]
splitAtProcPoints formals entry_label procPoints g@(LGraph entry _) =
  do let layout = layout_stack formals g
     pprTrace "stack layout" (ppr layout) $ return () 
     res <- procPointAnalysis procPoints g
     procMap <- liftM zdfFpFacts res
     let addBlock b@(Block bid _) graphEnv =
               case lookupBlockEnv procMap bid of
                 Just ProcPoint -> add graphEnv bid bid b
                 Just (ReachedBy set) ->
                   case uniqSetToList set of
                     []   -> graphEnv
                     [id] -> add graphEnv id bid b 
                     _ -> panic "Each block should be reachable from only one ProcPoint"
                 Nothing -> panic "block not reached by a proc point?"
         add graphEnv procId bid b = extendBlockEnv graphEnv procId graph'
               where graph  = lookupBlockEnv graphEnv procId `orElse` emptyBlockEnv
                     graph' = extendBlockEnv graph bid b
     graphEnv <- return $ fold_blocks addBlock emptyBlockEnv g
     -- Build a map from proc point BlockId to labels for their new procedures
     let add_label map pp = clabel pp >>= (\l -> return $ (pp, l) : map) 
         clabel procPoint = if procPoint == entry then return entry_label
                            else getUniqueM >>= return . to_label
         to_label u = mkEntryLabel (mkFCallName u "procpoint")
     procLabels <- foldM add_label [] (uniqSetToList procPoints)
     -- In each new graph, add blocks jumping off to the new procedures,
     -- and replace branches to procpoints with branches to the jump-off blocks
     let add_jump_block (env, bs) (pp, l) =
           do bid <- liftM mkBlockId getUniqueM
              let b = Block bid (ZLast (LastOther (LastJump $ CmmLit $ CmmLabel l)))
              return $ (extendBlockEnv env pp bid, b : bs)
         add_jumps newGraphEnv (guniq, blockEnv) =
           do (jumpEnv, jumpBlocks) <- foldM add_jump_block (emptyBlockEnv, []) procLabels
              let ppId = mkBlockId guniq
                  LGraph _ blockEnv' = replaceLabelsZ jumpEnv $ LGraph ppId blockEnv
                  blockEnv'' = foldl (flip insertBlock) blockEnv' jumpBlocks
              return $ extendBlockEnv newGraphEnv ppId $
                       runTx cmmCfgOptsZ $ LGraph ppId blockEnv''
     _ <- return $ replaceLabelsZ
     graphEnv <- foldM add_jumps emptyBlockEnv $ ufmToList graphEnv
     return $ pprTrace "procLabels" (ppr procLabels) $
              pprTrace "splitting graphs" (ppr graphEnv) $ [g]

------------------------------------------------------------------------
--                    Stack Layout (completely bogus for now)         --
------------------------------------------------------------------------

-- At some point, we'll do stack layout properly.
-- But for now, we can move forward on generating code by just producing
-- a brain dead layout, giving a separate slot to every variable,
-- and (incorrectly) assuming that all parameters are passed on the stack.

-- For now, variables are placed at explicit offsets from a virtual
-- frame pointer.
-- We may want to use abstract stack slots at some point.
data Placement = VFPMinus Int

instance Outputable Placement where
  ppr (VFPMinus k) = text "VFP - " <> int k

-- Build a map from registers to stack locations.
-- Return that map along with the offset to the end of the block
-- containing local registers.
layout_stack ::CmmFormalsWithoutKinds -> CmmGraph ->
               (Int, FiniteMap LocalReg Placement, FiniteMap LocalReg Placement)
layout_stack formals g = (ix', incomingMap, localMap)
    where (ix, incomingMap) = foldl (flip place) (1, emptyFM) formals -- IGNORES CC'S
                 -- 1 leaves space for the return infotable
          (ix', localMap) = foldUniqSet place (ix, emptyFM) regs
          place r (ix, map) = (ix', addToFM map r $ VFPMinus ix') where ix' = ix + 1
          regs = fold_blocks (fold_fwd_block (\_ y -> y) add addL) emptyRegSet g
          add  x y = foldRegsDefd extendRegSet y x
          addL (LastOther l) z = add l z
          addL LastExit      z = z


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
