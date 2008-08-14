
module CmmProcPointZ
    ( callProcPoints, minimalProcPointSet
    , addProcPointProtocols, splitAtProcPoints, procPointAnalysis
    , liveSlotAnal, cafAnal, layout, manifestSP, igraph, areaBuilder
    )
where

import Constants
import qualified Prelude as P
import Prelude hiding (zip, unzip, last)
import Util (sortLe)

import BlockId
import Bitmap
import CLabel
import Cmm hiding (blockId)
import CmmExpr
import CmmContFlowOpt
import CmmLiveZ
import CmmTx
import DFMonad
import FiniteMap
import IdInfo
import List (sortBy)
import Maybes
import MkZipCfgCmm hiding (CmmBlock, CmmGraph, CmmTopZ)
import Monad
import Name
import Outputable
import Panic
import SMRep (rET_SMALL)
import StgCmmClosure
import StgCmmUtils
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
          last _ (LastCall _ (Just id) _) = LastOutFacts [(id, ProcPoint)]
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
                      LastOther (LastCall _ (Just k) _) -> extendBlockSet set k
                      _ -> set

minimalProcPointSet callProcPoints g = extendPPSet g (postorder_dfs g) callProcPoints

type PPFix = FuelMonad (ForwardFixedPoint Middle Last Status ())

procPointAnalysis :: ProcPointSet -> CmmGraph -> FuelMonad (BlockEnv Status)
procPointAnalysis procPoints g =
  let addPP env id = extendBlockEnv env id ProcPoint
      initProcPoints = foldl addPP emptyBlockEnv (uniqSetToList procPoints)
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
           newPoint = listToMaybe (mapMaybe ppSuccessor blocks)
           ppSuccessor b@(Block id _ _) =
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
     return $ LGraph (lg_entry g) (lg_argoffset g) blocks''
    where optimize_calls liveness g =  -- see Note [Separate Adams optimization]
              let (protos, blocks') =
                      fold_blocks maybe_add_call (init_protocols, emptyBlockEnv) g
                  protos' = add_unassigned liveness procPoints protos
                  g'  = LGraph (lg_entry g) (lg_argoffset g) $
                               add_CopyIns callPPs protos' blocks'
              in  (protos', runTx removeUnreachableBlocksZ g')
          maybe_add_call :: CmmBlock -> (BlockEnv Protocol, BlockEnv CmmBlock)
                         -> (BlockEnv Protocol, BlockEnv CmmBlock)
          -- ^ If the block is a call whose continuation goes to a proc point
          -- whose protocol either matches the continuation's or is not yet set,
          -- redirect the call (cf 'newblock') and set the protocol if necessary
          maybe_add_call block (protos, blocks) =
              case goto_end $ unzip block of
                (h, LastOther (LastCall tgt (Just k) s))
                    | Just proto <- lookupBlockEnv protos k,
                      Just pee   <- branchesToProcPoint k
                    -> let newblock = zipht h (tailOfLast (LastCall tgt (Just pee) s))
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
              let (Block _ _ t) = lookupBlockEnv (lg_blocks g) id `orElse`
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

-- | For now, following a suggestion by Ben Lippmeier, we pass all
-- live variables as arguments, hoping that a clever register
-- allocator might help.

add_unassigned :: BlockEnv CmmLive -> ProcPointSet -> BlockEnv Protocol ->
                  BlockEnv Protocol
add_unassigned = pass_live_vars_as_args

pass_live_vars_as_args :: BlockEnv CmmLive -> ProcPointSet ->
                          BlockEnv Protocol -> BlockEnv Protocol
pass_live_vars_as_args _liveness procPoints protos = protos'
  where protos' = foldUniqSet addLiveVars protos procPoints
        addLiveVars :: BlockId -> BlockEnv Protocol -> BlockEnv Protocol
        addLiveVars id protos =
            case lookupBlockEnv protos id of
              Just _  -> protos
              Nothing -> let live = emptyBlockEnv
                                    --lookupBlockEnv _liveness id `orElse`
                                    --panic ("no liveness at block " ++ show id)
                             formals = uniqSetToList live
                             prot = Protocol Private formals $ CallArea $ Young id
                         in  extendBlockEnv protos id prot


-- | Add copy-in instructions to each proc point that did not arise from a call
-- instruction. (Proc-points that arise from calls already have their copy-in instructions.)

add_CopyIns :: ProcPointSet -> BlockEnv Protocol -> BlockEnv CmmBlock -> BlockEnv CmmBlock
add_CopyIns callPPs protos blocks = mapUFM maybe_insert_CopyIns blocks
    where maybe_insert_CopyIns :: CmmBlock -> CmmBlock
          maybe_insert_CopyIns b@(Block id off t) | not $ elementOfUniqSet id callPPs =
            case (off, lookupBlockEnv protos id) of
              (Just _, _) -> panic "shouldn't copy arguments twice into a block"
              (_, Just (Protocol c fs area)) -> Block id (Just off) $ foldr ZTail t copies
                where (off, copies) = copyIn c False area fs
              (_, Nothing) -> b
          maybe_insert_CopyIns b = b

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
          mb_copy_out b@(Block bid _ _) z | bid == lg_entry g = skip b z 
          mb_copy_out b z =
            case last $ unzip b of
              LastOther (LastCall _ _ _) -> skip b z -- copy out done by callee
              _ -> mb_copy_out' b z
          mb_copy_out' b z = fold_succs trySucc b init >>= finish
            where init = z >>= (\bmap -> return (b, bmap))
                  trySucc succId z =
                    if elemBlockSet succId procPoints then
                      case lookupBlockEnv protos succId of
                        Nothing -> z
                        Just (Protocol c fs area) ->
                          let (_, copies) = copyOut c Jump area $ map (CmmReg . CmmLocal) fs
                          in  insert z succId copies
                    else z
                  insert z succId m =
                    do (b, bmap) <- z
                       (b, bs)   <- insertBetween b m succId
                       pprTrace "insert for succ" (ppr succId <> ppr m) $
                        return $ (b, foldl (flip insertBlock) bmap bs)
                  finish (b@(Block bid _ _), bmap) =
                    return $ (extendBlockEnv bmap bid b)
          skip b@(Block bid _ _) bs =
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
                     BlockEnv SubAreaSet -> AreaMap -> CmmTopZ -> FuelMonad [CmmTopZ]
splitAtProcPoints entry_label callPPs procPoints procMap slotEnv areaMap
                  (CmmProc top_info top_l top_args g@(LGraph entry e_off blocks)) =
  do -- Build a map from procpoints to the blocks they reach
     let addBlock b@(Block bid _ _) graphEnv =
           case lookupBlockEnv procMap bid of
             Just ProcPoint -> add graphEnv bid bid b
             Just (ReachedBy set) ->
               case uniqSetToList set of
                 []   -> graphEnv
                 [id] -> add graphEnv id bid b 
                 _ -> panic "Each block should be reachable from only one ProcPoint"
             Nothing -> pprPanic "block not reached by a proc point?" (ppr bid)
         add graphEnv procId bid b = extendBlockEnv graphEnv procId graph'
               where graph  = lookupBlockEnv graphEnv procId `orElse` emptyBlockEnv
                     graph' = extendBlockEnv graph bid b
     graphEnv <- return $ fold_blocks addBlock emptyBlockEnv g
     -- Build a map from proc point BlockId to labels for their new procedures
     let add_label map pp = return $ addToFM map pp lbl
           where lbl = if pp == entry then entry_label else blockLbl pp
     procLabels <- foldM add_label emptyFM (uniqSetToList procPoints)
     -- Convert call and return instructions to jumps.
     let last (LastCall e _ n) = LastJump e n
         last l = l
     graphEnv <- return $ mapUFM (mapUFM (map_one_block id id last)) graphEnv
     -- In each new graph, add blocks jumping off to the new procedures,
     -- and replace branches to procpoints with branches to the jump-off blocks
     let add_jump_block (env, bs) (pp, l) =
           do bid <- liftM mkBlockId getUniqueM
              let b = Block bid Nothing (ZLast (LastOther jump))
                  argSpace = case lookupBlockEnv blocks pp of
                               Just (Block _ (Just s) _) -> s
                               Just (Block _ Nothing  _) -> panic "no args at procpoint"
                               _ -> panic "can't find procpoint block"
                  jump = LastJump (CmmLit (CmmLabel l)) argSpace
              return $ (extendBlockEnv env pp bid, b : bs)
         add_jumps newGraphEnv (guniq, blockEnv) =
           do (jumpEnv, jumpBlocks) <- foldM add_jump_block (emptyBlockEnv, [])
                                           $ fmToList procLabels
              let ppId = mkBlockId guniq
                  (b_off, b) =
                    case lookupBlockEnv blockEnv ppId of
                      Just (Block id (Just b_off) t) -> (b_off, Block id Nothing t)
                      Just b@(Block _ Nothing _)     -> (0, b)
                      Nothing -> panic "couldn't find entry block while splitting"
                  off = if ppId == entry then e_off else b_off
                  LGraph _ _ blockEnv' = pprTrace "jumpEnv" (ppr jumpEnv) $
                                         replaceLabelsZ jumpEnv $ LGraph ppId off blockEnv
                  blockEnv'' = foldl (flip insertBlock) (extendBlockEnv blockEnv' ppId b)
                                     jumpBlocks
              return $ extendBlockEnv newGraphEnv ppId $
                       runTx cmmCfgOptsZ $ LGraph ppId off blockEnv''
         upd_info_tbl srt' (CmmInfoTable p t typeinfo) = CmmInfoTable p t typeinfo'
           where typeinfo' = case typeinfo of
                   t@(ConstrInfo _ _ _)    -> t
                   (FunInfo    c _ a d e)  -> FunInfo c srt' a d e
                   (ThunkInfo  c _)        -> ThunkInfo c srt'
                   (ThunkSelectorInfo s _) -> ThunkSelectorInfo s srt'
                   (ContInfo vars _)       -> ContInfo vars srt'
         upd_info_tbl _ CmmNonInfoTable = CmmNonInfoTable 
         to_proc cafMap (ppUniq, g) | elementOfUniqSet bid callPPs =
           if bid == entry then 
             CmmProc (CmmInfo gc upd_fr (upd_info_tbl srt' info_tbl)) top_l top_args g
           else
            pprTrace "adding infotable for" (ppr bid) $
             CmmProc (CmmInfo Nothing Nothing $ infoTbl) lbl [] g
           where bid = mkBlockId ppUniq
                 lbl = expectJust "pp label" $ lookupFM procLabels bid
                 infoTbl = CmmInfoTable (ProfilingInfo zero zero) rET_SMALL
                                        (ContInfo stack_vars srt')
                 stack_vars = pprTrace "slotEnv" (ppr slotEnv) $
                               live_vars slotEnv areaMap bid
                 zero = CmmInt 0 wordWidth
                 srt' = expectJust "procpoint.infoTbl" $ lookupBlockEnv cafMap bid
                 CmmInfo gc upd_fr info_tbl = top_info
         to_proc _ (ppUniq, g) =
          pprTrace "not adding infotable for" (ppr bid) $
           CmmProc (CmmInfo Nothing Nothing CmmNonInfoTable) lbl [] g
             where bid = mkBlockId ppUniq
                   lbl = expectJust "pp label" $ lookupFM procLabels bid
     graphEnv <- foldM add_jumps emptyBlockEnv $ ufmToList graphEnv
     cafEnv <- cafAnal g
     (cafTable, blockCafs) <- buildCafs cafEnv
     procs <- return $ map (to_proc blockCafs) $ ufmToList graphEnv
     return $ pprTrace "procLabels" (ppr procLabels) $
              pprTrace "splitting graphs" (ppr graphEnv) $ cafTable ++ procs
splitAtProcPoints _ _ _ _ _ _ t@(CmmData _ _) = return [t]

------------------------------------------------------------------------
--                    Stack Layout                                    --
------------------------------------------------------------------------

-- | Before we lay out the stack, we need to know something about the
-- liveness of the stack slots. In particular, to decide whether we can
-- reuse a stack location to hold multiple stack slots, we need to know
-- when each of the stack slots is used.
-- Although tempted to use something simpler, we really need a full interference
-- graph. Consider the following case:
--   case <...> of
--     1 -> <spill x>; // y is dead out
--     2 -> <spill y>; // x is dead out
--     3 -> <spill x and y>
-- If we consider the arms in order and we use just the deadness information given by a
-- dataflow analysis, we might decide to allocate the stack slots for x and y
-- to the same stack location, which will lead to incorrect code in the third arm.
-- We won't make this mistake with an interference graph.

-- First, the liveness analysis.
-- We represent a slot with an area, an offset into the area, and a width.
-- Tracking the live slots is a bit tricky because there may be loads and stores
-- into only a part of a stack slot (e.g. loading the low word of a 2-word long),
-- e.g. Slot A 0 8 overlaps with Slot A 4 4.
--
-- The definition of a slot set is intended to reduce the number of overlap
-- checks we have to make. There's no reason to check for overlap between
-- slots in different areas, so we segregate the map by Area's.
-- We expect few slots in each Area, so we collect them in an unordered list.
-- To keep these lists short, any contiguous live slots are coalesced into
-- a single slot, on insertion.

type SubAreaSet   = FiniteMap Area [SubArea]
fold_subareas :: (SubArea -> z -> z) -> SubAreaSet -> z -> z
fold_subareas f m z = foldFM (\_ s z -> foldr (\a z -> f a z) z s) z m

liveGen :: SubArea -> [SubArea] -> (Bool, [SubArea])
liveGen s set = liveGen' s set []
  where liveGen' s [] z = (True, s : z)
        liveGen' s@(a, hi, w) (s'@(a', hi', w') : rst) z =
          if a /= a' || hi < lo' || lo > hi' then    -- no overlap
            liveGen' s rst (s' : z)
          else if s' `contains` s then               -- old contains new
            (False, set)
          else                                       -- overlap: coalesce the slots
            let new_hi = max hi hi'
                new_lo = min lo lo'
            in liveGen' (a, new_hi, new_hi - new_lo) rst z
          where lo  = hi  - w  -- remember: areas grow down
                lo' = hi' - w'
        contains (a, hi, w) (a', hi', w') =
          a == a' && hi >= hi' && hi - w <= hi' - w'

liveKill :: SubArea -> [SubArea] -> [SubArea]
liveKill (a, hi, w) set = pprTrace "killing slots in area" (ppr a) $ liveKill' set []
  where liveKill' [] z = z
        liveKill' (s'@(a', hi', w') : rst) z =
          if a /= a' || hi < lo' || lo > hi' then    -- no overlap
            liveKill' rst (s' : z)
          else                                       -- overlap: split the old slot
            let z'  = if hi' > hi  then (a, hi', hi' - hi)  : z else z
                z'' = if lo  > lo' then (a, lo,  lo  - lo') : z' else z'
            in liveKill' rst z''
          where lo  = hi  - w  -- remember: areas grow down
                lo' = hi' - w'

slotLattice :: DataflowLattice SubAreaSet
slotLattice = DataflowLattice "live slots" emptyFM add True
  where add new old = case foldFM addArea (False, old) new of
                        (True,  x) -> aTx  x
                        (False, x) -> noTx x
        addArea a newSlots z = foldr (addSlot a) z newSlots
        addSlot a slot (changed, map) =
          let (c, live) = liveGen slot $ lookupWithDefaultFM map [] a
          in (c || changed, addToFM map a live)

liveInSlots :: (DefinerOfSlots s, UserOfSlots s) => SubAreaSet -> s -> SubAreaSet
liveInSlots live x = foldSlotsUsed add (foldSlotsDefd remove live x) x
  where add    live (a, i, w) = liftToArea a (snd . liveGen  (a, i, w)) live
        remove live (a, i, w) = liftToArea a       (liveKill (a, i, w)) live
        liftToArea a f map = addToFM map a $ f (lookupWithDefaultFM map [] a)

-- Unlike the liveness transfer functions @gen@ and @kill@, this function collects
-- _any_ slot that is named.
--addNamedSlots :: (DefinerOfSlots s, UserOfSlots s) => SubAreaSet -> s -> SubAreaSet
--addNamedSlots live x = foldSlotsUsed add (foldSlotsDefd add live x) x
--  where add    live (a, i, w) = liftToArea a (snd . liveGen  (a, i, w)) live
--        liftToArea a f map = addToFM map a $ f (lookupWithDefaultFM map [] a)

-- Note: the stack slots that hold variables returned on the stack are not
-- considered live in to the block -- we treat the first node as a definition site.
-- BEWARE: I'm being a little careless here in failing to check for the
-- entry Id (which would use the CallArea Old).
liveTransfers :: BackwardTransfers Middle Last SubAreaSet
liveTransfers = BackwardTransfers first liveInSlots liveLastIn
    where first live id = delFromFM live (CallArea (Young id))

liveLastIn :: (BlockId -> SubAreaSet) -> Last -> SubAreaSet
liveLastIn env l = liveInSlots (liveLastOut env l) l

-- Don't forget to keep the outgoing parameters in the CallArea live.
liveLastOut :: (BlockId -> SubAreaSet) -> Last -> SubAreaSet
liveLastOut env l =
  case l of
    LastReturn n          -> add_area (CallArea Old)       n out
    LastJump _ n          -> add_area (CallArea Old)       n out
    LastCall _ Nothing  n -> add_area (CallArea Old)       n out
    LastCall _ (Just k) n -> add_area (CallArea (Young k)) n out
    _                     -> out
  where out = joinOuts slotLattice env l
add_area :: Area -> Int -> SubAreaSet -> SubAreaSet
add_area a n live =
  addToFM live a $ snd $ liveGen (a, n, n) $ lookupWithDefaultFM live [] a

type SlotFix a = FuelMonad (BackwardFixedPoint Middle Last SubAreaSet a)
liveSlotAnal :: LGraph Middle Last -> FuelMonad (BlockEnv SubAreaSet)
liveSlotAnal g = liftM zdfFpFacts (res :: SlotFix ())
  where res = zdfSolveFromL emptyBlockEnv "live slot analysis" slotLattice
                            liveTransfers (fact_bot slotLattice) g

-- The liveness analysis must be precise: otherwise, we won't know if a definition
-- should really kill a live-out stack slot.
-- But the interference graph does not have to be precise -- it might decide that
-- any live areas interfere. To maintain both a precise analysis and an imprecise
-- interference graph, we need to convert the live-out stack slots to graph nodes
-- at each and every instruction; rather than reconstruct a new list of nodes
-- every time, I provide a function to fold over the nodes, which should be a
-- reasonably efficient approach for the implementations we envision.
-- Of course, it will probably be much easier to program if we just return a list...
type Set x = FiniteMap x ()
type AreaMap = FiniteMap Area Int
data IGraphBuilder n =
  Builder { foldNodes     :: forall z. SubArea -> (n -> z -> z) -> z -> z
          , _wordsOccupied :: AreaMap -> AreaMap -> n -> [Int]
          }

areaBuilder :: IGraphBuilder Area
areaBuilder = Builder fold words
  where fold (a, _, _) f z = f a z
        words areaSize areaMap a =
          case lookupFM areaMap a of
            Just addr -> [addr .. addr + (lookupFM areaSize a `orElse`
                                          pprPanic "wordsOccupied: unknown area" (ppr a))]
            Nothing   -> []

--slotBuilder :: IGraphBuilder (Area, Int)
--slotBuilder = undefined

-- Now, we can build the interference graph.
-- The usual story: a definition interferes with all live outs and all other
-- definitions.
type IGraph x = FiniteMap x (Set x)
type IGPair x = (IGraph x, IGraphBuilder x)
igraph :: (Ord x) => IGraphBuilder x -> BlockEnv SubAreaSet -> LGraph Middle Last -> IGraph x
igraph builder env g = foldr interfere emptyFM (postorder_dfs g)
  where foldN = foldNodes builder
        interfere block igraph =
          let (h, l) = goto_end (unzip block)
              --heads :: ZHead Middle -> (IGraph x, SubAreaSet) -> IGraph x
              heads (ZFirst _ _) (igraph, _)       = igraph
              heads (ZHead h m)  (igraph, liveOut) =
                heads h (addEdges igraph m liveOut, liveInSlots liveOut m)
              -- add edges between a def and the other defs and liveouts
              addEdges igraph i out = fst $ foldSlotsDefd addDef (igraph, out) i
              addDef (igraph, out) def@(a, _, _) =
                (foldN def (addDefN out) igraph,
                 addToFM out a (snd $ liveGen def (lookupWithDefaultFM out [] a)))
              addDefN out n igraph =
                let addEdgeNO o igraph = foldN o addEdgeNN igraph
                    addEdgeNN n' igraph = addEdgeNN' n n' $ addEdgeNN' n' n igraph
                    addEdgeNN' n n' igraph = addToFM igraph n (addToFM set n' ())
                      where set = lookupWithDefaultFM igraph emptyFM n
                in foldFM (\ _ os igraph -> foldr addEdgeNO igraph os) igraph out
              env' bid = lookupBlockEnv env bid `orElse` panic "unknown blockId in igraph"
          in heads h $ case l of LastExit    -> (igraph, emptyFM)
                                 LastOther l -> (addEdges igraph l $ liveLastOut env' l,
                                                 liveLastIn env' l)

-- Before allocating stack slots, we need to collect one more piece of information:
-- what's the highest offset (in bytes) used in each Area?
-- We'll need to allocate that much space for each Area.
getAreaSize :: LGraph Middle Last -> AreaMap
getAreaSize g@(LGraph _ off _) =
  fold_blocks (fold_fwd_block first add add) (unitFM (CallArea Old) off) g
  where first _ z = z
        add   x z = foldSlotsUsed addSlot (foldSlotsDefd addSlot z x) x
        addSlot z (a, off, _) = addToFM z a $ max off $ lookupWithDefaultFM z 0 a


-- Find the Stack slots occupied by the subarea's conflicts
conflictSlots :: Ord x => IGPair x -> AreaMap -> AreaMap -> SubArea -> Set Int
conflictSlots (ig, Builder foldNodes wordsOccupied) areaSize areaMap subarea =
  foldNodes subarea foldNode emptyFM
  where foldNode n set = foldFM conflict set $ lookupWithDefaultFM ig emptyFM n
        conflict n' () set = liveInSlots areaMap n' set
        -- Add stack slots occupied by igraph node n
        liveInSlots areaMap n set = foldr setAdd set (wordsOccupied areaSize areaMap n)
        setAdd w s = addToFM s w ()

-- Find any open space on the stack, starting from the offset.
freeSlotFrom :: Ord x => IGPair x -> AreaMap -> Int -> AreaMap -> Area -> Int
freeSlotFrom ig areaSize offset areaMap area =
  let size = lookupFM areaSize area `orElse` 0
      conflicts = conflictSlots ig areaSize areaMap (area, size, size)
      -- Find a space big enough to hold the area
      findSpace curr 0 = curr
      findSpace curr cnt = -- target slot, considerand, # left to check
        if elemFM curr conflicts then
          findSpace (curr + size) size
        else findSpace (curr - 1) (cnt - 1)
  in findSpace (offset + size) size

-- Find an open space on the stack, and assign it to the area.
allocSlotFrom :: Ord x => IGPair x -> AreaMap -> Int -> AreaMap -> Area -> AreaMap
allocSlotFrom ig areaSize from areaMap area =
  if elemFM area areaMap then areaMap
  else addToFM areaMap area $ freeSlotFrom ig areaSize from areaMap area

-- | Greedy stack layout.
-- Compute liveness, build the interference graph, and allocate slots for the areas.
-- We visit each basic block in a (generally) forward order.
-- At each instruction that names a register subarea r, we immediately allocate
-- any available slot on the stack by the following procedure:
--  1. Find the nodes N' that conflict with r
--  2. Find the stack slots used for N'
--  3. Choose a contiguous stack space s not in N' (s must be large enough to hold r)
-- For a CallArea, we allocate the stack space only when we reach a function
-- call that returns to the CallArea's blockId.
-- We use a similar procedure, with one exception: the stack space
-- must be allocated below the youngest stack slot that is live out.

-- Note: The stack pointer only has to be younger than the youngest live stack slot
-- at proc points. Otherwise, the stack pointer can point anywhere.
layout :: ProcPointSet -> BlockEnv SubAreaSet -> LGraph Middle Last -> AreaMap
layout procPoints env g@(LGraph _ entrySp _) =
  let builder = areaBuilder
      ig = (igraph builder env g, builder)
      env' bid = lookupBlockEnv env bid `orElse` panic "unknown blockId in igraph"
      areaSize = getAreaSize g
      -- Find the slots that are live-in to the block
      live_in (ZTail m l) = liveInSlots (live_in l) m
      live_in (ZLast (LastOther l)) = liveLastIn env' l
      live_in (ZLast LastExit) = emptyFM 
      -- Find the youngest live stack slot
      youngest_live areaMap live = fold_subareas young_slot live 0
        where young_slot (a, o, _) z = case lookupFM areaMap a of
                                         Just top -> max z $ top + o
                                         Nothing  -> z
      -- Allocate space for spill slots and call areas
      allocVarSlot = allocSlotFrom ig areaSize 0
      allocCallSlot areaMap (Block id _ t) | elemBlockSet id procPoints =
        allocSlotFrom ig areaSize (youngest_live areaMap $ live_in t)
                      areaMap (CallArea (Young id))
      allocCallSlot areaMap _ = areaMap
      alloc i areaMap = foldSlotsDefd alloc' (foldSlotsUsed alloc' areaMap i) i
        where alloc' areaMap (a@(RegSlot _), _, _) = allocVarSlot areaMap a
              alloc' areaMap _ = areaMap
      layoutAreas areaMap b@(Block _ _ t) = layout areaMap t
        where layout areaMap (ZTail m t) = layout (alloc m areaMap) t
              layout areaMap (ZLast _) = allocCallSlot areaMap b
      areaMap = foldl layoutAreas (addToFM emptyFM (CallArea Old) 0) $ postorder_dfs g
  in pprTrace "ProcPoints" (ppr procPoints) $
       pprTrace "Area SizeMap" (ppr areaSize) $
         pprTrace "Entry SP" (ppr entrySp) $
           pprTrace "Area Map" (ppr areaMap) $ areaMap

-- After determining the stack layout, we can:
-- 1. Replace references to stack Areas with addresses relative to the stack
--    pointer.
-- 2. Insert adjustments to the stack pointer to ensure that it is at a
--    conventional location at each proc point.
--    Because we don't take interrupts on the execution stack, we only need the
--    stack pointer to be younger than the live values on the stack at proc points.
-- 3. At some point, we should check for stack overflow, but not just yet.
manifestSP :: ProcPointSet -> BlockEnv Status -> AreaMap ->
                LGraph Middle Last -> FuelMonad (LGraph Middle Last)
manifestSP procPoints procMap areaMap g@(LGraph entry args blocks) =
  liftM (LGraph entry args) blocks'
  where blocks' = foldl replB (return emptyBlockEnv) (postorder_dfs g)
        slot a = pprTrace "slot" (ppr a) $ lookupFM areaMap a `orElse` panic "unallocated Area"
        slot' id = pprTrace "slot'" (ppr id)$ slot $ CallArea (Young id)
        sp_on_entry id | id == entry = slot (CallArea Old) + args
        sp_on_entry id | elemBlockSet id procPoints =
          case lookupBlockEnv blocks id of
            Just (Block _ (Just o) _) -> slot' id + o
            Just (Block _ Nothing  _) -> slot' id
            Nothing -> panic "procpoint dropped from block env"
        sp_on_entry id =
          case lookupBlockEnv procMap id of
            Just (ReachedBy pp) -> case uniqSetToList pp of
                                     [id] -> sp_on_entry id
                                     _    -> panic "block not reached by single proc point"
            Just ProcPoint -> panic "procpoint not in procpoint set"
            Nothing -> panic "block not found in procmap"
        -- On entry to procpoints, the stack pointer is conventional;
        -- otherwise, we check the SP set by predecessors.
        replB :: FuelMonad (BlockEnv CmmBlock) -> CmmBlock -> FuelMonad (BlockEnv CmmBlock)
        replB blocks (Block id o t) =
          do bs <- replTail (Block id o) spIn t
             pprTrace "spIn" (ppr id <+> ppr spIn)$ liftM (flip (foldr insertBlock) bs) blocks
          where spIn = sp_on_entry id
        replTail :: (ZTail Middle Last -> CmmBlock) -> Int -> (ZTail Middle Last) -> 
                    FuelMonad ([CmmBlock])
        replTail h spOff (ZTail m t) = replTail (h . ZTail (middle spOff m)) spOff t
        replTail h spOff (ZLast (LastOther l)) = fixSp h spOff l
        replTail h _   l@(ZLast LastExit) = return [h l]
        middle spOff m = mapExpDeepMiddle (replSlot spOff) m
        last   spOff l = mapExpDeepLast   (replSlot spOff) l
        replSlot spOff (CmmStackSlot a i) = CmmRegOff (CmmGlobal Sp) (spOff - (slot a + i))
        replSlot _ e = e
        -- The block must establish the SP expected at each successsor.
        fixSp :: (ZTail Middle Last -> CmmBlock) -> Int -> Last -> FuelMonad ([CmmBlock])
        fixSp h spOff l@(LastReturn n)          = updSp h spOff (slot (CallArea Old) + n) l
        fixSp h spOff l@(LastJump _ n)          = updSp h spOff (slot (CallArea Old) + n) l
        fixSp h spOff l@(LastCall _ (Just k) n) = updSp h spOff (slot' k + n)             l
        fixSp h spOff l@(LastCall _ Nothing  n) = updSp h spOff (slot (CallArea Old) + n) l
        fixSp h spOff l@(LastBranch k) | elemBlockSet k procPoints =
          pprTrace "updSp" (ppr k <> ppr spOff <> ppr (sp_on_entry k)) $ updSp h spOff (sp_on_entry k) l
        fixSp h spOff l = liftM (uncurry (:)) $ fold_succs succ l $ return (b, [])
          where b = h (ZLast (LastOther (last spOff l)))
                succ succId z =
                  let succSp = sp_on_entry succId in
                  if elemBlockSet succId procPoints && succSp /= spOff then
                    do (b,  bs)  <- z
                       (b', bs') <- insertBetween b [setSpMid spOff succSp] succId
                       return (b', bs ++ bs')
                  else z
        updSp h old new l = return [h $ setSp old new $ ZLast $ LastOther (last new l)]
        setSpMid sp sp' = MidAssign (CmmGlobal Sp) e
          where e = CmmMachOp (MO_Add wordWidth) [CmmReg (CmmGlobal Sp), off]
                off = CmmLit $ CmmInt (toInteger $ sp - sp') wordWidth
        setSp sp sp' t = if sp == sp' then t else ZTail (setSpMid sp sp') t

----------------------------------------------------------------
-- Building InfoTables

type CAFSet = FiniteMap CLabel ()

-- First, an analysis to find live CAFs.
cafLattice :: DataflowLattice CAFSet
cafLattice = DataflowLattice "live cafs" emptyFM add True
  where add new old = if sizeFM new' > sizeFM old then aTx new' else noTx new'
          where new' = new `plusFM` old

cafTransfers :: BackwardTransfers Middle Last CAFSet
cafTransfers = BackwardTransfers first middle last
    where first  live _ = live
          middle live m = pprTrace "cafmiddle" (ppr m) $ foldExpDeepMiddle addCaf m live
          last   env  l = foldExpDeepLast addCaf l (joinOuts cafLattice env l)
          addCaf e set = case e of
                 CmmLit (CmmLabel c) -> add c set
                 CmmLit (CmmLabelOff c _) -> add c set
                 CmmLit (CmmLabelDiffOff c1 c2 _) -> add c1 $ add c2 set
                 _ -> set
          add c s = pprTrace "CAF analysis saw label" (ppr c) $
                     if hasCAF c then (pprTrace "has caf" (ppr c) $ addToFM s c ()) else (pprTrace "no cafs" (ppr c) $ s)

type CafFix a = FuelMonad (BackwardFixedPoint Middle Last CAFSet a)
cafAnal :: LGraph Middle Last -> FuelMonad (BlockEnv CAFSet)
cafAnal g = liftM zdfFpFacts (res :: CafFix ())
  where res = zdfSolveFromL emptyBlockEnv "live CAF analysis" cafLattice
                            cafTransfers (fact_bot cafLattice) g

-- Once we have found the CAFs, we need to do two things:
-- 1. Build a table of all the CAFs used in the procedure.
-- 2. Compute the C_SRT describing the subset of CAFs live at each procpoint.
buildCafs :: (BlockEnv CAFSet) -> FuelMonad ([CmmTopZ], BlockEnv C_SRT)
buildCafs blockCafs =
  -- This is surely the wrong way to get names, as in BlockId
  do top_lbl <- getUniqueM >>= \ u -> return $ mkSRTLabel (mkFCallName u "srt") MayHaveCafRefs
     let allCafs = foldBlockEnv (\_ x y -> plusFM x y) emptyFM blockCafs
         caf_entry (ix, map, tbl') caf = (ix + 1, addToFM map caf ix, entry : tbl')
           where entry = CmmStaticLit $ CmmLabel caf
         (_::Int, cafMap, tbl') = foldl caf_entry (0, emptyFM, []) $ keysFM allCafs
         top_tbl = CmmData RelocatableReadOnlyData $ CmmDataLabel top_lbl : reverse tbl'
         sub_srt id cafs z =
           do (tbls, blocks) <- z
              (top, srt)     <- procpointSRT top_lbl cafMap cafs
              let blocks' = extendBlockEnv blocks id srt
              case top of Just t  -> return (t:tbls, blocks')
                          Nothing -> return (tbls,   blocks')
     (sub_tbls, blockSRTs) <- foldBlockEnv sub_srt (return ([], emptyBlockEnv)) blockCafs
     return (top_tbl :  sub_tbls, blockSRTs) 

-- Construct an SRT bitmap.
-- Adapted from simpleStg/SRT.lhs, which expects Id's.
procpointSRT :: CLabel -> FiniteMap CLabel Int -> FiniteMap CLabel () ->
                FuelMonad (Maybe CmmTopZ, C_SRT)
procpointSRT top_srt top_table entries
 | isEmptyFM entries = pprTrace "nil SRT" (ppr top_srt) $ return (Nothing, NoC_SRT)
 | otherwise  = pprTrace "non-nil SRT" (ppr top_srt) $ bitmap `seq` to_SRT top_srt offset len bitmap
  where
    ints = map (expectJust "constructSRT" . lookupFM top_table) (keysFM entries)
    sorted_ints = sortLe (<=) ints
    offset = head sorted_ints
    bitmap_entries = map (subtract offset) sorted_ints
    len = P.last bitmap_entries + 1
    bitmap = intsToBitmap len bitmap_entries

-- Adapted from codeGen/StgCmmUtils, which converts from SRT to C_SRT.
to_SRT :: CLabel -> Int -> Int -> Bitmap -> FuelMonad (Maybe CmmTopZ, C_SRT)
to_SRT top_srt off len bmp
  | len > widthInBits wordWidth `div` 2 || bmp == [fromIntegral srt_escape]
  = do id <- getUniqueM
       let srt_desc_lbl = mkLargeSRTLabel id
           tbl = CmmData RelocatableReadOnlyData $
                   CmmDataLabel srt_desc_lbl : map CmmStaticLit
                     ( cmmLabelOffW top_srt off
                     : mkWordCLit (fromIntegral len)
                     : map mkWordCLit bmp)
       return (Just tbl, C_SRT srt_desc_lbl 0 srt_escape)
  | otherwise
  = return (Nothing, C_SRT top_srt off (fromIntegral (head bmp)))
	-- The fromIntegral converts to StgHalfWord

-- Given a block ID, we return a representation of the layout of the stack.
-- If the element is `Nothing`, then it represents an empty or dead
-- word on the stack.
-- If the element is `Just` a register, then it represents a live spill slot
-- for the register; note that a register may occupy multiple words.
-- The head of the list represents the young end of the stack where the infotable
-- pointer for the block `Bid` is stored.
-- The infotable pointer itself is not included in the list.
live_vars :: BlockEnv SubAreaSet -> AreaMap -> BlockId -> [Maybe LocalReg]
live_vars slotEnv areaMap bid = slotsToList youngByte liveSlots
  where slotsToList 0 [] = []
        slotsToList 0 ((_, r, _) : _)  = pprPanic "slot left off live_vars" (ppr r)
        slotsToList n _ | n < 0 = panic "stack slots not allocated on word boundaries?"
        slotsToList n ((n', r, w) : rst) =
          if n == n' then Just r : slotsToList (n - w) rst
          else Nothing : slotsToList (n - wORD_SIZE) rst
        slotsToList n [] = Nothing : slotsToList (n - wORD_SIZE) []
        liveSlots = sortBy (\ (_,off,_) (_,off',_) -> compare off' off)
                      (foldFM (\_ -> flip $ foldr add_slot) [] slots)
        add_slot (a@(RegSlot r@(LocalReg _ ty)), off, w) rst = 
          if off == w && widthInBytes (typeWidth ty) == w then
            (expectJust "add_slot" (lookupFM areaMap a), r, w) : rst
          else panic "live_vars: only part of a variable live at a proc point"
        add_slot (CallArea Old, off, w) rst =
          if off == wORD_SIZE && w == wORD_SIZE then
             rst -- the return infotable should be live
          else pprPanic "CallAreas must not be live across function calls" (ppr bid)
        add_slot (CallArea (Young _), _, _) _ =
          pprPanic "CallAreas must not be live across function calls" (ppr bid)
        slots = expectJust "live_vars slots" $ lookupBlockEnv slotEnv bid
        youngByte = expectJust "live_vars bid_pos" $ lookupFM areaMap (CallArea (Young bid))

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
