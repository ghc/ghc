{-# OPTIONS_GHC -XGADTs -XNoMonoLocalBinds #-}
-- Norman likes local bindings
-- If this module lives on I'd like to get rid of this flag in due course

-- Todo: remove
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#if __GLASGOW_HASKELL__ >= 701
-- GHC 7.0.1 improved incomplete pattern warnings with GADTs
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
#endif

module CmmStackLayout
    ( SlotEnv, liveSlotAnal, liveSlotTransfers, removeLiveSlotDefs
    , getSpEntryMap, layout, manifestSP, igraph, areaBuilder
    , stubSlotsOnDeath ) -- to help crash early during debugging
where

import Constants
import Prelude hiding (succ, zip, unzip, last)

import BlockId
import Cmm
import CmmExpr
import CmmProcPoint
import Maybes
import MkGraph (stackStubExpr)
import Control.Monad
import OptimizationFuel
import Outputable
import SMRep (ByteOff)

import Compiler.Hoopl

import Data.Map (Map)
import qualified Data.Map as Map
import qualified FiniteMap as Map

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

slotLattice :: DataflowLattice SubAreaSet
slotLattice = DataflowLattice "live slots" Map.empty add
  where add _ (OldFact old) (NewFact new) = case Map.foldRightWithKey addArea (False, old) new of
                                              (change, x) -> (changeIf change, x)
        addArea a newSlots z = foldr (addSlot a) z newSlots
        addSlot a slot (changed, map) =
          let (c, live) = liveGen slot $ Map.findWithDefault [] a map
          in (c || changed, Map.insert a live map)

slotLatticeJoin :: [SubAreaSet] -> SubAreaSet
slotLatticeJoin facts = foldr extend (fact_bot slotLattice) facts
  where extend fact res = snd $ fact_join slotLattice undefined (OldFact fact) (NewFact res)

type SlotEnv   = BlockEnv SubAreaSet
  -- The sub-areas live on entry to the block

liveSlotAnal :: CmmGraph -> FuelUniqSM SlotEnv
liveSlotAnal g = liftM snd $ dataflowPassBwd g [] $ analBwd slotLattice liveSlotTransfers

-- Add the subarea s to the subareas in the list-set (possibly coalescing it with
-- adjacent subareas), and also return whether s was a new addition.
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
liveKill (a, hi, w) set = -- pprTrace "killing slots in area" (ppr a) $
                          liveKill' set []
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

-- Note: the stack slots that hold variables returned on the stack are not
-- considered live in to the block -- we treat the first node as a definition site.
-- BEWARE?: Am I being a little careless here in failing to check for the
-- entry Id (which would use the CallArea Old).
liveSlotTransfers :: BwdTransfer CmmNode SubAreaSet
liveSlotTransfers = mkBTransfer3 frt mid lst
  where frt :: CmmNode C O -> SubAreaSet -> SubAreaSet
        frt (CmmEntry l) f = Map.delete (CallArea (Young l)) f

        mid :: CmmNode O O -> SubAreaSet -> SubAreaSet
        mid n f = foldSlotsUsed addSlot (removeLiveSlotDefs f n) n
        lst :: CmmNode O C -> FactBase SubAreaSet -> SubAreaSet
        lst n f = liveInSlots n $ case n of
          -- EZY: There's something fishy going on here: the old area is
          -- being kept alive too long.  In particular, the incoming
          -- parameters can be safely clobbered after they've been read
          -- out.
          CmmCall {cml_cont=Nothing, cml_args=args} -> add_area (CallArea Old) args out
          CmmCall {cml_cont=Just k, cml_args=args}  -> add_area (CallArea Old) args (add_area (CallArea (Young k)) args out)
          CmmForeignCall {succ=k, updfr=oldend}     -> add_area (CallArea Old) oldend (add_area (CallArea (Young k)) wORD_SIZE out)
          _                                         -> out
         where out = joinOutFacts slotLattice n f
               add_area _ n live | n == 0 = live
               add_area a n live = Map.insert a (snd $ liveGen (a, n, n) $ Map.findWithDefault [] a live) live

-- Slot sets: adding slots, removing slots, and checking for membership.
liftToArea :: Area -> ([SubArea] -> [SubArea]) -> SubAreaSet -> SubAreaSet 
addSlot, removeSlot :: SubAreaSet -> SubArea -> SubAreaSet
elemSlot            :: SubAreaSet -> SubArea -> Bool
liftToArea a f map = Map.insert a (f (Map.findWithDefault [] a map)) map
addSlot    live (a, i, w) = liftToArea a (snd . liveGen  (a, i, w)) live
removeSlot live (a, i, w) = liftToArea a       (liveKill (a, i, w)) live
elemSlot   live (a, i, w) =
  not $ fst $ liveGen  (a, i, w) (Map.findWithDefault [] a live)

removeLiveSlotDefs :: (DefinerOfSlots s, UserOfSlots s) => SubAreaSet -> s -> SubAreaSet
removeLiveSlotDefs = foldSlotsDefd removeSlot

liveInSlots :: (DefinerOfSlots s, UserOfSlots s) => s -> SubAreaSet -> SubAreaSet
liveInSlots x live = foldSlotsUsed addSlot (removeLiveSlotDefs live x) x

liveLastIn :: CmmNode O C -> (BlockId -> SubAreaSet) -> SubAreaSet
liveLastIn l env = liveInSlots l (liveLastOut env l)

-- Don't forget to keep the outgoing parameters in the CallArea live,
-- as well as the update frame.
-- Note: We have to keep the update frame live at a call because of the
-- case where the function doesn't return -- in that case, there won't
-- be a return to keep the update frame live. We'd still better keep the
-- info pointer in the update frame live at any call site;
-- otherwise we could screw up the garbage collector.
liveLastOut :: (BlockId -> SubAreaSet) -> CmmNode O C -> SubAreaSet
liveLastOut env l =
  case l of
    CmmCall _ Nothing n _ _ -> 
      add_area (CallArea Old) n out -- add outgoing args (includes upd frame)
    CmmCall _ (Just k) n _ _ ->
      add_area (CallArea Old) n (add_area (CallArea (Young k)) n out)
    CmmForeignCall { succ = k, updfr = oldend } ->
      add_area (CallArea Old) oldend (add_area (CallArea (Young k)) wORD_SIZE out)
    _ -> out
  where out = slotLatticeJoin $ map env $ successors l
        add_area _ n live | n == 0 = live
        add_area a n live =
          Map.insert a (snd $ liveGen (a, n, n) $ Map.findWithDefault [] a live) live

-- The liveness analysis must be precise: otherwise, we won't know if a definition
-- should really kill a live-out stack slot.
-- But the interference graph does not have to be precise -- it might decide that
-- any live areas interfere. To maintain both a precise analysis and an imprecise
-- interference graph, we need to convert the live-out stack slots to graph nodes
-- at each and every instruction; rather than reconstruct a new list of nodes
-- every time, I provide a function to fold over the nodes, which should be a
-- reasonably efficient approach for the implementations we envision.
-- Of course, it will probably be much easier to program if we just return a list...
type Set x = Map x ()
data IGraphBuilder n =
  Builder { foldNodes     :: forall z. SubArea -> (n -> z -> z) -> z -> z
          , _wordsOccupied :: AreaSizeMap -> AreaMap -> n -> [Int]
          }

areaBuilder :: IGraphBuilder Area
areaBuilder = Builder fold words
  where fold (a, _, _) f z = f a z
        words areaSize areaMap a =
          case Map.lookup a areaMap of
            Just addr -> [addr .. addr + (Map.lookup a areaSize `orElse`
                                          pprPanic "wordsOccupied: unknown area" (ppr areaSize <+> ppr a))]
            Nothing   -> []

--slotBuilder :: IGraphBuilder (Area, Int)
--slotBuilder = undefined

-- Now, we can build the interference graph.
-- The usual story: a definition interferes with all live outs and all other
-- definitions.
type IGraph x = Map x (Set x)
type IGPair x = (IGraph x, IGraphBuilder x)
igraph :: (Ord x) => IGraphBuilder x -> SlotEnv -> CmmGraph -> IGraph x
igraph builder env g = foldr interfere Map.empty (postorderDfs g)
  where foldN = foldNodes builder
        interfere block igraph = foldBlockNodesB3 (first, middle, last) block igraph
          where first _ (igraph, _) = igraph
                middle node (igraph, liveOut) =
                  (addEdges igraph node liveOut, liveInSlots node liveOut)
                last node igraph =
                  (addEdges igraph node $ liveLastOut env' node, liveLastIn node env')

                -- add edges between a def and the other defs and liveouts
                addEdges igraph i out = fst $ foldSlotsDefd addDef (igraph, out) i
                addDef (igraph, out) def@(a, _, _) =
                  (foldN def (addDefN out) igraph,
                   Map.insert a (snd $ liveGen def (Map.findWithDefault [] a out)) out)
                addDefN out n igraph =
                  let addEdgeNO o igraph = foldN o addEdgeNN igraph
                      addEdgeNN n' igraph = addEdgeNN' n n' $ addEdgeNN' n' n igraph
                      addEdgeNN' n n' igraph = Map.insert n (Map.insert n' () set) igraph
                        where set = Map.findWithDefault Map.empty n igraph
                  in Map.foldRightWithKey (\ _ os igraph -> foldr addEdgeNO igraph os) igraph out
                env' bid = mapLookup bid env `orElse` panic "unknown blockId in igraph"

-- Before allocating stack slots, we need to collect one more piece of information:
-- what's the highest offset (in bytes) used in each Area?
-- We'll need to allocate that much space for each Area.

-- Mapping of areas to area sizes (not offsets!)
type AreaSizeMap = AreaMap

-- JD: WHY CAN'T THIS COME FROM THE slot-liveness info?
getAreaSize :: ByteOff -> CmmGraph -> AreaSizeMap
  -- The domain of the returned mapping consists only of Areas
  -- used for (a) variable spill slots, and (b) parameter passing areas for calls
getAreaSize entry_off g =
  foldGraphBlocks (foldBlockNodesF3 (first, add_regslots, last))
              (Map.singleton (CallArea Old) entry_off) g
  where first _  z = z
        last :: CmmNode O C -> Map Area Int -> Map Area Int
        last l@(CmmCall _ Nothing args res _) z  =  add_regslots l (add (add z area args) area res)
          where area = CallArea Old
        last l@(CmmCall _ (Just k) args res _) z =  add_regslots l (add (add z area args) area res)
          where area = CallArea (Young k)
        last l@(CmmForeignCall {succ = k}) z     =  add_regslots l (add z area wORD_SIZE)
          where area = CallArea (Young k)
        last l z                                 =  add_regslots l z
        add_regslots i z = foldSlotsUsed addSlot (foldSlotsDefd addSlot z i) i
        addSlot z (a@(RegSlot (LocalReg _ ty)), _, _) =
          add z a $ widthInBytes $ typeWidth ty
        addSlot z _ = z
        add z a off = Map.insert a (max off (Map.findWithDefault 0 a z)) z
	-- The 'max' is important.  Two calls, to f and g, might share a common
	-- continuation (and hence a common CallArea), but their number of overflow
	-- parameters might differ.
        -- EZY: Ought to use insert with combining function...


-- Find the Stack slots occupied by the subarea's conflicts
conflictSlots :: Ord x => IGPair x -> AreaSizeMap -> AreaMap -> SubArea -> Set Int
conflictSlots (ig, Builder foldNodes wordsOccupied) areaSize areaMap subarea =
  foldNodes subarea foldNode Map.empty
  where foldNode n set = Map.foldRightWithKey conflict set $ Map.findWithDefault Map.empty n ig
        conflict n' () set = liveInSlots areaMap n' set
        -- Add stack slots occupied by igraph node n
        liveInSlots areaMap n set = foldr setAdd set (wordsOccupied areaSize areaMap n)
        setAdd w s = Map.insert w () s

-- Find any open space for 'area' on the stack, starting from the
-- 'offset'.  If the area is a CallArea or a spill slot for a pointer,
-- then it must be word-aligned.
freeSlotFrom :: Ord x => IGPair x -> AreaSizeMap -> Int -> AreaMap -> Area -> Int
freeSlotFrom ig areaSize offset areaMap area =
  let size = Map.lookup area areaSize `orElse` 0
      conflicts = conflictSlots ig areaSize areaMap (area, size, size)
      -- CallAreas and Ptrs need to be word-aligned (round up!)
      align = case area of CallArea _                                -> align'
                           RegSlot  r | isGcPtrType (localRegType r) -> align'
                           RegSlot  _                                -> id
      align' n = (n + (wORD_SIZE - 1)) `div` wORD_SIZE * wORD_SIZE
      -- Find a space big enough to hold the area
      findSpace curr 0 = curr
      findSpace curr cnt = -- part of target slot, # of bytes left to check
        if Map.member curr conflicts then
          findSpace (align (curr + size)) size -- try the next (possibly) open space
        else findSpace (curr - 1) (cnt - 1)
  in findSpace (align (offset + size)) size

-- Find an open space on the stack, and assign it to the area.
allocSlotFrom :: Ord x => IGPair x -> AreaSizeMap -> Int -> AreaMap -> Area -> AreaMap
allocSlotFrom ig areaSize from areaMap area =
  if Map.member area areaMap then areaMap
  else Map.insert area (freeSlotFrom ig areaSize from areaMap area) areaMap

-- Figure out all of the offsets from the slot location; this will be
-- non-zero for procpoints.
type SpEntryMap = BlockEnv Int
getSpEntryMap :: Int -> CmmGraph -> SpEntryMap
getSpEntryMap entry_off g@(CmmGraph {g_entry = entry})
    = foldGraphBlocks add_sp_off (mapInsert entry entry_off emptyBlockMap) g
  where add_sp_off :: CmmBlock -> BlockEnv Int -> BlockEnv Int
        add_sp_off b env =
          case lastNode b of
            CmmCall {cml_cont=Just succ, cml_ret_args=off} -> mapInsert succ off env
            CmmForeignCall {succ=succ}                     -> mapInsert succ wORD_SIZE env
            _                                              -> env

-- | Greedy stack layout.
-- Compute liveness, build the interference graph, and allocate slots for the areas.
-- We visit each basic block in a (generally) forward order.

-- At each instruction that names a register subarea r, we immediately allocate
-- any available slot on the stack by the following procedure:
--  1. Find the sub-areas S that conflict with r
--  2. Find the stack slots used for S
--  3. Choose a contiguous stack space s not in S (s must be large enough to hold r)

-- For a CallArea, we allocate the stack space only when we reach a function
-- call that returns to the CallArea's blockId.
-- Then, we allocate the Area subject to the following constraints:
--   a) It must be younger than all the sub-areas that are live on entry to the block
--         This constraint is only necessary for the successor of a call
--   b) It must not overlap with any already-allocated Area with which it conflicts
--   	   (ie at some point, not necessarily now, is live at the same time)
--   Part (b) is just the 1,2,3 part above

-- Note: The stack pointer only has to be younger than the youngest live stack slot
-- at proc points. Otherwise, the stack pointer can point anywhere.

layout :: ProcPointSet -> SpEntryMap -> SlotEnv -> ByteOff -> CmmGraph -> AreaMap
-- The domain of the returned map includes an Area for EVERY block
-- including each block that is not the successor of a call (ie is not a proc-point)
-- That's how we return the info of what the SP should be at the entry of every non
-- procpoint block.  However, note that procpoint blocks have their
-- /slot/ stored, which is not necessarily the value of the SP on entry
-- to the block (in fact, it probably isn't, due to argument passing).
-- See [Procpoint Sp offset]

layout procPoints spEntryMap env entry_off g =
  let ig = (igraph areaBuilder env g, areaBuilder)
      env' bid = mapLookup bid env `orElse` panic "unknown blockId in igraph"
      areaSize = getAreaSize entry_off g

      -- Find the youngest live stack slot that has already been allocated
      youngest_live :: AreaMap 	   -- Already allocated
                    -> SubAreaSet  -- Sub-areas live here
		    -> ByteOff     -- Offset of the youngest byte of any 
		       		   --    already-allocated, live sub-area
      youngest_live areaMap live = fold_subareas young_slot live 0
        where young_slot (a, o, _) z = case Map.lookup a areaMap of
                                         Just top -> max z $ top + o
                                         Nothing  -> z
              fold_subareas f m z = Map.foldRightWithKey (\_ s z -> foldr f z s) z m

      -- Allocate space for spill slots and call areas
      allocVarSlot = allocSlotFrom ig areaSize 0

      -- Update the successor's incoming SP.
      setSuccSPs inSp bid areaMap =
        case (Map.lookup area areaMap , mapLookup bid (toBlockMap g)) of
          (Just _, _) -> areaMap -- succ already knows incoming SP
          (Nothing, Just _) ->
            if setMember bid procPoints then
              let young = youngest_live areaMap $ env' bid
                  -- start = case returnOff stackInfo of Just b  -> max b young
                  --                                     Nothing -> young
                  start = young -- maybe wrong, but I don't understand
                                -- why the preceding is necessary...
              in  allocSlotFrom ig areaSize start areaMap area
            else Map.insert area inSp areaMap
          (_, Nothing) -> panic "Block not found in cfg"
        where area = CallArea (Young bid)

      layoutAreas areaMap block = foldBlockNodesF3 (flip const, allocMid, allocLast (entryLabel block)) block areaMap
      allocMid m areaMap = foldSlotsDefd alloc' (foldSlotsUsed alloc' areaMap m) m
      allocLast bid l areaMap =
        foldr (setSuccSPs inSp) areaMap' (successors l)
        where inSp = slot + spOffset -- [Procpoint Sp offset]
              -- If it's not in the map, we should use our previous
              -- calculation unchanged.
              spOffset = mapLookup bid spEntryMap `orElse` 0
              slot = expectJust "slot in" $ Map.lookup (CallArea (Young bid)) areaMap
              areaMap' = foldSlotsDefd alloc' (foldSlotsUsed alloc' areaMap l) l
      alloc' areaMap (a@(RegSlot _), _, _) = allocVarSlot areaMap a
      alloc' areaMap _ = areaMap

      initMap = Map.insert (CallArea (Young (g_entry g))) 0
              . Map.insert (CallArea Old)                 0
              $ Map.empty

      areaMap = foldl layoutAreas initMap (postorderDfs g)
  in -- pprTrace "ProcPoints" (ppr procPoints) $
     -- pprTrace "Area SizeMap" (ppr areaSize) $
     -- pprTrace "Entry offset" (ppr entry_off) $
     -- pprTrace "Area Map" (ppr areaMap) $
     areaMap

{- Note [Procpoint Sp offset]

The calculation of inSp is a little tricky.  (Un)fortunately, if you get
it wrong, you will get inefficient but correct code.  You know you've
got it wrong if the generated stack pointer bounces up and down for no
good reason.

Why can't we just set inSp to the location of the slot?  (This is what
the code used to do.)  The trouble is when we actually hit the proc
point the start of the slot will not be the same as the actual Sp due
to argument passing:

  a:
      I32[(young<b> + 4)] = cde;
      // Stack pointer is moved to young end (bottom) of young<b> for call
      // +-------+
      // | arg 1 |
      // +-------+ <- Sp
      call (I32[foobar::I32])(...) returns to Just b (4) (4) with update frame 4;
  b:
      // After call, stack pointer is above the old end (top) of
      // young<b> (the difference is spOffset)
      // +-------+ <- Sp
      // | arg 1 |
      // +-------+

If we blithely set the Sp to be the same as the slot (the young end of
young<b>), an adjustment will be necessary when we go to the next block.
This is wasteful.  So, instead, for the next block after a procpoint,
the actual Sp should be set to the same as the true Sp when we just
entered the procpoint.  Then manifestSP will automatically do the right
thing.

Questions you may ask:

1. Why don't we need to change the mapping for the procpoint itself?
   Because manifestSP does its own calculation of the true stack value,
   manifestSP will notice the discrepancy between the actual stack
   pointer and the slot start, and adjust all of its memory accesses
   accordingly.  So the only problem is when we adjust the Sp in
   preparation for the successor block; that's why this code is here and
   not in setSuccSPs.

2. Why don't we make the procpoint call area and the true offset match
   up?  If we did that, we would never use memory above the true value
   of the stack pointer, thus wasting all of the stack we used to store
   arguments.  You might think that some clever changes to the slot
   offsets, using negative offsets, might fix it, but this does not make
   semantic sense.

3. If manifestSP is already calculating the true stack value, why we can't
   do this trick inside manifestSP itself?  The reason is that if two
   branches join with inconsistent SPs, one of them has to be fixed: we
   can't know what the fix should be without already knowing what the
   chosen location of SP is on the next successor.  (This is
   the "succ already knows incoming SP" case), This calculation cannot
   be easily done in manifestSP, since it processes the nodes
   /backwards/.  So we need to have figured this out before we hit
   manifestSP.
-}

-- After determining the stack layout, we can:
-- 1. Replace references to stack Areas with addresses relative to the stack
--    pointer.
-- 2. Insert adjustments to the stack pointer to ensure that it is at a
--    conventional location at each proc point.
--    Because we don't take interrupts on the execution stack, we only need the
--    stack pointer to be younger than the live values on the stack at proc points.
-- 3. Compute the maximum stack offset used in the procedure and replace
--    the stack high-water mark with that offset.
manifestSP :: SpEntryMap -> AreaMap -> ByteOff -> CmmGraph -> FuelUniqSM CmmGraph
manifestSP spEntryMap areaMap entry_off g@(CmmGraph {g_entry=entry}) =
  ofBlockMap entry `liftM` foldl replB (return mapEmpty) (postorderDfs g)
  where slot a = -- pprTrace "slot" (ppr a) $
                   Map.lookup a areaMap `orElse` panic "unallocated Area"
        slot' (Just id) = slot $ CallArea (Young id)
        slot' Nothing   = slot $ CallArea Old
        sp_high = maxSlot slot g
        proc_entry_sp = slot (CallArea Old) + entry_off

        spOffset id = mapLookup id spEntryMap `orElse` 0

        sp_on_entry id | id == entry = proc_entry_sp
        sp_on_entry id = slot' (Just id) + spOffset id

        -- On entry to procpoints, the stack pointer is conventional;
        -- otherwise, we check the SP set by predecessors.
        replB :: FuelUniqSM (BlockEnv CmmBlock) -> CmmBlock -> FuelUniqSM (BlockEnv CmmBlock)
        replB blocks block =
          do let (head, middles, JustC tail :: MaybeC C (CmmNode O C)) = blockToNodeList block
                 middles' = map (middle spIn) middles
             bs <- replLast head middles' tail
             flip (foldr insertBlock) bs `liftM` blocks
          where spIn = sp_on_entry (entryLabel block)

                middle spOff m = mapExpDeep (replSlot spOff) m
                -- XXX there shouldn't be any global registers in the
                -- CmmCall, so there shouldn't be any slots in
                -- CmmCall... check that...
                last   spOff l = mapExpDeep (replSlot spOff) l
                replSlot spOff (CmmStackSlot a i) = CmmRegOff (CmmGlobal Sp) (spOff - (slot a + i))
                replSlot _ (CmmLit CmmHighStackMark) = -- replacing the high water mark
                  CmmLit (CmmInt (toInteger (max 0 (sp_high - proc_entry_sp))) (typeWidth bWord))
                replSlot _ e = e

                replLast :: MaybeC C (CmmNode C O) -> [CmmNode O O] -> CmmNode O C -> FuelUniqSM [CmmBlock]
                replLast h m l@(CmmCall _ k n _ _)       = updSp (slot' k + n) h m l
                -- JD: LastForeignCall probably ought to have an outgoing
                --     arg size, just like LastCall
                replLast h m l@(CmmForeignCall {succ=k}) = updSp (slot' (Just k) + wORD_SIZE) h m l
                replLast h m l@(CmmBranch k)             = updSp (sp_on_entry k) h m l
                replLast h m l                           = uncurry (:) `liftM` foldr succ (return (b, [])) (successors l)
                  where b :: CmmBlock
                        b = updSp' spIn h m l
                        succ succId z =
                          let succSp = sp_on_entry succId in
                          if succSp /= spIn then
                            do (b,  bs)  <- z
                               (b', bs') <- insertBetween b (adjustSp succSp) succId
                               return (b', bs' ++ bs)
                          else z

                updSp sp h m l = return [updSp' sp h m l]
                updSp' sp h m l | sp == spIn = blockOfNodeList (h, m, JustC $ last sp l)
                                | otherwise  = blockOfNodeList (h, m ++ adjustSp sp, JustC $ last sp l)
                adjustSp sp = [CmmAssign (CmmGlobal Sp) e]
                  where e = CmmMachOp (MO_Add wordWidth) [CmmReg (CmmGlobal Sp), off]
                        off = CmmLit $ CmmInt (toInteger $ spIn - sp) wordWidth


-- To compute the stack high-water mark, we fold over the graph and
-- compute the highest slot offset.
maxSlot :: (Area -> Int) -> CmmGraph -> Int
maxSlot slotOff g = foldGraphBlocks (foldBlockNodesF3 (flip const, highSlot, highSlot)) 0 g
  where highSlot i z = foldSlotsUsed add (foldSlotsDefd add z i) i
        add z (a, i, _) = max z (slotOff a + i)

-----------------------------------------------------------------------------
-- | Sanity check: stub pointers immediately after they die
-----------------------------------------------------------------------------
-- This will miss stack slots that are last used in a Last node,
-- but it should do pretty well...

stubSlotsOnDeath :: CmmGraph -> FuelUniqSM CmmGraph
stubSlotsOnDeath g = liftM fst $ dataflowPassBwd g [] $ analRewBwd slotLattice
                                                                   liveSlotTransfers
                                                                   rewrites
    where rewrites = mkBRewrite3 frt mid lst
          frt _ _ = return Nothing
          mid m liveSlots = return $ foldSlotsUsed (stub liveSlots m) Nothing m
          lst _ _ = return Nothing
          stub liveSlots m rst subarea@(a, off, w) =
            if elemSlot liveSlots subarea then rst
            else let store = mkMiddle $ CmmStore (CmmStackSlot a off)
                                                 (stackStubExpr (widthFromBytes w))
                 in case rst of Nothing -> Just (mkMiddle m <*> store)
                                Just g  -> Just (g <*> store)
