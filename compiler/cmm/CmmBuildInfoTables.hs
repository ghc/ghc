#if __GLASGOW_HASKELL__ >= 611
{-# OPTIONS_GHC -XNoMonoLocalBinds #-}
#endif
-- Norman likes local bindings
-- If this module lives on I'd like to get rid of this flag in due course

module CmmBuildInfoTables
    ( CAFSet, CAFEnv, CmmTopForInfoTables(..), cafAnal, localCAFInfo, mkTopCAFInfo
    , setInfoTableSRT, setInfoTableStackMap
    , TopSRT, emptySRT, srtToData
    , bundleCAFs
    , finishInfoTables, lowerSafeForeignCalls
    , cafTransfers, liveSlotTransfers
    , extendEnvWithSafeForeignCalls, extendEnvsForSafeForeignCalls )
where

#include "HsVersions.h"

import Constants
import Digraph
import qualified Prelude as P
import Prelude
import Util (sortLe)

import BlockId
import Bitmap
import CLabel
import Cmm hiding (blockId)
import CmmInfo
import CmmProcPointZ
import CmmStackLayout
import CmmTx
import DFMonad
import Module
import FastString
import FiniteMap
import ForeignCall
import IdInfo
import Data.List
import Maybes
import MkZipCfg
import MkZipCfgCmm hiding (CmmAGraph, CmmBlock, CmmTopZ, CmmZ, CmmGraph)
import Control.Monad
import Name
import Outputable
import SMRep
import StgCmmClosure
import StgCmmForeign
-- import StgCmmMonad
import StgCmmUtils
import UniqSupply
import ZipCfg hiding (zip, unzip, last)
import qualified ZipCfg as G
import ZipCfgCmmRep
import ZipDataflow

----------------------------------------------------------------
-- Building InfoTables


-----------------------------------------------------------------------
-- Stack Maps

-- Given a block ID, we return a representation of the layout of the stack,
-- as suspended before entering that block.
-- (For a return site to a function call, the layout does not include the
--  parameter passing area (or the "return address" on the stack)).
-- If the element is `Nothing`, then it represents a word of the stack that
-- does not contain a live pointer.
-- If the element is `Just` a register, then it represents a live spill slot
-- for a pointer; we assume that a pointer is the size of a word.
-- The head of the list represents the young end of the stack where the infotable
-- pointer for the block `Bid` is stored.
-- The infotable pointer itself is not included in the list.
-- Call areas are also excluded from the list: besides the stuff in the update
-- frame (and the return infotable), call areas should never be live across
-- function calls.

-- RTS Invariant: All pointers must be word-aligned because each bit in the bitmap
-- represents a word. Consequently, we have to be careful when we see a live slot
-- on the stack: if we have packed multiple sub-word values into a word,
-- we have to make sure that we only mark the entire word as a non-pointer.

-- Also, don't forget to stop at the old end of the stack (oldByte),
-- which may differ depending on whether there is an update frame.

type RegSlotInfo
   = ( Int	  -- Offset from oldest byte of Old area
     , LocalReg   -- The register
     , Int)       -- Width of the register

live_ptrs :: ByteOff -> BlockEnv SubAreaSet -> AreaMap -> BlockId -> [Maybe LocalReg]
live_ptrs oldByte slotEnv areaMap bid =
  -- pprTrace "live_ptrs for" (ppr bid <+> text (show oldByte ++ "-" ++ show youngByte) <+>
  --                           ppr liveSlots) $
  -- pprTrace ("stack layout for " ++ show bid ++ ": ") (ppr res) $ res
  res
  where res = reverse $ slotsToList youngByte liveSlots []
 
        slotsToList :: Int -> [RegSlotInfo] -> [Maybe LocalReg] -> [Maybe LocalReg]
        -- n starts at youngByte and is decremented down to oldByte
	-- Returns a list, one element per word, with 
	--    (Just r) meaning 'pointer register r is saved here', 
	--    Nothing  meaning 'non-pointer or empty'

        slotsToList n [] results | n == oldByte = results -- at old end of stack frame

        slotsToList n (s : _) _  | n == oldByte =
          pprPanic "slot left off live_ptrs" (ppr s <+> ppr oldByte <+>
               ppr n <+> ppr liveSlots <+> ppr youngByte)

        slotsToList n _ _ | n < oldByte =
          panic "stack slots not allocated on word boundaries?"

        slotsToList n l@((n', r, w) : rst) results =
          if n == (n' + w) then -- slot's young byte is at n
            ASSERT (not (isPtr r) ||
                    (n `mod` wORD_SIZE == 0 && w == wORD_SIZE)) -- ptrs must be aligned
            slotsToList next (dropWhile (non_ptr_younger_than next) rst)
                        (stack_rep : results)
          else slotsToList next (dropWhile (non_ptr_younger_than next) l)
                           (Nothing : results)
          where next = n - wORD_SIZE
                stack_rep = if isPtr r then Just r else Nothing

        slotsToList n [] results = slotsToList (n - wORD_SIZE) [] (Nothing : results)

        non_ptr_younger_than next (n', r, w) =
          n' + w > next &&
            ASSERT (not (isPtr r))
            True
        isPtr = isGcPtrType . localRegType

        liveSlots :: [RegSlotInfo]
        liveSlots = sortBy (\ (off,_,_) (off',_,_) -> compare off' off)
                           (foldFM (\_ -> flip $ foldl add_slot) [] slots)
                    
        add_slot :: [RegSlotInfo] -> SubArea -> [RegSlotInfo]
        add_slot rst (a@(RegSlot r@(LocalReg _ ty)), off, w) = 
          if off == w && widthInBytes (typeWidth ty) == w then
            (expectJust "add_slot" (lookupFM areaMap a), r, w) : rst
          else panic "live_ptrs: only part of a variable live at a proc point"
        add_slot rst (CallArea Old, _, _) =
          rst -- the update frame (or return infotable) should be live
              -- would be nice to check that only that part of the callarea is live...
        add_slot rst ((CallArea _), _, _) =
          rst
          -- JD: THIS ISN'T CURRENTLY A CORRECTNESS PROBLEM, BUT WE SHOULD REALLY
          -- MAKE LIVENESS INFO AROUND CALLS MORE PRECISE -- FOR NOW, A 32-BIT
          -- FLOAT PADS OUT TO 64 BITS, BUT WE ASSUME THE WHOLE PARAMETER-PASSING
          -- AREA IS LIVE (WHICH IT ISN'T...).  WE SHOULD JUST PUT THE LIVE AREAS
          -- IN THE CALL NODES, WHICH SHOULD EVENTUALLY HAVE LIVE REGISTER AS WELL,
          -- SO IT'S ALL GOING IN THE SAME DIRECTION.
          -- pprPanic "CallAreas must not be live across function calls" (ppr bid <+> ppr c)

        slots :: SubAreaSet	 -- The SubAreaSet for 'bid'
        slots = expectJust "live_ptrs slots" $ lookupBlockEnv slotEnv bid
        youngByte = expectJust "live_ptrs bid_pos" $ lookupFM areaMap (CallArea (Young bid))

-- Construct the stack maps for the given procedure.
setInfoTableStackMap :: SlotEnv -> AreaMap -> CmmTopForInfoTables -> CmmTopForInfoTables 
setInfoTableStackMap _ _ t@(NoInfoTable _) = t
setInfoTableStackMap slotEnv areaMap t@(FloatingInfoTable _ bid updfr_off) =
  updInfo (const (live_ptrs updfr_off slotEnv areaMap bid)) id t
setInfoTableStackMap slotEnv areaMap
     t@(ProcInfoTable (CmmProc (CmmInfo _ _ _) _ _ ((_, Just updfr_off), _)) procpoints) =
  case blockSetToList procpoints of
    [bid] -> updInfo (const (live_ptrs updfr_off slotEnv areaMap bid)) id t
    _ -> panic "setInfoTableStackMap: unexpected number of procpoints"
           -- until we stop splitting the graphs at procpoints in the native path
setInfoTableStackMap _ _ t = pprPanic "unexpected case for setInfoTableStackMap" (ppr t)
                 


-----------------------------------------------------------------------
-- SRTs

-- WE NEED AN EXAMPLE HERE.
-- IN PARTICULAR, WE NEED TO POINT OUT THE DISTINCTION BETWEEN
-- FUNCTIONS WITH STATIC CLOSURES AND THOSE THAT MUST BE CONSTRUCTED
-- DYNAMICALLY (AND HENCE CAN'T BE REFERENCED IN AN SRT).
-- IN THE LATTER CASE, WE HAVE TO TAKE ALL THE CAFs REFERENCED BY
-- THE CLOSURE AND INLINE THEM INTO ANY SRT THAT MAY MENTION THE CLOSURE.
-- (I.E. TAKE THE TRANSITIVE CLOSURE, but only for non-static closures).


-----------------------------------------------------------------------
-- Finding the CAFs used by a procedure

type CAFSet = FiniteMap CLabel ()
type CAFEnv = BlockEnv CAFSet

-- First, an analysis to find live CAFs.
cafLattice :: DataflowLattice CAFSet
cafLattice = DataflowLattice "live cafs" emptyFM add False
  where add new old = if sizeFM new' > sizeFM old then aTx new' else noTx new'
          where new' = new `plusFM` old

cafTransfers :: BackwardTransfers Middle Last CAFSet
cafTransfers = BackwardTransfers first middle last
  where first  _ live = live
        middle m live = foldExpDeepMiddle addCaf m live
        last   l env  = foldExpDeepLast   addCaf l (joinOuts cafLattice env l)
        addCaf e set = case e of
               CmmLit (CmmLabel c)              -> add c set
               CmmLit (CmmLabelOff c _)         -> add c set
               CmmLit (CmmLabelDiffOff c1 c2 _) -> add c1 $ add c2 set
               _ -> set
        add l s = if hasCAF l then addToFM s (cvtToClosureLbl l) () else s

type CafFix a = FuelMonad (BackwardFixedPoint Middle Last CAFSet a)
cafAnal :: LGraph Middle Last -> FuelMonad CAFEnv
cafAnal g = liftM zdfFpFacts (res :: CafFix ())
  where res = zdfSolveFromL emptyBlockEnv "live CAF analysis" cafLattice
                            cafTransfers (fact_bot cafLattice) g

-----------------------------------------------------------------------
-- Building the SRTs

-- Description of the SRT for a given module.
-- Note that this SRT may grow as we greedily add new CAFs to it.
data TopSRT = TopSRT { lbl      :: CLabel
                     , next_elt :: Int -- the next entry in the table
                     , rev_elts :: [CLabel]
                     , elt_map  :: FiniteMap CLabel Int }
                        -- map: CLabel -> its last entry in the table
instance Outputable TopSRT where
  ppr (TopSRT lbl next elts eltmap) =
    text "TopSRT:" <+> ppr lbl <+> ppr next <+> ppr elts <+> ppr eltmap

emptySRT :: MonadUnique m => m TopSRT
emptySRT =
  do top_lbl <- getUniqueM >>= \ u -> return $ mkSRTLabel (mkFCallName u "srt") NoCafRefs
     return TopSRT { lbl = top_lbl, next_elt = 0, rev_elts = [], elt_map = emptyFM }

cafMember :: TopSRT -> CLabel -> Bool
cafMember srt lbl = elemFM lbl (elt_map srt)

cafOffset :: TopSRT -> CLabel -> Maybe Int
cafOffset srt lbl = lookupFM (elt_map srt) lbl

addCAF :: CLabel -> TopSRT -> TopSRT
addCAF caf srt =
  srt { next_elt = last + 1
      , rev_elts = caf : rev_elts srt
      , elt_map  = addToFM (elt_map srt) caf last }
    where last  = next_elt srt

srtToData :: TopSRT -> CmmZ
srtToData srt = Cmm [CmmData RelocatableReadOnlyData (CmmDataLabel (lbl srt) : tbl)]
    where tbl = map (CmmStaticLit . CmmLabel) (reverse (rev_elts srt))

-- Once we have found the CAFs, we need to do two things:
-- 1. Build a table of all the CAFs used in the procedure.
-- 2. Compute the C_SRT describing the subset of CAFs live at each procpoint.
--
-- When building the local view of the SRT, we first make sure that all the CAFs are 
-- in the SRT. Then, if the number of CAFs is small enough to fit in a bitmap,
-- we make sure they're all close enough to the bottom of the table that the
-- bitmap will be able to cover all of them.
buildSRTs :: TopSRT -> FiniteMap CLabel CAFSet -> CAFSet ->
             FuelMonad (TopSRT, Maybe CmmTopZ, C_SRT)
buildSRTs topSRT topCAFMap cafs =
  do let liftCAF lbl () z = -- get CAFs for functions without static closures
           case lookupFM topCAFMap lbl of Just cafs -> z `plusFM` cafs
                                          Nothing   -> addToFM z lbl ()
         -- For each label referring to a function f without a static closure,
         -- replace it with the CAFs that are reachable from f.
         sub_srt topSRT localCafs =
           let cafs = keysFM (foldFM liftCAF emptyFM localCafs)
               mkSRT topSRT =
                 do localSRTs <- procpointSRT (lbl topSRT) (elt_map topSRT) cafs
                    return (topSRT, localSRTs)
           in if length cafs > maxBmpSize then
                mkSRT (foldl add_if_missing topSRT cafs)
              else -- make sure all the cafs are near the bottom of the srt
                mkSRT (add_if_too_far topSRT cafs)
         add_if_missing srt caf =
           if cafMember srt caf then srt else addCAF caf srt
         -- If a CAF is more than maxBmpSize entries from the young end of the
         -- SRT, then we add it to the SRT again.
         -- (Note: Not in the SRT => infinitely far.)
         add_if_too_far srt@(TopSRT {elt_map = m}) cafs =
           add srt (sortBy farthestFst cafs)
             where
               farthestFst x y = case (lookupFM m x, lookupFM m y) of
                                   (Nothing, Nothing) -> EQ
                                   (Nothing, Just _)  -> LT
                                   (Just _,  Nothing) -> GT
                                   (Just d, Just d')  -> compare d' d
               add srt [] = srt
               add srt@(TopSRT {next_elt = next}) (caf : rst) =
                 case cafOffset srt caf of
                   Just ix -> if next - ix > maxBmpSize then
                                add (addCAF caf srt) rst
                              else srt
                   Nothing -> add (addCAF caf srt) rst
     (topSRT, subSRTs) <- sub_srt topSRT cafs
     let (sub_tbls, blockSRTs) = subSRTs
     return (topSRT, sub_tbls, blockSRTs)

-- Construct an SRT bitmap.
-- Adapted from simpleStg/SRT.lhs, which expects Id's.
procpointSRT :: CLabel -> FiniteMap CLabel Int -> [CLabel] ->
                FuelMonad (Maybe CmmTopZ, C_SRT)
procpointSRT _ _ [] =
 return (Nothing, NoC_SRT)
procpointSRT top_srt top_table entries =
 do (top, srt) <- bitmap `seq` to_SRT top_srt offset len bitmap
    return (top, srt)
  where
    ints = map (expectJust "constructSRT" . lookupFM top_table) entries
    sorted_ints = sortLe (<=) ints
    offset = head sorted_ints
    bitmap_entries = map (subtract offset) sorted_ints
    len = P.last bitmap_entries + 1
    bitmap = intsToBitmap len bitmap_entries

maxBmpSize :: Int
maxBmpSize = widthInBits wordWidth `div` 2

-- Adapted from codeGen/StgCmmUtils, which converts from SRT to C_SRT.
to_SRT :: CLabel -> Int -> Int -> Bitmap -> FuelMonad (Maybe CmmTopZ, C_SRT)
to_SRT top_srt off len bmp
  | len > maxBmpSize || bmp == [fromIntegral srt_escape]
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

-- Gather CAF info for a procedure, but only if the procedure
-- doesn't have a static closure.
-- (If it has a static closure, it will already have an SRT to
--  keep its CAFs live.)
-- Any procedure referring to a non-static CAF c must keep live
-- any CAF that is reachable from c.
localCAFInfo :: CAFEnv -> CmmTopZ -> Maybe (CLabel, CAFSet)
localCAFInfo _      (CmmData _ _) = Nothing
localCAFInfo cafEnv (CmmProc (CmmInfo _ _ infoTbl) top_l _ (_, LGraph entry _)) =
  case infoTbl of
    CmmInfoTable False _ _ _ ->
      Just (cvtToClosureLbl top_l,
            expectJust "maybeBindCAFs" $ lookupBlockEnv cafEnv entry)
    _ -> Nothing

-- Once we have the local CAF sets for some (possibly) mutually
-- recursive functions, we can create an environment mapping
-- each function to its set of CAFs. Note that a CAF may
-- be a reference to a function. If that function f does not have
-- a static closure, then we need to refer specifically
-- to the set of CAFs used by f. Of course, the set of CAFs
-- used by f must be included in the local CAF sets that are input to
-- this function. To minimize lookup time later, we return
-- the environment with every reference to f replaced by its set of CAFs.
-- To do this replacement efficiently, we gather strongly connected
-- components, then we sort the components in topological order.
mkTopCAFInfo :: [(CLabel, CAFSet)] -> FiniteMap CLabel CAFSet
mkTopCAFInfo localCAFs = foldl addToTop emptyFM g
  where addToTop env (AcyclicSCC (l, cafset)) =
          addToFM env l (flatten env cafset)
        addToTop env (CyclicSCC nodes) =
          let (lbls, cafsets) = unzip nodes
              cafset  = foldl plusFM  emptyFM cafsets `delListFromFM` lbls
          in foldl (\env l -> addToFM env l (flatten env cafset)) env lbls
        flatten env cafset = foldFM (lookup env) emptyFM cafset
        lookup env caf () cafset' =
          case lookupFM env caf of Just cafs -> foldFM add cafset' cafs
                                   Nothing -> add caf () cafset'
        add caf () cafset' = addToFM cafset' caf ()
        g = stronglyConnCompFromEdgedVertices
              (map (\n@(l, cafs) -> (n, l, keysFM cafs)) localCAFs)

type StackLayout = [Maybe LocalReg]

-- Bundle the CAFs used at a procpoint.
bundleCAFs :: CAFEnv -> CmmTopForInfoTables -> (CAFSet, CmmTopForInfoTables)
bundleCAFs cafEnv t@(ProcInfoTable _ procpoints) =
  case blockSetToList procpoints of
    [bid] -> (expectJust "bundleCAFs" (lookupBlockEnv cafEnv bid), t)
    _     -> panic "setInfoTableStackMap: unexpect number of procpoints"
             -- until we stop splitting the graphs at procpoints in the native path
bundleCAFs cafEnv t@(FloatingInfoTable _ bid _) =
  (expectJust "bundleCAFs " (lookupBlockEnv cafEnv bid), t)
bundleCAFs _ t@(NoInfoTable _) = (emptyFM, t)

-- Construct the SRTs for the given procedure.
setInfoTableSRT :: FiniteMap CLabel CAFSet -> TopSRT -> (CAFSet, CmmTopForInfoTables) ->
                   FuelMonad (TopSRT, [CmmTopForInfoTables])
setInfoTableSRT topCAFMap topSRT (cafs, t@(ProcInfoTable _ procpoints)) =
  case blockSetToList procpoints of
    [_] -> setSRT cafs topCAFMap topSRT t
    _   -> panic "setInfoTableStackMap: unexpect number of procpoints"
           -- until we stop splitting the graphs at procpoints in the native path
setInfoTableSRT topCAFMap topSRT (cafs, t@(FloatingInfoTable _ _ _)) =
  setSRT cafs topCAFMap topSRT t
setInfoTableSRT _ topSRT (_, t@(NoInfoTable _)) = return (topSRT, [t])

setSRT :: CAFSet -> FiniteMap CLabel CAFSet -> TopSRT ->
          CmmTopForInfoTables -> FuelMonad (TopSRT, [CmmTopForInfoTables])
setSRT cafs topCAFMap topSRT t =
  do (topSRT, cafTable, srt) <- buildSRTs topSRT topCAFMap cafs
     let t' = updInfo id (const srt) t
     case cafTable of
       Just tbl -> return (topSRT, [t', NoInfoTable tbl])
       Nothing  -> return (topSRT, [t'])

updInfo :: (StackLayout -> StackLayout) -> (C_SRT -> C_SRT) ->
           CmmTopForInfoTables -> CmmTopForInfoTables 
updInfo toVars toSrt (ProcInfoTable (CmmProc info top_l top_args g) procpoints) =
  ProcInfoTable (CmmProc (updInfoTbl toVars toSrt info) top_l top_args g) procpoints
updInfo toVars toSrt (FloatingInfoTable info bid updfr_off) =
  FloatingInfoTable (updInfoTbl toVars toSrt info) bid updfr_off
updInfo _ _ (NoInfoTable _) = panic "can't update NoInfoTable"
updInfo _ _ _ = panic "unexpected arg to updInfo"

updInfoTbl :: (StackLayout -> StackLayout) -> (C_SRT -> C_SRT) -> CmmInfo -> CmmInfo 
updInfoTbl toVars toSrt (CmmInfo gc upd_fr (CmmInfoTable s p t typeinfo))
  = CmmInfo gc upd_fr (CmmInfoTable s p t typeinfo')
    where typeinfo' = case typeinfo of
            t@(ConstrInfo _ _ _)    -> t
            (FunInfo    c s a d e)  -> FunInfo c (toSrt s) a d e
            (ThunkInfo  c s)        -> ThunkInfo c (toSrt s)
            (ThunkSelectorInfo x s) -> ThunkSelectorInfo x (toSrt s)
            (ContInfo v s)          -> ContInfo (toVars v) (toSrt s)
updInfoTbl _ _ t@(CmmInfo _ _ CmmNonInfoTable) = t
  
-- Lower the CmmTopForInfoTables type down to good old CmmTopZ
-- by emitting info tables as data where necessary.
finishInfoTables :: CmmTopForInfoTables -> IO [CmmTopZ]
finishInfoTables (NoInfoTable t) = return [t]
finishInfoTables (ProcInfoTable p _) = return [p]
finishInfoTables (FloatingInfoTable (CmmInfo _ _ infotbl) bid _) =
  do uniq_supply <- mkSplitUniqSupply 'i'
     return $ mkBareInfoTable (retPtLbl bid) (uniqFromSupply uniq_supply) infotbl

----------------------------------------------------------------
-- Safe foreign calls:
-- Our analyses capture the dataflow facts at block boundaries, but we need
-- to extend the CAF and live-slot analyses to safe foreign calls as well,
-- which show up as middle nodes.
extendEnvWithSafeForeignCalls ::
  BackwardTransfers Middle Last a -> BlockEnv a -> CmmGraph -> BlockEnv a
extendEnvWithSafeForeignCalls transfers env g = fold_blocks block env g
  where block b z =
          tail (bt_last_in transfers l (lookup env)) z head
           where (head, last) = goto_end (G.unzip b)
                 l = case last of LastOther l -> l
                                  LastExit -> panic "extendEnvs lastExit"
        tail _ z (ZFirst _) = z
        tail fact env (ZHead h m@(MidForeignCall (Safe bid _) _ _ _)) =
          tail (mid m fact) (extendBlockEnv env bid fact) h
        tail fact env (ZHead h m) = tail (mid m fact) env h
        lookup map k = expectJust "extendEnvWithSafeFCalls" $ lookupBlockEnv map k
        mid = bt_middle_in transfers


extendEnvsForSafeForeignCalls :: CAFEnv -> SlotEnv -> CmmGraph -> (CAFEnv, SlotEnv)
extendEnvsForSafeForeignCalls cafEnv slotEnv g =
  fold_blocks block (cafEnv, slotEnv) g
    where block b z =
            tail ( bt_last_in cafTransfers      l (lookupFn cafEnv)
                 , bt_last_in liveSlotTransfers l (lookupFn slotEnv))
                 z head
             where (head, last) = goto_end (G.unzip b)
                   l = case last of LastOther l -> l
                                    LastExit -> panic "extendEnvs lastExit"
          tail _ z (ZFirst _) = z
          tail lives@(cafs, slots) (cafEnv, slotEnv)
               (ZHead h m@(MidForeignCall (Safe bid _) _ _ _)) =
            let slots'   = removeLiveSlotDefs slots m
                slotEnv' = extendBlockEnv slotEnv bid slots'
                cafEnv'  = extendBlockEnv cafEnv  bid cafs
            in  tail (upd lives m) (cafEnv', slotEnv') h
          tail lives z (ZHead h m) = tail (upd lives m) z h
          lookupFn map k = expectJust "extendEnvsForSafeFCalls" $ lookupBlockEnv map k
          upd (cafs, slots) m =
            (bt_middle_in cafTransfers m cafs, bt_middle_in liveSlotTransfers m slots)

-- Safe foreign calls: We need to insert the code that suspends and resumes
-- the thread before and after a safe foreign call.
-- Why do we do this so late in the pipeline?
-- Because we need this code to appear without interrruption: you can't rely on the
-- value of the stack pointer between the call and resetting the thread state;
-- you need to have an infotable on the young end of the stack both when
-- suspending the thread and making the foreign call.
-- All of this is much easier if we insert the suspend and resume calls here.

-- At the same time, we prepare for the stages of the compiler that
-- build the proc points. We have to do this at the same time because
-- the safe foreign calls need special treatment with respect to infotables.
-- A safe foreign call needs an infotable even though it isn't
-- a procpoint. The following datatype captures the information
-- needed to generate the infotables along with the Cmm data and procedures.

data CmmTopForInfoTables
  = NoInfoTable       CmmTopZ  -- must be CmmData
  | ProcInfoTable     CmmTopZ BlockSet -- CmmProc; argument is its set of procpoints
  | FloatingInfoTable CmmInfo BlockId UpdFrameOffset
instance Outputable CmmTopForInfoTables where
  ppr (NoInfoTable t) = text "NoInfoTable: " <+> ppr t
  ppr (ProcInfoTable t bids) = text "ProcInfoTable: " <+> ppr t <+> ppr bids
  ppr (FloatingInfoTable info bid upd) =
    text "FloatingInfoTable: " <+> ppr info <+> ppr bid <+> ppr upd

-- The `safeState' record collects the info we update while lowering the
-- safe foreign calls in the graph.
data SafeState = State { s_blocks    :: BlockEnv CmmBlock
                       , s_pps       :: ProcPointSet
                       , s_safeCalls :: [CmmTopForInfoTables]}

lowerSafeForeignCalls
  :: [[CmmTopForInfoTables]] -> CmmTopZ -> FuelMonad [[CmmTopForInfoTables]]
lowerSafeForeignCalls rst t@(CmmData _ _) = return $ [NoInfoTable t] : rst
lowerSafeForeignCalls rst (CmmProc info l args (off, g@(LGraph entry _))) = do
  let init = return $ State emptyBlockEnv emptyBlockSet []
  let block b@(Block bid _) z = do
        state@(State {s_pps = ppset, s_blocks = blocks}) <- z
        let ppset' = if bid == entry then extendBlockSet ppset bid else ppset
            state' = state { s_pps = ppset' }
        if hasSafeForeignCall b
         then lowerSafeCallBlock state' b
         else return (state' { s_blocks = insertBlock b blocks })
  State blocks' g_procpoints safeCalls <- fold_blocks block init g
  let proc = (CmmProc info l args (off, LGraph entry blocks'))
      procTable = case off of
                    (_, Just _) -> [ProcInfoTable proc g_procpoints]
                    _ -> [NoInfoTable proc] -- not a successor of a call
  return $ safeCalls : procTable : rst

-- Check for foreign calls -- if none, then we can avoid copying the block.
hasSafeForeignCall :: CmmBlock -> Bool
hasSafeForeignCall (Block _ t) = tail t
  where tail (ZTail (MidForeignCall (Safe _ _) _ _ _) _) = True
        tail (ZTail _ t) = tail t
        tail (ZLast _)   = False

-- Lower each safe call in the block, update the CAF and slot environments
-- to include each of those calls, and insert the new block in the blockEnv.
lowerSafeCallBlock :: SafeState-> CmmBlock -> FuelMonad SafeState
lowerSafeCallBlock state b = tail (return state) (ZBlock head (ZLast last))
  where (head, last) = goto_end (G.unzip b)
        tail s b@(ZBlock (ZFirst _) _) =
          do state <- s
             return $ state { s_blocks = insertBlock (G.zip b) (s_blocks state) }
        tail s (ZBlock (ZHead h m@(MidForeignCall (Safe bid updfr_off) _ _ _)) t) =
          do state <- s
             let state' = state
                   { s_safeCalls = FloatingInfoTable emptyContInfoTable bid updfr_off :
                                     s_safeCalls state }
             (state'', t') <- lowerSafeForeignCall state' m t
             tail (return state'') (ZBlock h t')
        tail s (ZBlock (ZHead h m) t) = tail s (ZBlock h (ZTail m t))
           

-- Late in the code generator, we want to insert the code necessary
-- to lower a safe foreign call to a sequence of unsafe calls.
lowerSafeForeignCall ::
  SafeState -> Middle -> ZTail Middle Last -> FuelMonad (SafeState, ZTail Middle Last)
lowerSafeForeignCall state m@(MidForeignCall (Safe infotable _) _ _ _) tail = do
    let newTemp rep = getUniqueM >>= \u -> return (LocalReg u rep)
    -- Both 'id' and 'new_base' are KindNonPtr because they're
    -- RTS-only objects and are not subject to garbage collection
    id <- newTemp bWord
    new_base <- newTemp (cmmRegType (CmmGlobal BaseReg))
    let (caller_save, caller_load) = callerSaveVolatileRegs 
    load_tso <- newTemp gcWord -- TODO FIXME NOW
    let suspendThread = CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "suspendThread")))
        resumeThread  = CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "resumeThread")))
        suspend = mkStore (CmmReg spReg) (CmmLit (CmmBlock infotable)) <*>
                  saveThreadState <*>
                  caller_save <*>
                  mkUnsafeCall (ForeignTarget suspendThread
                                  (ForeignConvention CCallConv [AddrHint] [AddrHint]))
                    [id] [CmmReg (CmmGlobal BaseReg)]
        resume = mkUnsafeCall (ForeignTarget resumeThread
                                  (ForeignConvention CCallConv [AddrHint] [AddrHint]))
                    [new_base] [CmmReg (CmmLocal id)] <*>
                 -- Assign the result to BaseReg: we
                 -- might now have a different Capability!
                 mkAssign (CmmGlobal BaseReg) (CmmReg (CmmLocal new_base)) <*>
                 caller_load <*>
                 loadThreadState load_tso
    Graph tail' blocks' <-
      liftUniq (graphOfAGraph (suspend <*> mkMiddle m <*> resume <*> mkZTail tail))
    return (state {s_blocks = s_blocks state `plusBlockEnv` blocks'}, tail')
lowerSafeForeignCall _ _ _ = panic "lowerSafeForeignCall was passed something else"
