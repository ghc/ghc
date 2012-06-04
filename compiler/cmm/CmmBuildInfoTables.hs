{-# LANGUAGE GADTs, NoMonoLocalBinds #-}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- Norman likes local bindings
-- If this module lives on I'd like to get rid of the NoMonoLocalBinds
-- extension in due course

-- Todo: remove -fno-warn-warnings-deprecations
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module CmmBuildInfoTables
    ( CAFSet, CAFEnv, cafAnal, localCAFInfo, mkTopCAFInfo
    , setInfoTableSRT, setInfoTableStackMap
    , TopSRT, emptySRT, srtToData
    , bundleCAFs
    , lowerSafeForeignCalls
    , cafTransfers, liveSlotTransfers
    , mkLiveness )
where

#include "HsVersions.h"

-- These should not be imported here!
import StgCmmForeign
import StgCmmUtils

import Constants
import Digraph
import qualified Prelude as P
import Prelude hiding (succ)
import Util

import BlockId
import Bitmap
import CLabel
import Cmm
import CmmUtils
import CmmStackLayout
import Module
import FastString
import ForeignCall
import IdInfo
import Data.List
import Maybes
import MkGraph as M
import Control.Monad
import Name
import OptimizationFuel
import Outputable
import Platform
import SMRep
import UniqSupply

import Compiler.Hoopl

import Data.Map (Map)
import qualified Data.Map as Map
import qualified FiniteMap as Map

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

live_ptrs :: ByteOff -> BlockEnv SubAreaSet -> AreaMap -> BlockId -> StackLayout
live_ptrs oldByte slotEnv areaMap bid =
  -- pprTrace "live_ptrs for" (ppr bid <+> text (show oldByte ++ "-" ++ show youngByte) <+>
  --                           ppr liveSlots) $
  -- pprTrace ("stack layout for " ++ show bid ++ ": ") (ppr res) $ res
  res
  where 
        res = mkLiveness (reverse $ slotsToList youngByte liveSlots [])
 
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
                           (Map.foldRightWithKey (\_ -> flip $ foldl add_slot) [] slots)
                    
        add_slot :: [RegSlotInfo] -> SubArea -> [RegSlotInfo]
        add_slot rst (a@(RegSlot r@(LocalReg _ ty)), off, w) = 
          if off == w && widthInBytes (typeWidth ty) == w then
            (expectJust "add_slot" (Map.lookup a areaMap), r, w) : rst
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
        slots = expectJust "live_ptrs slots" $ mapLookup bid slotEnv
        youngByte = expectJust "live_ptrs bid_pos" $ Map.lookup (CallArea (Young bid)) areaMap

-- Construct the stack maps for a procedure _if_ it needs an infotable.
-- When wouldn't a procedure need an infotable? If it is a procpoint that
-- is not the successor of a call.
setInfoTableStackMap :: SlotEnv -> AreaMap -> CmmDecl -> CmmDecl
setInfoTableStackMap slotEnv areaMap
     t@(CmmProc (TopInfo {stack_info=StackInfo {updfr_space = Just updfr_off}}) _ 
                (CmmGraph {g_entry = eid}))
  = updInfo (const (live_ptrs updfr_off slotEnv areaMap eid)) id t
setInfoTableStackMap _ _ t = t
                 


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

type CAFSet = Map CLabel ()
type CAFEnv = BlockEnv CAFSet

-- First, an analysis to find live CAFs.
cafLattice :: DataflowLattice CAFSet
cafLattice = DataflowLattice "live cafs" Map.empty add
  where add _ (OldFact old) (NewFact new) = case old `Map.union` new of
                                              new' -> (changeIf $ Map.size new' > Map.size old, new')

cafTransfers :: Platform -> BwdTransfer CmmNode CAFSet
cafTransfers platform = mkBTransfer3 first middle last
  where first  _ live = live
        middle m live = foldExpDeep addCaf m live
        last   l live = foldExpDeep addCaf l (joinOutFacts cafLattice l live)
        addCaf e set = case e of
               CmmLit (CmmLabel c)              -> add c set
               CmmLit (CmmLabelOff c _)         -> add c set
               CmmLit (CmmLabelDiffOff c1 c2 _) -> add c1 $ add c2 set
               _ -> set
        add l s = if hasCAF l then Map.insert (toClosureLbl platform l) () s
                              else s

cafAnal :: Platform -> CmmGraph -> FuelUniqSM CAFEnv
cafAnal platform g
    = liftM snd $ dataflowPassBwd g [] $ analBwd cafLattice (cafTransfers platform)

-----------------------------------------------------------------------
-- Building the SRTs

-- Description of the SRT for a given module.
-- Note that this SRT may grow as we greedily add new CAFs to it.
data TopSRT = TopSRT { lbl      :: CLabel
                     , next_elt :: Int -- the next entry in the table
                     , rev_elts :: [CLabel]
                     , elt_map  :: Map CLabel Int }
                        -- map: CLabel -> its last entry in the table
instance PlatformOutputable TopSRT where
  pprPlatform platform (TopSRT lbl next elts eltmap) =
    text "TopSRT:" <+> pprPlatform platform lbl
                   <+> ppr next
                   <+> pprPlatform platform elts
                   <+> pprPlatform platform eltmap

emptySRT :: MonadUnique m => m TopSRT
emptySRT =
  do top_lbl <- getUniqueM >>= \ u -> return $ mkSRTLabel (mkFCallName u "srt") NoCafRefs
     return TopSRT { lbl = top_lbl, next_elt = 0, rev_elts = [], elt_map = Map.empty }

cafMember :: TopSRT -> CLabel -> Bool
cafMember srt lbl = Map.member lbl (elt_map srt)

cafOffset :: TopSRT -> CLabel -> Maybe Int
cafOffset srt lbl = Map.lookup lbl (elt_map srt)

addCAF :: CLabel -> TopSRT -> TopSRT
addCAF caf srt =
  srt { next_elt = last + 1
      , rev_elts = caf : rev_elts srt
      , elt_map  = Map.insert caf last (elt_map srt) }
    where last  = next_elt srt

srtToData :: TopSRT -> CmmGroup
srtToData srt = [CmmData RelocatableReadOnlyData (Statics (lbl srt) tbl)]
    where tbl = map (CmmStaticLit . CmmLabel) (reverse (rev_elts srt))

-- Once we have found the CAFs, we need to do two things:
-- 1. Build a table of all the CAFs used in the procedure.
-- 2. Compute the C_SRT describing the subset of CAFs live at each procpoint.
--
-- When building the local view of the SRT, we first make sure that all the CAFs are 
-- in the SRT. Then, if the number of CAFs is small enough to fit in a bitmap,
-- we make sure they're all close enough to the bottom of the table that the
-- bitmap will be able to cover all of them.
buildSRTs :: TopSRT -> Map CLabel CAFSet -> CAFSet ->
             FuelUniqSM (TopSRT, Maybe CmmDecl, C_SRT)
buildSRTs topSRT topCAFMap cafs =
  do let liftCAF lbl () z = -- get CAFs for functions without static closures
           case Map.lookup lbl topCAFMap of Just cafs -> z `Map.union` cafs
                                            Nothing   -> Map.insert lbl () z
         -- For each label referring to a function f without a static closure,
         -- replace it with the CAFs that are reachable from f.
         sub_srt topSRT localCafs =
           let cafs = Map.keys (Map.foldRightWithKey liftCAF Map.empty localCafs)
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
               farthestFst x y = case (Map.lookup x m, Map.lookup y m) of
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
procpointSRT :: CLabel -> Map CLabel Int -> [CLabel] ->
                FuelUniqSM (Maybe CmmDecl, C_SRT)
procpointSRT _ _ [] =
 return (Nothing, NoC_SRT)
procpointSRT top_srt top_table entries =
 do (top, srt) <- bitmap `seq` to_SRT top_srt offset len bitmap
    return (top, srt)
  where
    ints = map (expectJust "constructSRT" . flip Map.lookup top_table) entries
    sorted_ints = sortLe (<=) ints
    offset = head sorted_ints
    bitmap_entries = map (subtract offset) sorted_ints
    len = P.last bitmap_entries + 1
    bitmap = intsToBitmap len bitmap_entries

maxBmpSize :: Int
maxBmpSize = widthInBits wordWidth `div` 2

-- Adapted from codeGen/StgCmmUtils, which converts from SRT to C_SRT.
to_SRT :: CLabel -> Int -> Int -> Bitmap -> FuelUniqSM (Maybe CmmDecl, C_SRT)
to_SRT top_srt off len bmp
  | len > maxBmpSize || bmp == [fromIntegral srt_escape]
  = do id <- getUniqueM
       let srt_desc_lbl = mkLargeSRTLabel id
           tbl = CmmData RelocatableReadOnlyData $
                   Statics srt_desc_lbl $ map CmmStaticLit
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
localCAFInfo :: Platform -> CAFEnv -> CmmDecl -> Maybe (CLabel, CAFSet)
localCAFInfo _        _      (CmmData _ _) = Nothing
localCAFInfo platform cafEnv (CmmProc top_info top_l (CmmGraph {g_entry=entry})) =
  case info_tbl top_info of
    CmmInfoTable { cit_rep = rep } 
      | not (isStaticRep rep) 
      -> Just (toClosureLbl platform top_l,
               expectJust "maybeBindCAFs" $ mapLookup entry cafEnv)
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
mkTopCAFInfo :: [(CLabel, CAFSet)] -> Map CLabel CAFSet
mkTopCAFInfo localCAFs = foldl addToTop Map.empty g
  where addToTop env (AcyclicSCC (l, cafset)) =
          Map.insert l (flatten env cafset) env
        addToTop env (CyclicSCC nodes) =
          let (lbls, cafsets) = unzip nodes
              cafset  = lbls `Map.deleteList` foldl Map.union Map.empty cafsets
          in foldl (\env l -> Map.insert l (flatten env cafset) env) env lbls
        flatten env cafset = Map.foldRightWithKey (lookup env) Map.empty cafset
        lookup env caf () cafset' =
          case Map.lookup caf env of Just cafs -> Map.foldRightWithKey add cafset' cafs
                                     Nothing -> add caf () cafset'
        add caf () cafset' = Map.insert caf () cafset'
        g = stronglyConnCompFromEdgedVertices
              (map (\n@(l, cafs) -> (n, l, Map.keys cafs)) localCAFs)

-- Bundle the CAFs used at a procpoint.
bundleCAFs :: CAFEnv -> CmmDecl -> (CAFSet, CmmDecl)
bundleCAFs cafEnv t@(CmmProc _ _ (CmmGraph {g_entry=entry})) =
  (expectJust "bundleCAFs" (mapLookup entry cafEnv), t)
bundleCAFs _ t = (Map.empty, t)

-- Construct the SRTs for the given procedure.
setInfoTableSRT :: Map CLabel CAFSet -> TopSRT -> (CAFSet, CmmDecl) ->
                   FuelUniqSM (TopSRT, [CmmDecl])
setInfoTableSRT topCAFMap topSRT (cafs, t) =
  setSRT cafs topCAFMap topSRT t

setSRT :: CAFSet -> Map CLabel CAFSet -> TopSRT ->
          CmmDecl -> FuelUniqSM (TopSRT, [CmmDecl])
setSRT cafs topCAFMap topSRT t =
  do (topSRT, cafTable, srt) <- buildSRTs topSRT topCAFMap cafs
     let t' = updInfo id (const srt) t
     case cafTable of
       Just tbl -> return (topSRT, [t', tbl])
       Nothing  -> return (topSRT, [t'])

type StackLayout = Liveness

updInfo :: (StackLayout -> StackLayout) -> (C_SRT -> C_SRT) -> CmmDecl -> CmmDecl
updInfo toVars toSrt (CmmProc top_info top_l g) =
  CmmProc (top_info {info_tbl=updInfoTbl toVars toSrt (info_tbl top_info)}) top_l g
updInfo _ _ t = t

updInfoTbl :: (StackLayout -> StackLayout) -> (C_SRT -> C_SRT) -> CmmInfoTable -> CmmInfoTable
updInfoTbl toVars toSrt info_tbl@(CmmInfoTable {})
  = info_tbl { cit_srt = toSrt (cit_srt info_tbl)
             , cit_rep = case cit_rep info_tbl of
                           StackRep ls -> StackRep (toVars ls)
                           other       -> other }
updInfoTbl _ _ t@CmmNonInfoTable = t
  
----------------------------------------------------------------
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

-- JD: Why not do this while splitting procedures?
lowerSafeForeignCalls :: AreaMap -> CmmDecl -> FuelUniqSM CmmDecl
lowerSafeForeignCalls _ t@(CmmData _ _) = return t
lowerSafeForeignCalls areaMap (CmmProc info l g@(CmmGraph {g_entry=entry})) = do
  let block b mblocks = mblocks >>= lowerSafeCallBlock entry areaMap b
  blocks <- foldGraphBlocks block (return mapEmpty) g
  return $ CmmProc info l (ofBlockMap entry blocks)

-- If the block ends with a safe call in the block, lower it to an unsafe
-- call (with appropriate saves and restores before and after).
lowerSafeCallBlock :: BlockId -> AreaMap -> CmmBlock -> BlockEnv CmmBlock
                              -> FuelUniqSM (BlockEnv CmmBlock)
lowerSafeCallBlock entry areaMap b blocks =
  case blockToNodeList b of
    (JustC (CmmEntry id), m, JustC l@(CmmForeignCall {})) -> lowerSafeForeignCall entry areaMap blocks id m l
    _                                                    -> return $ insertBlock b blocks

-- Late in the code generator, we want to insert the code necessary
-- to lower a safe foreign call to a sequence of unsafe calls.
lowerSafeForeignCall :: BlockId -> AreaMap -> BlockEnv CmmBlock -> BlockId -> [CmmNode O O] -> CmmNode O C
                                -> FuelUniqSM (BlockEnv CmmBlock)
lowerSafeForeignCall entry areaMap blocks bid m
    (CmmForeignCall {tgt=tgt, res=rs, args=as, succ=succ, updfr = updfr_off, intrbl = intrbl}) =
 do let newTemp rep = getUniqueM >>= \u -> return (LocalReg u rep)
    -- Both 'id' and 'new_base' are KindNonPtr because they're
    -- RTS-only objects and are not subject to garbage collection
    id <- newTemp bWord
    new_base <- newTemp (cmmRegType (CmmGlobal BaseReg))
    let (caller_save, caller_load) = callerSaveVolatileRegs
    load_tso <- newTemp gcWord -- TODO FIXME NOW
    load_stack <- newTemp gcWord -- TODO FIXME NOW
    let (<**>) = (M.<*>)
    let suspendThread = foreignLbl "suspendThread"
        resumeThread  = foreignLbl "resumeThread"
        foreignLbl name = CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit name)))
        suspend = saveThreadState <**>
                  caller_save <**>
                  mkUnsafeCall (ForeignTarget suspendThread
                                (ForeignConvention CCallConv [AddrHint, NoHint] [AddrHint]))
                               [id] [CmmReg (CmmGlobal BaseReg), CmmLit (CmmInt (fromIntegral (fromEnum intrbl)) wordWidth)]
        midCall = mkUnsafeCall tgt rs as
        resume  = mkUnsafeCall (ForeignTarget resumeThread
                                (ForeignConvention CCallConv [AddrHint] [AddrHint]))
                     [new_base] [CmmReg (CmmLocal id)] <**>
                  -- Assign the result to BaseReg: we
                  -- might now have a different Capability!
                  mkAssign (CmmGlobal BaseReg) (CmmReg (CmmLocal new_base)) <**>
                  caller_load <**>
                  loadThreadState load_tso load_stack
        -- We have to save the return value on the stack because its next use
        -- may appear in a different procedure due to procpoint splitting...
        saveRetVals = foldl (<**>) emptyAGraph $ map (M.mkMiddle . spill) rs
        spill r = CmmStore (regSlot r) (CmmReg $ CmmLocal r)
        regSlot r@(LocalReg _ _) = CmmRegOff (CmmGlobal Sp) (sp_off - offset)
          where offset = w + expectJust "lowerForeign" (Map.lookup (RegSlot r) areaMap)
                sp_off = wORD_SIZE + expectJust "lowerForeign" (Map.lookup (CallArea area) areaMap)
                area = if succ == entry then Old else Young succ
                w = widthInBytes $ typeWidth $ localRegType r
        -- Note: The successor must be a procpoint, and we have already split,
        --       so we use a jump, not a branch.
        succLbl = CmmLit (CmmLabel (infoTblLbl succ))
        jump = CmmCall { cml_target  = succLbl, cml_cont = Nothing
                       , cml_args    = widthInBytes wordWidth ,cml_ret_args = 0
                       , cml_ret_off = updfr_off}
    graph' <- liftUniq $ labelAGraph bid $ catAGraphs (map M.mkMiddle m) <**>
                                           suspend <**> midCall <**>
                                           resume  <**> saveRetVals <**> M.mkLast jump
    return $ blocks `mapUnion` toBlockMap graph'
lowerSafeForeignCall _ _ _ _ _ _ = panic "lowerSafeForeignCall was passed something else"

