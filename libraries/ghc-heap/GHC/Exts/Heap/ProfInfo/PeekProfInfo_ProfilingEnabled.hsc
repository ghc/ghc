{-# LANGUAGE CPP, DeriveGeneric #-}
module GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingEnabled(
    peekStgTSOProfInfo
) where

-- Manually defining PROFILING gives the #peek and #poke macros an accurate
-- representation of the C structures when hsc2hs runs. This is valid because
-- a non-profiling build would use
-- GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingDisabled.
#define PROFILING

#include "Rts.h"
#include "DerivedConstants.h"

import Prelude
import Foreign
import Foreign.C.String
import GHC.Exts.Heap.ProfInfo.Types

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Control.Monad.Trans.State
import Control.Monad.IO.Class

import GHC.Exts.Heap.Ptr.Utils

-- Use Int based containers for pointers (addresses) for better performance.
-- These will be queried a lot!
type AddressSet = IntSet
type AddressMap = IntMap

-- TODO: Remove cache? Looks like it's not needed anymore due to loop breakers
data Cache = Cache {
    ccCache :: AddressMap CostCentre,
    ccsCache :: AddressMap CostCentreStack,
    indexTableCache :: AddressMap IndexTable
}
type DecoderMonad a = StateT Cache IO a

peekStgTSOProfInfo :: Ptr a -> IO (Maybe StgTSOProfInfo)
#if __GLASGOW_HASKELL__ < 811
peekStgTSOProfInfo _ = return Nothing
#else
peekStgTSOProfInfo tsoPtr = do
    print $ "peekStgTSOProfInfo - tsoPtr : " ++ show tsoPtr
    cccs_ptr <- peekByteOff tsoPtr cccsOffset
    cccs' <- evalStateT (peekCostCentreStack IntSet.empty cccs_ptr) $ Cache IntMap.empty IntMap.empty IntMap.empty

    return $ Just StgTSOProfInfo {
        cccs = cccs'
    }

cccsOffset :: Int
cccsOffset = (#const OFFSET_StgTSO_cccs) + (#size StgHeader)

peekCostCentreStack :: AddressSet -> Ptr costCentreStack -> DecoderMonad (Maybe CostCentreStack)
peekCostCentreStack _ ptr | ptr == nullPtr = return Nothing
peekCostCentreStack loopBreakers ptr | IntSet.member (ptrToInt ptr) loopBreakers = return Nothing
peekCostCentreStack loopBreakers ptr = do
    cache <- get
    case IntMap.lookup ptrAsInt (ccsCache cache) of
        found@(Just a) -> do
                            liftIO $ print $ "CCS Cache hit : " ++ show ptr
                            return found
        Nothing -> do
                    liftIO $ print $ "peekCostCentreStack - ptr : " ++ show ptr
                    ccs_ccsID' <- liftIO $ (#peek struct CostCentreStack_, ccsID) ptr
                    ccs_cc_ptr <- liftIO $ (#peek struct CostCentreStack_, cc) ptr
                    ccs_cc' <- peekCostCentre ccs_cc_ptr
                    ccs_prevStack_ptr <- liftIO $ (#peek struct CostCentreStack_, prevStack) ptr
                    let loopBreakers' = (IntSet.insert ptrAsInt loopBreakers)
                    ccs_prevStack' <- peekCostCentreStack loopBreakers' ccs_prevStack_ptr
                    ccs_indexTable_ptr <- liftIO $ (#peek struct CostCentreStack_, indexTable) ptr
                    ccs_indexTable' <- peekIndexTable loopBreakers' ccs_indexTable_ptr
                    ccs_root_ptr <- liftIO $ (#peek struct CostCentreStack_, root) ptr
                    ccs_root' <- peekCostCentreStack loopBreakers' ccs_root_ptr
                    ccs_depth' <- liftIO $ (#peek struct CostCentreStack_, depth) ptr
                    ccs_scc_count' <- liftIO $ (#peek struct CostCentreStack_, scc_count) ptr
                    ccs_selected' <- liftIO $ (#peek struct CostCentreStack_, selected) ptr
                    ccs_time_ticks' <- liftIO $ (#peek struct CostCentreStack_, time_ticks) ptr
                    ccs_mem_alloc' <- liftIO $ (#peek struct CostCentreStack_, mem_alloc) ptr
                    ccs_inherited_alloc' <- liftIO $ (#peek struct CostCentreStack_, inherited_alloc) ptr
                    ccs_inherited_ticks' <- liftIO $ (#peek struct CostCentreStack_, inherited_ticks) ptr

                    let result = CostCentreStack {
                        ccs_ccsID = ccs_ccsID',
                        ccs_cc = ccs_cc',
                        ccs_prevStack = ccs_prevStack',
                        ccs_indexTable = ccs_indexTable',
                        ccs_root = ccs_root',
                        ccs_depth = ccs_depth',
                        ccs_scc_count = ccs_scc_count',
                        ccs_selected = ccs_selected',
                        ccs_time_ticks = ccs_time_ticks',
                        ccs_mem_alloc = ccs_mem_alloc',
                        ccs_inherited_alloc = ccs_inherited_alloc',
                        ccs_inherited_ticks = ccs_inherited_ticks'
                    }

                    let updatedCCSCache = IntMap.insert ptrAsInt result (ccsCache cache)
                    put $ cache { ccsCache = updatedCCSCache }

                    return $ Just result
    where
        ptrAsInt = ptrToInt ptr

peekCostCentre :: Ptr costCentre -> DecoderMonad CostCentre
peekCostCentre ptr = do
    cache <- get
    let ptrAsInt = ptrToInt ptr
    if IntMap.member ptrAsInt (ccCache cache) then do
        liftIO $ print $ "CC Cache hit : " ++ show ptr
        return $ (ccCache cache) IntMap.! ptrAsInt
    else do
        cc_ccID' <- liftIO $ (#peek struct CostCentre_, ccID) ptr
        cc_label_ptr <- liftIO $ (#peek struct CostCentre_, label) ptr
        cc_label' <- liftIO $ peekCString cc_label_ptr
        cc_module_ptr <- liftIO $ (#peek struct CostCentre_, module) ptr
        cc_module' <- liftIO $ peekCString cc_module_ptr
        cc_srcloc_ptr <- liftIO $ (#peek struct CostCentre_, srcloc) ptr
        cc_srcloc' <- liftIO $ do
            if cc_srcloc_ptr == nullPtr then
                return Nothing
            else
                fmap Just (peekCString cc_srcloc_ptr)
        cc_mem_alloc' <- liftIO $ (#peek struct CostCentre_, mem_alloc) ptr
        cc_time_ticks' <- liftIO $ (#peek struct CostCentre_, time_ticks) ptr
        cc_is_caf' <- liftIO $ (#peek struct CostCentre_, is_caf) ptr
        cc_link_ptr <- liftIO $ (#peek struct CostCentre_, link) ptr
        cc_link' <- if cc_link_ptr == nullPtr then
            return Nothing
        else
            fmap Just (peekCostCentre cc_link_ptr)

        let result = CostCentre {
            cc_ccID = cc_ccID',
            cc_label = cc_label',
            cc_module = cc_module',
            cc_srcloc = cc_srcloc',
            cc_mem_alloc = cc_mem_alloc',
            cc_time_ticks = cc_time_ticks',
            cc_is_caf = cc_is_caf',
            cc_link = cc_link'
        }

        let updatedCCCache = IntMap.insert ptrAsInt result (ccCache cache)
        put $ cache { ccCache = updatedCCCache }

        return result

peekIndexTable :: AddressSet -> Ptr indexTable -> DecoderMonad (Maybe IndexTable)
peekIndexTable _ ptr | ptr == nullPtr = return Nothing
peekIndexTable loopBreakers ptr = do
    cache <- get
    let ptrAsInt = ptrToInt ptr
    if IntMap.member ptrAsInt (indexTableCache cache) then do
        liftIO $ print $ "IndexTable Cache hit : " ++ show ptr
        return $ Just $ (indexTableCache cache) IntMap.! ptrAsInt
    else do
        it_cc_ptr <- liftIO $ (#peek struct IndexTable_, cc) ptr
        it_cc' <- peekCostCentre it_cc_ptr
        it_ccs_ptr <- liftIO $ (#peek struct IndexTable_, ccs) ptr
        it_ccs' <- peekCostCentreStack loopBreakers it_ccs_ptr
        it_next_ptr <- liftIO $ (#peek struct IndexTable_, next) ptr
        it_next' <- peekIndexTable loopBreakers it_next_ptr
        it_back_edge' <- liftIO $ (#peek struct IndexTable_, back_edge) ptr

        let result = IndexTable {
            it_cc = it_cc',
            it_ccs = it_ccs',
            it_next = it_next',
            it_back_edge = it_back_edge'
        }

        let updatedIndexTableCache = IntMap.insert ptrAsInt result (indexTableCache cache)
        put $ cache { indexTableCache = updatedIndexTableCache }

        return $ Just result
#endif
