{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}

module GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingEnabled(
    peekStgTSOProfInfo
    , peekTopCCS
) where

#if __GLASGOW_HASKELL__ >= 811

-- See [hsc and CPP workaround]

#define PROFILING

#include "Rts.h"
#undef BLOCK_SIZE
#undef MBLOCK_SIZE
#undef BLOCKS_PER_MBLOCK
#include "DerivedConstants.h"

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Foreign
import           Foreign.C.String
import           GHC.Exts
import           GHC.Exts.Heap.ProfInfo.Types
import           Prelude

-- Use Int based containers for pointers (addresses) for better performance.
-- These will be queried a lot!
type AddressSet = IntSet
type AddressMap = IntMap

peekStgTSOProfInfo :: (Ptr b -> IO (Maybe CostCentreStack)) -> Ptr a -> IO (Maybe StgTSOProfInfo)
peekStgTSOProfInfo decodeCCS tsoPtr = do
    cccs_ptr <- peekByteOff tsoPtr cccsOffset
    cccs' <- decodeCCS cccs_ptr

    return $ Just StgTSOProfInfo {
        cccs = cccs'
    }

peekTopCCS :: Ptr b -> IO (Maybe CostCentreStack)
peekTopCCS cccs_ptr = do
  costCenterCacheRef <- newIORef IntMap.empty
  peekCostCentreStack IntSet.empty costCenterCacheRef cccs_ptr

cccsOffset :: Int
cccsOffset = (#const OFFSET_StgTSO_cccs) + (#size StgHeader)

peekCostCentreStack
    :: AddressSet
    -> IORef (AddressMap CostCentre)
    -> Ptr costCentreStack
    -> IO (Maybe CostCentreStack)
peekCostCentreStack _ _ ptr | ptr == nullPtr = return Nothing
peekCostCentreStack loopBreakers _ ptr | IntSet.member (ptrToInt ptr) loopBreakers = return Nothing
peekCostCentreStack loopBreakers costCenterCacheRef ptr = do
        ccs_ccsID' <- (#peek struct CostCentreStack_, ccsID) ptr
        ccs_cc_ptr <- (#peek struct CostCentreStack_, cc) ptr
        ccs_cc' <- peekCostCentre costCenterCacheRef ccs_cc_ptr
        ccs_prevStack_ptr <- (#peek struct CostCentreStack_, prevStack) ptr
        let loopBreakers' = (IntSet.insert ptrAsInt loopBreakers)
        ccs_prevStack' <- peekCostCentreStack loopBreakers' costCenterCacheRef ccs_prevStack_ptr
        ccs_indexTable_ptr <- (#peek struct CostCentreStack_, indexTable) ptr
        ccs_indexTable' <- peekIndexTable loopBreakers' costCenterCacheRef ccs_indexTable_ptr
        ccs_root_ptr <- (#peek struct CostCentreStack_, root) ptr
        ccs_root' <- peekCostCentreStack loopBreakers' costCenterCacheRef ccs_root_ptr
        ccs_depth' <- (#peek struct CostCentreStack_, depth) ptr
        ccs_scc_count' <- (#peek struct CostCentreStack_, scc_count) ptr
        ccs_selected' <- (#peek struct CostCentreStack_, selected) ptr
        ccs_time_ticks' <- (#peek struct CostCentreStack_, time_ticks) ptr
        ccs_mem_alloc' <- (#peek struct CostCentreStack_, mem_alloc) ptr
        ccs_inherited_alloc' <- (#peek struct CostCentreStack_, inherited_alloc) ptr
        ccs_inherited_ticks' <- (#peek struct CostCentreStack_, inherited_ticks) ptr

        return $ Just CostCentreStack {
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
    where
        ptrAsInt = ptrToInt ptr

peekCostCentre :: IORef (AddressMap CostCentre) -> Ptr costCentre -> IO CostCentre
peekCostCentre costCenterCacheRef ptr = do
    costCenterCache <- readIORef costCenterCacheRef
    case IntMap.lookup ptrAsInt costCenterCache of
        (Just a) -> return a
        Nothing -> do
                    cc_ccID' <- (#peek struct CostCentre_, ccID) ptr
                    cc_label_ptr <- (#peek struct CostCentre_, label) ptr
                    cc_label' <- peekCString cc_label_ptr
                    cc_module_ptr <- (#peek struct CostCentre_, module) ptr
                    cc_module' <- peekCString cc_module_ptr
                    cc_srcloc_ptr <- (#peek struct CostCentre_, srcloc) ptr
                    cc_srcloc' <- do
                        if cc_srcloc_ptr == nullPtr then
                            return Nothing
                        else
                            fmap Just (peekCString cc_srcloc_ptr)
                    cc_mem_alloc' <- (#peek struct CostCentre_, mem_alloc) ptr
                    cc_time_ticks' <- (#peek struct CostCentre_, time_ticks) ptr
                    cc_is_caf' <- (#peek struct CostCentre_, is_caf) ptr
                    cc_link_ptr <- (#peek struct CostCentre_, link) ptr
                    cc_link' <- if cc_link_ptr == nullPtr then
                        return Nothing
                    else
                        fmap Just (peekCostCentre costCenterCacheRef cc_link_ptr)

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

                    writeIORef costCenterCacheRef (IntMap.insert ptrAsInt result costCenterCache)

                    return result
    where
        ptrAsInt = ptrToInt ptr

peekIndexTable :: AddressSet -> IORef (AddressMap CostCentre) -> Ptr indexTable -> IO (Maybe IndexTable)
peekIndexTable _ _ ptr | ptr == nullPtr = return Nothing
peekIndexTable loopBreakers costCenterCacheRef ptr = do
        it_cc_ptr <- (#peek struct IndexTable_, cc) ptr
        it_cc' <- peekCostCentre costCenterCacheRef it_cc_ptr
        it_ccs_ptr <- (#peek struct IndexTable_, ccs) ptr
        it_ccs' <- peekCostCentreStack loopBreakers costCenterCacheRef it_ccs_ptr
        it_next_ptr <- (#peek struct IndexTable_, next) ptr
        it_next' <- peekIndexTable loopBreakers costCenterCacheRef it_next_ptr
        it_back_edge' <- (#peek struct IndexTable_, back_edge) ptr

        return $ Just IndexTable {
            it_cc = it_cc',
            it_ccs = it_ccs',
            it_next = it_next',
            it_back_edge = it_back_edge'
        }

-- | casts a @Ptr@ to an @Int@
ptrToInt :: Ptr a -> Int
ptrToInt (Ptr a##) = I## (addr2Int## a##)

#else
import Prelude
import Foreign

import GHC.Exts.Heap.ProfInfo.Types

peekStgTSOProfInfo :: (Ptr b -> IO (Maybe CostCentreStack)) -> Ptr a -> IO (Maybe StgTSOProfInfo)
peekStgTSOProfInfo _ _ = return Nothing

peekTopCCS :: Ptr a -> IO (Maybe CostCentreStack)
peekTopCCS _ = return Nothing
#endif
