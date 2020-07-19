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

peekStgTSOProfInfo :: Ptr a -> IO (Maybe StgTSOProfInfo)
#if __GLASGOW_HASKELL__ < 811
peekStgTSOProfInfo _ = return Nothing
#else
peekStgTSOProfInfo tsoPtr = do
-- TODO: Use getCurrentCCS# ? Or GHC.Stack.CCS.getCurrentCCS ?
    print $ "peekStgTSOProfInfo - tsoPtr : " ++ show tsoPtr
    cccs_ptr <- peekByteOff tsoPtr cccsOffset
    cccs' <- peekCostCentreStack cccs_ptr

    return $ Just StgTSOProfInfo {
        cccs = cccs'
    }

cccsOffset :: Int
cccsOffset = (#const OFFSET_StgTSO_cccs) + (#size StgHeader)

peekCostCentreStack :: Ptr a -> IO CostCentreStack
peekCostCentreStack ptr = do
    print $ "peekCostCentreStack - ptr : " ++ show ptr
    ccs_ccsID' <- (#peek struct CostCentreStack_, ccsID) ptr
    ccs_cc_ptr <- (#peek struct CostCentreStack_, cc) ptr
    print ccs_cc_ptr
    ccs_cc' <- peekCostCentre ccs_cc_ptr
    ccs_prevStack_ptr <- (#peek struct CostCentreStack_, prevStack) ptr
    print ccs_prevStack_ptr
    -- TODO: Extract function for this guard pattern
    ccs_prevStack' <- if ccs_prevStack_ptr == nullPtr then
            return Nothing
        else
            fmap Just (peekCostCentreStack ccs_prevStack_ptr)
    ccs_indexTable_ptr <- (#peek struct CostCentreStack_, indexTable) ptr
    ccs_indexTable' <- if ccs_indexTable_ptr == nullPtr then
            return Nothing
        else
            fmap Just (peekIndexTable ccs_indexTable_ptr)
    ccs_root_ptr <- (#peek struct CostCentreStack_, root) ptr
    ccs_root' <- peekCostCentreStack ccs_root_ptr
    ccs_depth' <- (#peek struct CostCentreStack_, depth) ptr
    ccs_scc_count' <- (#peek struct CostCentreStack_, scc_count) ptr
    ccs_selected' <- (#peek struct CostCentreStack_, selected) ptr
    ccs_time_ticks' <- (#peek struct CostCentreStack_, time_ticks) ptr
    ccs_mem_alloc' <- (#peek struct CostCentreStack_, mem_alloc) ptr
    ccs_inherited_alloc' <- (#peek struct CostCentreStack_, inherited_alloc) ptr
    ccs_inherited_ticks' <- (#peek struct CostCentreStack_, inherited_ticks) ptr

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

    print $ show "CostCentreStack : " ++ show result

    return result

peekCostCentre :: Ptr a -> IO CostCentre
peekCostCentre ptr = do
    print "a"
    cc_ccID' <- (#peek struct CostCentre_, ccID) ptr
    print cc_ccID'
    print "b"
    cc_label_ptr <- (#peek struct CostCentre_, label) ptr
    print "c"
    cc_label' <- peekCString cc_label_ptr
    print "d"
    cc_module_ptr <- (#peek struct CostCentre_, module) ptr
    print "e"
    cc_module' <- peekCString cc_module_ptr
    print "f"
    print ptr
    cc_srcloc_ptr <- (#peek struct CostCentre_, srcloc) ptr
    print "g"
    print $ "cc_srcloc_ptr : " ++ show cc_srcloc_ptr
    cc_srcloc' <- if cc_srcloc_ptr == nullPtr then
            return Nothing
        else
            fmap Just (peekCString cc_srcloc_ptr)
    print "h"
    cc_mem_alloc' <- (#peek struct CostCentre_, mem_alloc) ptr
    print "i"
    cc_time_ticks' <- (#peek struct CostCentre_, time_ticks) ptr
    print "j"
    cc_is_caf' <- (#peek struct CostCentre_, is_caf) ptr
    print "k"
    cc_link_ptr <- (#peek struct CostCentre_, link) ptr
    print $ "cc_link_ptr : " ++ show cc_link_ptr
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

    print $ show "CostCentre : " ++ show result

    return result

peekIndexTable :: Ptr a -> IO IndexTable
peekIndexTable ptr = do
    it_cc_ptr <- (#peek struct IndexTable_, cc) ptr
    it_cc' <- peekCostCentre it_cc_ptr
    it_ccs_ptr <- (#peek struct IndexTable_, ccs) ptr
    it_ccs' <- peekCostCentreStack it_ccs_ptr
    it_next_ptr <- (#peek struct IndexTable_, next) ptr
    it_next' <- peekIndexTable it_next_ptr
    it_back_edge' <- (#peek struct IndexTable_, back_edge) ptr

    let result = IndexTable {
        it_cc = it_cc',
        it_ccs = it_ccs',
        it_next = it_next',
        it_back_edge = it_back_edge'
    }

    print $ show "IndexTable : " ++ show result

    return result
#endif
