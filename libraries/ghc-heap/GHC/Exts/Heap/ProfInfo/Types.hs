{-# LANGUAGE DeriveGeneric #-}

module GHC.Exts.Heap.ProfInfo.Types where

import Prelude
import Data.Word
import GHC.Generics

data StgTSOProfInfo = StgTSOProfInfo {
    cccs :: Maybe CostCentreStack
} deriving (Show, Generic)

data CostCentreStack = CostCentreStack {
    ccs_ccsID :: Int,
    ccs_cc :: CostCentre,
    ccs_prevStack :: Maybe CostCentreStack,
    ccs_indexTable :: Maybe IndexTable,
    ccs_root :: Maybe CostCentreStack,
    ccs_depth :: Word,
    ccs_scc_count :: Word64,
    ccs_selected :: Word,
    ccs_time_ticks :: Word,
    ccs_mem_alloc :: Word64,
    ccs_inherited_alloc :: Word64,
    ccs_inherited_ticks :: Word
} deriving (Show, Generic, Eq)

data CostCentre = CostCentre {
    cc_ccID :: Int,
    cc_label :: String,
    cc_module :: String,
    cc_srcloc :: Maybe String,
    cc_mem_alloc :: Word64,
    cc_time_ticks :: Word,
    cc_is_caf :: Bool,
    cc_link :: Maybe CostCentre
} deriving (Show, Generic, Eq)

data IndexTable = IndexTable {
    it_cc :: CostCentre,
    it_ccs :: Maybe CostCentreStack,
    it_next :: Maybe IndexTable,
    it_back_edge :: Bool
} deriving (Show, Generic, Eq)
