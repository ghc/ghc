{-# LANGUAGE DeriveGeneric #-}

module GHC.Exts.Heap.ProfInfo.Types where

import Prelude
import Data.Word
import GHC.Generics

-- | This is a somewhat faithful representation of StgTSOProfInfo. See
-- <https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/rts/storage/TSO.h>
-- for more details on this data structure.
newtype StgTSOProfInfo = StgTSOProfInfo {
    cccs :: Maybe CostCentreStack
} deriving (Show, Generic, Eq, Ord)

-- | This is a somewhat faithful representation of CostCentreStack. See
-- <https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/rts/prof/CCS.h>
-- for more details on this data structure.
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
} deriving (Show, Generic, Eq, Ord)

-- | This is a somewhat faithful representation of CostCentre. See
-- <https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/rts/prof/CCS.h>
-- for more details on this data structure.
data CostCentre = CostCentre {
    cc_ccID :: Int,
    cc_label :: String,
    cc_module :: String,
    cc_srcloc :: Maybe String,
    cc_mem_alloc :: Word64,
    cc_time_ticks :: Word,
    cc_is_caf :: Bool,
    cc_link :: Maybe CostCentre
} deriving (Show, Generic, Eq, Ord)

-- | This is a somewhat faithful representation of IndexTable. See
-- <https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/rts/prof/CCS.h>
-- for more details on this data structure.
data IndexTable = IndexTable {
    it_cc :: CostCentre,
    it_ccs :: Maybe CostCentreStack,
    it_next :: Maybe IndexTable,
    it_back_edge :: Bool
} deriving (Show, Generic, Eq, Ord)
