{-# LANGUAGE ForeignFunctionInterface, MagicHash, CPP, BangPatterns #-}

import Prelude
import Foreign
import Foreign.C.Types
import GHC.Exts.Heap
import GHC.Exts
import Data.Functor

import GHC.Word
import Data.List (find)

import TestUtils

#include "ghcconfig.h"
#include "rts/Constants.h"

foreign import ccall unsafe "create_tso.h create_tso"
    c_create_tso:: IO (Ptr ())

createTSOClosure :: IO (GenClosure Box)
createTSOClosure = do
    ptr <- {-# SCC "MyCostCentre" #-} c_create_tso
    let addr = unpackAddr# ptr
    getClosureData ((unsafeCoerce# addr) :: LiftedClosure)

-- We can make some assumptions about the - otherwise dynamic - properties of
-- StgTSO and StgStack, because a new, non-running TSO is created with
-- create_tso() (create_tso.c).create_tso
main :: IO ()
main = do
    tso <- createTSOClosure

    let costCentre = prof tso >>= cccs <&> ccs_cc

    case costCentre of
        Nothing -> error $ "No CostCentre found in TSO: " ++ show tso
        Just _ -> case findMyCostCentre (linkedCostCentres costCentre) of
                    Just myCostCentre -> do
                        assertEqual (cc_label myCostCentre) "MyCostCentre"
                        assertEqual (cc_module myCostCentre) "Main"
                        assertEqual (cc_srcloc myCostCentre) (Just "prof_info.hs:23:39-50")
                        assertEqual (cc_mem_alloc myCostCentre) 0
                        assertEqual (cc_time_ticks myCostCentre) 0
                        assertEqual (cc_is_caf myCostCentre) False
                    Nothing -> error "MyCostCentre not found!"

linkedCostCentres :: Maybe CostCentre -> [CostCentre]
linkedCostCentres Nothing = []
linkedCostCentres (Just cc) = cc : linkedCostCentres (cc_link cc)

findMyCostCentre:: [CostCentre] -> Maybe CostCentre
findMyCostCentre ccs = find (\cc -> cc_label cc == "MyCostCentre") ccs
