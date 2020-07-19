{-# LANGUAGE ForeignFunctionInterface, MagicHash, CPP, BangPatterns #-}

import Prelude
import Foreign
import Foreign.C.Types
import GHC.Exts.Heap
import GHC.Exts
import Data.Functor

import GHC.Word
import Data.List (find)

#include "ghcconfig.h"
#include "rts/Constants.h"

foreign import ccall unsafe "create_tso.h create_tso"
    c_create_tso:: IO Word

-- Invent a type to bypass the type constraints of getClosureData.
-- Infact this will be a Word#, that is directly given to unpackClosure#
-- (which is a primop that expects a pointer to a closure).
data FoolStgTSO

createTSOClosure :: IO (GenClosure Box)
createTSOClosure = do
    ptr <- {-# SCC "MyCostCentre" #-} c_create_tso
    let wPtr = unpackWord# ptr
    getClosureData ((unsafeCoerce# wPtr) :: FoolStgTSO)

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
                        assertEqual (cc_ccID myCostCentre) 1
                        assertEqual (cc_label myCostCentre) "MyCostCentre"
                        assertEqual (cc_module myCostCentre) "Main"
                        assertEqual (cc_srcloc myCostCentre) (Just "prof_info.hs:26:39-50")
                        assertEqual (cc_mem_alloc myCostCentre) 0
                        assertEqual (cc_time_ticks myCostCentre) 0
                        assertEqual (cc_is_caf myCostCentre) False
                        assertEqual (cc_link myCostCentre) Nothing
                    Nothing -> error "MyCostCentre not found!"

unpackWord# :: Word -> Word#
unpackWord# (W# w#) = w#

linkedCostCentres :: Maybe CostCentre -> [CostCentre]
linkedCostCentres Nothing = []
linkedCostCentres (Just cc) = cc : linkedCostCentres (cc_link cc)

findMyCostCentre:: [CostCentre] -> Maybe CostCentre
findMyCostCentre ccs = find (\cc -> cc_label cc == "MyCostCentre") ccs

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual x y = if x == y then return () else error $ "assertEqual: " ++ show x ++ " /= " ++ show y
