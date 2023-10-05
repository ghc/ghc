{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import Control.Monad (forM_, unless)
import Data.List (find)
import Data.Word
import Foreign
import Foreign.C.Types
import GHC.IO ( IO(..) )
import GHC.Exts
import GHC.Exts.Heap
import qualified  GHC.Exts.Heap.FFIClosures as FFIClosures
import GHC.Word

import TestUtils

main :: IO ()
main = do
    (tso, stack) <- {-# SCC "MyCostCentre" #-} createAndUnpackTSOAndSTACKClosure
    assertEqual (getClosureType tso) TSO
    assertEqual (what_next tso) ThreadRunGHC
    assertEqual (why_blocked tso) NotBlocked
    assertEqual (saved_errno tso) 0
    forM_ (flags tso) $ \flag -> case flag of
        TsoFlagsUnknownValue _ -> error $ "Unknown flag: " ++ show flag
        _ | flag `elem`
            [ TsoLocked
            , TsoBlockx
            , TsoStoppedOnBreakpoint
            , TsoSqueezed
            ] -> error $ "Unexpected flag: " ++ show flag
        _ -> return ()

    assertEqual (getClosureType stack) STACK

#if defined(PROFILING)
    let costCentre = ccs_cc <$> (cccs =<< prof tso)
    case costCentre of
        Nothing -> error $ "No CostCentre found in TSO: " ++ show tso
        Just _ -> case findMyCostCentre (linkedCostCentres costCentre) of
                    Just myCostCentre -> do
                        assertEqual (cc_label myCostCentre) "MyCostCentre"
                        assertEqual (cc_module myCostCentre) "Main"
                        assertEqual (cc_srcloc myCostCentre) (Just "tso_and_stack_closures.hs:24:48-80")
                        assertEqual (cc_is_caf myCostCentre) False
                    Nothing -> error $ "MyCostCentre not found in:\n" ++ unlines (cc_label <$> linkedCostCentres costCentre)
#endif

linkedCostCentres :: Maybe CostCentre -> [CostCentre]
linkedCostCentres Nothing = []
linkedCostCentres (Just cc) = cc : linkedCostCentres (cc_link cc)

findMyCostCentre:: [CostCentre] -> Maybe CostCentre
findMyCostCentre ccs = find (\cc -> cc_label cc == "MyCostCentre") ccs

getClosureType :: GenClosure b -> ClosureType
getClosureType = tipe . info

type StgTso = Any
type StgStack = Any
data MBA a = MBA (MutableByteArray# a)
data BA = BA ByteArray#

foreign import ccall safe "create_tso.h create_and_unpack_tso_and_stack"
    c_create_and_unpack_tso_and_stack
        :: Ptr (Ptr StgTso)
        -> Ptr (Ptr StgInfoTable)
        -> Ptr CInt
        -> Ptr (Ptr Word8)
        -> Ptr CInt
        -> Ptr (Ptr (Ptr Any))
        -> Ptr (Ptr StgStack)
        -> Ptr (Ptr StgInfoTable)
        -> Ptr CInt
        -> Ptr (Ptr Word8)
        -> Ptr CInt
        -> Ptr (Ptr (Ptr Any))
        -> IO ()

createAndUnpackTSOAndSTACKClosure
    :: IO ( GenClosure (Ptr Any)
          , GenClosure (Ptr Any)
          )
createAndUnpackTSOAndSTACKClosure = do

    alloca $ \ptrPtrTso -> do
    alloca $ \ptrPtrTsoInfoTable -> do
    alloca $ \ptrTsoHeapRepSize -> do
    alloca $ \ptrPtrTsoHeapRep -> do
    alloca $ \ptrTsoPointersSize -> do
    alloca $ \ptrPtrPtrTsoPointers -> do

    alloca $ \ptrPtrStack -> do
    alloca $ \ptrPtrStackInfoTable -> do
    alloca $ \ptrStackHeapRepSize -> do
    alloca $ \ptrPtrStackHeapRep -> do
    alloca $ \ptrStackPointersSize -> do
    alloca $ \ptrPtrPtrStackPointers -> do

    c_create_and_unpack_tso_and_stack

        ptrPtrTso
        ptrPtrTsoInfoTable
        ptrTsoHeapRepSize
        ptrPtrTsoHeapRep
        ptrTsoPointersSize
        ptrPtrPtrTsoPointers

        ptrPtrStack
        ptrPtrStackInfoTable
        ptrStackHeapRepSize
        ptrPtrStackHeapRep
        ptrStackPointersSize
        ptrPtrPtrStackPointers

    let fromHeapRep
          ptrPtrClosureInfoTable
          ptrClosureHeapRepSize
          ptrPtrClosureHeapRep
          ptrClosurePointersSize
          ptrPtrPtrClosurePointers = do
            ptrInfoTable :: Ptr StgInfoTable <- peek ptrPtrClosureInfoTable

            heapRepSize :: Int <- fromIntegral <$> peek ptrClosureHeapRepSize
            let I# heapRepSize# = heapRepSize
            ptrHeapRep :: Ptr Word8 <- peek ptrPtrClosureHeapRep
            MBA mutHeapRepBA <- IO $ \s -> let
                (# s', mba# #) = newByteArray# heapRepSize# s
                in (# s', MBA mba# #)
            forM_ [0..heapRepSize-1] $ \i@(I# i#) -> do
                W8# w <- peekElemOff ptrHeapRep i
                IO (\s -> (# writeWord8Array# mutHeapRepBA i# w s, () #))
            BA heapRep <- IO $ \s -> let
                (# s', ba# #) = unsafeFreezeByteArray# mutHeapRepBA s
                in (# s', BA ba# #)

            pointersSize :: Int <- fromIntegral <$> peek ptrClosurePointersSize
            ptrPtrPointers :: Ptr (Ptr Any) <- peek ptrPtrPtrClosurePointers
            ptrPtrPointers :: [Ptr Any] <- sequence
                [ peekElemOff ptrPtrPointers i
                | i <- [0..pointersSize-1]
                ]

            getClosureDataFromHeapRep
                heapRep
                ptrInfoTable
                ptrPtrPointers

    tso <- fromHeapRep
        ptrPtrTsoInfoTable
        ptrTsoHeapRepSize
        ptrPtrTsoHeapRep
        ptrTsoPointersSize
        ptrPtrPtrTsoPointers

    stack <- fromHeapRep
        ptrPtrStackInfoTable
        ptrStackHeapRepSize
        ptrPtrStackHeapRep
        ptrStackPointersSize
        ptrPtrPtrStackPointers

    return (tso, stack)
