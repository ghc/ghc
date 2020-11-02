{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE CPP #-}

module Main where

#ifdef __GLASGOW_HASKELL__
#include "MachDeps.h"
#endif

import Data.Primitive.ByteArray
import GHC.Types
import GHC.Exts
import Data.Word
import Data.Int
import Data.Bits
import Control.Monad
import Unsafe.Coerce

#if WORD_SIZE_IN_BITS < 64
#define INT64 Int64#
#define WORD64 Word64#
#else
#define INT64 Int#
#define WORD64 Word#
#endif

data I64 = I64 INT64
data W64 = W64 WORD64

foreign import prim "test_lt" lt_s :: INT64 -> INT64 -> Int#
foreign import prim "test_gt" gt_s :: INT64 -> INT64 -> Int#
foreign import prim "test_le" le_s :: INT64 -> INT64 -> Int#
foreign import prim "test_ge" ge_s :: INT64 -> INT64 -> Int#

foreign import prim "test_eq" eq_s :: INT64 -> INT64 -> Int#
foreign import prim "test_ne" ne_s :: INT64 -> INT64 -> Int#

foreign import prim "test_ltu" lt_u :: WORD64 -> WORD64 -> Int#
foreign import prim "test_gtu" gt_u :: WORD64 -> WORD64 -> Int#
foreign import prim "test_leu" le_u :: WORD64 -> WORD64 -> Int#
foreign import prim "test_geu" ge_u :: WORD64 -> WORD64 -> Int#

wordValues :: [Word64]
wordValues = do
    lowerBits <- interestingValues
    higherBits <- interestingValues
    return $ (fromIntegral higherBits `shiftL` 32) .|. fromIntegral lowerBits

interestingValues :: [Word32]
interestingValues =
    [ 0x00000000
    , 0x00000001
    , 0x00000002

    , 0x7FFFFFFD
    , 0x7FFFFFFE
    , 0x7FFFFFFF

    , 0xFFFFFFFE
    , 0xFFFFFFFD
    , 0xFFFFFFFF

    , 0x80000000
    , 0x80000001
    , 0x80000002
    ]

intValues :: [Int64]
intValues = map fromIntegral wordValues

intOps :: [(INT64 -> INT64 -> Int#, String)]
intOps = [(lt_s, "lt_s")
         ,(gt_s, "gt_s")
         ,(le_s, "le_s")
         ,(ge_s, "ge_s")

         ,(eq_s, "eq_s")
         ,(ne_s, "ne_s")]

testInt :: Int64 -> Int64 -> (INT64 -> INT64 -> Int#) -> String -> IO ()
testInt x y op op_name = do
    (I64 w1,I64 w2) <- getInts x y
    let !res = I# (op w1 w2)
    putStrLn $ "(" ++ (show x) ++ " `" ++ op_name ++ "` " ++ show y ++ ") = " ++ show res
    return ()

testInts = do
    let tests = do
            (op,op_desc) <- intOps
            x <- intValues
            y <- intValues
            return $ testInt x y op op_desc
    sequence tests

wordOps :: [(WORD64 -> WORD64 -> Int#, String)]
wordOps = [(lt_u, "lt_u")
          ,(gt_u, "gt_u")
          ,(le_u, "le_u")
          ,(ge_u, "ge_u")]

testWord x y op op_name = do
    (W64 w1,W64 w2) <- getWords x y
    let !res = I# (op w1 w2)
    putStrLn $ "(" ++ (show x) ++ " `" ++ op_name ++ "` " ++ show y ++ ") = " ++ show res

testWords = do
    let tests = do
            (op,op_desc) <- wordOps
            x <- wordValues
            y <- wordValues
            return $ testWord x y op op_desc
    sequence tests

main = do
    testInts
    testWords

    print "done"
    print wordValues
    print intValues
    return ()


-- Moving values int Int64# / Word64# is currently
-- quite annoying. The only way to do this on both
-- 64 and 32bit platforms is to go through a byte
-- array.

getInts :: Int64 -> Int64 -> IO ( I64, I64 )
getInts a1 a2 = do
    mba@(MutableByteArray ba) <- newPinnedByteArray 16
    writeByteArray mba 0 a1
    writeByteArray mba 1 a2
    i1 <- readInt 0 ba
    i2 <- readInt 1 ba
    return ( i1, i2 )

getWords :: Word64 -> Word64 -> IO ( W64, W64 )
getWords a1 a2 = do
    mba@(MutableByteArray ba) <- newPinnedByteArray 16
    writeByteArray mba 0 a1
    writeByteArray mba 1 a2
    w1 <- readWord 0 ba :: IO W64
    w2 <- readWord 1 ba
    return ( w1, w2 )

readInt :: Int -> MutableByteArray# RealWorld -> IO I64
readInt (I# i) ba = IO $ \s ->
        case (readInt64Array# ba i s) of
            (# s', x #) -> (# s', I64 x #)

readWord :: Int -> MutableByteArray# RealWorld -> IO W64
readWord (I# i) ba = IO $ \s ->
        case (readWord64Array# ba i s) of
            (# s', x #) -> (# s', W64 x #)
