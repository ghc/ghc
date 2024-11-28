{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

-------------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2007
--
-- | Break Arrays
--
-- An array of Int64, indexed by a breakpoint number (breakpointId in Tickish)
-- containing the ignore count for every breakpoint.
-- There is one of these arrays per module.
--
-- For each Int64 with value n:
--   n > 1  : the corresponding breakpoint is enabled. Next time the bp is hit,
--            GHCi will decrement the ignore count and continue processing.
--   n == 0 : The breakpoint is enabled, GHCi will stop next time it hits
--            this breakpoint.
--   n == -1: This breakpoint is disabled.
--   n < -1 : Not used.
--
-------------------------------------------------------------------------------

module GHCi.BreakArray
    (
      BreakArray
          (BA) -- constructor is exported only for GHC.StgToByteCode
    , newBreakArray
    , getBreak
    , setupBreakpoint
    , breakOn
    , breakOff
    , showBreakArray
    ) where

import Prelude -- See note [Why do we import Prelude here?]
import Control.Monad

import GHC.Exts
import GHC.Int
import GHC.IO ( IO(..) )
import System.IO.Unsafe ( unsafeDupablePerformIO )

#include "MachDeps.h"

data BreakArray = BA (MutableByteArray# RealWorld)

breakOff, breakOn :: Int64
breakOn  = 0
breakOff = -1

showBreakArray :: BreakArray -> IO ()
showBreakArray array = do
    forM_ [0 .. (size array - 1)] $ \i -> do
        val <- readBreakArray array i
        putStr $ ' ' : show val
    putStr "\n"

setupBreakpoint :: BreakArray -> Int -> Int64 -> IO Bool
setupBreakpoint breakArray ind val
    | safeIndex breakArray ind = do
        writeBreakArray breakArray ind val
        return True
    | otherwise = return False

getBreak :: BreakArray -> Int -> IO (Maybe Int64)
getBreak array index
    | safeIndex array index = do
          val <- readBreakArray array index
          return $ Just val
    | otherwise = return Nothing

safeIndex :: BreakArray -> Int -> Bool
safeIndex array index = index < size array && index >= 0

size :: BreakArray -> Int
size (BA array) = size `div` SIZEOF_INT64
  where
    -- We want to keep this operation pure. The mutable byte array
    -- is never resized so this is safe.
    size = unsafeDupablePerformIO $ sizeofMutableByteArray array

    sizeofMutableByteArray :: MutableByteArray# RealWorld -> IO Int
    sizeofMutableByteArray arr =
        IO $ \s -> case getSizeofMutableByteArray# arr s of
                       (# s', n# #) -> (# s', I# n# #)

allocBA :: Int# -> IO BreakArray
allocBA sz# = IO $ \s1 ->
    case newByteArray# sz# s1 of { (# s2, array #) -> (# s2, BA array #) }

-- create a new break array and initialise all elements to breakOff.
newBreakArray :: Int -> IO BreakArray
newBreakArray (I# sz#) = do
    BA array <- allocBA (sz# *# SIZEOF_INT64#)
    case breakOff of
        I64# off -> do
           let loop n | isTrue# (n >=# sz#) = return ()
                      | otherwise = do writeBA# array n off; loop (n +# 1#)
           loop 0#
    return $ BA array

writeBA# :: MutableByteArray# RealWorld -> Int# -> Int64# -> IO ()
writeBA# array ind val = IO $ \s ->
    case writeInt64Array# array ind val s of { s -> (# s, () #) }

writeBreakArray :: BreakArray -> Int -> Int64 -> IO ()
writeBreakArray (BA array) (I# i) (I64# val) = writeBA# array i val

readBA# :: MutableByteArray# RealWorld -> Int# -> IO Int64
readBA# array i = IO $ \s ->
    case readInt64Array# array i s of { (# s, c #) -> (# s, I64# c #) }

readBreakArray :: BreakArray -> Int -> IO Int64
readBreakArray (BA array) (I# ind# ) = readBA# array ind#
