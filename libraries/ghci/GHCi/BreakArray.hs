{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

-------------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2007
--
-- | Break Arrays
--
-- An array of words, indexed by a breakpoint number (breakpointId in Tickish)
-- There is one of these arrays per module.
--
-- Each word with value n is
--   n > 1 : the corresponding breakpoint is enabled, but
--   n == 0: The breakpoint is enabled, GHCi will stop next time it hits
--           this breakpoing
--   n == -1: This breakpoint is disabled.
--   n < -1 : Not used!!
--
-------------------------------------------------------------------------------

module GHCi.BreakArray
    (
      BreakArray
          (BA) -- constructor is exported only for GHC.CoreToByteCode
    , newBreakArray
    , getBreak
    , setBreakCount
    , setBreakOn
    , setBreakOff
    , showBreakArray
    ) where

import Prelude -- See note [Why do we import Prelude here?]
import Control.Monad (forM_)
import GHC.Prim
import GHC.Types
import System.IO.Unsafe ( unsafeDupablePerformIO )

#include "MachDeps.h"

data BreakArray = BA (MutableByteArray# RealWorld)

breakOff, breakOn :: Int
breakOn  = 0
breakOff = -1

showBreakArray :: BreakArray -> IO ()
showBreakArray array = do
    forM_ [0 .. (size array - 1)] $ \i -> do
        val <- readBreakArray array i
        putStr $ ' ' : show val
    putStr "\n"

setBreakOn :: BreakArray -> Int -> IO Bool
setBreakOn array inx = do
    writeBreakArray array inx breakOn
    return True

setBreakOff :: BreakArray -> Int -> IO Bool
setBreakOff array inx = do
    writeBreakArray array inx breakOff
    return True

getBreak :: BreakArray -> Int -> IO (Maybe Int)
getBreak array index
    | safeIndex array index = do
          val <- readBreakArray array index
          return $ Just val
    | otherwise = return Nothing

safeIndex :: BreakArray -> Int -> Bool
safeIndex array index = index < size array && index >= 0

size :: BreakArray -> Int
size (BA array) = size `div` SIZEOF_HSWORD
  where
    -- We want to keep this operation pure. The mutable byte array
    -- is never resized so this is safe.
    size = unsafeDupablePerformIO $ sizeofMutableByteArray array

    sizeofMutableByteArray :: MutableByteArray# RealWorld -> IO Int
    sizeofMutableByteArray arr =
        IO $ \s -> case getSizeofMutableByteArray# arr s of
                       (# s', n# #) -> (# s', I# n# #)

setBreakCount :: BreakArray -> Int -> Int -> IO Bool
setBreakCount breakArray ind val
    | safeIndex breakArray ind = do
        writeBreakArray breakArray ind val
        return True
    | otherwise = return False



allocBA :: Int# -> IO BreakArray
allocBA sz# = IO $ \s1 ->
    case newByteArray# sz# s1 of { (# s2, array #) -> (# s2, BA array #) }

-- create a new break array and initialise elements to zero
newBreakArray :: Int -> IO BreakArray
newBreakArray (I# sz#) = do
    BA array <- allocBA (sz# *# SIZEOF_HSWORD#)
    case breakOff of
        I# off -> do
           let loop n | isTrue# (n >=# sz#) = return ()
                      | otherwise = do writeBA# array n off; loop (n +# 1#)
           loop 0#
    return $ BA array

writeBA# :: MutableByteArray# RealWorld -> Int# -> Int# -> IO ()
writeBA# array ind val = IO $ \s ->
    case writeIntArray# array ind val s of { s -> (# s, () #) }

writeBreakArray :: BreakArray -> Int -> Int -> IO ()
writeBreakArray (BA array) (I# i) (I# val) = writeBA# array i val

readBA# :: MutableByteArray# RealWorld -> Int# -> IO Int
readBA# array i = IO $ \s ->
    case readIntArray# array i s of { (# s, c #) -> (# s, I# c #) }

readBreakArray :: BreakArray -> Int -> IO Int
readBreakArray (BA array) (I# ind# ) = readBA# array ind#
