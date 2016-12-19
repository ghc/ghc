{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

-------------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2007
--
-- | Break Arrays
--
-- An array of bytes, indexed by a breakpoint number (breakpointId in Tickish)
-- There is one of these arrays per module.
--
-- Each byte is
--   1 if the corresponding breakpoint is enabled
--   0 otherwise
--
-------------------------------------------------------------------------------

module GHCi.BreakArray
    (
      BreakArray
          (BA) -- constructor is exported only for ByteCodeGen
    , newBreakArray
    , getBreak
    , setBreakOn
    , setBreakOff
    , showBreakArray
    ) where

import Control.Monad
import Data.Word
import GHC.Word

import GHC.Exts
import GHC.IO ( IO(..) )
import System.IO.Unsafe ( unsafeDupablePerformIO )

data BreakArray = BA (MutableByteArray# RealWorld)

breakOff, breakOn :: Word8
breakOn  = 1
breakOff = 0

showBreakArray :: BreakArray -> IO ()
showBreakArray array = do
    forM_ [0 .. (size array - 1)] $ \i -> do
        val <- readBreakArray array i
        putStr $ ' ' : show val
    putStr "\n"

setBreakOn :: BreakArray -> Int -> IO Bool
setBreakOn array index
    | safeIndex array index = do
          writeBreakArray array index breakOn
          return True
    | otherwise = return False

setBreakOff :: BreakArray -> Int -> IO Bool
setBreakOff array index
    | safeIndex array index = do
          writeBreakArray array index breakOff
          return True
    | otherwise = return False

getBreak :: BreakArray -> Int -> IO (Maybe Word8)
getBreak array index
    | safeIndex array index = do
          val <- readBreakArray array index
          return $ Just val
    | otherwise = return Nothing

safeIndex :: BreakArray -> Int -> Bool
safeIndex array index = index < size array && index >= 0

size :: BreakArray -> Int
size (BA array) = size
  where
    -- We want to keep this operation pure. The mutable byte array
    -- is never resized so this is safe.
    size = unsafeDupablePerformIO $ sizeofMutableByteArray array

    sizeofMutableByteArray :: MutableByteArray# RealWorld -> IO Int
    sizeofMutableByteArray arr =
        IO $ \s -> case getSizeofMutableByteArray# arr s of
                       (# s', n# #) -> (# s', I# n# #)

allocBA :: Int -> IO BreakArray
allocBA (I# sz) = IO $ \s1 ->
    case newByteArray# sz s1 of { (# s2, array #) -> (# s2, BA array #) }

-- create a new break array and initialise elements to zero
newBreakArray :: Int -> IO BreakArray
newBreakArray entries@(I# sz) = do
    BA array <- allocBA entries
    case breakOff of
        W8# off -> do
           let loop n | isTrue# (n ==# sz) = return ()
                      | otherwise = do writeBA# array n off; loop (n +# 1#)
           loop 0#
    return $ BA array

writeBA# :: MutableByteArray# RealWorld -> Int# -> Word# -> IO ()
writeBA# array i word = IO $ \s ->
    case writeWord8Array# array i word s of { s -> (# s, () #) }

writeBreakArray :: BreakArray -> Int -> Word8 -> IO ()
writeBreakArray (BA array) (I# i) (W8# word) = writeBA# array i word

readBA# :: MutableByteArray# RealWorld -> Int# -> IO Word8
readBA# array i = IO $ \s ->
    case readWord8Array# array i s of { (# s, c #) -> (# s, W8# c #) }

readBreakArray :: BreakArray -> Int -> IO Word8
readBreakArray (BA array) (I# i) = readBA# array i
