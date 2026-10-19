{-# LANGUAGE MagicHash #-}

-- Regression test for #28050.
--
-- A subword value must still be zero-extended after it has been tested by a
-- conditional branch.  Correct output:
--   (200,65480,4294967240)
--   200

module Main (main) where

import Data.Int
import Data.Word
import GHC.Exts
import GHC.Word

f8 :: Int8 -> Word
f8 i = if i < 0 then fromIntegral (fromIntegral i :: Word8) else 0
{-# NOINLINE f8 #-}

f16 :: Int16 -> Word
f16 i = if i < 0 then fromIntegral (fromIntegral i :: Word16) else 0
{-# NOINLINE f16 #-}

f32 :: Int32 -> Word
f32 i = if i < 0 then fromIntegral (fromIntegral i :: Word32) else 0
{-# NOINLINE f32 #-}

g8 :: Word8 -> Word
g8 (W8# w) = case eqWord8# w (wordToWord8# 100##) of
  1# -> 0
  _  -> W# (word8ToWord# w)
{-# NOINLINE g8 #-}

main :: IO ()
main = do
  print (f8 (-56), f16 (-56), f32 (-56))
  print (g8 200)
