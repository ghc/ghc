{-# LANGUAGE MagicHash, UnboxedTuples, BlockArguments #-}
module Main where

import GHC.Exts
import GHC.IO (IO(..))
import System.Mem (performMajorGC)

-- Lifted wrapper so SmallMutableArray# can be passed around.
data MArr = MArr (SmallMutableArray# RealWorld Integer)

-- Variant of T19048 for the LDV (biographical) profiler, +RTS -hb (#27585).
--
-- The array is promoted to the oldest generation *before* shrinking, so the
-- shrink-array slop marker is written into an old-generation block.  The next
-- major GC then runs LdvCensusForDead, whose linear heap scan
-- (processHeapForDead in rts/LdvProfile.c) must skip the slop correctly.
--
-- The array must stay below LARGE_OBJECT_THRESHOLD (409 words): large objects
-- live on the large_objects chain, which the census does not scan linearly.
main :: IO ()
main = do
  ma <- newArr
  fillArr ma 299
  -- Two major GCs promote the array to the oldest generation.
  performMajorGC
  performMajorGC
  -- Shrink: writes the slop marker over slots [10..299], in place, in an
  -- old-generation block.
  shrinkArr ma
  n <- getSize ma
  putStrLn $ "size after shrink = " ++ show n
  -- With -hb active, LdvCensusForDead scans the old blocks containing the
  -- slop marker.
  performMajorGC
  x <- readElem ma 0
  putStrLn $ "arr[0] = " ++ show x
  putStrLn "survived"

newArr :: IO MArr
newArr = IO \s -> case newSmallArray# 300# (0 :: Integer) s of
  (# s', ma #) -> (# s', MArr ma #)

-- Overwrite every slot with a distinct Integer so each holds a unique,
-- definitely non-zero heap pointer.
fillArr :: MArr -> Int -> IO ()
fillArr _ (-1) = pure ()
fillArr arr@(MArr ma) n@(I# n#) = do
  IO \s -> case writeSmallArray# ma n# (fromIntegral n :: Integer) s of
    s' -> (# s', () #)
  fillArr arr (n - 1)

shrinkArr :: MArr -> IO ()
shrinkArr (MArr ma) = IO \s ->
  case shrinkSmallMutableArray# ma 10# s of s' -> (# s', () #)

getSize :: MArr -> IO Int
getSize (MArr ma) = IO \s ->
  case getSizeofSmallMutableArray# ma s of (# s', n# #) -> (# s', I# n# #)

readElem :: MArr -> Int -> IO Integer
readElem (MArr ma) (I# i#) = IO \s -> readSmallArray# ma i# s
