{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Exts
import GHC.Prim
import GHC.IO

import Data.List (intercalate)

import System.Mem (performMinorGC)

type Resizer s =
  MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)

{-# NOINLINE testResize #-}
testResize ::
  Int#
  -> Resizer RealWorld
  -> IO ()
testResize initSize# resizer# =
  testCaseUnpinned initSize# resizer#
  >> testCasePinned initSize# resizer#
  >> testCaseAlignedPinned initSize# resizer#
  where
  testCaseUnpinned initSize# =
    testCase "unpinned" (newByteArray# initSize#)
  testCasePinned initSize# =
    testCase "pinned" (newPinnedByteArray# initSize#)
  testCaseAlignedPinned initSize# =
    testCase "aligned (8) & pinned" (newAlignedPinnedByteArray# initSize# 8#)

{-# NOINLINE testCase #-}
testCase ::
  String
  -> (State# RealWorld -> (# State# RealWorld, MutableByteArray# RealWorld #))
  -> Resizer RealWorld
  -> IO ()
testCase pinnedMessage alloc# resizer# = do
  performMinorGC
  testWithAlloc >>= putStrLn
  where
  -- by allocating these all in one go hopefully we can force the resize
  {-# NOINLINE testWithAlloc #-}
  testWithAlloc :: IO String
  testWithAlloc = IO $ \s0 -> case alloc# s0 of
    (# s1, mba1 #) -> case alloc# s1 of
      (# s2, mba2 #) -> case resizer# mba1 10# s2 of
        (# s3, mba1' #) -> case resizer# mba2 10# s3 of
          (# s4, mba2' #) ->
            (# s4
            , unlines
              [ "starts " <> pinnedMessage <> ":"
              , "first " <> checkArrayPair mba1 mba1'
              , "second " <> checkArrayPair mba2 mba2'
              ]
            #)
  checkArrayPair mba mba' =
    "array ends " <> isPinned mba' <> ", is " <> isSame mba mba'
  isPinned mba = if I# (isMutableByteArrayPinned# mba) == 1
    then "pinned"
    else "unpinned"
  isSame mba mba' = if I# (sameMutableByteArray# mba mba') == 1
    then "the same array"
    else "a different array"

main :: IO ()
main = do
  putStrLn "testing resizeMutableByteArray#"
  -- prefer to go to unpinned
  testResize 5# resizeMutableByteArray#
  putStrLn "===========\ntesting resizeMutableByteArrayKeepPinnedness#"
  -- keep pinnedness, both arrays stay the same
  testResize 5# resizeMutableByteArrayKeepPinnedness#
  putStrLn "===========\ntesting resizeMutableByteArrayPreferUnpinned#"
  -- prefer unpinned arrays when creating new ones, this is the same as resizeMutableByteArray#
  testResize 5# resizeMutableByteArrayPreferUnpinned#
  putStrLn "===========\ntesting resizeMutableByteArrayPreferPinned#"
  -- prefer pinned arrays when creating new ones
  testResize 5# resizeMutableByteArrayPreferPinned#
  putStrLn "===========\ntesting resizeMutableByteArrayPreferAlignedPinned#"
  -- prefer pinned arrays when creating new ones.
  testResize 5# (\mba size -> resizeMutableByteArrayPreferAlignedPinned# mba size 32#)
  putStrLn "===========\ntesting resizeMutableByteArrayMakeAlignedPinned#"
  -- guarantee pinned arrays.
  testResize 5# (\mba size -> resizeMutableByteArrayMakeAlignedPinned# mba size 32#)
