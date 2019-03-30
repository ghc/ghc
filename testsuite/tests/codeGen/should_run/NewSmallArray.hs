{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- Tests for creating and initializing a @SmallArray#@ including the
-- optimiziation where GHC inlines the code instead of calling the
-- @newSmallArray#@ primop if the length is small enough and known at compile
-- time.
module Main where

import GHC.Exts
import GHC.ST

import Control.Monad (forM_)


main :: IO ()
main = do
    let !a00 = newSmallArrayWith42 0
        !a01 = newSmallArrayWith42 1
        !a02 = newSmallArrayWith42 2
        !a03 = newSmallArrayWith42 3
        !a04 = newSmallArrayWith42 4
        !a05 = newSmallArrayWith42 5
        !a06 = newSmallArrayWith42 6
        !a07 = newSmallArrayWith42 7
        !a08 = newSmallArrayWith42 8
        !a09 = newSmallArrayWith42 9
        !a10 = newSmallArrayWith42 10
        !a11 = newSmallArrayWith42 11
        !a12 = newSmallArrayWith42 12
        !a13 = newSmallArrayWith42 13
        !a14 = newSmallArrayWith42 14
        !a15 = newSmallArrayWith42 15
        !a16 = newSmallArrayWith42 16
        !a17 = newSmallArrayWith42 17
        !a18 = newSmallArrayWith42 18
        !a19 = newSmallArrayWith42 19
        !a20 = newSmallArrayWith42 20
        !a21 = newSmallArrayWith42 21
        !a22 = newSmallArrayWith42 22
        !a23 = newSmallArrayWith42 23
        !a24 = newSmallArrayWith42 24
        !a25 = newSmallArrayWith42 25
        !a26 = newSmallArrayWith42 26
        !a27 = newSmallArrayWith42 27
        !a28 = newSmallArrayWith42 28
        !a29 = newSmallArrayWith42 29
        !a30 = newSmallArrayWith42 30
        !a31 = newSmallArrayWith42 31
        !a32 = newSmallArrayWith42 32
        !a33 = newSmallArrayWith42 33
        !a34 = newSmallArrayWith42 34
        !a35 = newSmallArrayWith42 35
        !a36 = newSmallArrayWith42 36
        !a37 = newSmallArrayWith42 37
        !a38 = newSmallArrayWith42 38
        !a39 = newSmallArrayWith42 39
        !all = [ a00, a01, a02, a03, a04, a05, a06, a07, a08, a09
               , a10, a11, a12, a13, a14, a15, a16, a17, a18, a19
               , a20, a21, a22, a23, a24, a25, a26, a27, a28, a29
               , a30, a31, a32, a33, a34, a35, a36, a37, a38, a39
               ]
    forM_ all (print . toListArray)


data Array a = Array { unArray :: SmallArray# a }

newSmallArrayWith42 :: Int -> Array Int
newSmallArrayWith42 n = (runST (newArray n 42))
-- inline to make sure the length is known at compile time
{-# INLINE newSmallArrayWith42 #-}

newArray :: Int -> a -> ST s (Array a)
newArray (I# n#) a = ST $ \s1# -> case newSmallArray# n# a s1# of
    (# s2#, marr# #) -> case unsafeFreezeSmallArray# marr# s2# of
        (# s3#, arr# #) -> (# s3#, Array arr# #)
-- inline to make sure the length is known at compile time
{-# INLINE newArray #-}

toListArray :: Array a -> [a]
toListArray arr = go 0
  where
    go i | i >= lengthArray arr = []
         | otherwise = indexArray arr i : go (i+1)

indexArray :: Array a -> Int -> a
indexArray arr i@(I# i#)
  | i < 0 || i >= len =
      error $ "bounds error, offset " ++ show i ++ ", length " ++ show len
  | otherwise = case indexSmallArray# (unArray arr) i# of
      (# a #) -> a
  where len = lengthArray arr

lengthArray :: Array a -> Int
lengthArray arr = I# (sizeofSmallArray# (unArray arr))
