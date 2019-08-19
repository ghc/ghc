{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import Prelude hiding (read)
import Control.Monad (unless)
import GHC.Exts
import GHC.Types
import System.Mem (performMajorGC)

-- The purpose of this test is to confirm that when the GC
-- copies (out of the nursery) a SmallMutableArray# that has
-- been shrunk, the array does not get corrupted.

data SmallArray = SA (SmallMutableArray# RealWorld Integer)

main :: IO ()
main = do
    let element = 42 :: Integer
    arr <- IO (\s0 -> case newSmallArray# 30# element s0 of
                        (# s1, ba# #) -> (# s1, SA ba# #))
    write arr 0 100
    write arr 13 113
    write arr 14 114
    write arr 15 115
    write arr 16 116
    shrink arr 14
    performMajorGC
    newSz <- getSize arr
    unless (newSz == 14) (fail "Wrong new size")
    e0 <- read arr 0
    unless (e0 == 100) $
      fail ("Wrong element 0: expected 100 but got " ++ show e0)
    e13 <- read arr 13
    unless (e13 == 113) $
      fail ("Wrong element 13: expected 113 but got " ++ show e13)

shrink :: SmallArray -> Int -> IO ()
shrink (SA ba#) (I# n#) = IO (\s ->
    case shrinkSmallMutableArray# ba# n# s of
      s' -> (# s', () #))

getSize :: SmallArray -> IO Int
getSize (SA ba#) = IO (\s ->
    case getSizeofSmallMutableArray# ba# s of
      (# s', n# #) -> (# s', I# n# #))

write :: SmallArray -> Int -> Integer -> IO ()
write (SA ba#) (I# i#) e = IO (\s ->
    case writeSmallArray# ba# i# e s of
      s' -> (# s', () #))

read :: SmallArray -> Int -> IO Integer
read (SA ba#) (I# i#) = IO (\s -> readSmallArray# ba# i# s)
