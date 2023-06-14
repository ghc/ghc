{-# LANGUAGE MagicHash, ExtendedLiterals #-}

module Main where

import GHC.Exts

data Test1 = Test1 Int8# Word8#
    deriving (Show)

data Test2 = Test2 Int16# Word16#
    deriving (Show)

data Test3 = Test3 Int32# Word32#
    deriving (Show)

data Test4 = Test4 Int64# Word64#
    deriving (Show)

test1 :: Test1
test1 = Test1 1#Int8 2#Word8

test2 :: Test2
test2 = Test2 1#Int16 2#Word16

test3 :: Test3
test3 = Test3 1#Int32 2#Word32

test4 :: Test4
test4 = Test4 -9223372036854775808#Int64 18446744073709551610#Word64

main :: IO ()
main = do
  print test1
  print test2
  print test3
  print test4
