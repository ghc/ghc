{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts

data Test1 = Test1 Int8# Word8#
    deriving (Show)

data Test2 = Test2 Int16# Word16#
    deriving (Show)

test1 :: Test1
test1 = Test1 (intToInt8# 1#) (wordToWord8# 2##)

test2 :: Test2
test2 = Test2 (intToInt16# 1#) (wordToWord16# 2##)

main :: IO ()
main = do
  print test1
  print test2
