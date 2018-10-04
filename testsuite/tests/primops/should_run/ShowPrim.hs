{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts

data Test = Test Int8# Word8#
    deriving (Show)

test1 :: Test
test1 = Test (narrowInt8# 1#) (narrowWord8# 2##)

main :: IO ()
main = print test1
