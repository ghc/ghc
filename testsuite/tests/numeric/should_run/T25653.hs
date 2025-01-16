module Main (main) where

import Data.Word (Word8)

myMod :: Word8 -> Word8
myMod i = mod i 7
{-# NOINLINE myMod #-}

main :: IO ()
main = print $ myMod 5
