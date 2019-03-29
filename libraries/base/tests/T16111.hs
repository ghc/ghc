module Main (main) where

import Data.Bits
import Data.Word

main :: IO ()
main = print $ toInteger (shiftL 1 hm :: Word64)
            == toInteger (shiftL 1 hm :: Word64)

hm :: Int
hm = -1
{-# NOINLINE hm #-}

