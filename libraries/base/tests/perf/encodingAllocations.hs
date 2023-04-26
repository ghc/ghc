{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -dno-typeable-binds -O2 #-}

module Main (main) where

import System.IO
import Data.Bits
import GHC.Int
import GHC.Exts
import System.Environment
import Distribution.Simple.Utils


main :: IO ()
main = withTempFile "." "encodingAllocations.tmp" (const $ loop 1000000)

loop :: Int -> Handle -> IO ()
loop 0  !_ = pure ()
loop !n !h = do
  hPutChar h $! dummy_char n
  loop (n-1) h

-- unsafe efficient version of `chr`
my_chr :: Int -> Char
my_chr (I# i) = C# (chr# i)

-- return either a or b
dummy_char :: Int -> Char
dummy_char !i = my_chr ((i .&. 1) + 97)
