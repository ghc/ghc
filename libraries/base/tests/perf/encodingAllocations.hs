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
main = withTempFile
#if !MIN_VERSION_Cabal(3,15,0)
        "."
#endif
        "encodingAllocations.tmp" (const $ loop 1000000)

loop :: Int -> FilePath -> Handle -> IO ()
loop 0  !_ !_ = pure ()
loop !n !fp !h = do
  hPutChar h $! dummy_char n
  loop (n-1) fp h

-- unsafe efficient version of `chr`
my_chr :: Int -> Char
my_chr (I# i) = C# (chr# i)

-- return either a or b
dummy_char :: Int -> Char
dummy_char !i = my_chr ((i .&. 1) + 97)
