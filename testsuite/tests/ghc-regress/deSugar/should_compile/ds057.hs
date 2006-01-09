{-# OPTIONS_GHC -fglasgow-exts #-}
module ShouldCompile where

import Data.Word
import GHC.Ptr
import GHC.Exts

f# :: Int# -> (# Char#, Int# #)
f# a# = (# '\0'#, a#  #)

g :: Int -> (Char, Int)
g (I# a#) = ( C# c#, I# b# )
  where (# c#, b# #) = f# a#
