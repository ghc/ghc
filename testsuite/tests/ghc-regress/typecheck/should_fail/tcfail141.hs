{-# OPTIONS -fglasgow-exts #-}

-- Should fail, because f :: (# Int#, ByteArray# #)

module ShouldFail where

import GHC.Prim

main :: IO ()
main = let f = int2Integer# 0# in putStrLn ""

