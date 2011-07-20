{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- Should fail, because f :: (# Int#, ByteArray# #)

module ShouldFail where

import GHC.Prim (Int#, ByteArray#)

main :: IO ()
main = let f = int2Integer# 0# in putStrLn ""


int2Integer# :: Int# -> (# Int#, ByteArray# #)
int2Integer# = undefined
-- This function doesn't have to work!
-- We just need it for its type.

