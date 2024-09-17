{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ExtendedLiterals #-}
-- !!! test broadcasting, packing and unpacking for vector types

import GHC.Int
import GHC.Exts

main :: IO ()
main = do
    -- IntX2#
    case unpackInt64X2# (broadcastInt64X2# 6#Int64) of
        (# a, b #) -> print (I64# a, I64# b)
    case unpackInt64X2# (packInt64X2# (# 7#Int64,9#Int64 #)) of
        (# a, b #) -> print (I64# a, I64# b)
