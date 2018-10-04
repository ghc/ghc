{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Exts

foreign import ccall "add_all_int8"
    add_all_int8
        :: Int8# -> Int8# -> Int8# -> Int8# -> Int8#
        -> Int8# -> Int8# -> Int8# -> Int8# -> Int8#
        -> Int8#

main :: IO ()
main = do
    let a = narrowInt8# 0#
        b = narrowInt8# 1#
        c = narrowInt8# 2#
        d = narrowInt8# 3#
        e = narrowInt8# 4#
        f = narrowInt8# 5#
        g = narrowInt8# 6#
        h = narrowInt8# 7#
        i = narrowInt8# 8#
        j = narrowInt8# 9#
        x = I# (extendInt8# (add_all_int8 a b c d e f g h i j))
    print x
