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
    let a = intToInt8# 0#
        b = intToInt8# 1#
        c = intToInt8# 2#
        d = intToInt8# 3#
        e = intToInt8# 4#
        f = intToInt8# 5#
        g = intToInt8# 6#
        h = intToInt8# 7#
        i = intToInt8# 8#
        j = intToInt8# 9#
        x = I# (int8ToInt# (add_all_int8 a b c d e f g h i j))
    print x
