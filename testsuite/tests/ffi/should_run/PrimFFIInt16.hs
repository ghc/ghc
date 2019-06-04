{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Exts

foreign import ccall "add_all_int16"
    add_all_int16
        :: Int16# -> Int16# -> Int16# -> Int16# -> Int16#
        -> Int16# -> Int16# -> Int16# -> Int16# -> Int16#
        -> Int16#

main :: IO ()
main = do
    let a = intToInt16# 0#
        b = intToInt16# 1#
        c = intToInt16# 2#
        d = intToInt16# 3#
        e = intToInt16# 4#
        f = intToInt16# 5#
        g = intToInt16# 6#
        h = intToInt16# 7#
        i = intToInt16# 8#
        j = intToInt16# 9#
        x = I# (int16ToInt# (add_all_int16 a b c d e f g h i j))
    print x
