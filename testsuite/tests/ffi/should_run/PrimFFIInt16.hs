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
    let a = narrowInt16# 0#
        b = narrowInt16# 1#
        c = narrowInt16# 2#
        d = narrowInt16# 3#
        e = narrowInt16# 4#
        f = narrowInt16# 5#
        g = narrowInt16# 6#
        h = narrowInt16# 7#
        i = narrowInt16# 8#
        j = narrowInt16# 9#
        x = I# (extendInt16# (add_all_int16 a b c d e f g h i j))
    print x
