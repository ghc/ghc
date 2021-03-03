{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Exts

foreign import ccall "add_all_int32"
    add_all_int32
        :: Int32# -> Int32# -> Int32# -> Int32# -> Int32#
        -> Int32# -> Int32# -> Int32# -> Int32# -> Int32#
        -> Int32#

main :: IO ()
main = do
    let a = intToInt32# 0#
        b = intToInt32# 1#
        c = intToInt32# 2#
        d = intToInt32# 3#
        e = intToInt32# 4#
        f = intToInt32# 5#
        g = intToInt32# 6#
        h = intToInt32# 7#
        i = intToInt32# 8#
        j = intToInt32# 9#
        x = I# (int32ToInt# (add_all_int32 a b c d e f g h i j))
    print x
