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
    let a = narrowInt32# 0#
        b = narrowInt32# 1#
        c = narrowInt32# 2#
        d = narrowInt32# 3#
        e = narrowInt32# 4#
        f = narrowInt32# 5#
        g = narrowInt32# 6#
        h = narrowInt32# 7#
        i = narrowInt32# 8#
        j = narrowInt32# 9#
        x = I# (extendInt32# (add_all_int32 a b c d e f g h i j))
    print x
