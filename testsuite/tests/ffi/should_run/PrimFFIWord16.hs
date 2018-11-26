{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Exts

foreign import ccall "add_all_word16"
    add_all_word16
        :: Word16# -> Word16# -> Word16# -> Word16# -> Word16#
        -> Word16# -> Word16# -> Word16# -> Word16# -> Word16#
        -> Word16#

main :: IO ()
main = do
    let a = narrowWord16# 0##
        b = narrowWord16# 1##
        c = narrowWord16# 2##
        d = narrowWord16# 3##
        e = narrowWord16# 4##
        f = narrowWord16# 5##
        g = narrowWord16# 6##
        h = narrowWord16# 7##
        i = narrowWord16# 8##
        j = narrowWord16# 9##
        x = W# (extendWord16# (add_all_word16 a b c d e f g h i j))
    print x
