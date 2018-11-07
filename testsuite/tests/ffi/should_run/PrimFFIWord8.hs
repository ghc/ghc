{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Exts

foreign import ccall "add_all_word8"
    add_all_word8
        :: Word8# -> Word8# -> Word8# -> Word8# -> Word8#
        -> Word8# -> Word8# -> Word8# -> Word8# -> Word8#
        -> Word8#

main :: IO ()
main = do
    let a = narrowWord8# 0##
        b = narrowWord8# 1##
        c = narrowWord8# 2##
        d = narrowWord8# 3##
        e = narrowWord8# 4##
        f = narrowWord8# 5##
        g = narrowWord8# 6##
        h = narrowWord8# 7##
        i = narrowWord8# 8##
        j = narrowWord8# 9##
        x = W# (extendWord8# (add_all_word8 a b c d e f g h i j))
    print x
