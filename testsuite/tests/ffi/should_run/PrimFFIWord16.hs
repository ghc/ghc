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
    let a = wordToWord16# 0##
        b = wordToWord16# 1##
        c = wordToWord16# 2##
        d = wordToWord16# 3##
        e = wordToWord16# 4##
        f = wordToWord16# 5##
        g = wordToWord16# 6##
        h = wordToWord16# 7##
        i = wordToWord16# 8##
        j = wordToWord16# 9##
        x = W# (word16ToWord# (add_all_word16 a b c d e f g h i j))
    print x
