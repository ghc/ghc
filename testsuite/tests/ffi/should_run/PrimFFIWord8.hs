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
    let a = wordToWord8# 0##
        b = wordToWord8# 1##
        c = wordToWord8# 2##
        d = wordToWord8# 3##
        e = wordToWord8# 4##
        f = wordToWord8# 5##
        g = wordToWord8# 6##
        h = wordToWord8# 7##
        i = wordToWord8# 8##
        j = wordToWord8# 9##
        x = W# (word8ToWord# (add_all_word8 a b c d e f g h i j))
    print x
