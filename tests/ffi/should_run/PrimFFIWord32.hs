{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Exts

foreign import ccall "add_all_word32"
    add_all_word32
        :: Word32# -> Word32# -> Word32# -> Word32# -> Word32#
        -> Word32# -> Word32# -> Word32# -> Word32# -> Word32#
        -> Word32#

main :: IO ()
main = do
    let a = wordToWord32# 0##
        b = wordToWord32# 1##
        c = wordToWord32# 2##
        d = wordToWord32# 3##
        e = wordToWord32# 4##
        f = wordToWord32# 5##
        g = wordToWord32# 6##
        h = wordToWord32# 7##
        i = wordToWord32# 8##
        j = wordToWord32# 9##
        x = W# (word32ToWord# (add_all_word32 a b c d e f g h i j))
    print x
