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
    let a = narrowWord32# 0##
        b = narrowWord32# 1##
        c = narrowWord32# 2##
        d = narrowWord32# 3##
        e = narrowWord32# 4##
        f = narrowWord32# 5##
        g = narrowWord32# 6##
        h = narrowWord32# 7##
        i = narrowWord32# 8##
        j = narrowWord32# 9##
        x = W# (extendWord32# (add_all_word32 a b c d e f g h i j))
    print x
