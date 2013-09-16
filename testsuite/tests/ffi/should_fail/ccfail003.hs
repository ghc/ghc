{-# LANGUAGE UnliftedFFITypes, MagicHash #-}
-- !!! illegal types in foreign export delarations
module ShouldFail where

import GHC.Exts

foreign export ccall foo :: Int# -> IO ()
foo i | isTrue# (i ==# 0#) = return ()

foreign export ccall bar :: Int -> Int#
bar _ = 42#
