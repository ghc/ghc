-- !!! illegal types in foreign export delarations

module ShouldFail where

import PrelGHC

foreign export ccall foo :: Int# -> IO ()
foo i | i ==# 0# = return ()

foreign export ccall bar :: Int -> Int#
bar _ = 42#
