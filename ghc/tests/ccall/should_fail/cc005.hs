-- !!! illegal types in foreign export delarations

module ShouldFail where

import PrelGHC

foreign export foo :: Int# -> IO ()
foo i | i ==# 0# = return ()

foreign export bar :: Int -> Int#
bar _ = 42#
