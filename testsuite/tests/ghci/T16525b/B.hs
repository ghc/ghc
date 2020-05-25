module B (b) where

{-# NOINLINE b #-}
b :: () -> IO Int
b () = return 999999999
