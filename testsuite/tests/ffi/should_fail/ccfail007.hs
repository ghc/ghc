module Ccfail007 where

foreign import ccall "wrapper" f :: (Int -> Int) -> Int
