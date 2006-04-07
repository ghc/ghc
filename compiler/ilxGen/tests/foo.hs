{-# OPTIONS -fglasgow-exts  #-}
module Foo where
import PrelGHC
import PrelNum
import PrelBase
integer2Intx :: Integer -> Int
integer2Intx (S# i)   = I# i
integer2Intx (J# s d) = case (integer2Int# s d) of { n# -> I# n# }

