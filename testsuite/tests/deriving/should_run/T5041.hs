{-# LANGUAGE MagicHash #-}

module Main where

data T = T1# | T2# Int deriving( Read, Show )

foo :: [T]
foo = read "[ T1#, T2# 4, T2# 5 ]"
main = print foo

