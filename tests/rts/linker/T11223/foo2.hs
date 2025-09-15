module Main where

foreign import ccall "a" a_exp :: Int

main = print a_exp
