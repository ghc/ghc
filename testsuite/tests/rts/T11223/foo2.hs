module Main where

foreign import ccall "a" c_exp :: Int

main = print c_exp
