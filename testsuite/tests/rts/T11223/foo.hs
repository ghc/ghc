module Main where

foreign import ccall "c" c_exp :: Int

main = print c_exp
