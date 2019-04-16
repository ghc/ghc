module Main where

foreign import ccall "a" a_exp :: Int
foreign import ccall "c" c_exp :: Int

main = print $ c_exp * a_exp
