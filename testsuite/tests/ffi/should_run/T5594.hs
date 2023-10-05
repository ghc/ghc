module Lib (hello) where

foreign export ccall "hello" hello :: IO ()

hello :: IO ()
hello = putStrLn "Hello!"
