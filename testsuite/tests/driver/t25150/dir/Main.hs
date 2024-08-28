{-# LANGUAGE ForeignFunctionInterface #-}
module Main where
foreign import ccall "hello"
  hello :: IO ()
main :: IO ()
main = hello
