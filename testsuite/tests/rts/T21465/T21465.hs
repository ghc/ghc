{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

foreign import ccall "test_c" testC :: IO ()

helper :: IO ()
helper = putStrLn "This is the helper function"

foreign export ccall helper :: IO ()

main :: IO ()
main = do
    x <- testC
    putStrLn "Done."
