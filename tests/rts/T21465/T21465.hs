{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import System.IO

foreign import ccall "test_c" testC :: IO ()

helper :: IO ()
helper = putStrLn "This is the helper function" >> hFlush stdout

foreign export ccall helper :: IO ()

main :: IO ()
main = do
    x <- testC
    putStrLn "Done."
