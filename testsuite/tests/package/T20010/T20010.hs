module Main where

import Foreign.C.Types

foreign import ccall unsafe "hello" hello :: CInt -> IO ()

main :: IO ()
main = do
    hello 42

