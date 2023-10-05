{-# LANGUAGE ForeignFunctionInterface #-}

-- | Test that unsafe FFI calls crash the RTS if they attempt to re-enter
-- Haskell-land
module Main where

import Foreign

foreign import ccall "wrapper" wrap_f :: IO () -> IO (FunPtr (IO ()))
foreign import ccall unsafe hello :: FunPtr (IO ()) -> IO ()

f :: IO ()
f = putStrLn "Back in Haskell"

main :: IO ()
main = do
    putStrLn "In Haskell"
    wrap_f f >>= hello
    putStrLn "Finished"
