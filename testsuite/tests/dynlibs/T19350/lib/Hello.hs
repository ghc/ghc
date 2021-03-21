module Hello (hello) where

foreign import ccall "hello" hello :: IO ()
