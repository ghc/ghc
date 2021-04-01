module Hello (hello) where

foreign import ccall "hello_world" hello :: IO ()
