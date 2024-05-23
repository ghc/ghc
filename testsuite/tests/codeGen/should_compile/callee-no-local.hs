module Test where

foreign import ccall unsafe "foo" c_foo :: IO ()
