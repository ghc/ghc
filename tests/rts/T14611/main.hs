module Main where

foreign import ccall "foo" c_foo :: Int

main = print c_foo
