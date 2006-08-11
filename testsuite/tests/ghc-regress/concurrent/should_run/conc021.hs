module Main where

-- !!! test for uncaught exception

foreign export ccall foo :: Int -> IO Int
foreign import ccall safe "foo" foo_imported :: Int -> IO Int

foo n = error "wurble"

main = foo_imported 3
