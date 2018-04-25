module Main where

foreign import ccall "power2" power2 :: Int -> Int

main = print $ power2 4
