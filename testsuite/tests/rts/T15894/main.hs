module Main where

foreign import ccall "with_copysign" c_with_copysign :: Double -> Double

main = print $ c_with_copysign 8.5
