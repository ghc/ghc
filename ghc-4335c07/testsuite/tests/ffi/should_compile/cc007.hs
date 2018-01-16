-- !!! cc007 -- foreign import with external name equal to Haskell name.
module ShouldCompile where

foreign import ccall sine :: Double -> Double
