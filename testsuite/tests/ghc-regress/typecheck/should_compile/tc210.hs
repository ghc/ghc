{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

f :: forall a. a -> forall b. b -> Int
f = error "urk"

-- Both these should be ok, but an early GHC 6.6 failed

g1 = [ (+) :: Int -> Int -> Int, f ]
g2 = [ f, (+) :: Int -> Int -> Int ]

