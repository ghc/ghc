{-# LANGUAGE RankNTypes #-}

module ShouldCompile where

f :: forall a. a -> forall b. b -> Int
f x = error "urk"

-- Both these should be ok, but an early GHC 6.6 failed

g1 = [ (+) :: Int -> Int -> Int, \x -> f x ]
g2 = [ \x -> f x, (+) :: Int -> Int -> Int ]

