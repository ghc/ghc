{-# LANGUAGE TypeFamilies #-}

module ShouldCompile where

type family F a 
type instance F [a] = [F a]

foo :: (F [a] ~ a) => a
foo = undefined
