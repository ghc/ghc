{-# OPTIONS -ftype-families -fglasgow-exts #-}

module ShouldFail where

type family C a :: *
type instance C Int = forall a. [a]

