{-# OPTIONS_GHC -XTypeFamilies #-}

module ShouldCompile where

type family T a

f :: a -> T a -> Int
f p x = x `seq` 3
