{-# OPTIONS_GHC -XTypeFamilies #-}

module ShouldCompile where

type family T a

f :: T a -> Int
f x = x `seq` 3
