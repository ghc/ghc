{-# LANGUAGE Generics, TypeOperators #-}

-- Trac #2573

module ShouldCompile where

import GHC.Base

class Tag a where
   nCons :: a -> Int
   nCons {| a :*: b |} _ = 1
   nCons {| a :+: b |} _ = 1
   nCons {| Unit |}    _ = 1
