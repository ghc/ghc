{-# LANGUAGE TypeFamilies #-}

module ShouldCompile where

import Data.Kind (Type)

data family C9 a b :: Type
data instance C9 Int Int = C9IntInt
data instance C9 [a] Int = C9ListInt
data instance C9 [Int] [a]   = C9ListList2

type family D a
type instance D (Int, a) = (Int, a)
type instance D (a, Int) = (Int, Int)

type family E a
type instance E (Char, b) = ([Char], b)
type instance E (a, Int)  = (String, Int)
