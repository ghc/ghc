{-# OPTIONS -fglasgow-exts #-}

-- Don't do the cunning new newtype-deriving thing
-- when the type constructor is recursive

module ShouldCompile where

newtype A = A [A] deriving (Eq)

test :: A -> A -> Bool
test x y = x == y
