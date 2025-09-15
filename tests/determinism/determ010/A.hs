{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module ShouldCompile where

-- tests deterministic order of fixities in the interface file

infixl 1 `f`
infixr 2 \\\
infix  3 :==>
infix  4 `MkFoo`

data Foo = MkFoo Int | Float :==> Double

f :: a -> b -> a
x `f` y = x

(\\\) :: (Eq a) => [a] -> [a] -> [a]
(\\\) xs ys =  xs
