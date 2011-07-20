-- !!! tests fixity reading and printing
module ShouldCompile where

infixl 1 `f`
infixr 2 \\\
infix  3 :==>
infix  4 `MkFoo`

data Foo = MkFoo Int | Float :==> Double

x `f` y = x

(\\\) :: (Eq a) => [a] -> [a] -> [a]
(\\\) xs ys =  xs
