{-# LANGUAGE ExistentialQuantification, FlexibleContexts,
             MultiParamTypeClasses #-}

-- !!! Existential data tyes
-- Hugs didn't like this one

module Main (main) where


class MyClass a b where
	foo :: a -> b -> Int

data Special = forall b. (MyClass Int b)=> MkSpecial b
data General a = forall b. (MyClass a b)=> MkGeneral b

instance MyClass Int Bool where
   foo x False = -x
   foo x True  = x

xs :: [General Int]
xs = [MkGeneral True, MkGeneral False]

main = print [foo (3::Int) x | MkGeneral x <- xs]
	-- Without the (::Int) part we get an 
	-- incomprehensible error message :-(
