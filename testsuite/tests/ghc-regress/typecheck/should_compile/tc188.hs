{-# OPTIONS -fglasgow-exts #-}

-- Test infix type constructors for type synonyms

module ShouldCompile where

infix 9 :-+-:
type (f :-+-: g) t o1 o2 = Either (f t o1 o2) (g t o1 o2)

data Foo a b c = Foo (a,b,c)

type App f = f Int Bool Int

f :: (Foo :-+-: Foo) Bool Int Bool
f = error "urk"

g :: App (Foo :-+-: Foo) 
g = error "urk"

-------- classes --------

class (Eq a, Eq b) => a :&: b where
   op :: a -> b

h :: (a :&: b) => a -> b
h x = op x
