-- !!! Print001.hs: printing of types (esp for interfaces)

module Print001 where

import Ix

data Foo d e f = MkFoo [((d->Int)->d)->e] (d->e, e->e) ()
data Bar a = BarNil
	   | BarCon (Foo a a a) (Bar a)

mkFoo = MkFoo

f :: Eq a => (a -> b -> c) -> (a -> b -> c)
f x = x

f2 :: (Eq a, Ord a, Ix c) => (a -> b -> c) -> (a -> b -> c)
f2 x = x

g :: Foo Int (a -> b) (a -> [(a, Double, Int)]) -> Float
g x = 2.0
