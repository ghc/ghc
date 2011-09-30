module T5514 where

class Foo a where
	foo :: a -> a

instance (Foo a, Foo b) => Foo (a, b) where
	foo = foo' ()

-- foo' :: () -> b -> b
foo' es = const id (unitId es)

unitId :: () -> ()
unitId = id
