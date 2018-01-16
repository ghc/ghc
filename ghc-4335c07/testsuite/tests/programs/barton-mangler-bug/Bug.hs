module Bug where

data Eq a => Foo a = Foo { x :: a }

foo :: Foo Int
foo = Foo{}
