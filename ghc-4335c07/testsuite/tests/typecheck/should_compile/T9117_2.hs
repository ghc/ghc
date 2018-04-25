module T9117_2 where


import Data.Coerce

newtype Foo a = Foo (Foo a)
newtype Age = MkAge Int

ex1 :: (Foo Age) -> (Foo Int)
ex1 = coerce
