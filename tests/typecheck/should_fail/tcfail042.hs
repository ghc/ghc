module ShouldFail where

-- !!! weird class/instance examples off the haskell list
--

class Foo a          where foo :: a -> a
class Foo a => Bar a where bar :: a -> a


instance Num a => Foo [a] where
    foo []     = []
    foo (x:xs) = map (x+) xs


instance (Eq a, Show a) => Bar [a] where
    bar []     = []
    bar (x:xs) = foo xs where u = x==x
                              v = show x

------------------------------------------

{-
class Foo a => Bar2 a where bar2 :: a -> a

instance (Eq a, Show a) => Foo [a]

instance Num a => Bar2 [a]

data X a = X a
-}
