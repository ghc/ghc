{-# LANGUAGE Haskell2010 #-}
module Classes where


class Foo f where

    bar :: f a -> f b -> f (a, b)
    baz :: f ()

    baz = undefined


class Quux q where

    (+++), (///) :: q -> q -> q
    (***), logBase :: q -> q -> q
    foo, quux :: q -> q -> q
