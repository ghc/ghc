{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module AssocTypes where


class Foo a where

    type Bar a b
    type Baz a

    type Baz a = [(a, a)]

    bar :: Bar a a
    bar = undefined


instance Foo [a] where

    type Bar [a] Int = [(a, Bool)]
    type Bar [a] Bool = [(Int, a)]

    type Baz [a] = (a, a, a)
