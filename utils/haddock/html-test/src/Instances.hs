{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}


module Instances where


newtype (<~~) a b = Xyzzy (b -> (a, a))


class Foo f where

    foo :: f Int -> a -> f a
    foo' :: f (f a) -> Int -> f (f Int)

    foo = undefined
    foo' = undefined

instance Foo Maybe
instance Foo []
instance (Eq a, Foo f) => Foo ((,) (f a))
instance Foo (Either a)
instance Foo ((,,) a a)
instance Foo ((->) a)
instance Foo ((<~~) a)


class Foo f => Bar f a where

    bar :: f a -> f Bool -> a
    bar' :: f (f a) -> f (f (f b))
    bar0, bar1 :: (f a, f a) -> (f b, f c)

    bar = undefined
    bar' = undefined
    bar0 = undefined
    bar1 = undefined


instance Bar Maybe Bool
instance Bar Maybe [a]
instance Bar [] (a, a)
instance Foo f => Bar (Either a) (f a)
instance Foo ((,,) a b) => Bar ((,,) a b) (a, b, a)


class Baz a where

    baz :: a -> (forall a. a -> a) -> (b, forall c. c -> a) -> (b, c)
    baz' :: b -> (forall b. b -> a) -> (forall b. b -> a) -> [(b, a)]
    baz'' :: b -> (forall b. (forall b. b -> a) -> c) -> (forall c. c -> b)

    baz = undefined
    baz' = undefined
    baz'' = undefined


instance Baz (a -> b)
instance Baz [c]
instance Baz (a, b, c)
instance Baz (a, [b], b, a)


data Quux a b c = Qx a | Qux a b | Quux a b c

instance Foo (Quux a b)
instance Bar (Quux a c) (Quux a b c)
instance Baz (Quux a b c)


class Norf a b where

    type Plugh a c b
    data Thud a c

    norf :: Plugh a c b -> a -> (a -> c) -> b

    norf = undefined


instance Norf Int Bool where

    type Plugh Int [a] Bool = a
    type Plugh Int (a, b) Bool = (a, [b])

    data Thud Int (Quux a [a] c) = Thuud a | Thuuud Int Int
    data Thud Int [a] = Thuuuud Bool


instance Norf [a] [b] where

    type Plugh [a] (Maybe a) [b] = a
    type Plugh [a] [b] [b] = Quux a b (a, b)

    data Thud [a] (a, a, a) = Thd a
