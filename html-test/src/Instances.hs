{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}


module Instances where


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
