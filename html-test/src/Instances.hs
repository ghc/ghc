{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


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

    bar = undefined
    bar' = undefined

instance Bar Maybe Bool
instance Bar Maybe [a]
instance Bar [] (a, a)
instance Foo f => Bar (Either a) (f a)
instance Foo ((,,) a b) => Bar ((,,) a b) (a, b, a)
