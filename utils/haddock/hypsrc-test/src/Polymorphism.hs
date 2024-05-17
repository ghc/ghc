{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Polymorphism where


foo :: a -> a -> a
foo = undefined

foo' :: forall a. a -> a -> a
foo' = undefined

bar :: a -> b -> (a, b)
bar = undefined

bar' :: forall a b. a -> b -> (a, b)
bar' = undefined

baz :: a -> (a -> [a -> a] -> b) -> b
baz = undefined

baz' :: forall a b. a -> (a -> [a -> a] -> b) -> b
baz' = undefined

quux :: a -> (forall a. a -> a) -> a
quux x f = f x

quux' :: forall a. a -> (forall a. a -> a) -> a
quux' x f = f x


num :: Num a => a -> a -> a
num = undefined

num' :: forall a. Num a => a -> a -> a
num' = undefined

eq :: (Eq a, Eq b) => [a] -> [b] -> (a, b)
eq = undefined

eq' :: forall a b. (Eq a, Eq b) => [a] -> [b] -> (a, b)
eq' = undefined

mon :: Monad m => (a -> m a) -> m a
mon = undefined

mon' :: forall m a. Monad m => (a -> m a) -> m a
mon' = undefined


norf :: a -> (forall a. Ord a => a -> a) -> a
norf x f = x

norf' :: forall a. a -> (forall a. Ord a => a -> a) -> a
norf' x f = x


plugh :: forall a. a -> a
plugh x = x :: a

thud :: forall a b. (a -> b) -> a -> (a, b)
thud f x =
    (x :: a, y) :: (a, b)
  where
    y = (f :: a -> b) x :: b
