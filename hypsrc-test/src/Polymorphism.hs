{-# LANGUAGE RankNTypes #-}


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
quux = undefined

quux' :: forall a. a -> (forall a. a -> a) -> a
quux' = undefined


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
norf = undefined

norf' :: forall a. a -> (forall a. Ord a => a -> a) -> a
norf' = undefined
