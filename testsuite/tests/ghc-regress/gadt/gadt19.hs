{-# OPTIONS -fglasgow-exts #-}

-- Involves an equality that is not an existential

module Foo2 where

data T t a where
  T :: a -> T () a

foo :: (a -> a) -> T t a -> T t a
foo f (T x) = T (f x)

bar :: T t Int -> T t Int
bar t@(T _) = foo (+1) t


