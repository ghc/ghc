{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module T2203b where

class Foo a where
   type TheFoo a
   foo :: TheFoo a -> a
   foo' :: a -> Int

class Bar b where
   bar :: b -> Int

instance (b ~ TheFoo a, Foo a) => Bar (Either a b) where
   bar (Left a) = foo' a
   bar (Right b) = foo' (foo b :: a)

instance Foo Int where
   type TheFoo Int = Int
   foo = id
   foo' = id

val :: Either Int Int
val = Left 5

res :: Int
res = bar val