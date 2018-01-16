{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleInstances #-}

module T2203a where

class Foo a where
   type TheFoo a
   foo :: TheFoo a -> a
   foo' :: a -> Int

class Bar b where
   bar :: b -> Int

instance Foo a => Bar (Either a (TheFoo a)) where
   bar (Left a) = foo' a
   bar (Right b) = foo' (foo b :: a)
