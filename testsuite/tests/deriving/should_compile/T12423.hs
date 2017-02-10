{-# LANGUAGE DefaultSignatures, DeriveAnyClass #-}
module T12423 where

class Eq1 f where
  (==#) :: Eq a => f a -> f a -> Bool
  default (==#) :: Eq (f a) => f a -> f a -> Bool
  (==#) = (==)

data Foo a = Foo (Either a a)
    deriving (Eq, Eq1)
