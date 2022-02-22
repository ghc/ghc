{-# LANGUAGE
    DataKinds,
    GADTs,
    QuantifiedConstraints,
    TypeFamilies
  #-}

module T20231 where

type family Foo x where
  Foo (Maybe a) = a

data Bug1 a b c where
  Bug1 :: (a ~ 'True => Show b, a ~ 'False => Show c) => Bug1 a b c

data Bug2 f a b where
  Bug2 :: (a ~ 'True => f b) => Bug2 f a b

bug :: b ~ Maybe (Foo b) => Bug2 Show a (Foo b) -> Bug1 a (Foo b) Int
bug Bug2 = Bug1
