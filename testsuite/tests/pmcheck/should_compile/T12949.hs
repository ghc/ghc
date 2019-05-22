{-# LANGUAGE ScopedTypeVariables #-}
module T12949 where

class Foo a where
  foo :: Maybe a

data Result a b = Neither | This a | That b | Both a b

q :: forall a b . (Foo a, Foo b) => Result a b
q = case foo :: Maybe a of
      Nothing -> case foo :: Maybe b of
                   Nothing -> Neither
                   Just c -> That c
      Just i -> case foo :: Maybe b of
                   Nothing -> This i
                   Just c -> Both i c
