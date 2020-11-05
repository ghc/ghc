{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module T18914 where

type T f = forall a. f a

class C f where
  m1 :: T f
  m2 :: forall a. f a

newtype N f a = MkN (f a)
  deriving C
