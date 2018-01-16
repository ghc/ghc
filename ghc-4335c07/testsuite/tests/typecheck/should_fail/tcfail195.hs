{-# LANGUAGE RankNTypes, GADTs #-}

module ShouldFail where

data Foo a where
  Foo :: Int -> Foo (forall a. a)
