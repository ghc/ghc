{-# LANGUAGE DataKinds, GADTs #-}

module T11611 where

data A a where
  A :: A (a:as) -> a -> A as

deriving instance Show a => Show (A a)
