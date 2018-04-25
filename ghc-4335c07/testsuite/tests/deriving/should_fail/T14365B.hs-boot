{-# LANGUAGE StandaloneDeriving #-}
module T14365B where

data Foo a
  deriving (Functor)

deriving instance Foldable Foo
