{-# LANGUAGE GADTs, StandaloneDeriving #-}

module T4528 where

data Foo a where
   A, B :: Foo Int
   C :: Foo Bool

deriving instance Enum (Foo a)
deriving instance Bounded (Foo a)

