{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

data Foo a = F Int | A a
  deriving Show

data Foo1 a = F1 Int | A1 a
  deriving (Show)

data Foo2 a = F2 Int | A2 a
  deriving (Show, Eq)

data FooStock = FS Int
  deriving stock Show

data FooAnyClass = Fa Int
  deriving anyclass Show

newtype FooNewType = Fn Int
  deriving newtype (Show)
