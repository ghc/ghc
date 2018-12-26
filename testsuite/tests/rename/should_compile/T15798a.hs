{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS_GHC -Wmissing-deriving-strategies #-}

module T15798a () where

data Foo a = Foo a
  deriving stock (Eq)

data Bar a = Bar a
  deriving (Eq, Show)
  deriving stock (Ord)
