{-# LANGUAGE EmptyCase, GADTs #-}

module T13990 where

data T a where
  TInt :: T Int

absurd :: T Bool -> a
absurd v = case v of {}

data Foo = Foo !(T Bool)

absurdFoo :: Foo -> a
absurdFoo (Foo x) = absurd x
