{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

data Foo where
  MkFoo :: forall a. a -> (a -> String) -> Foo

foo :: Foo -> String
foo (MkFoo @a x f) = f (x :: a)

main = do
  print (foo (MkFoo "hello" reverse))
