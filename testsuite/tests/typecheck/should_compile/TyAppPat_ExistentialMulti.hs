{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

data Foo where
  MkFoo :: forall a b. a -> (a -> String) -> b -> (b -> String) -> Foo

foo :: Foo -> String
foo (MkFoo @u @v x f y g) = f (x :: u) ++ g (y :: v)

main = do
  print (foo (MkFoo "hello" reverse True show))
