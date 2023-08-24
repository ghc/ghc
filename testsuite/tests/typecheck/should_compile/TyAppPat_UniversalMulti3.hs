{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

data Foo a where
  MkFoo :: a -> (a -> String) -> Foo a

foo :: Foo String -> Foo String -> String
foo (MkFoo @a x f) (MkFoo @b y g) = f (x :: a) ++ g (y :: b)

main = do
  print (foo (MkFoo "hello" reverse) (MkFoo "goodbye" reverse))
