{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

data Foo a where
  MkFoo :: a -> (a -> String) -> Foo a

foo :: Foo String -> String
foo (MkFoo @a x f) = f (x :: a)

main = do
  print (foo (MkFoo "hello" reverse))
