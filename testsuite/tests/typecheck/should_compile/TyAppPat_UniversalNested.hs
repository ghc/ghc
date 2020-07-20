{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

data Foo a where
  MkFoo :: Maybe a -> (a -> String) -> Foo a

foo :: Foo (Maybe String) -> String
foo (MkFoo @a (Nothing @b) f) = "nothing"
foo (MkFoo @a (Just @b x) f) = f ((x :: Maybe a) :: b)

main = do
  print (foo (MkFoo "hello" reverse))
