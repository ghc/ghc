{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

data Foo a where
  MkFoo :: Maybe a -> (a -> String) -> Foo a

foo :: Foo String -> String
foo (MkFoo @a (Nothing @b) f) = "nothing"
foo (MkFoo @a (Just @b x) f) = f ((x :: b) :: a)

main = do
  print (foo (MkFoo (Just "hello") reverse))
