{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

data Foo a b where
  MkFoo :: a -> (a -> String) -> b -> Foo a b

-- Shouldn't work because we don't accept multiple occurrences of a binding variable.
foo :: Foo String String -> String
foo (MkFoo @a @a x f y) = f (x ++ y :: a)

main = do
  print (foo (MkFoo "hello" reverse "goodbye"))
