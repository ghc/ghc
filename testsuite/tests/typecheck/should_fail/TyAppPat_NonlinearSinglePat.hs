{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Main where

data Foo a where
  MkFoo :: a -> (a -> String) -> Foo a

-- Shouldn't work because we don't accept multiple occurrences of a binding variable.
foo :: Foo (String, String) -> String
foo (MkFoo @(a,a) (x,y) f) = f (x :: a, y :: a)

main = do
  print (foo (MkFoo ("hello", "goodbye") (\(x,y) -> reverse y ++ reverse x)))
