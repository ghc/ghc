{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

data Foo a where
  MkFoo :: a -> (a -> String) -> Foo a

foo :: Foo (Int, [Char], Maybe String -> Bool) -> String
foo (MkFoo @(u, [v], f w -> x) x f) = f (unwords [show @u 5, show @v 'c', show (fmap @f not (Just (True :: x)) :: w)])

main = do
  print (foo (MkFoo (6,"hello") reverse))
