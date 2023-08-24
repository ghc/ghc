{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Data.Maybe

data Foo a where
  MkFoo :: a -> (String -> String) -> Foo a

foo :: Foo (Int, [Char], Maybe String -> Bool) -> String
foo (MkFoo @(u, [v], f w -> x) x f) = f (unwords [show @u 5, show @v 'c', show (fmap @f not (Just (True :: x))) :: w])

main = do
  print (foo (MkFoo (6,"hello",isJust) reverse))
