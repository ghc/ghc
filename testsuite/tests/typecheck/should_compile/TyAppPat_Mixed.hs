{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

data Foo b where
  MkFoo :: forall b a. a -> (b -> a -> String) -> Foo b

foo :: Foo b -> b -> String
foo (MkFoo @b @a x f) u = f (u :: b) (x :: a)

main = do
  print (foo (MkFoo "hello" (\x y -> reverse y ++ show x)) (6 :: Integer))
