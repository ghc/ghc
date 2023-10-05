{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

data Foo a b where
  MkFoo :: forall b a. a -> (a -> b -> String) -> Foo a b

foo :: Foo a b -> b -> String
foo (MkFoo @c @d x f) t = f (x :: d) (t :: c)

main = do
  print (foo (MkFoo True (\x y -> show x ++ show y) :: Foo Bool Integer) (6 :: Integer))
