{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

data Foo :: * -> * -> * where
  MkFoo :: forall b a. a -> (a -> b -> String) -> Foo a b

foo :: Foo a b -> b -> String
foo (MkFoo @c @d x f) t = f (x :: d) (t :: c)

main = do
  print (foo (MkFoo True (\x y -> show x ++ show y) :: Foo Bool Integer) (6 :: Integer))
