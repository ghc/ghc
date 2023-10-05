{-# Options_GHC -dcore-lint #-}
{-# Language PartialTypeSignatures #-}

module T22065 where

data Foo where
  Apply :: (x -> Int) -> x -> Foo

foo :: Foo
foo = Apply f x :: forall a. _ where

  f :: [_] -> Int
  f = length @[] @_

  x :: [_]
  x = mempty @[_]

{-
Smaller version I used when debuggging

apply :: (x->Int) -> x -> Bool
apply = apply

foo :: Bool
foo = apply f x :: forall a. _
    where
      f = length @[]
      x = mempty

-}
