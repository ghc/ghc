{- Check that the context of a type does not
   constrain any in-scope variables, and only constrains
   type variables free in the type.
-}

module Foo where

instance Eq a => Eq Bool where
  (==) = error "help"


f :: Eq a => Int -> Int
f x = x


class Foo a where
  op :: Eq a => a -> a
