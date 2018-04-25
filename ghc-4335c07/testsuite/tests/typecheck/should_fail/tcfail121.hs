
{-# LANGUAGE FlexibleInstances #-}

module ShouldFail where

class Foo a where
  op :: a -> a

instance {-# OVERLAPPABLE #-} Foo a => Foo [a] 
instance {-# OVERLAPPING #-} Foo [Int]

foo :: Foo a => [a] -> [a]
foo x = op x
-- Correct instance depends on instantiation of 'a' 
