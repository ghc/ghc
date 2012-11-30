{-# LANGUAGE RankNTypes #-}

-- !!! Rank 2 polymorphism
-- Both f and g are rejected by Hugs [April 2001]

module Foo  where

data T = T { t1 :: forall a. a -> a , t2 :: forall a b. a->b->b }

-- Test pattern bindings for polymorphic fields
f :: T -> (Int,Char)
f t = let T { t1 = my_t1 } = t
      in
      (my_t1 3, my_t1 'c')

-- Test record update with polymorphic fields
g :: T -> T
g t = t { t2 = \x y -> y }
