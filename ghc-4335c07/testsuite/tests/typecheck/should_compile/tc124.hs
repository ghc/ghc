{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -O #-}    -- -O casused a Lint error in the simplifier, so I'm putting that in
                          -- all the time, so we don't miss it in a fast validate

-- !!! Rank 2 polymorphism
-- Both f and g are rejected by Hugs [April 2001]

module Foo  where

data T = T { t1 :: forall a. a -> a
           , t2 :: forall b c. b->c->c }

-- Test pattern bindings for polymorphic fields
f :: T -> (Int,Char, Char)
f t = let T { t1 = my_t1, t2 = my_t2 } = t
      in
      (my_t1 3, my_t1 'c', my_t2 2 'c')

f2 :: T -> (Int,Char, Char)
f2 t = let T { t1 = my_t1, t2 = my_t2 } = t
       in
       (my_t1 3, my_t1 'c', my_t2 2 'c')

-- Test record update with polymorphic fields
g :: T -> T
g t = t { t2 = \x y -> y }
