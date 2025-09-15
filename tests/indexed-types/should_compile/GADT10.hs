{-# LANGUAGE TypeFamilies, GADTs, RankNTypes #-}

module GADT10 where

-- [Sept 2010] Now works in GHC 7.0!

-- This fails with
--
-- GADT10.hs:37:0:
--     All of the type variables in the constraint `x ~
--                                                  y' are already in scope
--         (at least one must be universally quantified here)
--     In the type signature for `foo':
--       foo :: EQUAL x y -> ((x ~ y) => t) -> t
-- 
-- GADT10.hs:38:4:
--     Couldn't match expected type `y' against inferred type `x'
--       `y' is a rigid type variable bound by
--           the type signature for `foo' at GADT10.hs:8:15
--       `x' is a rigid type variable bound by
--           the type signature for `foo' at GADT10.hs:8:13
--     In the pattern: EQUAL
--     In the definition of `foo': foo EQUAL t = t
--
-- The first error can be fixed by using FlexibleContexts but I don't think that
-- should be required here. In fact, if we remove RankNTypes, we get
--
--     Illegal polymorphic or qualified type: forall (co_wild_B1 :: x ~
--                                                                  y).
--                                            t
--     In the type signature for `foo':
--       foo :: EQUAL x y -> ((x ~ y) => t) -> t
--
-- which seems to contradict (at least sort of) the first error message.

data EQUAL x y where
  EQUAL :: EQUAL x x

foo :: EQUAL x y -> (x~y => t) -> t
foo EQUAL t = t

bar :: EQUAL x y -> x -> y
bar equ x = foo equ x

