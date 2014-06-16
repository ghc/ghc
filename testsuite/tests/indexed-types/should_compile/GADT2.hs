{-# LANGUAGE TypeFamilies, GADTs #-}

-- Fails with
--
--   Couldn't match expected type `y' against inferred type `x'

module GADT2 where

data EQUAL x y where
  EQUAL :: x~y => EQUAL x y

foo :: EQUAL x y -> x -> y
foo EQUAL x = x

