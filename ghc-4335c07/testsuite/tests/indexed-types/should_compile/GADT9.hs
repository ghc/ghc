{-# LANGUAGE TypeFamilies, GADTs #-}

-- Fails with
--
--   Couldn't match expected type `z' against inferred type `y'
--
-- See also GADT2

module GADT2 where

data EQUAL x y where
  EQUAL :: x~y => EQUAL x y

foo :: EQUAL x y -> EQUAL y z -> x -> z
foo EQUAL EQUAL x = x

