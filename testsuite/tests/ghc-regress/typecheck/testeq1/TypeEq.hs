{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

--
-- Test case adopted from the HList library
-- http://www.cwi.nl/~ralf/HList/
--

module TypeEq where

import FakePrelude

--
-- Type-level type equality;
-- defined in terms of type-level cast
--
instance TypeEq x x HTrue
instance (HBool b, TypeCast HFalse b) => TypeEq x y b
--
-- NOTE! instance TypeEq x y HFalse -- would violate functional dependency
--
