{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

--
-- Test case adopted from the HList library
-- http://www.cwi.nl/~ralf/HList/
--

module TypeCast where

import FakePrelude

--
-- We are ready to reveal the definition of type cast.
--
instance TypeCast x x where typeCast = id
