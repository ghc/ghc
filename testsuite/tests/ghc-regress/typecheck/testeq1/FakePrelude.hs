{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

--
-- Test case adopted from the HList library
-- http://www.cwi.nl/~ralf/HList/
--

module FakePrelude where


--
-- Type-level Booleans; nothing weird
--
data HTrue; hTrue :: HTrue; hTrue = undefined
data HFalse; hFalse :: HFalse; hFalse = undefined
class HBool x; instance HBool HTrue; instance HBool HFalse
instance Show HTrue where show _ = "HTrue"
instance Show HFalse where show _ = "HFalse"


--
-- Type-level type equality
--
class HBool b => TypeEq x y b | x y -> b


--
-- Value-level incarnation; nothing too weird.
-- Rely on lazy show for type-level Booleans
--
typeEq :: TypeEq t t' b => t -> t' -> b
typeEq = undefined


--
-- Type-level cast
--
class TypeCast x y | x -> y, y -> x
 where
  typeCast :: x -> y
