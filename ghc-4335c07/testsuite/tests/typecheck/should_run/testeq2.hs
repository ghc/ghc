
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, FlexibleContexts,
             OverlappingInstances, UndecidableInstances #-}

--
-- Test case adopted from the HList library
-- http://www.cwi.nl/~ralf/HList/
--
-- Tests functional dependencies, overlapping instances....

module Main where


--
-- Type-level Booleans; nothing weird
--
data HTrue; hTrue :: HTrue; hTrue = undefined
data HFalse; hFalse :: HFalse; hFalse = undefined
class HBool x; instance HBool HTrue; instance HBool HFalse
instance Show HTrue where show _ = "HTrue"
instance Show HFalse where show _ = "HFalse"


--
-- Value-level incarnation; nothing too weird.
-- Rely on lazy show for type-level Booleans
--
typeEq :: TypeEq t t' b => t -> t' -> b
typeEq = undefined


--
-- Type-level cast
--
class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x
 

--
-- Type-level type equality
--

class TypeEq' () x y b => TypeEq x y b | x y -> b
class TypeEq' q x y b | q x y -> b
class TypeEq'' q x y b | q x y -> b
instance TypeEq' () x y b => TypeEq x y b
-- This instance used to work <= GHC 6.2
-- instance TypeEq' () x x HTrue
-- There were some problems however with GHC CVS 6.3.
-- So we favour the following, more stable (?) instance instead.
instance TypeCast b HTrue => TypeEq' () x x b
instance TypeEq'' q x y b => TypeEq' q x y b
instance TypeEq'' () x y HFalse


--
-- Let's test.
-- The following should print "(HTrue,HFalse)".
--

main = print $ ( typeEq "42" "88"
               , typeEq "42" (42::Int)
               )
