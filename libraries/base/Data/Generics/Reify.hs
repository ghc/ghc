-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Reify
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.vu.nl/boilerplate/>.
--
-----------------------------------------------------------------------------

module Data.Generics.Reify ( 

	-- * Types as values
	TypeVal,		-- view type "a" as "a -> ()"
	typeVal,		-- :: TypeVal a
	sameType,		-- two type values are the same
	typeValOf,		-- :: a -> TypeVal a
	undefinedType,		-- :: TypeVal a -> a
	withType,		-- :: a -> TypeVal a -> a
	argType,		-- :: (a -> b) -> TypeVal a
	resType,		-- :: (a -> b) -> TypeVal b
	paraType,		-- :: t a -> TypeVal a
	TypeFun,		-- functions on types

	-- * Generic operations to reify terms
	glength,
	gcount,
	gnodecount,
	gtypecount,

	-- * Generic operations to reify types
	constrArity,
	typeReachableFrom

 ) where


------------------------------------------------------------------------------


import Data.Generics.Basics
import Data.Generics.Aliases
import Data.Generics.Schemes



-------------------------------------------------------------
--
--	Types as values
--
-------------------------------------------------------------

{- 

This group provides a style of encoding types as values and using
them. This style is seen as an alternative to the pragmatic style used
in Data.Typeable.typeOf and elsewhere, i.e., simply use an "undefined"
to denote a type argument. This pragmatic style suffers from lack
of robustness: one feels tempted to pattern match on undefineds.
Maybe Data.Typeable.typeOf etc. should be rewritten accordingly.

-}


-- | Type as values to stipulate use of undefineds
type TypeVal a = a -> ()


-- | The value that denotes a type
typeVal :: TypeVal a
typeVal = const ()


-- | Test for type equivalence
sameType :: (Typeable a, Typeable b) => TypeVal a -> TypeVal b -> Bool
sameType tva tvb = typeOf (undefinedType tva) ==
                   typeOf (undefinedType tvb)


-- | Map a value to its type
typeValOf :: a -> TypeVal a
typeValOf _ = typeVal


-- | Stipulate this idiom!
undefinedType :: TypeVal a -> a
undefinedType _ = undefined


-- | Constrain a type
withType :: a -> TypeVal a -> a
withType x _ = x


-- | The argument type of a function
argType :: (a -> b) -> TypeVal a
argType _ = typeVal


-- | The result type of a function
resType :: (a -> b) -> TypeVal b
resType _ = typeVal


-- | The parameter type of type constructor
paraType :: t a -> TypeVal a
paraType _ = typeVal


-- Type functions,
-- i.e., functions mapping types to values
--
type TypeFun a r = TypeVal a -> r



-- Generic type functions,
-- i.e., functions mapping types to values
--
type GTypeFun r  = forall a. Typeable a => TypeFun a r



------------------------------------------------------------------------------
--
--	Generic operations to reify terms
--
------------------------------------------------------------------------------


-- | Count the number of immediate subterms of the given term
glength :: GenericQ Int
glength = length . gmapQ (const ())


-- | Determine the number of all suitable nodes in a given term
gcount :: GenericQ Bool -> GenericQ Int
gcount p =  everything (+) (\x -> if p x then 1 else 0)


-- | Determine the number of all nodes in a given term
gnodecount :: GenericQ Int
gnodecount = gcount (const True)


-- | Determine the number of nodes of a given type in a given term
gtypecount :: Typeable a => (a -> ()) -> GenericQ Int
gtypecount f = gcount (False `mkQ` (const True . f))



------------------------------------------------------------------------------
--
--	Generic operations to reify types
--
------------------------------------------------------------------------------

-- | Compute arity of a constructor against a type argument
constrArity :: Data a => (a -> ()) -> Constr -> Int
constrArity ta c = glength $ withType (fromConstr c) ta


--
-- Reachability relation on types:
--  Test if nodes of type "a" are reachable from nodes of type "b".
--  This is a naive, inefficient encoding.
--  As of writing, it does not even cope with recursive types.
--
typeReachableFrom :: (Data a, Data b) => TypeVal a -> TypeVal b -> Bool
typeReachableFrom (a::TypeVal a) (b::TypeVal b) =
  or ( sameType a b
     : map (recurse . fromConstr) (dataTypeCons $ dataTypeOf b)
     )
  where

    -- See if a is reachable from immediate subterms of a kind of b 
    recurse :: b -> Bool
    recurse = or
            . gmapQ ( typeReachableFrom a 
                    . typeValOf
                    )
