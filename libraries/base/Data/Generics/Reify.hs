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
-- See <http://www.cs.vu.nl/boilerplate/>. The present module provides
-- some preliminary support some sort of structural reflection. This
-- module is presumably less common sense that most other boilerplate
-- modules. Also, it is a bit less easy-going.
--
-----------------------------------------------------------------------------

module Data.Generics.Reify ( 

	-- * Types as values
	TypeVal,		-- view type "a" as "a -> ()"
	typeVal,		-- :: TypeVal a
	sameType,		-- two type values are the same
	val2type,		-- :: a -> TypeVal a
	type2val,		-- :: TypeVal a -> a
	withType,		-- :: a -> TypeVal a -> a
	argType,		-- :: (a -> b) -> TypeVal a
	resType,		-- :: (a -> b) -> TypeVal b
	paraType,		-- :: t a -> TypeVal a
	TypeFun,		-- functions on types
	GTypeFun,		-- polymorphic functions on types
        extType,                -- extend a function on types

	-- * Generic operations to reify terms
	glength,
	gcount,
	gnodecount,
	gtypecount,
	gfindtype,

	-- * Generic operations to reify types
        gmapType,               -- query all constructors of a type
        gmapConstr,             -- query all subterm types of a constructor
	constrArity,            -- compute arity of constructor
        gmapSubtermTypes,       -- query all subterm types of a type
        gmapSubtermTypesConst,  -- variation on gmapSubtermTypes
        gcountSubtermTypes,     -- count all types of immediate subterms
	reachableType,          -- test for reachability on types
        depthOfType,            -- compute minimum depth of type
        depthOfConstr           -- compute minimum depth of constructor

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
sameType tva tvb = typeOf (type2val tva) ==
                   typeOf (type2val tvb)


-- | Map a value to its type
val2type :: a -> TypeVal a
val2type _ = typeVal


-- | Stipulate this idiom!
type2val :: TypeVal a -> a
type2val _ = undefined


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
type GTypeFun r  = forall a. Data a => TypeFun a r


-- | Extend a type function
extType :: (Data a, Typeable r) => GTypeFun r -> TypeFun a r -> GTypeFun r
extType f = maybe f id . cast



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


-- | Find (unambiguously) an immediate subterm of a given type
gfindtype :: (Data x, Data y) => x -> Maybe y
gfindtype = singleton
          . foldl unJust []
          . gmapQ (Nothing `mkQ` Just)
 where
  unJust l (Just x) = x:l
  unJust l Nothing  = l
  singleton [s] = Just s
  singleton _   = Nothing



------------------------------------------------------------------------------
--
--	Generic operations to reify types
--
------------------------------------------------------------------------------


-- | Query all constructors of a given type

gmapType :: ([(Constr,r')] -> r)
         -> GTypeFun (Constr -> r')
         -> GTypeFun r

gmapType (o::[(Constr,r')] -> r) f (t::TypeVal a)
 = 
   o $ zip cons query

 where

  -- All constructors of the given type
  cons :: [Constr]
  cons  = dataTypeCons $ dataTypeOf $ type2val t

  -- Query constructors
  query :: [r']
  query = map (f t) cons


-- | Query all subterm types of a given constructor

gmapConstr :: ([r] -> r')
           -> GTypeFun r
           -> GTypeFun (Constr -> r')

gmapConstr (o::[r] -> r') f (t::TypeVal a) c
 = 
   o $ query

 where

  -- Term for the given constructor
  term :: a
  term = fromConstr c

  -- Query subterm types
  query ::  [r]
  query = gmapQ (f . val2type) term


-- | Compute arity of a given constructor
constrArity :: GTypeFun (Constr -> Int)
constrArity t c = glength $ withType (fromConstr c) t


-- | Query all immediate subterm types of a given type
gmapSubtermTypes :: (Data a, Typeable r) 
         => (r -> r -> r) -> r -> GTypeFun r -> TypeVal a -> r
gmapSubtermTypes o (r::r) f (t::TypeVal a)
  =
    reduce (concat (map (gmapQ (query . val2type)) terms))
           (GTypeFun' f)

 where

  -- All constructors of the given type
  cons :: [Constr]
  cons  = dataTypeCons $ dataTypeOf $ type2val t

  -- Terms for all constructors
  terms :: [a]
  terms =  map fromConstr cons

  -- Query a subterm type
  query :: Data b => TypeVal b -> GTypeFun' r -> (r,GTypeFun' r)
  query t f = (unGTypeFun' f t, GTypeFun' (disable t (unGTypeFun' f)))

  -- Constant out given type
  disable :: Data b => TypeVal b -> GTypeFun r -> GTypeFun r
  disable (t::TypeVal b) f = f `extType` \(_::TypeVal b) -> r

  -- Reduce all subterm types
  reduce :: [GTypeFun' r -> (r,GTypeFun' r)] -> GTypeFun' r -> r
  reduce [] _ = r
  reduce (xy:z) g = fst (xy g) `o` reduce z (snd (xy g))


-- First-class polymorphic variation on GTypeFun
newtype GTypeFun' r = GTypeFun' (GTypeFun r)
unGTypeFun' (GTypeFun' f) = f


-- | Query all immediate subterm types.
--   There is an extra argument to "constant out" the type at hand.
--   This can be used to avoid cycles.

gmapSubtermTypesConst :: (Data a, Typeable r)
                      => (r -> r -> r)
                      -> r
                      -> GTypeFun r 
                      -> TypeVal a 
                      -> r
gmapSubtermTypesConst o (r::r) f (t::TypeVal a)
  =
    gmapSubtermTypes o r f' t
  where
    f' :: GTypeFun r
    f' = f `extType` \(_::TypeVal a) -> r


-- Count all distinct subterm types
gcountSubtermTypes :: Data a => TypeVal a -> Int
gcountSubtermTypes = gmapSubtermTypes (+) (0::Int) (const 1)


-- | A simplied variation on gmapSubtermTypes.
--   Weakness: no awareness of doubles.
--   Strength: easy to comprehend as it uses gmapType and gmapConstr.

_gmapSubtermTypes :: (Data a, Typeable r) 
                  => (r -> r -> r) -> r -> GTypeFun r -> TypeVal a -> r
_gmapSubtermTypes o (r::r) f
  =
    gmapType otype (gmapConstr oconstr f)

 where

  otype :: [(Constr,r)] -> r
  otype = foldr (\x y -> snd x `o` y) r

  oconstr :: [r] -> r
  oconstr = foldr o r


-- | Reachability relation on types, i.e.,
--   test if nodes of type "a" are reachable from nodes of type "b".
--   The relation is defined to be reflexive.

reachableType :: (Data a, Data b) => TypeVal a -> TypeVal b -> Bool
reachableType (a::TypeVal a) (b::TypeVal b)
  =
    or [ sameType a b
       , gmapSubtermTypesConst (\x y -> or [x,y]) False (reachableType a) b
       ]


-- | Depth of a datatype as the constructor with the minimum depth.
--   The outermost "Nothing" denotes a type without constructors.
--   The innermost "Nothing" denotes potentially infinite.

depthOfType :: GTypeFun Bool -> GTypeFun (Maybe (Constr, Maybe Int))
depthOfType p (t::TypeVal a)
  = 
    gmapType o f t

 where
   
  o :: [(Constr, Maybe Int)] -> Maybe (Constr, Maybe Int)
  o l = if null l then Nothing else Just (foldr1 min' l)

  f :: GTypeFun (Constr -> Maybe Int)
  f = depthOfConstr p'

  -- Specific minimum operator
  min' :: (Constr, Maybe Int) -> (Constr, Maybe Int) -> (Constr, Maybe Int)
  min' x (_, Nothing) = x
  min' (_, Nothing) x = x
  min' (c, Just i) (c', Just i') | i <= i' = (c, Just i)
  min' (c, Just i) (c', Just i')           = (c', Just i')

  -- Updated predicate for unblocked types
  p' :: GTypeFun Bool
  p' = p `extType` \(_::TypeVal a) -> False


-- | Depth of a constructor.
--   Depth is viewed as the maximum depth of all subterm types + 1.
--   "Nothing" denotes potentially infinite.

depthOfConstr :: GTypeFun Bool -> GTypeFun (Constr -> Maybe Int)
depthOfConstr p (t::TypeVal a) c
  =
    gmapConstr o f t c

 where

  o :: [Maybe Int] -> Maybe Int
  o = inc' . foldr max' (Just 0)

  f :: GTypeFun (Maybe Int)
  f t' = if p t'
            then
                 case depthOfType p t' of
                   Nothing     -> Just 0
                   Just (_, x) -> x
            else Nothing

  -- Specific maximum operator
  max' Nothing _ = Nothing
  max' _ Nothing = Nothing
  max' (Just i) (Just i') | i >= i' = Just i
  max' (Just i) (Just i')           = Just i'

  -- Specific increment operator
  inc' Nothing = Nothing
  inc' (Just i) = Just (i+1)
