{-# OPTIONS -fglasgow-exts #-}

{-

The following examples illustrate the reification facilities for type
structure. Most notably, we generate shallow terms using the depth of
types and constructors as means to steer the generation.

-}

module Main where
import Data.Maybe
import Data.Generics
import Control.Monad.State
import CompanyDatatypes



------------------------------------------------------------------------------
--
--	Encoding types as values; some other way.
--
------------------------------------------------------------------------------

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
extType f x = maybe f id (cast x)



------------------------------------------------------------------------------
--
--	Mapping operators to map over type structure
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
  cons  = if isAlgType $ dataTypeOf $ type2val t
           then dataTypeConstrs $ dataTypeOf $ type2val t
	   else []

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
  cons  = if isAlgType $ dataTypeOf $ type2val t
           then dataTypeConstrs $ dataTypeOf $ type2val t
           else []

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
--   There is an extra argument to \"constant out\" the type at hand.
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


------------------------------------------------------------------------------
--
--	Some reifying relations on types
--
------------------------------------------------------------------------------


-- | Reachability relation on types, i.e.,
--   test if nodes of type @a@ are reachable from nodes of type @b@.
--   The relation is defined to be reflexive.

reachableType :: (Data a, Data b) => TypeVal a -> TypeVal b -> Bool
reachableType (a::TypeVal a) (b::TypeVal b)
  =
    or [ sameType a b
       , gmapSubtermTypesConst (\x y -> or [x,y]) False (reachableType a) b
       ]


-- | Depth of a datatype as the constructor with the minimum depth.
--   The outermost 'Nothing' denotes a type without constructors.
--   The innermost 'Nothing' denotes potentially infinite.

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
--   'Nothing' denotes potentially infinite.

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


------------------------------------------------------------------------------
--
--	Build a shallow term 
--
------------------------------------------------------------------------------

shallowTerm :: (forall a. Data a => Maybe a) -> (forall b. Data b => b)
shallowTerm cust :: b
  =
    maybe gdefault id cust

 where

  -- The worker, also used for type disambiguation
  gdefault :: b
  gdefault = case con of
              Just (con, Just _) -> fromConstrB (shallowTerm cust) con
              _ -> error "no shallow term!"

  -- The type to be constructed
  typeVal :: TypeVal b
  typeVal = val2type gdefault

  -- The most shallow constructor if any 
  con :: Maybe (Constr, Maybe Int)
  con = depthOfType (const True) typeVal



-- For testing shallowTerm
shallowTermBase :: GenericR Maybe
shallowTermBase =        Nothing 
                  `extR` Just (1.23::Float)
                  `extR` Just ("foo"::String)



-- Sample datatypes
data T1 = T1a               deriving (Typeable, Data) -- just a constant
data T2 = T2 T1             deriving (Typeable, Data) -- little detour
data T3 = T3a T3 | T3b T2   deriving (Typeable, Data) -- recursive case
data T4 = T4 T3 T3          deriving (Typeable, Data) -- sum matters



-- Sample type arguments
t0 = typeVal :: TypeVal Int
t1 = typeVal :: TypeVal T1
t2 = typeVal :: TypeVal T2
t3 = typeVal :: TypeVal T3
t4 = typeVal :: TypeVal T4
tCompany  = typeVal :: TypeVal Company
tPerson   = typeVal :: TypeVal Person
tEmployee = typeVal :: TypeVal Employee
tDept     = typeVal :: TypeVal Dept



-- Test cases
test0   = t1 `reachableType` t1 -- True
test1   = t1 `reachableType` t2 -- True
test2   = t2 `reachableType` t1 -- False
test3   = t1 `reachableType` t3
test4   = tPerson `reachableType` tCompany
test5   = gcountSubtermTypes tPerson
test6   = gcountSubtermTypes tEmployee
test7   = gcountSubtermTypes tDept
test8   = shallowTerm shallowTermBase :: Person
test9   = shallowTerm shallowTermBase :: Employee
test10  = shallowTerm shallowTermBase :: Dept



main = print $ (   test0
               , ( test1
               , ( test2
               , ( test3
               , ( test4
               , ( test5
               , ( test6
               , ( test7
               , ( test8
               , ( test9
               , ( test10
               )))))))))))
