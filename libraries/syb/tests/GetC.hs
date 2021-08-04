{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}

module GetC (tests) where

import Test.Tasty.HUnit

{-

Ralf Laemmel, 5 November 2004

Joe Stoy suggested the idiom to test for the outermost constructor.

Given is a term t
and a constructor f (say the empty constructor application).

isC f t returns True if the outermost constructor of t is f.
isC f t returns False otherwise.
Modulo type checking, i.e., the data type of f and t must be the same.
If not, we want to see a type error, of course.

-}

import Data.Typeable  -- to cast t's subterms, which will be reused for f.
import Data.Generics  -- to access t's subterms and constructors.


-- Some silly data types
data T1 = T1a Int String | T1b String Int     deriving (Typeable, Data)
data T2 = T2a Int Int    | T2b String String  deriving (Typeable, Data)
data T3 = T3  !Int                            deriving (Typeable, Data)


-- Test cases
tests = show [ isC T1a (T1a 1 "foo")   -- typechecks, returns True
             , isC T1a (T1b "foo" 1)   -- typechecks, returns False
             , isC T3  (T3 42)]        -- works for strict data too
        @=? output
-- err = show $ isC T2b (T1b "foo" 1)  -- must not typecheck

output = show [True,False,True]

--
-- We look at a datum a.
-- We look at a constructor function f.
-- The class GetT checks that f constructs data of type a.
-- The class GetC computes maybe the constructor ...
-- ... if the subterms of the datum at hand fit for f.
-- Finally we compare the constructors.
--

isC :: (Data a, GetT f a, GetC f) => f -> a -> Bool
isC f t = maybe False ((==) (toConstr t)) con
 where
  kids = gmapQ ExTypeable t -- homogenify subterms in list for reuse
  con  = getC f kids        -- compute constructor from constructor application


--
-- We prepare for a list of kids using existential envelopes.
-- We could also just operate on TypeReps for non-strict datatypes.
--

data ExTypeable = forall a. Typeable a => ExTypeable a
unExTypeable (ExTypeable a) = cast a


--
-- Compute the result type of a function type.
-- Beware: the TypeUnify constraint causes headache.
-- We can't have GetT t t because the FD will be violated then.
-- We can't omit the FD because unresolvable overlapping will hold then.
--

class GetT f t | f -> t -- FD is optional
instance GetT g t => GetT (x -> g) t
instance TypeUnify t t' => GetT t t'


--
-- Obtain the constructor if term can be completed
--

class GetC f
 where
  getC :: f -> [ExTypeable] -> Maybe Constr

instance (Typeable x, GetC g) => GetC (x -> g)
 where
  getC _ [] = Nothing
  getC (f::x->g) (h:t)
    =
      do
         (x::x) <- unExTypeable h
         getC (f x) t

instance Data t => GetC t
 where
  getC y []    = Just $ toConstr y
  getC _ (_:_) = Nothing


--
-- Type unification; we could try this:
--  class TypeUnify a b | a -> b, b -> a
--  instance TypeUnify a a
--
-- However, if the instance is placed in the present module,
-- then type improvement would inline this instance. Sigh!!!
--
-- So we need type unification with type improvement blocker
-- The following solution works with GHC for ages.
-- Other solutions; see the HList paper.
--

class    TypeUnify   a  b   |    a -> b,   b -> a
class    TypeUnify'  x  a b |  x a -> b, x b -> a
class    TypeUnify'' x  a b |  x a -> b, x b -> a
instance TypeUnify'  () a b => TypeUnify    a b
instance TypeUnify'' x  a b => TypeUnify' x a b
instance TypeUnify'' () a a
