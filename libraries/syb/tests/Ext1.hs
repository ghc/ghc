{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE CPP #-}

module Ext1 (tests) where

{-

This example records some experiments with polymorphic datatypes.

-}

import Test.Tasty.HUnit

import Data.Generics
import GHC.Exts (unsafeCoerce#)
#if MIN_VERSION_base(4,8,0)
import GHC.Base hiding(foldr)
#else
import GHC.Base
#endif

-- Unsafe coerce
unsafeCoerce :: a -> b
unsafeCoerce = unsafeCoerce#


-- Handy type constructors
newtype ID x = ID { unID :: x }
newtype CONST c a = CONST { unCONST :: c }


-- Extension of a query with a para. poly. list case
extListQ' :: Data d
          => (d -> q)
          -> (forall d. [d] -> q)
          -> d -> q
extListQ' def ext d =
  if isList d
    then ext (unsafeCoerce d)
    else def d


-- Test extListQ'
foo1 :: Data d => d -> Int
foo1 = const 0 `extListQ'` length
t1 = foo1 True -- should count as 0
t2 = foo1 [True,True] -- should count as 2


-- Infeasible extension of a query with a data-polymorphic list case
extListQ'' :: Data d
           => (d -> q)
           -> (forall d. Data d => [d] -> q)
           -> d -> q
extListQ'' def ext d =
  if isList d
    then undefined -- hard to avoid an ambiguous type
    else def d


-- Test extListQ from Data.Generics.Aliases
foo2 :: Data a => a -> Int
foo2 = const 0 `ext1Q` list
 where
  list :: Data a => [a] -> Int
  list l = foldr (+) 0 $ map glength l

t3 = foo2 (True,True) -- should count as 0
t4 = foo2 [(True,True),(True,True)] -- should count as 2+2=4


-- Customisation for lists without type cast
foo3 :: Data a => a -> Int
foo3 x = if isList x
          then foldr (+) 0 $ gmapListQ glength x
          else 0

t5 = foo3 (True,True) -- should count as 0
t6 = foo3 [(True,True),(True,True)] -- should count as 2+2=4


-- Test for list datatype
isList :: Data a => a -> Bool
isList x = typeRepTyCon (typeOf x) ==
           typeRepTyCon (typeOf (undefined::[()]))


-- Test for nil
isNil :: Data a => a -> Bool
isNil x = toConstr x == toConstr ([]::[()])


-- Test for cons
isCons :: Data a => a -> Bool
isCons x = toConstr x == toConstr (():[])


-- gmapQ for polymorphic lists
gmapListQ :: forall a q. Data a => (forall a. Data a => a -> q) -> a -> [q]
gmapListQ f x =
  if not $ isList x
    then error "gmapListQ"
    else if isNil x
           then []
           else if isCons x
                  then ( gmapQi 0 f x : gmapQi 1 (gmapListQ f) x )
                  else error "gmapListQ"


-- Build nil
mkNil :: Data a => a
mkNil = fromConstr $ toConstr ([]::[()])


-- Build cons
mkCons :: Data a => a
mkCons = fromConstr $ toConstr ((undefined:undefined)::[()])


-- Main function for testing
tests = ( t1
        , ( t2
        , ( t3
        , ( t4
        , ( t5
        , ( t6
        )))))) @=? output

output = (0,(2,(0,(4,(0,4)))))
