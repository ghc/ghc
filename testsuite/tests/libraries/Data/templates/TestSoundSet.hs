{-


Test instanciations.

: REFMODULE : TESTMODULE  : TESTCOLL    : A   :
-----------------------------------------------
: Set       : Data.Set    : L.Set Int   : Int :
: Set       : Data.Set    : L.Set Nasty : Nasty :
: Set       : Data.IntSet : L.IntSet    : Int :


-}

-- Module to test the interface of Set-like types.

import qualified REFMODULE as R
import qualified TESTMODULE as L

import LibTest
import Control.Monad
import Data.List as List

type REFCOLL = [A]

comparing f = \x y -> f x `compare` f y
testing f = \x y -> f x == f y

rIn :: REFCOLL -> REFCOLL
rIn = List.nub

lIn :: REFCOLL -> TESTCOLL
lIn = L.fromList . rIn

rOut :: REFCOLL -> REFCOLL
rOut = List.sort 

lOut :: TESTCOLL -> REFCOLL
lOut = L.toAscList

main = runTests fileName propNames propTests

--FIXME: better test functions are in order. (non-commutative stuff, things really depending on arguments, etc.)

class TestSet a where
    a_bool :: a -> Bool
    a_a :: a -> a
    a_a_a :: a -> a -> a
    
instance TestSet Int where
    a_bool = even
    a_a = (+1)
    a_a_a = (+)

instance TestSet Nasty where
    a_bool (Nasty' k v) = even (k+v)
    a_a (Nasty' k v) = Nasty' (k+1) v
    a_a_a (Nasty' k1 v1) (Nasty' k2 v2) = Nasty' (k1+k2) (v2+v2)

a_b = a_a


-- !!! EVERYTHING BELOW THIS LINE WILL BE DELETED !!! --
 
prop_null :: REFCOLL -> Bool
prop_null refcoll1
  = structEq (L.null (lIn refcoll1)) (R.null (rIn refcoll1))
 
prop_size :: REFCOLL -> Bool
prop_size refcoll1
  = structEq (L.size (lIn refcoll1)) (R.size (rIn refcoll1))
 
prop_member :: A -> REFCOLL -> Bool
prop_member a1 refcoll2
  = structEq (L.member a1 (lIn refcoll2))
      (R.member a1 (rIn refcoll2))
 
prop_isSubsetOf :: REFCOLL -> REFCOLL -> Bool
prop_isSubsetOf refcoll1 refcoll2
  = structEq (L.isSubsetOf (lIn refcoll1) (lIn refcoll2))
      (R.isSubsetOf (rIn refcoll1) (rIn refcoll2))
 
prop_isProperSubsetOf :: REFCOLL -> REFCOLL -> Bool
prop_isProperSubsetOf refcoll1 refcoll2
  = structEq (L.isProperSubsetOf (lIn refcoll1) (lIn refcoll2))
      (R.isProperSubsetOf (rIn refcoll1) (rIn refcoll2))
 
prop_empty :: Bool
prop_empty = structEq (lOut L.empty) (rOut R.empty)
 
prop_singleton :: A -> Bool
prop_singleton a1
  = structEq (lOut (L.singleton a1)) (rOut (R.singleton a1))
 
prop_insert :: A -> REFCOLL -> Bool
prop_insert a1 refcoll2
  = structEq (lOut (L.insert a1 (lIn refcoll2)))
      (rOut (R.insert a1 (rIn refcoll2)))
 
prop_delete :: A -> REFCOLL -> Bool
prop_delete a1 refcoll2
  = structEq (lOut (L.delete a1 (lIn refcoll2)))
      (rOut (R.delete a1 (rIn refcoll2)))
 
prop_union :: REFCOLL -> REFCOLL -> Bool
prop_union refcoll1 refcoll2
  = structEq (lOut (L.union (lIn refcoll1) (lIn refcoll2)))
      (rOut (R.union (rIn refcoll1) (rIn refcoll2)))
 
prop_unions :: [REFCOLL] -> Bool
prop_unions refcoll1
  = structEq (lOut (L.unions (fmap lIn refcoll1)))
      (rOut (R.unions (fmap rIn refcoll1)))
 
prop_difference :: REFCOLL -> REFCOLL -> Bool
prop_difference refcoll1 refcoll2
  = structEq (lOut (L.difference (lIn refcoll1) (lIn refcoll2)))
      (rOut (R.difference (rIn refcoll1) (rIn refcoll2)))
 
prop_intersection :: REFCOLL -> REFCOLL -> Bool
prop_intersection refcoll1 refcoll2
  = structEq (lOut (L.intersection (lIn refcoll1) (lIn refcoll2)))
      (rOut (R.intersection (rIn refcoll1) (rIn refcoll2)))
 
prop_filter :: REFCOLL -> Bool
prop_filter refcoll1
  = structEq (lOut (L.filter a_bool (lIn refcoll1)))
      (rOut (R.filter a_bool (rIn refcoll1)))
 
prop_partition :: REFCOLL -> Bool
prop_partition refcoll1
  = structEq
      (let (set_a2, set_a3) = L.partition a_bool (lIn refcoll1) in
         (lOut set_a2, lOut set_a3))
      (let (set_a2, set_a3) = R.partition a_bool (rIn refcoll1) in
         (rOut set_a2, rOut set_a3))
 
prop_map :: REFCOLL -> Bool
prop_map refcoll1
  = structEq (lOut (L.map a_b (lIn refcoll1)))
      (rOut (R.map a_b (rIn refcoll1)))
 
prop_elems :: REFCOLL -> Bool
prop_elems refcoll1
  = structEq (L.elems (lIn refcoll1)) (R.elems (rIn refcoll1))
 
prop_toList :: REFCOLL -> Bool
prop_toList refcoll1
  = structEq (L.toList (lIn refcoll1)) (R.toList (rIn refcoll1))
 
prop_toAscList :: REFCOLL -> Bool
prop_toAscList refcoll1
  = structEq (L.toAscList (lIn refcoll1))
      (R.toAscList (rIn refcoll1))
