{-


Test instanciations.

: REFMODULE : TESTMODULE  : TESTCOLL        : K     : A   :
-----------------------------------------------------------
: Map       : Data.Map    : L.Map Int Int   : Int   : Int :
: Map       : Data.Map    : L.Map Nasty Int : Nasty : Int :
: Map       : Data.IntMap : L.IntMap Int    : Int   : Int :


-}

-- Module to test the interface of Map-like types.

-- These are all
-- black-box testing: we never check the internal data structures.
-- We are thus independant of the underlying representation.

import qualified REFMODULE as R
import qualified TESTMODULE as L

import LibTest
import Control.Monad
import Data.List as List

type REFCOLL = [(K,A)]

comparing f = \x y -> f x `compare` f y
testing f = \x y -> f x == f y

rIn :: REFCOLL -> REFCOLL
rIn = List.nubBy (testing fst)

lIn :: REFCOLL -> TESTCOLL
lIn = L.fromList . rIn

rOut :: REFCOLL -> REFCOLL
rOut = List.sortBy (comparing fst) 

lOut :: TESTCOLL -> REFCOLL
lOut = L.toAscList

main = runTests fileName propNames propTests

--FIXME: better test functions are in order. (non-commutative stuff, things really depending on arguments, etc.)

class TestSet k a where
    k_a_a :: k -> a -> a
    
instance TestSet Int Int where
    k_a_a k a = k+a

instance TestSet Nasty Int where
    k_a_a k a = nastyKey k+a

a_a_a = (+)

k_a_a_a k = a_a_a

a_a = (+1)

a_maybe_a a = if even a then Nothing else Just a

k_a_maybe_a k = a_maybe_a

a_b_maybe_a a b = if even a then Nothing else Just (a+b)

k_a_b_maybe_a k a b = if even a then Nothing else Just (a+b)

a_b_c = a_a_a

k_a_b = k_a_a

k_a_b_c = k_a_a_a


-- !!! EVERYTHING BELOW THIS LINE WILL BE DELETED !!! --
 
prop_null :: REFCOLL -> Bool
prop_null refcoll1
  = structEq (L.null (lIn refcoll1)) (R.null (rIn refcoll1))
 
prop_size :: REFCOLL -> Bool
prop_size refcoll1
  = structEq (L.size (lIn refcoll1)) (R.size (rIn refcoll1))
 
prop_member :: K -> REFCOLL -> Bool
prop_member k1 refcoll2
  = structEq (L.member k1 (lIn refcoll2))
      (R.member k1 (rIn refcoll2))
 
prop_findWithDefault :: A -> K -> REFCOLL -> Bool
prop_findWithDefault a1 k2 refcoll3
  = structEq (L.findWithDefault a1 k2 (lIn refcoll3))
      (R.findWithDefault a1 k2 (rIn refcoll3))
 
prop_empty :: Bool
prop_empty = structEq (lOut L.empty) (rOut R.empty)
 
prop_singleton :: K -> A -> Bool
prop_singleton k1 a2
  = structEq (lOut (L.singleton k1 a2)) (rOut (R.singleton k1 a2))
 
prop_insert :: K -> A -> REFCOLL -> Bool
prop_insert k1 a2 refcoll3
  = structEq (lOut (L.insert k1 a2 (lIn refcoll3)))
      (rOut (R.insert k1 a2 (rIn refcoll3)))
 
prop_insertWith :: K -> A -> REFCOLL -> Bool
prop_insertWith k1 a2 refcoll3
  = structEq (lOut (L.insertWith a_a_a k1 a2 (lIn refcoll3)))
      (rOut (R.insertWith a_a_a k1 a2 (rIn refcoll3)))
 
prop_insertWithKey :: K -> A -> REFCOLL -> Bool
prop_insertWithKey k1 a2 refcoll3
  = structEq (lOut (L.insertWithKey k_a_a_a k1 a2 (lIn refcoll3)))
      (rOut (R.insertWithKey k_a_a_a k1 a2 (rIn refcoll3)))
 
prop_insertLookupWithKey :: K -> A -> REFCOLL -> Bool
prop_insertLookupWithKey k1 a2 refcoll3
  = structEq
      (let (maybe_a4, map_k_a5)
             = L.insertLookupWithKey k_a_a_a k1 a2 (lIn refcoll3)
         in (maybe_a4, lOut map_k_a5))
      (let (maybe_a4, map_k_a5)
             = R.insertLookupWithKey k_a_a_a k1 a2 (rIn refcoll3)
         in (maybe_a4, rOut map_k_a5))
 
prop_delete :: K -> REFCOLL -> Bool
prop_delete k1 refcoll2
  = structEq (lOut (L.delete k1 (lIn refcoll2)))
      (rOut (R.delete k1 (rIn refcoll2)))
 
prop_adjust :: K -> REFCOLL -> Bool
prop_adjust k1 refcoll2
  = structEq (lOut (L.adjust a_a k1 (lIn refcoll2)))
      (rOut (R.adjust a_a k1 (rIn refcoll2)))
 
prop_adjustWithKey :: K -> REFCOLL -> Bool
prop_adjustWithKey k1 refcoll2
  = structEq (lOut (L.adjustWithKey k_a_a k1 (lIn refcoll2)))
      (rOut (R.adjustWithKey k_a_a k1 (rIn refcoll2)))
 
prop_update :: K -> REFCOLL -> Bool
prop_update k1 refcoll2
  = structEq (lOut (L.update a_maybe_a k1 (lIn refcoll2)))
      (rOut (R.update a_maybe_a k1 (rIn refcoll2)))
 
prop_updateWithKey :: K -> REFCOLL -> Bool
prop_updateWithKey k1 refcoll2
  = structEq (lOut (L.updateWithKey k_a_maybe_a k1 (lIn refcoll2)))
      (rOut (R.updateWithKey k_a_maybe_a k1 (rIn refcoll2)))
 
prop_updateLookupWithKey :: K -> REFCOLL -> Bool
prop_updateLookupWithKey k1 refcoll2
  = structEq
      (let (maybe_a3, map_k_a4)
             = L.updateLookupWithKey k_a_maybe_a k1 (lIn refcoll2)
         in (maybe_a3, lOut map_k_a4))
      (let (maybe_a3, map_k_a4)
             = R.updateLookupWithKey k_a_maybe_a k1 (rIn refcoll2)
         in (maybe_a3, rOut map_k_a4))
 
prop_union :: REFCOLL -> REFCOLL -> Bool
prop_union refcoll1 refcoll2
  = structEq (lOut (L.union (lIn refcoll1) (lIn refcoll2)))
      (rOut (R.union (rIn refcoll1) (rIn refcoll2)))
 
prop_unionWith :: REFCOLL -> REFCOLL -> Bool
prop_unionWith refcoll1 refcoll2
  = structEq (lOut (L.unionWith a_a_a (lIn refcoll1) (lIn refcoll2)))
      (rOut (R.unionWith a_a_a (rIn refcoll1) (rIn refcoll2)))
 
prop_unionWithKey :: REFCOLL -> REFCOLL -> Bool
prop_unionWithKey refcoll1 refcoll2
  = structEq
      (lOut (L.unionWithKey k_a_a_a (lIn refcoll1) (lIn refcoll2)))
      (rOut (R.unionWithKey k_a_a_a (rIn refcoll1) (rIn refcoll2)))
 
prop_unions :: [REFCOLL] -> Bool
prop_unions refcoll1
  = structEq (lOut (L.unions (fmap lIn refcoll1)))
      (rOut (R.unions (fmap rIn refcoll1)))
 
prop_unionsWith :: [REFCOLL] -> Bool
prop_unionsWith refcoll1
  = structEq (lOut (L.unionsWith a_a_a (fmap lIn refcoll1)))
      (rOut (R.unionsWith a_a_a (fmap rIn refcoll1)))
 
prop_difference :: REFCOLL -> REFCOLL -> Bool
prop_difference refcoll1 refcoll2
  = structEq (lOut (L.difference (lIn refcoll1) (lIn refcoll2)))
      (rOut (R.difference (rIn refcoll1) (rIn refcoll2)))
 
prop_differenceWith :: REFCOLL -> REFCOLL -> Bool
prop_differenceWith refcoll1 refcoll2
  = structEq
      (lOut (L.differenceWith a_b_maybe_a (lIn refcoll1) (lIn refcoll2)))
      (rOut (R.differenceWith a_b_maybe_a (rIn refcoll1) (rIn refcoll2)))
 
prop_differenceWithKey :: REFCOLL -> REFCOLL -> Bool
prop_differenceWithKey refcoll1 refcoll2
  = structEq
      (lOut
         (L.differenceWithKey k_a_b_maybe_a (lIn refcoll1) (lIn refcoll2)))
      (rOut
         (R.differenceWithKey k_a_b_maybe_a (rIn refcoll1) (rIn refcoll2)))
 
prop_intersection :: REFCOLL -> REFCOLL -> Bool
prop_intersection refcoll1 refcoll2
  = structEq (lOut (L.intersection (lIn refcoll1) (lIn refcoll2)))
      (rOut (R.intersection (rIn refcoll1) (rIn refcoll2)))
 
prop_intersectionWith :: REFCOLL -> REFCOLL -> Bool
prop_intersectionWith refcoll1 refcoll2
  = structEq
      (lOut (L.intersectionWith a_b_c (lIn refcoll1) (lIn refcoll2)))
      (rOut (R.intersectionWith a_b_c (rIn refcoll1) (rIn refcoll2)))
 
prop_intersectionWithKey :: REFCOLL -> REFCOLL -> Bool
prop_intersectionWithKey refcoll1 refcoll2
  = structEq
      (lOut
         (L.intersectionWithKey k_a_b_c (lIn refcoll1) (lIn refcoll2)))
      (rOut
         (R.intersectionWithKey k_a_b_c (rIn refcoll1) (rIn refcoll2)))
 
prop_mapWithKey :: REFCOLL -> Bool
prop_mapWithKey refcoll1
  = structEq (lOut (L.mapWithKey k_a_b (lIn refcoll1)))
      (rOut (R.mapWithKey k_a_b (rIn refcoll1)))
 
prop_elems :: REFCOLL -> Bool
prop_elems refcoll1
  = structEq (L.elems (lIn refcoll1)) (R.elems (rIn refcoll1))
 
prop_keys :: REFCOLL -> Bool
prop_keys refcoll1
  = structEq (L.keys (lIn refcoll1)) (R.keys (rIn refcoll1))
 
prop_assocs :: REFCOLL -> Bool
prop_assocs refcoll1
  = structEq (L.assocs (lIn refcoll1)) (R.assocs (rIn refcoll1))
 
prop_toAscList :: REFCOLL -> Bool
prop_toAscList refcoll1
  = structEq (L.toAscList (lIn refcoll1))
      (R.toAscList (rIn refcoll1))
