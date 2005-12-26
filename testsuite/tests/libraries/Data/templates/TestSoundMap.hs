{-


Test instanciations.

: REFMODULE : TESTMODULE  : K   : A   :
---------------------------------------
: Map       : Data.Map    : Int : Int :


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

lIn :: REFCOLL -> L.Map K A
lIn = L.fromList . rIn

rOut :: REFCOLL -> REFCOLL
rOut = List.sortBy (comparing fst) 

lOut :: L.Map K A -> REFCOLL
lOut = L.toAscList

main = runTests fileName propNames propTests

--FIXME: better test functions are in order. (non-commutative stuff, things really depending on arguments, etc.)

a_a_a = (+)

k_a_a_a k = a_a_a

a_a = (+1)

k_a_a k a = a+k

a_maybe_a a = if even a then Nothing else Just a

k_a_maybe_a k = a_maybe_a

a_b_maybe_a a b = if even a then Nothing else Just (a+b)

k_a_b_maybe_a k a b = if even a then Nothing else Just (a+b)

a_b_c = a_a_a

k_a_b = k_a_a

k_a_b_c = k_a_a_a


-- !!! EVERYTHING BELOW THIS LINE WILL BE DELETED !!! --
 
prop_null :: REFCOLL -> Bool
prop_null refcol1
  = (==) (L.null (lIn refcol1)) (R.null (rIn refcol1))
 
prop_size :: REFCOLL -> Bool
prop_size refcol1
  = (==) (L.size (lIn refcol1)) (R.size (rIn refcol1))
 
prop_member :: K -> REFCOLL -> Bool
prop_member k1 refcol2
  = (==) (L.member k1 (lIn refcol2)) (R.member k1 (rIn refcol2))
 
prop_findWithDefault :: A -> K -> REFCOLL -> Bool
prop_findWithDefault a1 k2 refcol3
  = (==) (L.findWithDefault a1 k2 (lIn refcol3))
      (R.findWithDefault a1 k2 (rIn refcol3))
 
prop_empty :: Bool
prop_empty = (==) (lOut L.empty) (rOut R.empty)
 
prop_singleton :: K -> A -> Bool
prop_singleton k1 a2
  = (==) (lOut (L.singleton k1 a2)) (rOut (R.singleton k1 a2))
 
prop_insert :: K -> A -> REFCOLL -> Bool
prop_insert k1 a2 refcol3
  = (==) (lOut (L.insert k1 a2 (lIn refcol3)))
      (rOut (R.insert k1 a2 (rIn refcol3)))
 
prop_insertWith :: K -> A -> REFCOLL -> Bool
prop_insertWith k1 a2 refcol3
  = (==) (lOut (L.insertWith a_a_a k1 a2 (lIn refcol3)))
      (rOut (R.insertWith a_a_a k1 a2 (rIn refcol3)))
 
prop_insertWithKey :: K -> A -> REFCOLL -> Bool
prop_insertWithKey k1 a2 refcol3
  = (==) (lOut (L.insertWithKey k_a_a_a k1 a2 (lIn refcol3)))
      (rOut (R.insertWithKey k_a_a_a k1 a2 (rIn refcol3)))
 
prop_insertLookupWithKey :: K -> A -> REFCOLL -> Bool
prop_insertLookupWithKey k1 a2 refcol3
  = (==)
      (let (maybe_a4, map_k_a5)
             = L.insertLookupWithKey k_a_a_a k1 a2 (lIn refcol3)
         in (maybe_a4, lOut map_k_a5))
      (let (maybe_a4, map_k_a5)
             = R.insertLookupWithKey k_a_a_a k1 a2 (rIn refcol3)
         in (maybe_a4, rOut map_k_a5))
 
prop_delete :: K -> REFCOLL -> Bool
prop_delete k1 refcol2
  = (==) (lOut (L.delete k1 (lIn refcol2)))
      (rOut (R.delete k1 (rIn refcol2)))
 
prop_adjust :: K -> REFCOLL -> Bool
prop_adjust k1 refcol2
  = (==) (lOut (L.adjust a_a k1 (lIn refcol2)))
      (rOut (R.adjust a_a k1 (rIn refcol2)))
 
prop_adjustWithKey :: K -> REFCOLL -> Bool
prop_adjustWithKey k1 refcol2
  = (==) (lOut (L.adjustWithKey k_a_a k1 (lIn refcol2)))
      (rOut (R.adjustWithKey k_a_a k1 (rIn refcol2)))
 
prop_update :: K -> REFCOLL -> Bool
prop_update k1 refcol2
  = (==) (lOut (L.update a_maybe_a k1 (lIn refcol2)))
      (rOut (R.update a_maybe_a k1 (rIn refcol2)))
 
prop_updateWithKey :: K -> REFCOLL -> Bool
prop_updateWithKey k1 refcol2
  = (==) (lOut (L.updateWithKey k_a_maybe_a k1 (lIn refcol2)))
      (rOut (R.updateWithKey k_a_maybe_a k1 (rIn refcol2)))
 
prop_updateLookupWithKey :: K -> REFCOLL -> Bool
prop_updateLookupWithKey k1 refcol2
  = (==)
      (let (maybe_a3, map_k_a4)
             = L.updateLookupWithKey k_a_maybe_a k1 (lIn refcol2)
         in (maybe_a3, lOut map_k_a4))
      (let (maybe_a3, map_k_a4)
             = R.updateLookupWithKey k_a_maybe_a k1 (rIn refcol2)
         in (maybe_a3, rOut map_k_a4))
 
prop_union :: REFCOLL -> REFCOLL -> Bool
prop_union refcol1 refcol2
  = (==) (lOut (L.union (lIn refcol1) (lIn refcol2)))
      (rOut (R.union (rIn refcol1) (rIn refcol2)))
 
prop_unionWith :: REFCOLL -> REFCOLL -> Bool
prop_unionWith refcol1 refcol2
  = (==) (lOut (L.unionWith a_a_a (lIn refcol1) (lIn refcol2)))
      (rOut (R.unionWith a_a_a (rIn refcol1) (rIn refcol2)))
 
prop_unionWithKey :: REFCOLL -> REFCOLL -> Bool
prop_unionWithKey refcol1 refcol2
  = (==) (lOut (L.unionWithKey k_a_a_a (lIn refcol1) (lIn refcol2)))
      (rOut (R.unionWithKey k_a_a_a (rIn refcol1) (rIn refcol2)))
 
prop_unions :: [REFCOLL] -> Bool
prop_unions refcol1
  = (==) (lOut (L.unions (fmap lIn refcol1)))
      (rOut (R.unions (fmap rIn refcol1)))
 
prop_unionsWith :: [REFCOLL] -> Bool
prop_unionsWith refcol1
  = (==) (lOut (L.unionsWith a_a_a (fmap lIn refcol1)))
      (rOut (R.unionsWith a_a_a (fmap rIn refcol1)))
 
prop_difference :: REFCOLL -> REFCOLL -> Bool
prop_difference refcol1 refcol2
  = (==) (lOut (L.difference (lIn refcol1) (lIn refcol2)))
      (rOut (R.difference (rIn refcol1) (rIn refcol2)))
 
prop_differenceWith :: REFCOLL -> REFCOLL -> Bool
prop_differenceWith refcol1 refcol2
  = (==)
      (lOut (L.differenceWith a_b_maybe_a (lIn refcol1) (lIn refcol2)))
      (rOut (R.differenceWith a_b_maybe_a (rIn refcol1) (rIn refcol2)))
 
prop_differenceWithKey :: REFCOLL -> REFCOLL -> Bool
prop_differenceWithKey refcol1 refcol2
  = (==)
      (lOut
         (L.differenceWithKey k_a_b_maybe_a (lIn refcol1) (lIn refcol2)))
      (rOut
         (R.differenceWithKey k_a_b_maybe_a (rIn refcol1) (rIn refcol2)))
 
prop_intersection :: REFCOLL -> REFCOLL -> Bool
prop_intersection refcol1 refcol2
  = (==) (lOut (L.intersection (lIn refcol1) (lIn refcol2)))
      (rOut (R.intersection (rIn refcol1) (rIn refcol2)))
 
prop_intersectionWith :: REFCOLL -> REFCOLL -> Bool
prop_intersectionWith refcol1 refcol2
  = (==)
      (lOut (L.intersectionWith a_b_c (lIn refcol1) (lIn refcol2)))
      (rOut (R.intersectionWith a_b_c (rIn refcol1) (rIn refcol2)))
 
prop_intersectionWithKey :: REFCOLL -> REFCOLL -> Bool
prop_intersectionWithKey refcol1 refcol2
  = (==)
      (lOut (L.intersectionWithKey k_a_b_c (lIn refcol1) (lIn refcol2)))
      (rOut (R.intersectionWithKey k_a_b_c (rIn refcol1) (rIn refcol2)))
 
prop_mapWithKey :: REFCOLL -> Bool
prop_mapWithKey refcol1
  = (==) (lOut (L.mapWithKey k_a_b (lIn refcol1)))
      (rOut (R.mapWithKey k_a_b (rIn refcol1)))
 
prop_elems :: REFCOLL -> Bool
prop_elems refcol1
  = (==) (L.elems (lIn refcol1)) (R.elems (rIn refcol1))
 
prop_keys :: REFCOLL -> Bool
prop_keys refcol1
  = (==) (L.keys (lIn refcol1)) (R.keys (rIn refcol1))
 
prop_assocs :: REFCOLL -> Bool
prop_assocs refcol1
  = (==) (L.assocs (lIn refcol1)) (R.assocs (rIn refcol1))
 
prop_toAscList :: REFCOLL -> Bool
prop_toAscList refcol1
  = (==) (L.toAscList (lIn refcol1)) (R.toAscList (rIn refcol1))
