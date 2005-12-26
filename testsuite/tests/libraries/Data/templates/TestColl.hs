
{-


Test instanciations.

: MODULE       :   COLL          : ELEM      :
----------------------------------------------
: Data.Map     :   C.Map Int Int : (Int,Int) :
: Data.IntMap  :   C.IntMap Int  : (Int,Int) :
: Data.Set     :   C.Set Int     : (Int)     :
: Data.IntSet  :   C.IntSet      : (Int)     :


-}

-- Module to test the interface of Map-like types.

-- These are all
-- black-box testing: we never check the internal data structures.
-- We are thus independant of the underlying representation.

import qualified MODULE as C

import qualified Data.List as List

import LibTest

import Control.Monad

main = runTests fileName propNames propTests

-------------------
--  Arbitrary maps

instance Arbitrary (COLL) where
    arbitrary = return C.fromList `ap` arbitrary

prop_UnionAssoc :: COLL -> COLL -> COLL -> Bool
prop_UnionAssoc t1 t2 t3
  = C.union t1 (C.union t2 t3) == C.union (C.union t1 t2) t3

