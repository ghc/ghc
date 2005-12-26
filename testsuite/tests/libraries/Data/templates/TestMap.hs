
{-


Test instanciations.

: MAPMODULE    :   MAPK      : KEY : EL  :
------------------------------------------
: Data.Map     :   M.Map Int : Int : Int : 
: Data.IntMap  :   M.IntMap  : Int : Int :


-}

-- Module to test the interface of Map-like types.

-- These are all
-- black-box testing: we never check the internal data structures.
-- We are thus independant of the underlying representation.

import qualified MAPMODULE as M

import qualified Data.List as List

import LibTest

import Control.Monad

type MAP = MAPK EL

main = runTests fileName propNames propTests

-------------------
--  Arbitrary maps

instance Arbitrary MAP where
    arbitrary = return M.fromList `ap` arbitrary

prop_Split :: MAP -> KEY -> Bool
prop_Split s k = all (< k) (M.keys l) && all (> k) (M.keys r)
    where (l,r) = M.split k s

prop_SplitUnion :: MAP -> KEY -> Property
prop_SplitUnion s k = not (M.member k s) ==> M.union l r == s
    where (l,r) = M.split k s

prop_SplitLookup :: MAP -> KEY -> Bool
prop_SplitLookup s k = all (< k) (M.keys l) && all (> k) (M.keys r) && found == M.lookup k s
    where (l,found,r) = M.splitLookup k s

prop_Single :: KEY -> EL -> Bool
prop_Single k x
    = (M.insert k x M.empty == M.singleton k x)

prop_InsertDelete :: KEY -> EL -> MAP -> Property
prop_InsertDelete k x t
  = (M.lookup k t == Nothing) ==> M.delete k (M.insert k x t) == t

prop_UnionInsert :: KEY -> EL -> MAP -> Bool
prop_UnionInsert k x t
  = M.union (M.singleton k x) t == M.insert k x t

prop_UnionAssoc :: MAP -> MAP -> MAP -> Bool
prop_UnionAssoc t1 t2 t3
  = M.union t1 (M.union t2 t3) == M.union (M.union t1 t2) t3

prop_UnionComm :: MAP -> MAP -> Bool
prop_UnionComm t1 t2
  = (M.union t1 t2 == M.unionWith (\x y -> y) t2 t1)

prop_UnionWith :: [(KEY,EL)] -> [(KEY,EL)] -> Bool
prop_UnionWith xs ys
  = sum (M.elems (M.unionWith (+) (M.fromListWith (+) xs) (M.fromListWith (+) ys))) 
    == (sum (Prelude.map snd xs) + sum (Prelude.map snd ys))

prop_Diff :: [(KEY,EL)] -> [(KEY,EL)] -> Bool
prop_Diff xs ys
  =  List.sort (M.keys (M.difference (M.fromListWith (+) xs) (M.fromListWith (+) ys))) 
    == List.sort ((List.\\) (List.nub (Prelude.map fst xs))  (List.nub (Prelude.map fst ys)))

prop_Int :: [(KEY,EL)] -> [(KEY,EL)] -> Bool
prop_Int xs ys
  = List.sort (M.keys (M.intersection (M.fromListWith (+) xs) (M.fromListWith (+) ys))) 
    == List.sort (List.nub ((List.intersect) (Prelude.map fst xs)  (Prelude.map fst ys)))

prop_Ordered
  = forAll (choose (5,100)) $ \n ->
    let xs = [(x,()) | x <- [0..n::Int]]
    in M.fromAscList xs == M.fromList xs

prop_toAscList :: [KEY] -> Bool
prop_toAscList xs
  = (List.sort $ List.nub $ xs) == (map fst $ M.toAscList $ M.fromList $ zip xs (repeat ()))


prop_toList :: [KEY] -> Bool
prop_toList xs
  = (List.sort $ List.nub $ xs) == (map fst $ M.toList $ M.fromList $ zip xs (repeat ()))

                    
