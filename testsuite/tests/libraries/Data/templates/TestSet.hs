
{-

Test instanciations.

: SETMODULE    :   SET         : EL    :
----------------------------------------
: Data.Set     :   S.Set Int   : Int   :
: Data.Set     :   S.Set Nasty : Nasty :
: Data.IntSet  :   S.IntSet    : Int   :

-}

import qualified SETMODULE as S

import LibTest

import Control.Monad
import qualified Data.List as List

main = runTests fileName propNames propTests


instance Arbitrary (SET) where
    arbitrary = return S.fromList `ap` arbitrary

prop_Split :: SET -> EL -> Bool
prop_Split s k = all (< k) (S.elems l) && all (> k) (S.elems r)
    where (l,r) = S.split k s

prop_SplitUnion :: SET -> EL -> Property
prop_SplitUnion s k = not (S.member k s) ==> S.union l r == s
    where (l,r) = S.split k s

prop_SplitMember :: SET -> EL -> Bool
prop_SplitMember s k = all (<  k) (S.elems l) && all (> k) (S.elems r) && found == S.member k s
    where (l,found,r) = S.splitMember k s

prop_Single :: EL -> Bool
prop_Single x
  = (S.insert x S.empty == S.singleton x)

prop_InsertDelete :: EL -> SET -> Property
prop_InsertDelete k t
  = not (S.member k t) ==> S.delete k (S.insert k t) == t

prop_UnionInsert :: EL -> SET -> Bool
prop_UnionInsert x t
  = S.union t (S.singleton x) == S.insert x t

prop_UnionComm :: SET -> SET -> Bool
prop_UnionComm t1 t2
  = (S.union t1 t2 == S.union t2 t1)

prop_Diff :: [EL] -> [EL] -> Bool
prop_Diff xs ys
  =  S.toAscList (S.difference (S.fromList xs) (S.fromList ys))
    == List.sort ((List.\\) (List.nub xs) (List.nub ys))

prop_Int :: [EL] -> [EL] -> Bool
prop_Int xs ys
  =  S.toAscList (S.intersection (S.fromList xs) (S.fromList ys))
    == List.sort (List.nub ((List.intersect) (xs)  (ys)))

prop_Ordered :: [EL] -> Bool
prop_Ordered xs = S.fromAscList (List.sort xs) == S.fromList xs

prop_toAscList :: [EL] -> Bool
prop_toAscList xs
  = (List.sort (List.nub xs) == S.toAscList (S.fromList xs))

-- the following is actually not specified. user cannot rely on it, but we check it anyway.
prop_toList :: [EL] -> Bool
prop_toList xs
  = (List.sort (List.nub xs) == S.toList (S.fromList xs))

