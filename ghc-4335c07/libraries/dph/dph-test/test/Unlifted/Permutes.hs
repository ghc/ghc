import DPH.Testsuite
import DPH.Testsuite.Utils as TU ( limitRange, update )
import DPH.Arbitrary.Int
import DPH.Arbitrary.Perm
import Data.Array.Parallel.Unlifted as U
import qualified Data.Vector        as V
import Prelude as P
import Data.List ( sort )

(!:) = index "test:Permutes"


$(testcases [ ""        <@ [t| ( Bool, Int ) |]
            , "acc"     <@ [t| ( Int       ) |]
            , "num"     <@ [t| ( Int       ) |]
            , "ord"     <@ [t| ( Bool, Int ) |]
            , "enum"    <@ [t| ( Bool, Int ) |]
            ]
  [d|
  prop_permute :: (Elt a, Eq a) => Array a -> Perm -> Bool
  prop_permute arr (Perm ixs) =
    toList (permute arr ixs') == TU.update (toList arr) (toList $ U.zip ixs' arr)
    where ixs' = U.filter (< U.length arr) 
               $ U.fromList $ V.toList ixs

  prop_bpermute :: (Elt a, Eq a) => Array a -> Array Int -> Bool
  prop_bpermute arr ixs = 
    bpermute arr ixs' == U.map (arr!:) ixs'
    where ixs' = limitRange (U.length arr) ixs

  prop_mbpermute :: (Elt a, Eq b, Elt b) => (a -> b) -> Array a -> Array Int -> Bool
  prop_mbpermute f arr ixs =
    mbpermute f arr ixs' == U.map (\i -> f (arr!:i)) ixs'
    where ixs' = limitRange (U.length arr) ixs

  prop_bpermuteDft :: (Elt a, Eq a) => Len -> (Int -> a) -> Array (Int, a) -> Bool
  prop_bpermuteDft (Len n) init pairs =
    toList (bpermuteDft n init pairs') == TU.update (P.map init [0..n-1]) (toList pairs')
    where pairs' = limitRange n (fsts pairs) `U.zip` (snds pairs)

  prop_update :: (Eq a, Elt a) => Array a -> Array (Int, a) -> Bool
  prop_update arr pairs =
    toList (U.update arr pairs') == TU.update (toList arr) (toList pairs')
    where pairs' = limitRange (U.length arr) (fsts pairs) `U.zip` (snds pairs)
 |])

