import DPH.Testsuite
import Data.Array.Parallel.Unlifted as U
import Prelude as P

$(testcases [ ""        <@ [t| ( Bool, Int ) |]
            , "acc"     <@ [t| ( Int       ) |]
            , "num"     <@ [t| ( Int       ) |]
            , "ord"     <@ [t| ( Bool, Int ) |]
            , "enum"    <@ [t| ( Bool, Int ) |]
            ]
  [d|
  -- Searching
  -- ---------
{-
  prop_elem :: (Eq e, Elt e) => e -> Array e -> Bool
  prop_elem x arr =
    U.elem x arr == P.elem x (toList arr)

  prop_notElemU :: (Eq e, Elt e) => e -> Array e -> Bool
  prop_notElemU x arr =
    notElemU x arr == notElem x (toList arr)
-}
  -- Logic operations
  -- ----------------

  prop_and :: Array Bool -> Bool
  prop_and arr =
    U.and arr == P.and (toList arr)
{-
  prop_orU :: Array Bool -> Bool
  prop_orU arr =
    orU arr == or (toList arr)

  prop_anyU :: Elt e => (e -> Bool) -> Array e -> Bool
  prop_anyU f arr =
    anyU f arr == any f (toList arr)

  prop_allU :: Elt e => (e -> Bool) -> Array e -> Bool
  prop_allU f arr =
    allU f arr == all f (toList arr)
-}
  -- Arithmetic operations
  -- ---------------------

  prop_sum :: (Eq num, Elt num, Num num) => Array num -> Bool
  prop_sum arr =
    U.sum arr == P.sum (toList arr)
{-
  prop_productU :: (Eq num, Elt num, Num num) => Array num -> Bool
  prop_productU arr =
    productU arr == product (toList arr)

  prop_maximumU :: (Ord ord, Elt ord) => Array ord -> Property
  prop_maximumU arr =
    not (nullU arr)
    ==> maximumU arr == maximum (toList arr)

  prop_minimumU :: (Ord ord, Elt ord) => Array ord -> Property
  prop_minimumU arr =
    not (nullU arr)
    ==> minimumU arr == minimum (toList arr)
-}
 |])

