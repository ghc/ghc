import Testsuite

import Data.Array.Parallel.Unlifted

$(testcases [ ""        <@ [t| ( (), Char, Bool, Int ) |]
            , "acc"     <@ [t| ( (), Int             ) |]
            , "num"     <@ [t| ( Int                 ) |]
            , "ord"     <@ [t| ( (), Char, Bool, Int ) |]
            , "enum"    <@ [t| ( (), Char, Bool, Int ) |]
            ]
  [d|
  -- Searching
  -- ---------
  prop_elemU :: (Eq e, UA e) => e -> UArr e -> Bool
  prop_elemU x arr =
    elemU x arr == elem x (fromU arr)

  prop_notElemU :: (Eq e, UA e) => e -> UArr e -> Bool
  prop_notElemU x arr =
    notElemU x arr == notElem x (fromU arr)

  -- Logic operations
  -- ----------------

  prop_andU :: UArr Bool -> Bool
  prop_andU arr =
    andU arr == and (fromU arr)

  prop_orU :: UArr Bool -> Bool
  prop_orU arr =
    orU arr == or (fromU arr)

  prop_anyU :: UA e => (e -> Bool) -> UArr e -> Bool
  prop_anyU f arr =
    anyU f arr == any f (fromU arr)

  prop_allU :: UA e => (e -> Bool) -> UArr e -> Bool
  prop_allU f arr =
    allU f arr == all f (fromU arr)

  -- Arithmetic operations
  -- ---------------------

  prop_sumU :: (Eq num, UA num, Num num) => UArr num -> Bool
  prop_sumU arr =
    sumU arr == sum (fromU arr)

  prop_productU :: (Eq num, UA num, Num num) => UArr num -> Bool
  prop_productU arr =
    productU arr == product (fromU arr)

  prop_maximumU :: (Ord ord, UA ord) => UArr ord -> Property
  prop_maximumU arr =
    not (nullU arr)
    ==> maximumU arr == maximum (fromU arr)

  prop_minimumU :: (Ord ord, UA ord) => UArr ord -> Property
  prop_minimumU arr =
    not (nullU arr)
    ==> minimumU arr == minimum (fromU arr)
 |])

