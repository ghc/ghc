import Testsuite

import Data.Array.Parallel.Arr.BUArr
import Data.Array.Parallel.Base.Hyperstrict

instance (UAE a, Arbitrary a) => Arbitrary (BUArr a) where
  arbitrary = fmap toBU arbitrary
  coarbitrary = coarbitrary . fromBU

$(testcases [ ""        <@ [t| ( (), Bool, Char, Int ) |]
            , "acc"     <@ [t| ( (), Int             ) |]
            , "num"     <@ [t| ( Int                 ) |]
            ]
  [d|
  -- if this doesn't work nothing else will, so run this first
  prop_fromBU_toBU :: (Eq a, UAE a) => [a] -> Bool
  prop_fromBU_toBU xs = fromBU (toBU xs) == xs

  -- Basic operations
  -- ----------------

  prop_lengthBU :: UAE a => BUArr a -> Bool
  prop_lengthBU arr = lengthBU arr == length (fromBU arr)

  prop_emptyBU :: (Eq a, UAE a) => a -> Bool
  prop_emptyBU x = fromBU emptyBU == tail [x]
 
  prop_unitsBU :: Len -> Bool
  prop_unitsBU (Len n) =
    fromBU (unitsBU n) == replicate n ()

  prop_replicateBU :: (Eq a, UAE a) => Len -> a -> Bool
  prop_replicateBU (Len n) x =
    fromBU (replicateBU n x) == replicate n x

  prop_indexBU :: (Eq a, UAE a) => BUArr a -> Len -> Property
  prop_indexBU arr (Len i) =
    i < lengthBU arr
    ==> (arr `indexBU` i) == (fromBU arr !! i)

  prop_sliceBU :: (Eq a, UAE a) => BUArr a -> Len -> Len -> Property
  prop_sliceBU arr (Len i) (Len n) =
    i <= lengthBU arr && n <= lengthBU arr - i
    ==> fromBU (sliceBU arr i n) == take n (drop i $ fromBU arr)
  
  prop_extractBU :: (Eq a, UAE a) => BUArr a -> Len -> Len -> Property
  prop_extractBU arr (Len i) (Len n) =
    i <= lengthBU arr && n <= lengthBU arr - i
    ==> fromBU (extractBU arr i n) == take n (drop i $ fromBU arr)

  -- Higher-order operations
  -- -----------------------

  prop_mapBU :: (Eq b, UAE a, UAE b) => (a -> b) -> BUArr a -> Bool
  prop_mapBU f arr =
    fromBU (mapBU f arr) == map f (fromBU arr)
  
  prop_foldlBU :: (Eq a, UAE b) => (a -> b -> a) -> a -> BUArr b -> Bool
  prop_foldlBU f z arr =
    foldlBU f z arr == foldl f z (fromBU arr)

  -- missing: foldBU

  
  prop_scanlBU :: (Eq a, UAE a, UAE b) => (a -> b -> a) -> a -> BUArr b -> Bool
  prop_scanlBU f z arr =
    fromBU (scanlBU f z arr) == init (scanl f z (fromBU arr))

  -- missing: scanBU
  -- missing: loopBU

  -- Arithmetic operations
  -- ---------------------

  prop_sumBU :: (Eq num, UAE num, Num num) => BUArr num -> Bool
  prop_sumBU arr =
    sumBU arr == sum (fromBU arr)

  -- Equality
  -- --------

  prop_eqBU_1 :: (Eq a, UAE a) => BUArr a -> Bool
  prop_eqBU_1 arr = arr == arr

  prop_eqBU_2 :: (Eq a, UAE a) => BUArr a -> BUArr a -> Bool
  prop_eqBU_2 arr brr = (arr == brr) == (fromBU arr == fromBU brr)

  -- Fusion
  -- ------
  
  prop_loopBU_replicateBU
    :: (UAE e, Eq acc, Eq e', UAE e')
    => EFL acc e e' -> acc -> Len -> e -> Bool
  prop_loopBU_replicateBU mf start (Len n) v =
    loopBU mf start (replicateBU n v)
    == loopBU (\a _ -> mf a v) start (unitsBU n)

  {- FIXME: disabled - too many type variables 
  prop_fusion2 :: (Eq acc2, Eq e3, UAE e1, UAE e2, UAE e3)
               => LoopFn acc1 e1 e2
               -> LoopFn acc2 e2 e3
               -> acc1 -> acc2 -> BUArr e1 -> Bool
  prop_fusion2 mf1 mf2 start1 start2 arr =
    loopBU mf2 start2 (loopArr (loopBU mf1 start1 arr)) ==
      let
        mf (acc1 :*: acc2) e = 
          case mf1 acc1 e of
            (acc1' :*: Nothing) -> ((acc1' :*: acc2) :*: Nothing)
  	    (acc1' :*: Just e') ->
  	      case mf2 acc2 e' of
  	        (acc2' :*: res) -> ((acc1' :*: acc2') :*: res)
      in
      loopSndAcc (loopBU mf (start1 :*: start2) arr)
  -}
  |])

