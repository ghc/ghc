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
  prop_map :: (Elt a, Eq b, Elt b) => (a -> b) -> Array a -> Bool
  prop_map f arr =
    toList (U.map f arr) == P.map f (toList arr)

  prop_filter :: (Eq a, Elt a) => (a -> Bool) -> Array a -> Bool
  prop_filter pred arr =
    toList (U.filter pred arr) == P.filter pred (toList arr)

  prop_pack :: (Eq a, Elt a) => Array a -> Array Bool -> Bool
  prop_pack arr flags =
    toList (U.pack arr flags) == pack (toList arr) (toList flags)
      where pack (x : xs) (True  : fs) = x : pack xs fs
            pack (_ : xs) (False : fs) =     pack xs fs
            pack _        _            = []

  prop_combine :: (Eq a, Elt a) => Array Bool -> Array a -> Array a -> Property
  prop_combine sel arr brr =
    (count sel True <= U.length arr && count sel False <= U.length brr) 
    ==> toList (combine sel arr brr) == combine' (toList sel) (toList arr) (toList brr)
      where 
      -- combine' :: [Bool] -> [a] -> [a] -> [a]
         combine' [] [] [] = []
         combine' (True  : ss) (x : xs) ys       = x : combine' ss xs ys
         combine' (False : ss) xs       (y : ys) = y : combine' ss xs ys
         combine' _ _ _ = []

  -- missing: combine2

  prop_zip :: (Eq a, Elt a, Eq b, Elt b) => Array a -> Array b -> Bool
  prop_zip arr brr = 
    toList (U.zip arr brr) == P.zip (toList arr) (toList brr)

  prop_unzip :: (Eq a, Elt a, Eq b, Elt b) => Array (a, b) -> Bool
  prop_unzip abrr =
    (toList arr, toList brr) == P.unzip (toList abrr)
      where (arr, brr) = U.unzip abrr

  prop_fsts :: (Eq a, Elt a, Elt b) => Array (a, b) -> Bool
  prop_fsts arr =
    fsts arr == fst (U.unzip arr)

  prop_snds :: (Elt a, Eq b, Elt b) => Array (a, b) -> Bool
  prop_snds arr =
    snds arr == snd (U.unzip arr)

  prop_zipWith :: (Elt a, Elt b, Eq c, Elt c) => (a -> b -> c) -> Array a -> Array b -> Bool
  prop_zipWith f arr brr =
    toList (U.zipWith f arr brr) == P.zipWith f (toList arr) (toList brr)

  prop_zipWith3 :: (Elt a, Elt b, Elt c, Eq d, Elt d) 
                => (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Bool
  prop_zipWith3 f arr brr crr =
    toList (U.zipWith3 f arr brr crr) == P.zipWith3 f (toList arr) (toList brr) (toList crr)

  {-
  -- TODO: Guarrantee associativity of the operator and 
  --       neutrality of initial value wrt this operator.

  prop_fold :: (Elt a, Eq a) => (a -> a -> a) -> a -> Array a -> Bool
  prop_fold f z arr =
    U.fold f z arr == P.foldl f z (toList arr)

  prop_fold1 :: (Elt a, Eq a) => (a -> a -> a) -> Array a -> Property
  prop_fold1 f arr =
    arr /= empty
    ==> U.fold1 f arr == P.foldl1 f (toList arr)

  prop_scan :: (Elt a, Eq a) => (a -> a -> a) -> a -> Array a -> Bool
  prop_scan f z arr =
    toList (U.scan f z arr) == P.init (P.scanl f z (toList arr))
  -}

  |])

