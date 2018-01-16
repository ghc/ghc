{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}

#include "containers.h"

import Data.Sequence.Internal
  ( Sized (..)
  , Seq (Seq)
  , FingerTree(..)
  , Node(..)
  , Elem(..)
  , Digit (..)
  , node2
  , node3
  , deep )

import Data.Sequence

import Control.Applicative (Applicative(..), liftA2)
import Control.Arrow ((***))
import Control.Monad.Trans.State.Strict
import Data.Array (listArray)
import Data.Foldable (Foldable(foldl, foldl1, foldr, foldr1, foldMap, fold), toList, all, sum, foldl', foldr')
import Data.Functor ((<$>), (<$))
import Data.Maybe
import Data.Monoid (Monoid(..), All(..), Endo(..), Dual(..))
import Data.Traversable (Traversable(traverse), sequenceA)
import Prelude hiding (
  lookup, null, length, take, drop, splitAt,
  foldl, foldl1, foldr, foldr1, scanl, scanl1, scanr, scanr1,
  filter, reverse, replicate, zip, zipWith, zip3, zipWith3,
  all, sum)
import qualified Prelude
import qualified Data.List
import Test.QuickCheck hiding ((><))
import Test.QuickCheck.Poly
#if __GLASGOW_HASKELL__ >= 800
import Test.QuickCheck.Property
#endif
import Test.QuickCheck.Function
import Test.Framework
import Test.Framework.Providers.QuickCheck2
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip (..))
#endif
import Control.DeepSeq (deepseq)


main :: IO ()
main = defaultMain
       [ testProperty "fmap" prop_fmap
       , testProperty "(<$)" prop_constmap
       , testProperty "foldr" prop_foldr
       , testProperty "foldr'" prop_foldr'
       , testProperty "foldr1" prop_foldr1
       , testProperty "foldl" prop_foldl
       , testProperty "foldl'" prop_foldl'
       , testProperty "foldl1" prop_foldl1
       , testProperty "(==)" prop_equals
       , testProperty "compare" prop_compare
       , testProperty "mappend" prop_mappend
       , testProperty "singleton" prop_singleton
       , testProperty "(<|)" prop_cons
       , testProperty "(|>)" prop_snoc
       , testProperty "(><)" prop_append
       , testProperty "fromList" prop_fromList
       , testProperty "fromFunction" prop_fromFunction
       , testProperty "fromArray" prop_fromArray
       , testProperty "replicate" prop_replicate
       , testProperty "replicateA" prop_replicateA
       , testProperty "replicateM" prop_replicateM
       , testProperty "iterateN" prop_iterateN
       , testProperty "unfoldr" prop_unfoldr
       , testProperty "unfoldl" prop_unfoldl
       , testProperty "null" prop_null
       , testProperty "length" prop_length
       , testProperty "viewl" prop_viewl
       , testProperty "viewr" prop_viewr
       , testProperty "scanl" prop_scanl
       , testProperty "scanl1" prop_scanl1
       , testProperty "scanr" prop_scanr
       , testProperty "scanr1" prop_scanr1
       , testProperty "tails" prop_tails
       , testProperty "inits" prop_inits
       , testProperty "takeWhileL" prop_takeWhileL
       , testProperty "takeWhileR" prop_takeWhileR
       , testProperty "dropWhileL" prop_dropWhileL
       , testProperty "dropWhileR" prop_dropWhileR
       , testProperty "spanl" prop_spanl
       , testProperty "spanr" prop_spanr
       , testProperty "breakl" prop_breakl
       , testProperty "breakr" prop_breakr
       , testProperty "partition" prop_partition
       , testProperty "filter" prop_filter
       , testProperty "sort" prop_sort
       , testProperty "sortBy" prop_sortBy
       , testProperty "unstableSort" prop_unstableSort
       , testProperty "unstableSortBy" prop_unstableSortBy
       , testProperty "index" prop_index
       , testProperty "(!?)" prop_safeIndex
       , testProperty "adjust" prop_adjust
       , testProperty "insertAt" prop_insertAt
       , testProperty "deleteAt" prop_deleteAt
       , testProperty "update" prop_update
       , testProperty "take" prop_take
       , testProperty "drop" prop_drop
       , testProperty "splitAt" prop_splitAt
       , testProperty "chunksOf" prop_chunksOf
       , testProperty "elemIndexL" prop_elemIndexL
       , testProperty "elemIndicesL" prop_elemIndicesL
       , testProperty "elemIndexR" prop_elemIndexR
       , testProperty "elemIndicesR" prop_elemIndicesR
       , testProperty "findIndexL" prop_findIndexL
       , testProperty "findIndicesL" prop_findIndicesL
       , testProperty "findIndexR" prop_findIndexR
       , testProperty "findIndicesR" prop_findIndicesR
       , testProperty "foldlWithIndex" prop_foldlWithIndex
       , testProperty "foldrWithIndex" prop_foldrWithIndex
       , testProperty "mapWithIndex" prop_mapWithIndex
       , testProperty "foldMapWithIndex/foldlWithIndex" prop_foldMapWithIndexL
       , testProperty "foldMapWithIndex/foldrWithIndex" prop_foldMapWithIndexR
       , testProperty "traverseWithIndex" prop_traverseWithIndex
       , testProperty "reverse" prop_reverse
       , testProperty "zip" prop_zip
       , testProperty "zipWith" prop_zipWith
       , testProperty "zip3" prop_zip3
       , testProperty "zipWith3" prop_zipWith3
       , testProperty "zip4" prop_zip4
       , testProperty "zipWith4" prop_zipWith4
#if MIN_VERSION_base(4,4,0)
       , testProperty "mzip-naturality" prop_mzipNaturality
       , testProperty "mzip-preservation" prop_mzipPreservation
       , testProperty "munzip-lazy" prop_munzipLazy
#endif
       , testProperty "<*>" prop_ap
       , testProperty "<*> NOINLINE" prop_ap_NOINLINE
       , testProperty "liftA2" prop_liftA2
       , testProperty "*>" prop_then
       , testProperty "cycleTaking" prop_cycleTaking
       , testProperty "intersperse" prop_intersperse
       , testProperty ">>=" prop_bind
#if __GLASGOW_HASKELL__ >= 800
       , testProperty "Empty pattern" prop_empty_pat
       , testProperty "Empty constructor" prop_empty_con
       , testProperty "Left view pattern" prop_viewl_pat
       , testProperty "Left view constructor" prop_viewl_con
       , testProperty "Right view pattern" prop_viewr_pat
       , testProperty "Right view constructor" prop_viewr_con
#endif
       ]

------------------------------------------------------------------------
-- Arbitrary
------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Seq a) where
    arbitrary = Seq <$> arbitrary
    shrink (Seq x) = map Seq (shrink x)

instance Arbitrary a => Arbitrary (Elem a) where
    arbitrary = Elem <$> arbitrary

instance (Arbitrary a, Sized a) => Arbitrary (FingerTree a) where
    arbitrary = sized arb
      where
        arb :: (Arbitrary b, Sized b) => Int -> Gen (FingerTree b)
        arb 0 = return EmptyT
        arb 1 = Single <$> arbitrary
        arb n = do
            pr <- arbitrary
            sf <- arbitrary
            let n_pr = Prelude.length (toList pr)
            let n_sf = Prelude.length (toList sf)
            -- adding n `div` 7 ensures that n_m >= 0, and makes more Singles
            let n_m = max (n `div` 7) ((n - n_pr - n_sf) `div` 3)
            m <- arb n_m
            return $ deep pr m sf

    shrink (Deep _ (One a) EmptyT (One b)) = [Single a, Single b]
    shrink (Deep _ pr m sf) =
        [deep pr' m sf | pr' <- shrink pr] ++
        [deep pr m' sf | m' <- shrink m] ++
        [deep pr m sf' | sf' <- shrink sf]
    shrink (Single x) = map Single (shrink x)
    shrink EmptyT = []

instance (Arbitrary a, Sized a) => Arbitrary (Node a) where
    arbitrary = oneof [
        node2 <$> arbitrary <*> arbitrary,
        node3 <$> arbitrary <*> arbitrary <*> arbitrary]

    shrink (Node2 _ a b) =
        [node2 a' b | a' <- shrink a] ++
        [node2 a b' | b' <- shrink b]
    shrink (Node3 _ a b c) =
        [node2 a b, node2 a c, node2 b c] ++
        [node3 a' b c | a' <- shrink a] ++
        [node3 a b' c | b' <- shrink b] ++
        [node3 a b c' | c' <- shrink c]

instance Arbitrary a => Arbitrary (Digit a) where
    arbitrary = oneof [
        One <$> arbitrary,
        Two <$> arbitrary <*> arbitrary,
        Three <$> arbitrary <*> arbitrary <*> arbitrary,
        Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary]

    shrink (One a) = map One (shrink a)
    shrink (Two a b) = [One a, One b]
    shrink (Three a b c) = [Two a b, Two a c, Two b c]
    shrink (Four a b c d) = [Three a b c, Three a b d, Three a c d, Three b c d]

------------------------------------------------------------------------
-- Valid trees
------------------------------------------------------------------------

class Valid a where
    valid :: a -> Bool

instance Valid (Elem a) where
    valid _ = True

instance Valid (Seq a) where
    valid (Seq xs) = valid xs

instance (Sized a, Valid a) => Valid (FingerTree a) where
    valid EmptyT = True
    valid (Single x) = valid x
    valid (Deep s pr m sf) =
        s == size pr + size m + size sf && valid pr && valid m && valid sf

instance (Sized a, Valid a) => Valid (Node a) where
    valid node = size node == sum (fmap size node) && all valid node

instance Valid a => Valid (Digit a) where
    valid = all valid

{--------------------------------------------------------------------
  The general plan is to compare each function with a list equivalent.
  Each operation should produce a valid tree representing the same
  sequence as produced by its list counterpart on corresponding inputs.
  (The list versions are often lazier, but these properties ignore
  strictness.)
--------------------------------------------------------------------}

-- utilities for partial conversions

infix 4 ~=

(~=) :: Eq a => Maybe a -> a -> Bool
(~=) = maybe (const False) (==)

-- Partial conversion of an output sequence to a list.
toList' :: Seq a -> Maybe [a]
toList' xs
  | valid xs = Just (toList xs)
  | otherwise = Nothing

toListList' :: Seq (Seq a) -> Maybe [[a]]
toListList' xss = toList' xss >>= mapM toList'

toListPair' :: (Seq a, Seq b) -> Maybe ([a], [b])
toListPair' (xs, ys) = (,) <$> toList' xs <*> toList' ys

-- Extra "polymorphic" test type
newtype D = D{ unD :: Integer }
  deriving ( Eq )

instance Show D where
  showsPrec n (D x) = showsPrec n x

instance Arbitrary D where
  arbitrary    = (D . (+1) . abs) `fmap` arbitrary
  shrink (D x) = [ D x' | x' <- shrink x, x' > 0 ]

instance CoArbitrary D where
  coarbitrary = coarbitrary . unD

-- instances

prop_fmap :: Seq Int -> Bool
prop_fmap xs =
    toList' (fmap f xs) ~= map f (toList xs)
  where f = (+100)

prop_constmap :: A -> Seq A -> Bool
prop_constmap x xs =
    toList' (x <$ xs) ~= map (const x) (toList xs)

prop_foldr :: Seq A -> Property
prop_foldr xs =
    foldr f z xs === Prelude.foldr f z (toList xs)
  where
    f = (:)
    z = []

prop_foldr' :: Seq A -> Property
prop_foldr' xs =
    foldr' f z xs === foldr' f z (toList xs)
  where
    f = (:)
    z = []

prop_foldr1 :: Seq Int -> Property
prop_foldr1 xs =
    not (null xs) ==> foldr1 f xs == Data.List.foldr1 f (toList xs)
  where f = (-)

prop_foldl :: Seq A -> Property
prop_foldl xs =
    foldl f z xs === Prelude.foldl f z (toList xs)
  where
    f = flip (:)
    z = []

prop_foldl' :: Seq A -> Property
prop_foldl' xs =
    foldl' f z xs === foldl' f z (toList xs)
  where
    f = flip (:)
    z = []

prop_foldl1 :: Seq Int -> Property
prop_foldl1 xs =
    not (null xs) ==> foldl1 f xs == Data.List.foldl1 f (toList xs)
  where f = (-)

prop_equals :: Seq OrdA -> Seq OrdA -> Bool
prop_equals xs ys =
    (xs == ys) == (toList xs == toList ys)

prop_compare :: Seq OrdA -> Seq OrdA -> Bool
prop_compare xs ys =
    compare xs ys == compare (toList xs) (toList ys)

prop_mappend :: Seq A -> Seq A -> Bool
prop_mappend xs ys =
    toList' (mappend xs ys) ~= toList xs ++ toList ys

-- * Construction

{-
    toList' empty ~= []
-}

prop_singleton :: A -> Bool
prop_singleton x =
    toList' (singleton x) ~= [x]

prop_cons :: A -> Seq A -> Bool
prop_cons x xs =
    toList' (x <| xs) ~= x : toList xs

prop_snoc :: Seq A -> A -> Bool
prop_snoc xs x =
    toList' (xs |> x) ~= toList xs ++ [x]

prop_append :: Seq A -> Seq A -> Bool
prop_append xs ys =
    toList' (xs >< ys) ~= toList xs ++ toList ys

prop_fromList :: [A] -> Bool
prop_fromList xs =
    toList' (fromList xs) ~= xs

prop_fromFunction :: [A] -> Bool
prop_fromFunction xs =
    toList' (fromFunction (Prelude.length xs) (xs!!)) ~= xs

prop_fromArray :: [A] -> Bool
prop_fromArray xs =
    toList' (fromArray (listArray (42, 42+Prelude.length xs-1) xs)) ~= xs

-- ** Repetition

prop_replicate :: NonNegative Int -> A -> Bool
prop_replicate (NonNegative m) x =
    toList' (replicate n x) ~= Prelude.replicate n x
  where n = m `mod` 10000

prop_replicateA :: NonNegative Int -> Bool
prop_replicateA (NonNegative m) =
    traverse toList' (replicateA n a) ~= sequenceA (Prelude.replicate n a)
  where
    n = m `mod` 10000
    a = Action 1 0 :: M Int

prop_replicateM :: NonNegative Int -> Bool
prop_replicateM (NonNegative m) =
    traverse toList' (replicateM n a) ~= sequence (Prelude.replicate n a)
  where
    n = m `mod` 10000
    a = Action 1 0 :: M Int

-- ** Iterative construction

prop_iterateN :: NonNegative Int -> Int -> Bool
prop_iterateN (NonNegative m) x =
    toList' (iterateN n f x) ~= Prelude.take n (Prelude.iterate f x)
  where
    n = m `mod` 10000
    f = (+1)

prop_unfoldr :: [A] -> Bool
prop_unfoldr z =
    toList' (unfoldr f z) ~= Data.List.unfoldr f z
  where
    f [] = Nothing
    f (x:xs) = Just (x, xs)

prop_unfoldl :: [A] -> Bool
prop_unfoldl z =
    toList' (unfoldl f z) ~= Data.List.reverse (Data.List.unfoldr (fmap swap . f) z)
  where
    f [] = Nothing
    f (x:xs) = Just (xs, x)
    swap (x,y) = (y,x)

-- * Deconstruction

-- ** Queries

prop_null :: Seq A -> Bool
prop_null xs =
    null xs == Prelude.null (toList xs)

prop_length :: Seq A -> Bool
prop_length xs =
    length xs == Prelude.length (toList xs)

-- ** Views

prop_viewl :: Seq A -> Bool
prop_viewl xs =
    case viewl xs of
    EmptyL ->   Prelude.null (toList xs)
    x :< xs' -> valid xs' && toList xs == x : toList xs'

prop_viewr :: Seq A -> Bool
prop_viewr xs =
    case viewr xs of
    EmptyR ->   Prelude.null (toList xs)
    xs' :> x -> valid xs' && toList xs == toList xs' ++ [x]

-- * Scans

prop_scanl :: [A] -> Seq A -> Bool
prop_scanl z xs =
    toList' (scanl f z xs) ~= Data.List.scanl f z (toList xs)
  where f = flip (:)

prop_scanl1 :: Seq Int -> Property
prop_scanl1 xs =
    not (null xs) ==> toList' (scanl1 f xs) ~= Data.List.scanl1 f (toList xs)
  where f = (-)

prop_scanr :: [A] -> Seq A -> Bool
prop_scanr z xs =
    toList' (scanr f z xs) ~= Data.List.scanr f z (toList xs)
  where f = (:)

prop_scanr1 :: Seq Int -> Property
prop_scanr1 xs =
    not (null xs) ==> toList' (scanr1 f xs) ~= Data.List.scanr1 f (toList xs)
  where f = (-)

-- * Sublists

prop_tails :: Seq A -> Bool
prop_tails xs =
    toListList' (tails xs) ~= Data.List.tails (toList xs)

prop_inits :: Seq A -> Bool
prop_inits xs =
    toListList' (inits xs) ~= Data.List.inits (toList xs)

-- ** Sequential searches
-- We use predicates with varying density.

prop_takeWhileL :: Positive Int -> Seq Int -> Bool
prop_takeWhileL (Positive n) xs =
    toList' (takeWhileL p xs) ~= Prelude.takeWhile p (toList xs)
  where p x = x `mod` n == 0

prop_takeWhileR :: Positive Int -> Seq Int -> Bool
prop_takeWhileR (Positive n) xs =
    toList' (takeWhileR p xs) ~= Prelude.reverse (Prelude.takeWhile p (Prelude.reverse (toList xs)))
  where p x = x `mod` n == 0

prop_dropWhileL :: Positive Int -> Seq Int -> Bool
prop_dropWhileL (Positive n) xs =
    toList' (dropWhileL p xs) ~= Prelude.dropWhile p (toList xs)
  where p x = x `mod` n == 0

prop_dropWhileR :: Positive Int -> Seq Int -> Bool
prop_dropWhileR (Positive n) xs =
    toList' (dropWhileR p xs) ~= Prelude.reverse (Prelude.dropWhile p (Prelude.reverse (toList xs)))
  where p x = x `mod` n == 0

prop_spanl :: Positive Int -> Seq Int -> Bool
prop_spanl (Positive n) xs =
    toListPair' (spanl p xs) ~= Data.List.span p (toList xs)
  where p x = x `mod` n == 0

prop_spanr :: Positive Int -> Seq Int -> Bool
prop_spanr (Positive n) xs =
    toListPair' (spanr p xs) ~= (Prelude.reverse *** Prelude.reverse) (Data.List.span p (Prelude.reverse (toList xs)))
  where p x = x `mod` n == 0

prop_breakl :: Positive Int -> Seq Int -> Bool
prop_breakl (Positive n) xs =
    toListPair' (breakl p xs) ~= Data.List.break p (toList xs)
  where p x = x `mod` n == 0

prop_breakr :: Positive Int -> Seq Int -> Bool
prop_breakr (Positive n) xs =
    toListPair' (breakr p xs) ~= (Prelude.reverse *** Prelude.reverse) (Data.List.break p (Prelude.reverse (toList xs)))
  where p x = x `mod` n == 0

prop_partition :: Positive Int -> Seq Int -> Bool
prop_partition (Positive n) xs =
    toListPair' (partition p xs) ~= Data.List.partition p (toList xs)
  where p x = x `mod` n == 0

prop_filter :: Positive Int -> Seq Int -> Bool
prop_filter (Positive n) xs =
    toList' (filter p xs) ~= Prelude.filter p (toList xs)
  where p x = x `mod` n == 0

-- * Sorting

prop_sort :: Seq OrdA -> Bool
prop_sort xs =
    toList' (sort xs) ~= Data.List.sort (toList xs)

prop_sortBy :: Seq (OrdA, B) -> Bool
prop_sortBy xs =
    toList' (sortBy f xs) ~= Data.List.sortBy f (toList xs)
  where f (x1, _) (x2, _) = compare x1 x2

prop_unstableSort :: Seq OrdA -> Bool
prop_unstableSort xs =
    toList' (unstableSort xs) ~= Data.List.sort (toList xs)

prop_unstableSortBy :: Seq OrdA -> Bool
prop_unstableSortBy xs =
    toList' (unstableSortBy compare xs) ~= Data.List.sort (toList xs)

-- * Indexing

prop_index :: Seq A -> Property
prop_index xs =
    not (null xs) ==> forAll (choose (0, length xs-1)) $ \ i ->
    index xs i == toList xs !! i

prop_safeIndex :: Seq A -> Property
prop_safeIndex xs =
    forAll (choose (-3, length xs + 3)) $ \i ->
    ((i < 0 || i >= length xs) .&&. lookup i xs === Nothing) .||.
    lookup i xs === Just (toList xs !! i)

prop_insertAt :: A -> Seq A -> Property
prop_insertAt x xs =
  forAll (choose (-3, length xs + 3)) $ \i ->
      let res = insertAt i x xs
      in valid res .&&. res === case splitAt i xs of (front, back) -> front >< x <| back

prop_deleteAt :: Seq A -> Property
prop_deleteAt xs =
  forAll (choose (-3, length xs + 3)) $ \i ->
      let res = deleteAt i xs
      in valid res .&&.
          (((0 <= i && i < length xs) .&&. res === case splitAt i xs of (front, back) -> front >< drop 1 back)
            .||. ((i < 0 || i >= length xs) .&&. res === xs))

prop_adjust :: Int -> Int -> Seq Int -> Bool
prop_adjust n i xs =
    toList' (adjust f i xs) ~= adjustList f i (toList xs)
  where f = (+n)

prop_update :: Int -> A -> Seq A -> Bool
prop_update i x xs =
    toList' (update i x xs) ~= adjustList (const x) i (toList xs)

prop_take :: Int -> Seq A -> Bool
prop_take n xs =
    toList' (take n xs) ~= Prelude.take n (toList xs)

prop_drop :: Int -> Seq A -> Bool
prop_drop n xs =
    toList' (drop n xs) ~= Prelude.drop n (toList xs)

prop_splitAt :: Int -> Seq A -> Bool
prop_splitAt n xs =
    toListPair' (splitAt n xs) ~= Prelude.splitAt n (toList xs)

prop_chunksOf :: Seq A -> Property
prop_chunksOf xs =
  forAll (choose (1, length xs + 3)) $ \n ->
    let chunks = chunksOf n xs
    in valid chunks .&&.
       conjoin [valid c .&&. 1 <= length c && length c <= n | c <- toList chunks] .&&.
       fold chunks === xs

adjustList :: (a -> a) -> Int -> [a] -> [a]
adjustList f i xs =
    [if j == i then f x else x | (j, x) <- Prelude.zip [0..] xs]

-- ** Indexing with predicates
-- The elem* tests have poor coverage, but for find* we use predicates
-- of varying density.

prop_elemIndexL :: A -> Seq A -> Bool
prop_elemIndexL x xs =
    elemIndexL x xs == Data.List.elemIndex x (toList xs)

prop_elemIndicesL :: A -> Seq A -> Bool
prop_elemIndicesL x xs =
    elemIndicesL x xs == Data.List.elemIndices x (toList xs)

prop_elemIndexR :: A -> Seq A -> Bool
prop_elemIndexR x xs =
    elemIndexR x xs == listToMaybe (Prelude.reverse (Data.List.elemIndices x (toList xs)))

prop_elemIndicesR :: A -> Seq A -> Bool
prop_elemIndicesR x xs =
    elemIndicesR x xs == Prelude.reverse (Data.List.elemIndices x (toList xs))

prop_findIndexL :: Positive Int -> Seq Int -> Bool
prop_findIndexL (Positive n) xs =
    findIndexL p xs == Data.List.findIndex p (toList xs)
  where p x = x `mod` n == 0

prop_findIndicesL :: Positive Int -> Seq Int -> Bool
prop_findIndicesL (Positive n) xs =
    findIndicesL p xs == Data.List.findIndices p (toList xs)
  where p x = x `mod` n == 0

prop_findIndexR :: Positive Int -> Seq Int -> Bool
prop_findIndexR (Positive n) xs =
    findIndexR p xs == listToMaybe (Prelude.reverse (Data.List.findIndices p (toList xs)))
  where p x = x `mod` n == 0

prop_findIndicesR :: Positive Int -> Seq Int -> Bool
prop_findIndicesR (Positive n) xs =
    findIndicesR p xs == Prelude.reverse (Data.List.findIndices p (toList xs))
  where p x = x `mod` n == 0

-- * Folds

prop_foldlWithIndex :: [(Int, A)] -> Seq A -> Bool
prop_foldlWithIndex z xs =
    foldlWithIndex f z xs == Data.List.foldl (uncurry . f) z (Data.List.zip [0..] (toList xs))
  where f ys n y = (n,y):ys

prop_foldrWithIndex :: [(Int, A)] -> Seq A -> Bool
prop_foldrWithIndex z xs =
    foldrWithIndex f z xs == Data.List.foldr (uncurry f) z (Data.List.zip [0..] (toList xs))
  where f n y ys = (n,y):ys

prop_foldMapWithIndexL :: (Fun (B, Int, A) B) -> B -> Seq A -> Bool
prop_foldMapWithIndexL (Fun _ f) z t = foldlWithIndex f' z t ==
  appEndo (getDual (foldMapWithIndex (\i -> Dual . Endo . flip (flip f' i)) t)) z
  where f' b i a = f (b, i, a)

prop_foldMapWithIndexR :: (Fun (Int, A, B) B) -> B -> Seq A -> Bool
prop_foldMapWithIndexR (Fun _ f) z t = foldrWithIndex f' z t ==
   appEndo (foldMapWithIndex (\i -> Endo . f' i) t) z
  where f' i a b = f (i, a, b)

-- * Transformations

prop_mapWithIndex :: Seq A -> Bool
prop_mapWithIndex xs =
    toList' (mapWithIndex f xs) ~= map (uncurry f) (Data.List.zip [0..] (toList xs))
  where f = (,)

prop_traverseWithIndex :: Seq Int -> Bool
prop_traverseWithIndex xs =
    runState (traverseWithIndex (\i x -> modify ((i,x) :)) xs) [] ==
    runState (sequenceA . mapWithIndex (\i x -> modify ((i,x) :)) $ xs) [] 

prop_reverse :: Seq A -> Bool
prop_reverse xs =
    toList' (reverse xs) ~= Prelude.reverse (toList xs)

-- ** Zips

prop_zip :: Seq A -> Seq B -> Bool
prop_zip xs ys =
    toList' (zip xs ys) ~= Prelude.zip (toList xs) (toList ys)

prop_zipWith :: Seq A -> Seq B -> Bool
prop_zipWith xs ys =
    toList' (zipWith f xs ys) ~= Prelude.zipWith f (toList xs) (toList ys)
  where f = (,)

prop_zip3 :: Seq A -> Seq B -> Seq C -> Bool
prop_zip3 xs ys zs =
    toList' (zip3 xs ys zs) ~= Prelude.zip3 (toList xs) (toList ys) (toList zs)

prop_zipWith3 :: Seq A -> Seq B -> Seq C -> Bool
prop_zipWith3 xs ys zs =
    toList' (zipWith3 f xs ys zs) ~= Prelude.zipWith3 f (toList xs) (toList ys) (toList zs)
  where f = (,,)

prop_zip4 :: Seq A -> Seq B -> Seq C -> Seq Int -> Bool
prop_zip4 xs ys zs ts =
    toList' (zip4 xs ys zs ts) ~= Data.List.zip4 (toList xs) (toList ys) (toList zs) (toList ts)

prop_zipWith4 :: Seq A -> Seq B -> Seq C -> Seq Int -> Bool
prop_zipWith4 xs ys zs ts =
    toList' (zipWith4 f xs ys zs ts) ~= Data.List.zipWith4 f (toList xs) (toList ys) (toList zs) (toList ts)
  where f = (,,,)

#if MIN_VERSION_base(4,4,0)
-- This comes straight from the MonadZip documentation
prop_mzipNaturality :: Fun A C -> Fun B D -> Seq A -> Seq B -> Property
prop_mzipNaturality f g sa sb =
  fmap (apply f *** apply g) (mzip sa sb) ===
  mzip (apply f <$> sa) (apply g <$> sb)

-- This is a slight optimization of the MonadZip preservation
-- law that works because sequences don't have any decorations.
prop_mzipPreservation :: Fun A B -> Seq A -> Property
prop_mzipPreservation f sa =
  let sb = fmap (apply f) sa
  in munzip (mzip sa sb) === (sa, sb)

-- We want to ensure that
--
-- munzip xs = xs `seq` (fmap fst x, fmap snd x)
--
-- even in the presence of bottoms (alternatives are all balance-
-- fragile).
prop_munzipLazy :: Seq (Integer, B) -> Bool
prop_munzipLazy pairs = deepseq ((`seq` ()) <$> repaired) True
  where
    partialpairs = mapWithIndex (\i a -> update i err pairs) pairs
    firstPieces = fmap (fst . munzip) partialpairs
    repaired = mapWithIndex (\i s -> update i 10000 s) firstPieces
    err = error "munzip isn't lazy enough"
#endif

-- Applicative operations

prop_ap :: Seq A -> Seq B -> Bool
prop_ap xs ys =
    toList' ((,) <$> xs <*> ys) ~= ( (,) <$> toList xs <*> toList ys )

prop_ap_NOINLINE :: Seq A -> Seq B -> Bool
prop_ap_NOINLINE xs ys =
    toList' (((,) <$> xs) `apNOINLINE` ys) ~= ( (,) <$> toList xs <*> toList ys )

{-# NOINLINE apNOINLINE #-}
apNOINLINE :: Seq (a -> b) -> Seq a -> Seq b
apNOINLINE fs xs = fs <*> xs

prop_liftA2 :: Seq A -> Seq B -> Property
prop_liftA2 xs ys = valid q .&&.
    toList q === liftA2 (,) (toList xs) (toList ys)
  where
    q = liftA2 (,) xs ys

prop_then :: Seq A -> Seq B -> Bool
prop_then xs ys =
    toList' (xs *> ys) ~= (toList xs *> toList ys)

prop_intersperse :: A -> Seq A -> Bool
prop_intersperse x xs =
    toList' (intersperse x xs) ~= Data.List.intersperse x (toList xs)

prop_cycleTaking :: Int -> Seq A -> Property
prop_cycleTaking n xs =
    (n <= 0 || not (null xs)) ==> toList' (cycleTaking n xs) ~= Data.List.take n (Data.List.cycle (toList xs))

#if __GLASGOW_HASKELL__ >= 800
prop_empty_pat :: Seq A -> Bool
prop_empty_pat xs@Empty = null xs
prop_empty_pat xs = not (null xs)

prop_empty_con :: Bool
prop_empty_con = null Empty

prop_viewl_pat :: Seq A -> Property
prop_viewl_pat xs@(y :<| ys)
  | z :< zs <- viewl xs = y === z .&&. ys === zs
  | otherwise = property failed
prop_viewl_pat xs = property . liftBool $ null xs

prop_viewl_con :: A -> Seq A -> Property
prop_viewl_con x xs = x :<| xs === x <| xs

prop_viewr_pat :: Seq A -> Property
prop_viewr_pat xs@(ys :|> y)
  | zs :> z <- viewr xs = y === z .&&. ys === zs
  | otherwise = property failed
prop_viewr_pat xs = property . liftBool $ null xs

prop_viewr_con :: Seq A -> A -> Property
prop_viewr_con xs x = xs :|> x === xs |> x
#endif

-- Monad operations

prop_bind :: Seq A -> Fun A (Seq B) -> Bool
prop_bind xs (Fun _ f) =
    toList' (xs >>= f) ~= (toList xs >>= toList . f)

-- Simple test monad

data M a = Action Int a
    deriving (Eq, Show)

instance Functor M where
    fmap f (Action n x) = Action n (f x)

instance Applicative M where
    pure x = Action 0 x
    Action m f <*> Action n x = Action (m+n) (f x)

instance Monad M where
    return x = Action 0 x
    Action m x >>= f = let Action n y = f x in Action (m+n) y

instance Foldable M where
    foldMap f (Action _ x) = f x

instance Traversable M where
    traverse f (Action n x) = Action n <$> f x
