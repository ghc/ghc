{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}

-- | Tests for the 'Data.HashSet' module.  We test functions by
-- comparing them to a simpler model, a list.

module Main (main) where

import qualified Data.Foldable as Foldable
import Data.Hashable (Hashable(hashWithSalt))
import qualified Data.List as L
import qualified Data.HashSet as S
import qualified Data.Set as Set
import Data.Ord (comparing)
import Test.QuickCheck (Arbitrary, Property, (==>), (===))
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- Key type that generates more hash collisions.
newtype Key = K { unK :: Int }
            deriving (Arbitrary, Enum, Eq, Integral, Num, Ord, Read, Show, Real)

instance Hashable Key where
    hashWithSalt salt k = hashWithSalt salt (unK k) `mod` 20

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Instances

pEq :: [Key] -> [Key] -> Bool
pEq xs = (Set.fromList xs ==) `eq` (S.fromList xs ==)

pNeq :: [Key] -> [Key] -> Bool
pNeq xs = (Set.fromList xs /=) `eq` (S.fromList xs /=)

-- We cannot compare to `Data.Map` as ordering is different.
pOrd1 :: [Key] -> Bool
pOrd1 xs = compare x x == EQ
  where
    x = S.fromList xs

pOrd2 :: [Key] -> [Key] -> [Key] -> Bool
pOrd2 xs ys zs = case (compare x y, compare y z) of
    (EQ, o)  -> compare x z == o
    (o,  EQ) -> compare x z == o
    (LT, LT) -> compare x z == LT
    (GT, GT) -> compare x z == GT
    (LT, GT) -> True -- ys greater than xs and zs.
    (GT, LT) -> True
  where
    x = S.fromList xs
    y = S.fromList ys
    z = S.fromList zs

pOrd3 :: [Key] -> [Key] -> Bool
pOrd3 xs ys = case (compare x y, compare y x) of
    (EQ, EQ) -> True
    (LT, GT) -> True
    (GT, LT) -> True
    _        -> False
  where
    x = S.fromList xs
    y = S.fromList ys

pOrdEq :: [Key] -> [Key] -> Bool
pOrdEq xs ys = case (compare x y, x == y) of
    (EQ, True)  -> True
    (LT, False) -> True
    (GT, False) -> True
    _           -> False
  where
    x = S.fromList xs
    y = S.fromList ys

pReadShow :: [Key] -> Bool
pReadShow xs = Set.fromList xs == read (show (Set.fromList xs))

pFoldable :: [Int] -> Bool
pFoldable = (L.sort . Foldable.foldr (:) []) `eq`
            (L.sort . Foldable.foldr (:) [])

pPermutationEq :: [Key] -> [Int] -> Bool
pPermutationEq xs is = S.fromList xs == S.fromList ys
  where
    ys = shuffle is xs
    shuffle idxs = L.map snd
                 . L.sortBy (comparing fst)
                 . L.zip (idxs ++ [L.maximum (0:is) + 1 ..])

pHashable :: [Key] -> [Int] -> Int -> Property
pHashable xs is salt =
    x == y ==> hashWithSalt salt x === hashWithSalt salt y
  where
    xs' = L.nub xs
    ys = shuffle is xs'
    x = S.fromList xs'
    y = S.fromList ys
    shuffle idxs = L.map snd
                 . L.sortBy (comparing fst)
                 . L.zip (idxs ++ [L.maximum (0:is) + 1 ..])

------------------------------------------------------------------------
-- ** Basic interface

pSize :: [Key] -> Bool
pSize = Set.size `eq` S.size

pMember :: Key -> [Key] -> Bool
pMember k = Set.member k `eq` S.member k

pInsert :: Key -> [Key] -> Bool
pInsert a = Set.insert a `eq_` S.insert a

pDelete :: Key -> [Key] -> Bool
pDelete a = Set.delete a `eq_` S.delete a

------------------------------------------------------------------------
-- ** Combine

pUnion :: [Key] -> [Key] -> Bool
pUnion xs ys = Set.union (Set.fromList xs) `eq_`
               S.union (S.fromList xs) $ ys

------------------------------------------------------------------------
-- ** Transformations

pMap :: [Key] -> Bool
pMap = Set.map (+ 1) `eq_` S.map (+ 1)

------------------------------------------------------------------------
-- ** Folds

pFoldr :: [Int] -> Bool
pFoldr = (L.sort . foldrSet (:) []) `eq`
         (L.sort . S.foldr (:) [])

foldrSet :: (a -> b -> b) -> b -> Set.Set a -> b
#if MIN_VERSION_containers(0,4,2)
foldrSet = Set.foldr
#else
foldrSet = Foldable.foldr
#endif

pFoldl' :: Int -> [Int] -> Bool
pFoldl' z0 = foldl'Set (+) z0 `eq` S.foldl' (+) z0

foldl'Set :: (a -> b -> a) -> a -> Set.Set b -> a
#if MIN_VERSION_containers(0,4,2)
foldl'Set = Set.foldl'
#else
foldl'Set = Foldable.foldl'
#endif

------------------------------------------------------------------------
-- ** Filter

pFilter :: [Key] -> Bool
pFilter = Set.filter odd `eq_` S.filter odd

------------------------------------------------------------------------
-- ** Conversions

pToList :: [Key] -> Bool
pToList = Set.toAscList `eq` toAscList

------------------------------------------------------------------------
-- * Test list

tests :: [Test]
tests =
    [
    -- Instances
      testGroup "instances"
      [ testProperty "==" pEq
      , testProperty "Permutation ==" pPermutationEq
      , testProperty "/=" pNeq
      , testProperty "compare reflexive" pOrd1
      , testProperty "compare transitive" pOrd2
      , testProperty "compare antisymmetric" pOrd3
      , testProperty "Ord => Eq" pOrdEq
      , testProperty "Read/Show" pReadShow
      , testProperty "Foldable" pFoldable
      , testProperty "Hashable" pHashable
      ]
    -- Basic interface
    , testGroup "basic interface"
      [ testProperty "size" pSize
      , testProperty "member" pMember
      , testProperty "insert" pInsert
      , testProperty "delete" pDelete
      ]
    -- Combine
    , testProperty "union" pUnion
    -- Transformations
    , testProperty "map" pMap
    -- Folds
    , testGroup "folds"
      [ testProperty "foldr" pFoldr
      , testProperty "foldl'" pFoldl'
      ]
    -- Filter
    , testGroup "filter"
      [ testProperty "filter" pFilter
      ]
    -- Conversions
    , testGroup "conversions"
      [ testProperty "toList" pToList
      ]
    ]

------------------------------------------------------------------------
-- * Model

-- Invariant: the list is sorted in ascending order, by key.
type Model a = Set.Set a

-- | Check that a function operating on a 'HashMap' is equivalent to
-- one operating on a 'Model'.
eq :: (Eq a, Hashable a, Ord a, Eq b)
   => (Model a -> b)      -- ^ Function that modifies a 'Model' in the same
                          -- way
   -> (S.HashSet a -> b)  -- ^ Function that modified a 'HashSet'
   -> [a]                 -- ^ Initial content of the 'HashSet' and 'Model'
   -> Bool                -- ^ True if the functions are equivalent
eq f g xs = g (S.fromList xs) == f (Set.fromList xs)

eq_ :: (Eq a, Hashable a, Ord a)
    => (Model a -> Model a)          -- ^ Function that modifies a 'Model'
    -> (S.HashSet a -> S.HashSet a)  -- ^ Function that modified a
                                     -- 'HashSet' in the same way
    -> [a]                           -- ^ Initial content of the 'HashSet'
                                     -- and 'Model'
    -> Bool                          -- ^ True if the functions are
                                     -- equivalent
eq_ f g = (Set.toAscList . f) `eq` (toAscList . g)

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests

------------------------------------------------------------------------
-- * Helpers

toAscList :: Ord a => S.HashSet a -> [a]
toAscList = L.sort . S.toList
