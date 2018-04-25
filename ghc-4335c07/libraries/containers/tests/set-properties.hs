{-# LANGUAGE CPP #-}
import qualified Data.IntSet as IntSet
import Data.List (nub,sort)
import qualified Data.List as List
import Data.Monoid (mempty)
import Data.Maybe
import Data.Set
import Prelude hiding (lookup, null, map, filter, foldr, foldl, all, take, drop, splitAt)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test, Testable)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Poly
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad (liftM, liftM3)
import Data.Functor.Identity
import Data.Foldable (all)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..), (<$>))
#endif

main :: IO ()
main = defaultMain [ testCase "lookupLT" test_lookupLT
                   , testCase "lookupGT" test_lookupGT
                   , testCase "lookupLE" test_lookupLE
                   , testCase "lookupGE" test_lookupGE
                   , testCase "lookupIndex" test_lookupIndex
                   , testCase "findIndex" test_findIndex
                   , testCase "elemAt" test_elemAt
                   , testCase "deleteAt" test_deleteAt
                   , testProperty "prop_Valid" prop_Valid
                   , testProperty "prop_Single" prop_Single
                   , testProperty "prop_Member" prop_Member
                   , testProperty "prop_NotMember" prop_NotMember
                   , testProperty "prop_LookupLT" prop_LookupLT
                   , testProperty "prop_LookupGT" prop_LookupGT
                   , testProperty "prop_LookupLE" prop_LookupLE
                   , testProperty "prop_LookupGE" prop_LookupGE
                   , testProperty "prop_InsertValid" prop_InsertValid
                   , testProperty "prop_InsertDelete" prop_InsertDelete
                   , testProperty "prop_InsertBiased" prop_InsertBiased
                   , testProperty "prop_DeleteValid" prop_DeleteValid
                   , testProperty "prop_Link" prop_Link
                   , testProperty "prop_Merge" prop_Merge
                   , testProperty "prop_UnionValid" prop_UnionValid
                   , testProperty "prop_UnionInsert" prop_UnionInsert
                   , testProperty "prop_UnionAssoc" prop_UnionAssoc
                   , testProperty "prop_UnionComm" prop_UnionComm
                   , testProperty "prop_UnionBiased" prop_UnionBiased
                   , testProperty "prop_DiffValid" prop_DiffValid
                   , testProperty "prop_Diff" prop_Diff
                   , testProperty "prop_IntValid" prop_IntValid
                   , testProperty "prop_Int" prop_Int
                   , testProperty "prop_IntBiased" prop_IntBiased
                   , testProperty "prop_Ordered" prop_Ordered
                   , testProperty "prop_DescendingOrdered" prop_DescendingOrdered
                   , testProperty "prop_List" prop_List
                   , testProperty "prop_DescList" prop_DescList
                   , testProperty "prop_AscDescList" prop_AscDescList
                   , testProperty "prop_fromList" prop_fromList
                   , testProperty "prop_fromListDesc" prop_fromListDesc
                   , testProperty "prop_isProperSubsetOf" prop_isProperSubsetOf
                   , testProperty "prop_isProperSubsetOf2" prop_isProperSubsetOf2
                   , testProperty "prop_isSubsetOf" prop_isSubsetOf
                   , testProperty "prop_isSubsetOf2" prop_isSubsetOf2
                   , testProperty "prop_size" prop_size
                   , testProperty "prop_lookupMax" prop_lookupMax
                   , testProperty "prop_lookupMin" prop_lookupMin
                   , testProperty "prop_findMax" prop_findMax
                   , testProperty "prop_findMin" prop_findMin
                   , testProperty "prop_ord" prop_ord
                   , testProperty "prop_readShow" prop_readShow
                   , testProperty "prop_foldR" prop_foldR
                   , testProperty "prop_foldR'" prop_foldR'
                   , testProperty "prop_foldL" prop_foldL
                   , testProperty "prop_foldL'" prop_foldL'
                   , testProperty "prop_map" prop_map
                   , testProperty "prop_map2" prop_map2
                   , testProperty "prop_mapMonotonic" prop_mapMonotonic
                   , testProperty "prop_maxView" prop_maxView
                   , testProperty "prop_minView" prop_minView
                   , testProperty "prop_split" prop_split
                   , testProperty "prop_splitMember" prop_splitMember
                   , testProperty "prop_splitRoot" prop_splitRoot
                   , testProperty "prop_partition" prop_partition
                   , testProperty "prop_filter" prop_filter
                   , testProperty "takeWhileAntitone"    prop_takeWhileAntitone
                   , testProperty "dropWhileAntitone"    prop_dropWhileAntitone
                   , testProperty "spanAntitone"         prop_spanAntitone
                   , testProperty "take"                 prop_take
                   , testProperty "drop"                 prop_drop
                   , testProperty "splitAt"              prop_splitAt
                   ]

-- A type with a peculiar Eq instance designed to make sure keys
-- come from where they're supposed to.
data OddEq a = OddEq a Bool deriving (Show)

getOddEq :: OddEq a -> (a, Bool)
getOddEq (OddEq b a) = (b, a)
instance Arbitrary a => Arbitrary (OddEq a) where
  arbitrary = OddEq <$> arbitrary <*> arbitrary
instance Eq a => Eq (OddEq a) where
  OddEq x _ == OddEq y _ = x == y
instance Ord a => Ord (OddEq a) where
  OddEq x _ `compare` OddEq y _ = x `compare` y

----------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------

test_lookupLT :: Assertion
test_lookupLT = do
    lookupLT 3 (fromList [3, 5]) @?= Nothing
    lookupLT 5 (fromList [3, 5]) @?= Just 3

test_lookupGT :: Assertion
test_lookupGT = do
   lookupGT 4 (fromList [3, 5]) @?= Just 5
   lookupGT 5 (fromList [3, 5]) @?= Nothing

test_lookupLE :: Assertion
test_lookupLE = do
   lookupLE 2 (fromList [3, 5]) @?= Nothing
   lookupLE 4 (fromList [3, 5]) @?= Just 3
   lookupLE 5 (fromList [3, 5]) @?= Just 5

test_lookupGE :: Assertion
test_lookupGE = do
   lookupGE 3 (fromList [3, 5]) @?= Just 3
   lookupGE 4 (fromList [3, 5]) @?= Just 5
   lookupGE 6 (fromList [3, 5]) @?= Nothing

{--------------------------------------------------------------------
  Indexed
--------------------------------------------------------------------}

test_lookupIndex :: Assertion
test_lookupIndex = do
    isJust   (lookupIndex 2 (fromList [5,3])) @?= False
    fromJust (lookupIndex 3 (fromList [5,3])) @?= 0
    fromJust (lookupIndex 5 (fromList [5,3])) @?= 1
    isJust   (lookupIndex 6 (fromList [5,3])) @?= False

test_findIndex :: Assertion
test_findIndex = do
    findIndex 3 (fromList [5,3]) @?= 0
    findIndex 5 (fromList [5,3]) @?= 1

test_elemAt :: Assertion
test_elemAt = do
    elemAt 0 (fromList [5,3]) @?= 3
    elemAt 1 (fromList [5,3]) @?= 5

test_deleteAt :: Assertion
test_deleteAt = do
    deleteAt 0 (fromList [5,3]) @?= singleton 5
    deleteAt 1 (fromList [5,3]) @?= singleton 3

{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}

-- | The IsInt class lets us constrain a type variable to be Int in an entirely
-- standard way. The constraint @ IsInt a @ is essentially equivalent to the
-- GHC-only constraint @ a ~ Int @, but @ IsInt @ requires manual intervention
-- to use. If ~ is ever standardized, we should certainly use it instead.
-- Earlier versions used an Enum constraint, but this is confusing because
-- not all Enum instances will work properly for the Arbitrary instance here.
class (Show a, Read a, Integral a, Arbitrary a) => IsInt a where
  fromIntF :: f Int -> f a

instance IsInt Int where
  fromIntF = id

-- | Convert an Int to any instance of IsInt
fromInt :: IsInt a => Int -> a
fromInt = runIdentity . fromIntF . Identity

{- We don't actually need this, but we can add it if we ever do
toIntF :: IsInt a => g a -> g Int
toIntF = unf . fromIntF . F $ id

newtype F g a b = F {unf :: g b -> a}

toInt :: IsInt a => a -> Int
toInt = runIdentity . toIntF . Identity -}


-- How much the minimum value of an arbitrary set should vary
positionFactor :: Int
positionFactor = 1

-- How much the gap between consecutive elements in an arbitrary
-- set should vary
gapRange :: Int
gapRange = 5

instance IsInt a => Arbitrary (Set a) where
  arbitrary = sized (\sz0 -> do
        sz <- choose (0, sz0)
        middle <- choose (-positionFactor * (sz + 1), positionFactor * (sz + 1))
        let shift = (sz * (gapRange) + 1) `quot` 2
            start = middle - shift
        t <- evalStateT (mkArb step sz) start
        if valid t then pure t else error "Test generated invalid tree!")
    where
      step = do
        i <- get
        diff <- lift $ choose (1, gapRange)
        let i' = i + diff
        put i'
        pure (fromInt i')

class Monad m => MonadGen m where
  liftGen :: Gen a -> m a
instance MonadGen Gen where
  liftGen = id
instance MonadGen m => MonadGen (StateT s m) where
  liftGen = lift . liftGen

-- | Given an action that produces successively larger elements and
-- a size, produce a set of arbitrary shape with exactly that size.
mkArb :: MonadGen m => m a -> Int -> m (Set a)
mkArb step n
  | n <= 0 = return Tip
  | n == 1 = singleton `liftM` step
  | n == 2 = do
     dir <- liftGen arbitrary
     p <- step
     q <- step
     if dir
       then return (Bin 2 q (singleton p) Tip)
       else return (Bin 2 p Tip (singleton q))
  | otherwise = do
      -- This assumes a balance factor of delta = 3
      let upper = (3*(n - 1)) `quot` 4
      let lower = (n + 2) `quot` 4
      ln <- liftGen $ choose (lower, upper)
      let rn = n - ln - 1
      liftM3 (\lt x rt -> Bin n x lt rt) (mkArb step ln) step (mkArb step rn)

-- | Given a strictly increasing list of elements, produce an arbitrarily
-- shaped set with exactly those elements.
setFromList :: [a] -> Gen (Set a)
setFromList xs = flip evalStateT xs $ mkArb step (length xs)
  where
    step = do
      x : xs <- get
      put xs
      pure x

data TwoSets = TwoSets (Set Int) (Set Int) deriving (Show)

data TwoLists a = TwoLists [a] [a]

data Options2 = One2 | Two2 | Both2 deriving (Bounded, Enum)
instance Arbitrary Options2 where
  arbitrary = arbitraryBoundedEnum

-- We produce two lists from a simple "universe". This instance
-- is intended to give good results when the two lists are then
-- combined with each other; if other elements are used with them,
-- they may or may not behave particularly well.
instance IsInt a => Arbitrary (TwoLists a) where
  arbitrary = sized $ \sz0 -> do
    sz <- choose (0, sz0)
    let universe = [0,3..3*(fromInt sz - 1)]
    divide2Gen universe

instance Arbitrary TwoSets where
  arbitrary = do
    TwoLists l r <- arbitrary
    TwoSets <$> setFromList l <*> setFromList r

divide2Gen :: [a] -> Gen (TwoLists a)
divide2Gen [] = pure (TwoLists [] [])
divide2Gen (x : xs) = do
  way <- arbitrary
  TwoLists ls rs <- divide2Gen xs
  case way of
    One2 -> pure (TwoLists (x : ls) rs)
    Two2 -> pure (TwoLists ls (x : rs))
    Both2 -> pure (TwoLists (x : ls) (x : rs))

{--------------------------------------------------------------------
  Valid trees
--------------------------------------------------------------------}
forValid :: (IsInt a,Testable b) => (Set a -> b) -> Property
forValid f = forAll arbitrary $ \t ->
    classify (size t == 0) "empty" $
    classify (size t > 0  && size t <= 10) "small" $
    classify (size t > 10 && size t <= 64) "medium" $
    classify (size t > 64) "large" $ f t

forValidUnitTree :: Testable a => (Set Int -> a) -> Property
forValidUnitTree f = forValid f

prop_Valid :: Property
prop_Valid = forValidUnitTree $ \t -> valid t

{--------------------------------------------------------------------
  Single, Member, Insert, Delete
--------------------------------------------------------------------}
prop_Single :: Int -> Bool
prop_Single x = (insert x empty == singleton x)

prop_Member :: [Int] -> Int -> Bool
prop_Member xs n =
  let m  = fromList xs
  in all (\k -> k `member` m == (k `elem` xs)) (n : xs)

prop_NotMember :: [Int] -> Int -> Bool
prop_NotMember xs n =
  let m  = fromList xs
  in all (\k -> k `notMember` m == (k `notElem` xs)) (n : xs)

test_LookupSomething :: (Int -> Set Int -> Maybe Int) -> (Int -> Int -> Bool) -> [Int] -> Bool
test_LookupSomething lookup' cmp xs =
  let odd_sorted_xs = filter_odd $ nub $ sort xs
      t = fromList odd_sorted_xs
      test x = case List.filter (`cmp` x) odd_sorted_xs of
                 []             -> lookup' x t == Nothing
                 cs | 0 `cmp` 1 -> lookup' x t == Just (last cs) -- we want largest such element
                    | otherwise -> lookup' x t == Just (head cs) -- we want smallest such element
  in all test xs

  where filter_odd [] = []
        filter_odd [_] = []
        filter_odd (_ : o : xs) = o : filter_odd xs

prop_LookupLT :: [Int] -> Bool
prop_LookupLT = test_LookupSomething lookupLT (<)

prop_LookupGT :: [Int] -> Bool
prop_LookupGT = test_LookupSomething lookupGT (>)

prop_LookupLE :: [Int] -> Bool
prop_LookupLE = test_LookupSomething lookupLE (<=)

prop_LookupGE :: [Int] -> Bool
prop_LookupGE = test_LookupSomething lookupGE (>=)

prop_InsertValid :: Int -> Property
prop_InsertValid k = forValidUnitTree $ \t -> valid (insert k t)

prop_InsertDelete :: Int -> Set Int -> Property
prop_InsertDelete k t = not (member k t) ==> delete k (insert k t) == t

prop_InsertBiased :: Int -> Set Int -> Bool
prop_InsertBiased k t = (k, True) `member` kt
  where
    t' = mapMonotonic (`OddEq` False) t
    kt' = insert (OddEq k True) t'
    kt = mapMonotonic getOddEq kt'

prop_DeleteValid :: Int -> Property
prop_DeleteValid k = forValidUnitTree $ \t -> valid (delete k (insert k t))

{--------------------------------------------------------------------
  Balance
--------------------------------------------------------------------}
prop_Link :: Int -> Property
prop_Link x = forValidUnitTree $ \t ->
    let (l,r) = split x t
    in valid (link x l r)

prop_Merge :: Int -> Property
prop_Merge x = forValidUnitTree $ \t ->
    let (l,r) = split x t
    in valid (merge l r)

{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
prop_UnionValid :: Property
prop_UnionValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (union t1 t2)

prop_UnionInsert :: Int -> Set Int -> Bool
prop_UnionInsert x t = union t (singleton x) == insert x t

prop_UnionAssoc :: Set Int -> Set Int -> Set Int -> Bool
prop_UnionAssoc t1 t2 t3 = union t1 (union t2 t3) == union (union t1 t2) t3

prop_UnionComm :: TwoSets -> Bool
prop_UnionComm (TwoSets t1 t2) = (union t1 t2 == union t2 t1)

prop_UnionBiased :: TwoSets -> Property
prop_UnionBiased (TwoSets l r) = union l' r' === union l' (difference r' l')
  where
    l' = mapMonotonic (`OddEq` False) l
    r' = mapMonotonic (`OddEq` True) r

prop_IntBiased :: TwoSets -> Bool
prop_IntBiased (TwoSets l r) = all (\(OddEq _ b) -> not b) l'r'
  where
    l' = mapMonotonic (`OddEq` False) l
    r' = mapMonotonic (`OddEq` True) r
    l'r' = intersection l' r'

prop_DiffValid :: Property
prop_DiffValid = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (difference t1 t2)

prop_Diff :: [Int] -> [Int] -> Bool
prop_Diff xs ys = toAscList (difference (fromList xs) (fromList ys))
                  == List.sort ((List.\\) (nub xs)  (nub ys))

prop_IntValid :: Property
prop_IntValid = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (intersection t1 t2)

prop_Int :: [Int] -> [Int] -> Bool
prop_Int xs ys = toAscList (intersection (fromList xs) (fromList ys))
                 == List.sort (nub ((List.intersect) (xs)  (ys)))

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
prop_Ordered :: Property
prop_Ordered = forAll (choose (5,100)) $ \n ->
    let xs = [0..n::Int]
    in fromAscList xs === fromList xs

prop_DescendingOrdered :: Property
prop_DescendingOrdered = forAll (choose (5,100)) $ \n ->
    let xs = [n,n-1..0::Int]
    in fromDescList xs === fromList xs

prop_List :: [Int] -> Bool
prop_List xs = (sort (nub xs) == toList (fromList xs))

prop_DescList :: [Int] -> Bool
prop_DescList xs = (reverse (sort (nub xs)) == toDescList (fromList xs))

prop_AscDescList :: [Int] -> Bool
prop_AscDescList xs = toAscList s == reverse (toDescList s)
  where s = fromList xs

prop_fromList :: [Int] -> Property
prop_fromList xs =
           t === fromAscList sort_xs .&&.
           t === fromDistinctAscList nub_sort_xs .&&.
           t === List.foldr insert empty xs
  where t = fromList xs
        sort_xs = sort xs
        nub_sort_xs = List.map List.head $ List.group sort_xs

prop_fromListDesc :: [Int] -> Property
prop_fromListDesc xs =
           t === fromDescList sort_xs .&&.
           t === fromDistinctDescList nub_sort_xs .&&.
           t === List.foldr insert empty xs
  where t = fromList xs
        sort_xs = reverse (sort xs)
        nub_sort_xs = List.map List.head $ List.group sort_xs

{--------------------------------------------------------------------
  Set operations are like IntSet operations
--------------------------------------------------------------------}
toIntSet :: Set Int -> IntSet.IntSet
toIntSet = IntSet.fromList . toList

-- Check that Set Int.isProperSubsetOf is the same as Set.isProperSubsetOf.
prop_isProperSubsetOf :: TwoSets -> Bool
prop_isProperSubsetOf (TwoSets a b) = isProperSubsetOf a b == IntSet.isProperSubsetOf (toIntSet a) (toIntSet b)

-- In the above test, isProperSubsetOf almost always returns False (since a
-- random set is almost never a subset of another random set).  So this second
-- test checks the True case.
prop_isProperSubsetOf2 :: TwoSets -> Bool
prop_isProperSubsetOf2 (TwoSets a b) = isProperSubsetOf a c == (a /= c) where
  c = union a b

prop_isSubsetOf :: TwoSets -> Bool
prop_isSubsetOf (TwoSets a b) = isSubsetOf a b == IntSet.isSubsetOf (toIntSet a) (toIntSet b)

prop_isSubsetOf2 :: TwoSets -> Bool
prop_isSubsetOf2 (TwoSets a b) = isSubsetOf a (union a b)

prop_size :: Set Int -> Bool
prop_size s = size s == List.length (toList s)

prop_findMax :: Set Int -> Property
prop_findMax s = not (null s) ==> findMax s == maximum (toList s)

prop_findMin :: Set Int -> Property
prop_findMin s = not (null s) ==> findMin s == minimum (toList s)

prop_lookupMin :: Set Int -> Property
prop_lookupMin m = lookupMin m === (fst <$> minView m)

prop_lookupMax :: Set Int -> Property
prop_lookupMax m = lookupMax m === (fst <$> maxView m)

prop_ord :: TwoSets -> Bool
prop_ord (TwoSets s1 s2) = s1 `compare` s2 == toList s1 `compare` toList s2

prop_readShow :: Set Int -> Bool
prop_readShow s = s == read (show s)

prop_foldR :: Set Int -> Bool
prop_foldR s = foldr (:) [] s == toList s

prop_foldR' :: Set Int -> Bool
prop_foldR' s = foldr' (:) [] s == toList s

prop_foldL :: Set Int -> Bool
prop_foldL s = foldl (flip (:)) [] s == List.foldl (flip (:)) [] (toList s)

prop_foldL' :: Set Int -> Bool
prop_foldL' s = foldl' (flip (:)) [] s == List.foldl' (flip (:)) [] (toList s)

prop_map :: Set Int -> Bool
prop_map s = map id s == s

prop_map2 :: Fun Int Int -> Fun Int Int -> Set Int -> Property
prop_map2 f g s = map (apply f) (map (apply g) s) === map (apply f . apply g) s

prop_mapMonotonic :: Set Int -> Property
prop_mapMonotonic s = mapMonotonic id s === s

prop_maxView :: Set Int -> Bool
prop_maxView s = case maxView s of
    Nothing -> null s
    Just (m,s') -> m == maximum (toList s) && s == insert m s' && m `notMember` s'

prop_minView :: Set Int -> Bool
prop_minView s = case minView s of
    Nothing -> null s
    Just (m,s') -> m == minimum (toList s) && s == insert m s' && m `notMember` s'

prop_split :: Set Int -> Int -> Bool
prop_split s i = case split i s of
    (s1,s2) -> all (<i) (toList s1) && all (>i) (toList s2) && i `delete` s == union s1 s2

prop_splitMember :: Set Int -> Int -> Bool
prop_splitMember s i = case splitMember i s of
    (s1,t,s2) -> all (<i) (toList s1) && all (>i) (toList s2) && t == i `member` s && i `delete` s == union s1 s2

prop_splitRoot :: Set Int -> Bool
prop_splitRoot s = loop ls && (s == unions ls)
 where
  ls = splitRoot s
  loop [] = True
  loop (s1:rst) = List.null
                  [ (x,y) | x <- toList s1
                          , y <- toList (unions rst)
                          , x > y ]

prop_partition :: Set Int -> Int -> Bool
prop_partition s i = case partition odd s of
    (s1,s2) -> all odd (toList s1) && all even (toList s2) && s == s1 `union` s2

prop_filter :: Set Int -> Int -> Bool
prop_filter s i = partition odd s == (filter odd s, filter even s)

prop_take :: Int -> Set Int -> Property
prop_take n xs = valid taken .&&.
                 taken === fromDistinctAscList (List.take n (toList xs))
  where
    taken = take n xs

prop_drop :: Int -> Set Int -> Property
prop_drop n xs = valid dropped .&&.
                 dropped === fromDistinctAscList (List.drop n (toList xs))
  where
    dropped = drop n xs

prop_splitAt :: Int -> Set Int -> Property
prop_splitAt n xs = valid taken .&&.
                    valid dropped .&&.
                    taken === take n xs .&&.
                    dropped === drop n xs
  where
    (taken, dropped) = splitAt n xs

prop_takeWhileAntitone :: [Either Int Int] -> Property
prop_takeWhileAntitone xs' = valid tw .&&. tw === filter isLeft xs
  where
    xs = fromList xs'
    tw = takeWhileAntitone isLeft xs

prop_dropWhileAntitone :: [Either Int Int] -> Property
prop_dropWhileAntitone xs' = valid tw .&&. tw === filter (not . isLeft) xs
  where
    xs = fromList xs'
    tw = dropWhileAntitone isLeft xs

prop_spanAntitone :: [Either Int Int] -> Property
prop_spanAntitone xs' = valid tw .&&. valid dw
                        .&&. tw === takeWhileAntitone isLeft xs
                        .&&. dw === dropWhileAntitone isLeft xs
  where
    xs = fromList xs'
    (tw, dw) = spanAntitone isLeft xs

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
