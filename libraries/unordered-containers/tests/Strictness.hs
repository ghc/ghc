{-# LANGUAGE CPP, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.Hashable (Hashable(hashWithSalt))
import Test.ChasingBottoms.IsBottom
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(arbitrary), Property, (===), (.&&.))
import Test.QuickCheck.Function
import Test.QuickCheck.Poly (A)
import Data.Maybe (fromMaybe, isJust)
import Control.Arrow (second)
import Control.Monad (guard)
import Data.Foldable (foldl')
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$))
import Data.Foldable (all)
import Prelude hiding (all)
#endif

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

-- Key type that generates more hash collisions.
newtype Key = K { unK :: Int }
            deriving (Arbitrary, Eq, Ord, Show)

instance Hashable Key where
    hashWithSalt salt k = hashWithSalt salt (unK k) `mod` 20

instance (Arbitrary k, Arbitrary v, Eq k, Hashable k) =>
         Arbitrary (HashMap k v) where
    arbitrary = HM.fromList `fmap` arbitrary

instance Show (Int -> Int) where
    show _ = "<function>"

instance Show (Int -> Int -> Int) where
    show _ = "<function>"

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Strict module

pSingletonKeyStrict :: Int -> Bool
pSingletonKeyStrict v = isBottom $ HM.singleton (bottom :: Key) v

pSingletonValueStrict :: Key -> Bool
pSingletonValueStrict k = isBottom $ (HM.singleton k (bottom :: Int))

pLookupDefaultKeyStrict :: Int -> HashMap Key Int -> Bool
pLookupDefaultKeyStrict def m = isBottom $ HM.lookupDefault def bottom m

pFindWithDefaultKeyStrict :: Int -> HashMap Key Int -> Bool
pFindWithDefaultKeyStrict def m = isBottom $ HM.findWithDefault def bottom m

pAdjustKeyStrict :: (Int -> Int) -> HashMap Key Int -> Bool
pAdjustKeyStrict f m = isBottom $ HM.adjust f bottom m

pAdjustValueStrict :: Key -> HashMap Key Int -> Bool
pAdjustValueStrict k m
    | k `HM.member` m = isBottom $ HM.adjust (const bottom) k m
    | otherwise       = case HM.keys m of
        []     -> True
        (k':_) -> isBottom $ HM.adjust (const bottom) k' m

pInsertKeyStrict :: Int -> HashMap Key Int -> Bool
pInsertKeyStrict v m = isBottom $ HM.insert bottom v m

pInsertValueStrict :: Key -> HashMap Key Int -> Bool
pInsertValueStrict k m = isBottom $ HM.insert k bottom m

pInsertWithKeyStrict :: (Int -> Int -> Int) -> Int -> HashMap Key Int -> Bool
pInsertWithKeyStrict f v m = isBottom $ HM.insertWith f bottom v m

pInsertWithValueStrict :: (Int -> Int -> Int) -> Key -> Int -> HashMap Key Int
                       -> Bool
pInsertWithValueStrict f k v m
    | HM.member k m = isBottom $ HM.insertWith (const2 bottom) k v m
    | otherwise     = isBottom $ HM.insertWith f k bottom m

pFromListKeyStrict :: Bool
pFromListKeyStrict = isBottom $ HM.fromList [(undefined :: Key, 1 :: Int)]

pFromListValueStrict :: Bool
pFromListValueStrict = isBottom $ HM.fromList [(K 1, undefined)]

pFromListWithKeyStrict :: (Int -> Int -> Int) -> Bool
pFromListWithKeyStrict f =
    isBottom $ HM.fromListWith f [(undefined :: Key, 1 :: Int)]

-- The strictness properties of 'fromListWith' are not entirely
-- trivial.
-- fromListWith f kvs is strict in the first value seen for each
-- key, but potentially lazy in the rest: the combining function
-- could be lazy in the "new" value. fromListWith must, however,
-- be strict in whatever value is actually inserted into the map.
-- Getting all these properties specified efficiently seems tricky.
-- Since it's not hard, we verify that the converted HashMap has
-- no unforced values. Rather than trying to go into detail for the
-- rest, this test compares the strictness behavior of fromListWith
-- to that of insertWith. The latter should be easier to specify
-- and (if we choose to do so) test thoroughly.
--
-- We'll fake up a representation of things that are possibly
-- bottom by using Nothing to represent bottom. The combining
-- (partial) function is represented by a "lazy total" function
-- Maybe a -> Maybe a -> Maybe a, along with a function determining
-- whether the result should be non-bottom, Maybe a -> Maybe a -> Bool,
-- indicating how the combining function should behave if neither
-- argument, just the first argument, just the second argument,
-- or both arguments are bottom. It would be quite tempting to
-- just use Maybe A -> Maybe A -> Maybe A, but that would not
-- necessarily be continous.
pFromListWithValueResultStrict :: [(Key, Maybe A)]
                               -> Fun (Maybe A, Maybe A) A
                               -> Fun (Maybe A, Maybe A) Bool
                               -> Property
pFromListWithValueResultStrict lst comb_lazy calc_good_raw
         = all (all isJust) recovered .&&. (recovered === recover (fmap recover fake_map))
  where
    recovered :: Maybe (HashMap Key (Maybe A))
    recovered = recover (fmap recover real_map)
    -- What we get out of the conversion using insertWith
    fake_map = foldl' (\m (k,v) -> HM.insertWith real_comb k v m) HM.empty real_list

    -- A continuous version of calc_good_raw
    calc_good Nothing Nothing = cgr Nothing Nothing
    calc_good Nothing y@(Just _) = cgr Nothing Nothing || cgr Nothing y
    calc_good x@(Just _) Nothing = cgr Nothing Nothing || cgr x Nothing
    calc_good x y = cgr Nothing Nothing || cgr Nothing y || cgr x Nothing || cgr x y
    cgr = curry $ apply calc_good_raw

    -- The Maybe A -> Maybe A -> Maybe A that we're after, representing a
    -- potentially less total function than comb_lazy
    comb x y = apply comb_lazy (x, y) <$ guard (calc_good x y)

    -- What we get out of the conversion using fromListWith
    real_map = HM.fromListWith real_comb real_list

    -- A list that may have actual bottom values in it.
    real_list = map (second (fromMaybe bottom)) lst

    -- A genuinely partial function mirroring comb
    real_comb x y = fromMaybe bottom $ comb (recover x) (recover y)

    recover :: a -> Maybe a
    recover a = a <$ guard (not $ isBottom a)

------------------------------------------------------------------------
-- * Test list

tests :: [Test]
tests =
    [
    -- Basic interface
      testGroup "HashMap.Strict"
      [ testProperty "singleton is key-strict" pSingletonKeyStrict
      , testProperty "singleton is value-strict" pSingletonValueStrict
      , testProperty "member is key-strict" $ keyStrict HM.member
      , testProperty "lookup is key-strict" $ keyStrict HM.lookup
      , testProperty "lookupDefault is key-strict" pLookupDefaultKeyStrict
      , testProperty "findWithDefault is key-strict" pFindWithDefaultKeyStrict
      , testProperty "! is key-strict" $ keyStrict (flip (HM.!))
      , testProperty "delete is key-strict" $ keyStrict HM.delete
      , testProperty "adjust is key-strict" pAdjustKeyStrict
      , testProperty "adjust is value-strict" pAdjustValueStrict
      , testProperty "insert is key-strict" pInsertKeyStrict
      , testProperty "insert is value-strict" pInsertValueStrict
      , testProperty "insertWith is key-strict" pInsertWithKeyStrict
      , testProperty "insertWith is value-strict" pInsertWithValueStrict
      , testProperty "fromList is key-strict" pFromListKeyStrict
      , testProperty "fromList is value-strict" pFromListValueStrict
      , testProperty "fromListWith is key-strict" pFromListWithKeyStrict
      , testProperty "fromListWith is value-strict" pFromListWithValueResultStrict
      ]
    ]

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests

------------------------------------------------------------------------
-- * Utilities

keyStrict :: (Key -> HashMap Key Int -> a) -> HashMap Key Int -> Bool
keyStrict f m = isBottom $ f bottom m

const2 :: a -> b -> c -> a
const2 x _ _ = x
