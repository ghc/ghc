{-# LANGUAGE CPP #-}

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#endif

--------------------------------------------------------------------------------

-- | QuickCheck property tests for DNonEmpty.
module DNonEmptyProperties (test) where

--------------------------------------------------------------------------------

import qualified Control.Applicative as Applicative
import qualified Data.DList as DList
import Data.DList.DNonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Semigroup as Semigroup
import QuickCheckUtil
import Test.QuickCheck
import Text.Show.Functions ()
import Prelude hiding (head, map, tail)

--------------------------------------------------------------------------------

prop_model :: NonEmpty Int -> Bool
prop_model = eqWith id (toNonEmpty . fromNonEmpty)

prop_singleton :: Int -> Bool
prop_singleton = eqWith Applicative.pure (toNonEmpty . singleton)

prop_cons :: Int -> NonEmpty Int -> Bool
prop_cons c = eqWith (NonEmpty.cons c) (toNonEmpty . cons c . fromNonEmpty)

prop_snoc :: NonEmpty Int -> Int -> Bool
prop_snoc xs c =
  xs Semigroup.<> Applicative.pure c == toNonEmpty (snoc (fromNonEmpty xs) c)

prop_append :: NonEmpty Int -> NonEmpty Int -> Bool
prop_append xs ys =
  xs Semigroup.<> ys == toNonEmpty (fromNonEmpty xs `append` fromNonEmpty ys)

prop_head :: NonEmpty Int -> Bool
prop_head = eqWith NonEmpty.head (head . fromNonEmpty)

prop_tail :: NonEmpty Int -> Bool
prop_tail = eqWith NonEmpty.tail (DList.toList . tail . fromNonEmpty)

prop_unfoldr :: (Int -> (Int, Maybe Int)) -> Int -> Int -> Property
prop_unfoldr f n =
  eqOn
    (const (n >= 0))
    (NonEmpty.take n . NonEmpty.unfoldr f)
    (NonEmpty.take n . toNonEmpty . unfoldr f)

prop_map :: (Int -> Int) -> NonEmpty Int -> Bool
prop_map f = eqWith (NonEmpty.map f) (toNonEmpty . map f . fromNonEmpty)

prop_show_read :: NonEmpty Int -> Bool
prop_show_read = eqWith id (read . show)

prop_read_show :: NonEmpty Int -> Bool
prop_read_show x = eqWith id (show . f . read) $ "fromNonEmpty " ++ show x
  where
    f :: NonEmpty Int -> NonEmpty Int
    f = id

exampleList :: [Int]
exampleList = [1, 2, 3]

exampleDNonEmpty :: DNonEmpty Int
exampleDNonEmpty = 1 :| DList.fromList [2, 3]

prop_toList :: Bool
prop_toList = toList exampleDNonEmpty == exampleList

prop_fromList :: Bool
prop_fromList = exampleDNonEmpty == fromList exampleList

prop_Semigroup_append :: NonEmpty Int -> NonEmpty Int -> Bool
prop_Semigroup_append xs ys =
  (==)
    (xs Semigroup.<> ys)
    (toNonEmpty (fromNonEmpty xs Semigroup.<> fromNonEmpty ys))

--------------------------------------------------------------------------------

properties :: [(String, Property)]
properties =
  [ ("model", property prop_model),
    ("singleton", property prop_singleton),
    ("cons", property prop_cons),
    ("snoc", property prop_snoc),
    ("append", property prop_append),
    ("head", property prop_head),
    ("tail", property prop_tail),
    ("unfoldr", property prop_unfoldr),
    ("map", property prop_map),
    ("read . show", property prop_show_read),
    ("show . read", property prop_read_show),
    ("toList", property prop_toList),
    ("fromList", property prop_fromList),
    ("Semigroup <>", property prop_Semigroup_append)
  ]

test :: IO ()
test = quickCheckLabeledProperties properties
