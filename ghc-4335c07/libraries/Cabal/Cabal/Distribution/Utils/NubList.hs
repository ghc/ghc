{-# LANGUAGE DeriveDataTypeable #-}
module Distribution.Utils.NubList
    ( NubList    -- opaque
    , toNubList  -- smart construtor
    , fromNubList
    , overNubList

    , NubListR
    , toNubListR
    , fromNubListR
    , overNubListR
    ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Simple.Utils

import qualified Text.Read as R

-- | NubList : A de-duplicated list that maintains the original order.
newtype NubList a =
    NubList { fromNubList :: [a] }
    deriving (Eq, Typeable)

-- NubList assumes that nub retains the list order while removing duplicate
-- elements (keeping the first occurence). Documentation for "Data.List.nub"
-- does not specifically state that ordering is maintained so we will add a test
-- for that to the test suite.

-- | Smart constructor for the NubList type.
toNubList :: Ord a => [a] -> NubList a
toNubList list = NubList $ ordNub list

-- | Lift a function over lists to a function over NubLists.
overNubList :: Ord a => ([a] -> [a]) -> NubList a -> NubList a
overNubList f (NubList list) = toNubList . f $ list

-- | Monoid operations on NubLists.
-- For a valid Monoid instance we need to satistfy the required monoid laws;
-- identity, associativity and closure.
--
-- Identity : by inspection:
--      mempty `mappend` NubList xs == NubList xs `mappend` mempty
--
-- Associativity : by inspection:
--      (NubList xs `mappend` NubList ys) `mappend` NubList zs
--      == NubList xs `mappend` (NubList ys `mappend` NubList zs)
--
-- Closure : appending two lists of type a and removing duplicates obviously
-- does not change the type.

instance Ord a => Monoid (NubList a) where
    mempty = NubList []
    mappend = (<>)

instance Ord a => Semigroup (NubList a) where
    (NubList xs) <> (NubList ys) = NubList $ xs `listUnion` ys

instance Show a => Show (NubList a) where
    show (NubList list) = show list

instance (Ord a, Read a) => Read (NubList a) where
    readPrec = readNubList toNubList

-- | Helper used by NubList/NubListR's Read instances.
readNubList :: (Read a) => ([a] -> l a) -> R.ReadPrec (l a)
readNubList toList = R.parens . R.prec 10 $ fmap toList R.readPrec

-- | Binary instance for 'NubList a' is the same as for '[a]'. For 'put', we
-- just pull off constructor and put the list. For 'get', we get the list and
-- make a 'NubList' out of it using 'toNubList'.
instance (Ord a, Binary a) => Binary (NubList a) where
    put (NubList l) = put l
    get = fmap toNubList get

-- | NubListR : A right-biased version of 'NubList'. That is @toNubListR
-- ["-XNoFoo", "-XFoo", "-XNoFoo"]@ will result in @["-XFoo", "-XNoFoo"]@,
-- unlike the normal 'NubList', which is left-biased. Built on top of
-- 'ordNubRight' and 'listUnionRight'.
newtype NubListR a =
    NubListR { fromNubListR :: [a] }
    deriving Eq

-- | Smart constructor for the NubListR type.
toNubListR :: Ord a => [a] -> NubListR a
toNubListR list = NubListR $ ordNubRight list

-- | Lift a function over lists to a function over NubListRs.
overNubListR :: Ord a => ([a] -> [a]) -> NubListR a -> NubListR a
overNubListR f (NubListR list) = toNubListR . f $ list

instance Ord a => Monoid (NubListR a) where
  mempty = NubListR []
  mappend = (<>)

instance Ord a => Semigroup (NubListR a) where
  (NubListR xs) <> (NubListR ys) = NubListR $ xs `listUnionRight` ys

instance Show a => Show (NubListR a) where
  show (NubListR list) = show list

instance (Ord a, Read a) => Read (NubListR a) where
    readPrec = readNubList toNubListR
