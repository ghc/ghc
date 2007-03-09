-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid
-- Copyright   :  (c) Andy Gill 2001,
-- 		  (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The Monoid class with various general-purpose instances.
--
--	  Inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--		  Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Data.Monoid (
        -- * Monoid typeclass
 	Monoid(..),
	Dual(..),
	Endo(..),
        -- * Bool wrappers
	All(..),
	Any(..),
        -- * Num wrappers
	Sum(..),
	Product(..),
        -- * Maybe wrappers
        -- $MaybeExamples
	First(..),
	Last(..)
  ) where

import Prelude

{-
-- just for testing
import Data.Maybe
import Test.QuickCheck
-- -}

-- ---------------------------------------------------------------------------
-- | The monoid class.
-- A minimal complete definition must supply 'mempty' and 'mappend',
-- and these should satisfy the monoid laws.

class Monoid a where
	mempty  :: a
	-- ^ Identity of 'mappend'
	mappend :: a -> a -> a
	-- ^ An associative operation
	mconcat :: [a] -> a

	-- ^ Fold a list using the monoid.
	-- For most types, the default definition for 'mconcat' will be
	-- used, but the function is included in the class definition so
	-- that an optimized version can be provided for specific types.

	mconcat = foldr mappend mempty

-- Monoid instances.

instance Monoid [a] where
	mempty  = []
	mappend = (++)

instance Monoid b => Monoid (a -> b) where
	mempty _ = mempty
	mappend f g x = f x `mappend` g x

instance Monoid () where
	-- Should it be strict?
	mempty        = ()
	_ `mappend` _ = ()
	mconcat _     = ()

instance (Monoid a, Monoid b) => Monoid (a,b) where
	mempty = (mempty, mempty)
	(a1,b1) `mappend` (a2,b2) =
		(a1 `mappend` a2, b1 `mappend` b2)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
	mempty = (mempty, mempty, mempty)
	(a1,b1,c1) `mappend` (a2,b2,c2) =
		(a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
	mempty = (mempty, mempty, mempty, mempty)
	(a1,b1,c1,d1) `mappend` (a2,b2,c2,d2) =
		(a1 `mappend` a2, b1 `mappend` b2,
		 c1 `mappend` c2, d1 `mappend` d2)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
		Monoid (a,b,c,d,e) where
	mempty = (mempty, mempty, mempty, mempty, mempty)
	(a1,b1,c1,d1,e1) `mappend` (a2,b2,c2,d2,e2) =
		(a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2,
		 d1 `mappend` d2, e1 `mappend` e2)

-- lexicographical ordering
instance Monoid Ordering where
	mempty         = EQ
	LT `mappend` _ = LT
	EQ `mappend` y = y
	GT `mappend` _ = GT

-- | The dual of a monoid, obtained by swapping the arguments of 'mappend'.
newtype Dual a = Dual { getDual :: a }
	deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid a => Monoid (Dual a) where
	mempty = Dual mempty
	Dual x `mappend` Dual y = Dual (y `mappend` x)

-- | The monoid of endomorphisms under composition.
newtype Endo a = Endo { appEndo :: a -> a }

instance Monoid (Endo a) where
	mempty = Endo id
	Endo f `mappend` Endo g = Endo (f . g)

-- | Boolean monoid under conjunction.
newtype All = All { getAll :: Bool }
	deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
	mempty = All True
	All x `mappend` All y = All (x && y)

-- | Boolean monoid under disjunction.
newtype Any = Any { getAny :: Bool }
	deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
	mempty = Any False
	Any x `mappend` Any y = Any (x || y)

-- | Monoid under addition.
newtype Sum a = Sum { getSum :: a }
	deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum a) where
	mempty = Sum 0
	Sum x `mappend` Sum y = Sum (x + y)

-- | Monoid under multiplication.
newtype Product a = Product { getProduct :: a }
	deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
	mempty = Product 1
	Product x `mappend` Product y = Product (x * y)

-- $MaybeExamples
-- To implement @find@ or @findLast@ on any 'Foldable':
--
-- @
-- findLast :: Foldable t => (a -> Bool) -> t a -> Maybe a
-- findLast pred = getLast . foldMap (\x -> if pred x
--                                            then Last (Just x)
--                                            else Last Nothing)
-- @
--
-- Much of "Data.Map"'s interface can be implemented with
-- 'Data.Map.alter'. Some of the rest can be implemented with a new
-- @alterA@ function and either 'First' or 'Last':
--
-- > alterA :: (Applicative f, Ord k) =>
-- >           (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
-- >
-- > instance Monoid a => Applicative ((,) a)  -- from Control.Applicative
--
-- @
-- insertLookupWithKey :: Ord k => (k -> v -> v -> v) -> k -> v
--                     -> Map k v -> (Maybe v, Map k v)
-- insertLookupWithKey combine key value =
--   Arrow.first getFirst . alterA doChange key
--   where
--   doChange Nothing = (First Nothing, Just value)
--   doChange (Just oldValue) =
--     (First (Just oldValue),
--      Just (combine key value oldValue))
-- @

-- | Lift a semigroup into 'Maybe' forming a 'Monoid' according to
-- <http://en.wikipedia.org/wiki/Monoid>: \"Any semigroup @S@ may be
-- turned into a monoid simply by adjoining an element @e@ not in @S@
-- and defining @e*e = e@ and @e*s = s = s*e@ for all @s ∈ S@.\" Since
-- there is no \"Semigroup\" typeclass providing just 'mappend', we
-- use 'Monoid' instead.
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)


-- | Maybe monoid returning the leftmost non-Nothing value.
newtype First a = First { getFirst :: Maybe a }
#ifndef __HADDOCK__
	deriving (Eq, Ord, Read, Show)
#else  /* __HADDOCK__ */
instance Eq a => Eq (First a)
instance Ord a => Ord (First a)
instance Read a => Read (First a)
instance Show a => Show (First a)
#endif

instance Monoid (First a) where
	mempty = First Nothing
	r@(First (Just _)) `mappend` _ = r
	First Nothing `mappend` r = r

-- | Maybe monoid returning the rightmost non-Nothing value.
newtype Last a = Last { getLast :: Maybe a }
#ifndef __HADDOCK__
	deriving (Eq, Ord, Read, Show)
#else  /* __HADDOCK__ */
instance Eq a => Eq (Last a)
instance Ord a => Ord (Last a)
instance Read a => Read (Last a)
instance Show a => Show (Last a)
#endif

instance Monoid (Last a) where
	mempty = Last Nothing
	_ `mappend` r@(Last (Just _)) = r
	r `mappend` Last Nothing = r

{-
{--------------------------------------------------------------------
  Testing
--------------------------------------------------------------------}
instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = oneof [return Nothing, Just `fmap` arbitrary]

prop_mconcatMaybe :: [Maybe [Int]] -> Bool
prop_mconcatMaybe x =
  fromMaybe [] (mconcat x) == mconcat (catMaybes x)

prop_mconcatFirst :: [Maybe Int] -> Bool
prop_mconcatFirst x =
  getFirst (mconcat (map First x)) == listToMaybe (catMaybes x)
prop_mconcatLast :: [Maybe Int] -> Bool
prop_mconcatLast x =
  getLast (mconcat (map Last x)) == listLastToMaybe (catMaybes x)
	where listLastToMaybe [] = Nothing
              listLastToMaybe lst = Just (last lst)
-- -}
