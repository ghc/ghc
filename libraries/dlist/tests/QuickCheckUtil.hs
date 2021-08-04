{- ORMOLU_DISABLE -}
-- Options passed to GHC
{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#endif
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------

-- | QuickCheck utilities for testing.
module QuickCheckUtil where

--------------------------------------------------------------------------------

-- CPP: GHC >= 8 for NonEmpty
#if __GLASGOW_HASKELL__ >= 800
import Control.Applicative (liftA2)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (mapMaybe)
#endif
import Test.QuickCheck

--------------------------------------------------------------------------------

eqWith :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
eqWith f g x = f x == g x

eqOn :: Eq b => (a -> Bool) -> (a -> b) -> (a -> b) -> a -> Property
eqOn c f g x = c x ==> f x == g x

--------------------------------------------------------------------------------

quickCheckLabeledProperties :: [(String, Property)] -> IO ()
quickCheckLabeledProperties = quickCheck . conjoin . map (uncurry label)

--------------------------------------------------------------------------------

-- CPP: GHC >= 8 for NonEmpty
#if __GLASGOW_HASKELL__ >= 800

{-

We include the instances for NonEmpty because QuickCheck (>= 2.10) does not. We
could alternatively depend on quickcheck-instances (>= 0.3.15), but
quickcheck-instances has sometimes lagged behind newer GHC/base versions. By
including the instances here, we do not need to track the quickcheck-instances
version, thus simplifying dlist.cabal and reducing the maintenance effort.

-}

instance Arbitrary1 NonEmpty where
  liftArbitrary arb = liftA2 (:|) arb (liftArbitrary arb)
  liftShrink shr (x :| xs) = mapMaybe nonEmpty . liftShrink shr $ x : xs

instance Arbitrary a => Arbitrary (NonEmpty a) where
  {-# INLINABLE arbitrary #-}
  arbitrary = arbitrary1
  {-# INLINABLE shrink #-}
  shrink = shrink1

#endif
