-----------------------------------------------------------------------------
-- |
-- Module      :  Debug.QuickCheck.Utils
-- Copyright   :  (c) Andy Gill 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- These are some general purpose utilities for use with QuickCheck.
--
-----------------------------------------------------------------------------

module Debug.QuickCheck.Utils
  ( isAssociativeBy
  , isAssociative
  , isCommutableBy
  , isCommutable
  , isTotalOrder
  ) where

import Prelude

import Debug.QuickCheck

isAssociativeBy :: (Show a,Testable prop) 
		=> (a -> a -> prop) -> Gen a -> (a -> a -> a) -> Property
isAssociativeBy (===) src (**) = 
     	forAll src $ \ a ->
     	forAll src $ \ b ->
     	forAll src $ \ c ->
	((a ** b) ** c) === (a ** (b ** c))

isAssociative :: (Arbitrary a,Show a,Eq a) => (a -> a -> a) -> Property
isAssociative = isAssociativeBy (==) arbitrary

isCommutableBy :: (Show a,Testable prop) 
	       => (b -> b -> prop) -> Gen a -> (a -> a -> b) -> Property
isCommutableBy (===) src (**) =
	forAll src $ \ a ->
	forAll src $ \ b ->
	(a ** b) === (b ** a)

isCommutable :: (Arbitrary a,Show a,Eq b) => (a -> a -> b) -> Property
isCommutable = isCommutableBy (==) arbitrary

isTotalOrder :: (Arbitrary a,Show a,Ord a) => a -> a -> Property
isTotalOrder x y = 
    classify (x > y)  "less than" $
    classify (x == y) "equals" $
    classify (x < y)  "greater than" $
    x < y || x == y || x > y
