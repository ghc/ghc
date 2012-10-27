{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The Either type, and associated operations.
--
-----------------------------------------------------------------------------

module Data.Either (
   Either(..),
   either,
   lefts,
   rights,
   partitionEithers,
 ) where

#include "Typeable.h"

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Show
import GHC.Read
#endif

import Data.Typeable

#ifdef __GLASGOW_HASKELL__
{-
-- just for testing
import Test.QuickCheck
-}

{-|

The 'Either' type represents values with two possibilities: a value of
type @'Either' a b@ is either @'Left' a@ or @'Right' b@.

The 'Either' type is sometimes used to represent a value which is
either correct or an error; by convention, the 'Left' constructor is
used to hold an error value and the 'Right' constructor is used to
hold a correct value (mnemonic: \"right\" also means \"correct\").
-}
data  Either a b  =  Left a | Right b
  deriving (Eq, Ord, Read, Show)

instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)

instance Monad (Either e) where
    return = Right
    Left  l >>= _ = Left l
    Right r >>= k = k r

-- | Case analysis for the 'Either' type.
-- If the value is @'Left' a@, apply the first function to @a@;
-- if it is @'Right' b@, apply the second function to @b@.
either                  :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y
#endif  /* __GLASGOW_HASKELL__ */

INSTANCE_TYPEABLE2(Either,eitherTc,"Either")

-- | Extracts from a list of 'Either' all the 'Left' elements
-- All the 'Left' elements are extracted in order.

lefts   :: [Either a b] -> [a]
lefts x = [a | Left a <- x]

-- | Extracts from a list of 'Either' all the 'Right' elements
-- All the 'Right' elements are extracted in order.

rights   :: [Either a b] -> [b]
rights x = [a | Right a <- x]

-- | Partitions a list of 'Either' into two lists
-- All the 'Left' elements are extracted, in order, to the first
-- component of the output.  Similarly the 'Right' elements are extracted
-- to the second component of the output.

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers = foldr (either left right) ([],[])
 where
  left  a ~(l, r) = (a:l, r)
  right a ~(l, r) = (l, a:r)

{-
{--------------------------------------------------------------------
  Testing
--------------------------------------------------------------------}
prop_partitionEithers :: [Either Int Int] -> Bool
prop_partitionEithers x =
  partitionEithers x == (lefts x, rights x)
-}

