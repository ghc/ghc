-- |
-- Module      : Basement.NonEmpty
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
-- A newtype wrapper around a non-empty Collection.

module Basement.NonEmpty
    ( NonEmpty(..)
    ) where

import           Basement.Exception
import           Basement.Compat.Base

-- | NonEmpty property for any Collection
newtype NonEmpty a = NonEmpty { getNonEmpty :: a }
    deriving (Show,Eq)

instance IsList c => IsList (NonEmpty c) where
    type Item (NonEmpty c) = Item c
    toList      = toList . getNonEmpty
    fromList [] = throw NonEmptyCollectionIsEmpty
    fromList l  = NonEmpty . fromList $ l
