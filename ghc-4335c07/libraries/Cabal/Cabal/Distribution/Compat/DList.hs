-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Compat.DList
-- Copyright   :  (c) Ben Gamari 2015-2019
-- License     :  BSD3
--
-- Maintainer  :  cabal-dev@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A very simple difference list.
module Distribution.Compat.DList (
    DList,
    runDList,
    singleton,
    fromList,
    snoc,
) where

import Prelude ()
import Distribution.Compat.Prelude

-- | Difference list.
newtype DList a = DList ([a] -> [a])

runDList :: DList a -> [a]
runDList (DList run) = run []

-- | Make 'DList' with containing single element.
singleton :: a -> DList a
singleton a = DList (a:)

fromList :: [a] -> DList a
fromList as = DList (as ++)

snoc :: DList a -> a -> DList a
snoc xs x = xs <> singleton x

instance Monoid (DList a) where
  mempty = DList id
  mappend = (<>)

instance Semigroup (DList a) where
  DList a <> DList b = DList (a . b)
