-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Compat.SnocList
-- License     :  BSD3
--
-- Maintainer  :  cabal-dev@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A very reversed list. Has efficient `snoc`
module Distribution.Compat.SnocList (
    SnocList,
    runSnocList,
    snoc,
) where

import Prelude ()
import Distribution.Compat.Prelude

newtype SnocList a = SnocList [a]

snoc :: SnocList a -> a -> SnocList a
snoc (SnocList xs) x = SnocList (x : xs)

runSnocList :: SnocList a -> [a]
runSnocList (SnocList xs) = reverse xs

instance Semigroup (SnocList a) where
    SnocList xs <> SnocList ys = SnocList (ys <> xs)

instance Monoid (SnocList a) where
    mempty = SnocList []
    mappend = (<>)
