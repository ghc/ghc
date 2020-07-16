{-# Language PolyKinds          #-}
{-# Language TypeApplications   #-}
{-# Language LiberalTypeSynonyms #-}

module T15859 where

import Data.Kind

-- A :: forall (k :: Type) -> k -> Type
data A k :: k -> Type

-- KindOf :: forall (k::Type). k -> Type
type KindOf (a :: k) = k

-- This variant requires impredicative instantiation of KindOf
--    KindOf @(forall k -> k -> Type) A
-- which GHC does not (yet) support at the kind level, even
-- with Quick Look
a = (undefined :: KindOf A) @Int
