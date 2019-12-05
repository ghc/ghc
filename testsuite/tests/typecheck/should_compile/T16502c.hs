{-# LANGUAGE QuantifiedConstraints, PolyKinds, TypeFamilies,
             GADTSyntax, FlexibleInstances, UndecidableInstances,
             FlexibleContexts, ScopedTypeVariables #-}

module T16502c where

import Data.Kind

type family Sing :: k -> Type

newtype WrappedSing :: forall k. k -> Type where
  WrapSing :: forall k (a :: k). { unwrapSing :: Sing a } -> WrappedSing a

class    (forall (z :: k). ShowSing' z) => ShowSing k
instance (forall (z :: k). ShowSing' z) => ShowSing k

class    (forall (sing :: k -> Type). sing ~ Sing => Show (sing z))
                       => ShowSing' (z :: k)
instance Show (Sing z) => ShowSing' (z :: k)

instance ShowSing k => Show (WrappedSing (a :: k)) where
  showsPrec p (WrapSing s) = showParen (p >= 11) $
    showString "WrapSing {unwrapSing = " . showsPrec 0 s . showChar '}'
      :: ShowSing' a => ShowS
