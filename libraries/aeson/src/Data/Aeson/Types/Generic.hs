{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#include "overlapping-compat.h"

-- |
-- Module:      Data.Aeson.Types.Generic
-- Copyright:   (c) 2012-2016 Bryan O'Sullivan
--              (c) 2011, 2012 Bas Van Dijk
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Helpers for generic derivations.

module Data.Aeson.Types.Generic
    (
      IsRecord
    , AllNullary
    , Tagged2(..)
    , True
    , False
    , And
    , Zero
    , One
    , ProductSize(..)
    , (:*)(..)
    ) where

import Prelude.Compat

import GHC.Generics

--------------------------------------------------------------------------------

class IsRecord (f :: * -> *) isRecord | f -> isRecord

instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
#if MIN_VERSION_base(4,9,0)
instance OVERLAPPING_ IsRecord (M1 S ('MetaSel 'Nothing u ss ds) f) False
#else
instance OVERLAPPING_ IsRecord (M1 S NoSelector f) False
#endif
instance (IsRecord f isRecord) => IsRecord (M1 S c f) isRecord
instance IsRecord (K1 i c) True
instance IsRecord Par1 True
instance IsRecord (Rec1 f) True
instance IsRecord (f :.: g) True
instance IsRecord U1 False

--------------------------------------------------------------------------------

class AllNullary (f :: * -> *) allNullary | f -> allNullary

instance ( AllNullary a allNullaryL
         , AllNullary b allNullaryR
         , And allNullaryL allNullaryR allNullary
         ) => AllNullary (a :+: b) allNullary
instance AllNullary a allNullary => AllNullary (M1 i c a) allNullary
instance AllNullary (a :*: b) False
instance AllNullary (a :.: b) False
instance AllNullary (K1 i c) False
instance AllNullary Par1 False
instance AllNullary (Rec1 f) False
instance AllNullary U1 True

newtype Tagged2 (s :: * -> *) b = Tagged2 {unTagged2 :: b}
  deriving Functor

--------------------------------------------------------------------------------

data True
data False

class    And bool1 bool2 bool3 | bool1 bool2 -> bool3

instance And True  True  True
instance And False False False
instance And False True  False
instance And True  False False

--------------------------------------------------------------------------------

-- | A type-level indicator that 'ToJSON' or 'FromJSON' is being derived generically.
data Zero

-- | A type-level indicator that 'ToJSON1' or 'FromJSON1' is being derived generically.
data One

--------------------------------------------------------------------------------

class ProductSize f where
    productSize :: Tagged2 f Int

instance (ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    productSize = Tagged2 $ unTagged2 (productSize :: Tagged2 a Int) +
                            unTagged2 (productSize :: Tagged2 b Int)

instance ProductSize (S1 s a) where
    productSize = Tagged2 1

--------------------------------------------------------------------------------

-- | Simple extensible tuple type to simplify passing around many parameters.
data a :* b = a :* b

infixr 1 :*
