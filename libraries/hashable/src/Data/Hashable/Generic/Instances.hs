{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, KindSignatures,
             ScopedTypeVariables, TypeOperators,
             MultiParamTypeClasses, GADTs, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.Hashable.Generic.Instances
-- Copyright   :  (c) Bryan O'Sullivan 2012
-- SPDX-License-Identifier : BSD-3-Clause
-- Maintainer  :  bos@serpentine.com
-- Stability   :  provisional
-- Portability :  GHC >= 7.4
--
-- Internal module defining orphan instances for "GHC.Generics"
--
module Data.Hashable.Generic.Instances () where

import Data.Hashable.Class
import GHC.Generics

#if MIN_VERSION_base(4,9,0)
import Data.Kind (Type)
#else
#define Type *
#endif


-- Type without constructors
instance GHashable arity V1 where
    ghashWithSalt _ salt _ = hashWithSalt salt ()

-- Constructor without arguments
instance GHashable arity U1 where
    ghashWithSalt _ salt U1 = hashWithSalt salt ()

instance (GHashable arity a, GHashable arity b) => GHashable arity (a :*: b) where
    ghashWithSalt toHash salt (x :*: y) =
      (ghashWithSalt toHash (ghashWithSalt toHash salt x) y)

-- Metadata (constructor name, etc)
instance GHashable arity a => GHashable arity (M1 i c a) where
    ghashWithSalt targs salt = ghashWithSalt targs salt . unM1

-- Constants, additional parameters, and rank-1 recursion
instance Hashable a => GHashable arity (K1 i a) where
    ghashWithSalt _ = hashUsing unK1

instance GHashable One Par1 where
    ghashWithSalt (HashArgs1 h) salt = h salt . unPar1

instance Hashable1 f => GHashable One (Rec1 f) where
    ghashWithSalt (HashArgs1 h) salt = liftHashWithSalt h salt . unRec1

instance (Hashable1 f, GHashable One g) => GHashable One (f :.: g) where
    ghashWithSalt targs salt = liftHashWithSalt (ghashWithSalt targs) salt . unComp1

class SumSize f => GSum arity f where
    hashSum :: HashArgs arity a -> Int -> Int -> f a -> Int
    -- hashSum args salt index value = ...

-- [Note: Hashing a sum type]
--
-- The tree structure is used in GHC.Generics to represent the sum (and
-- product) part of the generic represention of the type, e.g.:
--
--   (C0 ... :+: C1 ...) :+: (C2 ... :+: (C3 ... :+: C4 ...))
--
-- The value constructed with C2 constructor is represented as (R1 (L1 ...)).
-- Yet, if we think that this tree is a flat (heterogenous) list:
--
--   [C0 ..., C1 ..., C2 ..., C3 ..., C4... ]
--
-- then the value constructed with C2 is a (dependent) pair (2, ...), and
-- hashing it is simple:
--
--   salt `hashWithSalt` (2 :: Int) `hashWithSalt` ...
--
-- This is what we do below. When drilling down the tree, we count how many
-- leafs are to the left (`index` variable). At the leaf case C1, we'll have an
-- actual index into the sum.
--
-- This works well for balanced data. However for recursive types like:
--
--   data Nat = Z | S Nat
--
-- the `hashWithSalt salt (S (S (S Z)))` is
--
--   salt `hashWithSalt` (1 :: Int) -- first S
--        `hashWithSalt` (1 :: Int) -- second S
--        `hashWithSalt` (1 :: Int) -- third S
--        `hashWithSalt` (0 :: Int) -- Z
--        `hashWithSalt` ()         -- U1
--
-- For that type the manual implementation:
--
--    instance Hashable Nat where
--        hashWithSalt salt n = hashWithSalt salt (natToInteger n)
--
-- would be better performing CPU and hash-quality wise (assuming that
-- Integer's Hashable is of high quality).
--
instance (GSum arity a, GSum arity b) => GHashable arity (a :+: b) where
    ghashWithSalt toHash salt = hashSum toHash salt 0

instance (GSum arity a, GSum arity b) => GSum arity (a :+: b) where
    hashSum toHash !salt !index s = case s of
        L1 x -> hashSum toHash salt index x
        R1 x -> hashSum toHash salt (index + sizeL) x
      where
        sizeL = unTagged (sumSize :: Tagged a)
    {-# INLINE hashSum #-}

instance GHashable arity a => GSum arity (C1 c a) where
    hashSum toHash !salt !index (M1 x) = ghashWithSalt toHash (hashWithSalt salt index) x
    {-# INLINE hashSum #-}

class SumSize f where
    sumSize :: Tagged f

newtype Tagged (s :: Type -> Type) = Tagged {unTagged :: Int}

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
    sumSize = Tagged $ unTagged (sumSize :: Tagged a) +
                       unTagged (sumSize :: Tagged b)

instance SumSize (C1 c a) where
    sumSize = Tagged 1
