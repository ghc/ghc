{-# LANGUAGE Safe #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

-- |
-- This module is an internal GHC module.  It declares the constants used
-- in the implementation of type-level natural numbers.  The programmer interface
-- for working with type-level naturals should be defined in a separate module.
--
-- @since 4.10.0.0
--

module GHC.TypeNats
    (-- *  Nat Kind
     Natural,
     Nat,
     -- *  Linking type and value level
     KnownNat(natSing),
     natVal,
     natVal',
     SomeNat(..),
     someNatVal,
     sameNat,
     decideNat,
     -- **  Singleton values
     SNat,
     pattern SNat,
     fromSNat,
     withSomeSNat,
     withKnownNat,
     -- *  Functions on type literals
     type (<=),
     type (<=?),
     type (+),
     type (*),
     type (^),
     type (-),
     CmpNat,
     cmpNat,
     Div,
     Mod,
     Log2
     ) where

import GHC.Internal.TypeNats
