{-# LANGUAGE Safe #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

-- |
--
-- GHC's @DataKinds@ language extension lifts data constructors, natural
-- numbers, and strings to the type level. This module provides the
-- primitives needed for working with type-level numbers (the 'Nat' kind),
-- strings (the 'Symbol' kind), and characters (the 'Char' kind). It also defines the 'TypeError' type
-- family, a feature that makes use of type-level strings to support user
-- defined type errors.
--
-- For now, this module is the API for working with type-level literals.
-- However, please note that it is a work in progress and is subject to change.
-- Once the design of the @DataKinds@ feature is more stable, this will be
-- considered only an internal GHC module, and the programmer interface for
-- working with type-level data will be defined in a separate library.
--
-- @since 4.6.0.0
--

module GHC.TypeLits
    (-- *  Kinds
     Natural,
     Nat,
     Symbol,
     -- *  Linking type and value level
     KnownNat(natSing),
     natVal,
     natVal',
     KnownSymbol(symbolSing),
     symbolVal,
     symbolVal',
     KnownChar(charSing),
     charVal,
     charVal',
     SomeNat(..),
     SomeSymbol(..),
     SomeChar(..),
     someNatVal,
     someSymbolVal,
     someCharVal,
     sameNat,
     sameSymbol,
     sameChar,
     decideNat,
     decideSymbol,
     decideChar,
     OrderingI(..),
     cmpNat,
     cmpSymbol,
     cmpChar,
     -- **  Singleton values
     SNat,
     SSymbol,
     SChar,
     pattern SNat,
     pattern SSymbol,
     pattern SChar,
     fromSNat,
     fromSSymbol,
     fromSChar,
     withSomeSNat,
     withSomeSSymbol,
     withSomeSChar,
     withKnownNat,
     withKnownSymbol,
     withKnownChar,
     -- *  Functions on type literals
     type (<=),
     type (<=?),
     type (+),
     type (*),
     type (^),
     type (-),
     type Div,
     type Mod,
     type Log2,
     AppendSymbol,
     CmpNat,
     CmpSymbol,
     CmpChar,
     ConsSymbol,
     UnconsSymbol,
     CharToNat,
     NatToChar,
     -- *  User-defined type errors
     TypeError,
     ErrorMessage(..)
     ) where

import GHC.Internal.TypeLits
