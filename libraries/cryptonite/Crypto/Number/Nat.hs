-- |
-- Module      : Crypto.Number.Nat
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- Numbers at type level.
--
-- This module provides extensions to "GHC.TypeLits" and "GHC.TypeNats" useful
-- to work with cryptographic algorithms parameterized with a variable bit
-- length.  Constraints like @'IsDivisibleBy8' n@ ensure that the type-level
-- parameter is applicable to the algorithm.
--
-- Functions are also provided to test whether constraints are satisfied from
-- values known at runtime.  The following example shows how to discharge
-- 'IsDivisibleBy8' in a computation @fn@ requiring this constraint:
--
-- > withDivisibleBy8 :: Integer
-- >                  -> (forall proxy n . (KnownNat n, IsDivisibleBy8 n) => proxy n -> a)
-- >                  -> Maybe a
-- > withDivisibleBy8 len fn = do
-- >     SomeNat p <- someNatVal len
-- >     Refl <- isDivisibleBy8 p
-- >     pure (fn p)
--
-- Function @withDivisibleBy8@ above returns 'Nothing' when the argument @len@
-- is negative or not divisible by 8.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Crypto.Number.Nat
    ( type IsDivisibleBy8
    , type IsAtMost, type IsAtLeast
    , isDivisibleBy8
    , isAtMost
    , isAtLeast
    ) where

import           Data.Type.Equality
import           GHC.TypeLits
import           Unsafe.Coerce (unsafeCoerce)

import           Crypto.Internal.Nat

-- | get a runtime proof that the constraint @'IsDivisibleBy8' n@ is satified
isDivisibleBy8 :: KnownNat n => proxy n -> Maybe (IsDiv8 n n :~: 'True)
isDivisibleBy8 n
    | mod (natVal n) 8 == 0 = Just (unsafeCoerce Refl)
    | otherwise             = Nothing

-- | get a runtime proof that the constraint @'IsAtMost' value bound@ is
-- satified
isAtMost :: (KnownNat value, KnownNat bound)
         => proxy value -> proxy' bound -> Maybe ((value <=? bound) :~: 'True)
isAtMost x y
    | natVal x <= natVal y  = Just (unsafeCoerce Refl)
    | otherwise             = Nothing

-- | get a runtime proof that the constraint @'IsAtLeast' value bound@ is
-- satified
isAtLeast :: (KnownNat value, KnownNat bound)
          => proxy value -> proxy' bound -> Maybe ((bound <=? value) :~: 'True)
isAtLeast = flip isAtMost
