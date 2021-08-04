{-# LANGUAGE CPP                       #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ConstraintKinds           #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType              #-}
#endif
module Basement.Nat
    ( Nat
    , KnownNat
    , natVal
    , type (<=), type (<=?), type (+), type (*), type (^), type (-)
    , CmpNat
    -- * Nat convertion
    , natValNatural
    , natValInt
    , natValInt8
    , natValInt16
    , natValInt32
    , natValInt64
    , natValWord
    , natValWord8
    , natValWord16
    , natValWord32
    , natValWord64
    -- * Maximum bounds
    , NatNumMaxBound
    -- * Constraint
    , NatInBoundOf
    , NatWithinBound
    ) where

#include "MachDeps.h"

import           GHC.TypeLits
import           Basement.Compat.Base
import           Basement.Compat.Natural
import           Basement.Types.Char7 (Char7)
import           Basement.Types.Word128 (Word128)
import           Basement.Types.Word256 (Word256)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Word (Word8, Word16, Word32, Word64)
import qualified Prelude (fromIntegral)

#if __GLASGOW_HASKELL__ >= 800
import           Data.Type.Bool
#endif

natValNatural :: forall n proxy . KnownNat n => proxy n -> Natural
natValNatural n = Prelude.fromIntegral (natVal n)

natValInt :: forall n proxy . (KnownNat n, NatWithinBound Int n) => proxy n -> Int
natValInt n = Prelude.fromIntegral (natVal n)

natValInt64 :: forall n proxy . (KnownNat n, NatWithinBound Int64 n) => proxy n -> Int64
natValInt64 n = Prelude.fromIntegral (natVal n)

natValInt32 :: forall n proxy . (KnownNat n, NatWithinBound Int32 n) => proxy n -> Int32
natValInt32 n = Prelude.fromIntegral (natVal n)

natValInt16 :: forall n proxy . (KnownNat n, NatWithinBound Int16 n) => proxy n -> Int16
natValInt16 n = Prelude.fromIntegral (natVal n)

natValInt8 :: forall n proxy . (KnownNat n, NatWithinBound Int8 n) => proxy n -> Int8
natValInt8 n = Prelude.fromIntegral (natVal n)

natValWord :: forall n proxy . (KnownNat n, NatWithinBound Word n) => proxy n -> Word
natValWord n = Prelude.fromIntegral (natVal n)

natValWord64 :: forall n proxy . (KnownNat n, NatWithinBound Word64 n) => proxy n -> Word64
natValWord64 n = Prelude.fromIntegral (natVal n)

natValWord32 :: forall n proxy . (KnownNat n, NatWithinBound Word32 n) => proxy n -> Word32
natValWord32 n = Prelude.fromIntegral (natVal n)

natValWord16 :: forall n proxy . (KnownNat n, NatWithinBound Word16 n) => proxy n -> Word16
natValWord16 n = Prelude.fromIntegral (natVal n)

natValWord8 :: forall n proxy . (KnownNat n, NatWithinBound Word8 n) => proxy n -> Word8
natValWord8 n = Prelude.fromIntegral (natVal n)

-- | Get Maximum bounds of different Integral / Natural types related to Nat
type family NatNumMaxBound ty :: Nat

type instance NatNumMaxBound Char   = 0x10ffff
type instance NatNumMaxBound Char7  = 0x7f
type instance NatNumMaxBound Int64  = 0x7fffffffffffffff
type instance NatNumMaxBound Int32  = 0x7fffffff
type instance NatNumMaxBound Int16  = 0x7fff
type instance NatNumMaxBound Int8   = 0x7f
type instance NatNumMaxBound Word256 = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
type instance NatNumMaxBound Word128 = 0xffffffffffffffffffffffffffffffff
type instance NatNumMaxBound Word64 = 0xffffffffffffffff
type instance NatNumMaxBound Word32 = 0xffffffff
type instance NatNumMaxBound Word16 = 0xffff
type instance NatNumMaxBound Word8  = 0xff
#if WORD_SIZE_IN_BITS == 64
type instance NatNumMaxBound Int    = NatNumMaxBound Int64
type instance NatNumMaxBound Word   = NatNumMaxBound Word64
#else
type instance NatNumMaxBound Int    = NatNumMaxBound Int32
type instance NatNumMaxBound Word   = NatNumMaxBound Word32
#endif

-- | Check if a Nat is in bounds of another integral / natural types
type family NatInBoundOf ty n where
    NatInBoundOf Integer n = 'True
    NatInBoundOf Natural n = 'True
    NatInBoundOf ty      n = n <=? NatNumMaxBound ty

-- | Constraint to check if a natural is within a specific bounds of a type.
--
-- i.e. given a Nat `n`, is it possible to convert it to `ty` without losing information
#if __GLASGOW_HASKELL__ >= 800
type family NatWithinBound ty (n :: Nat) where
    NatWithinBound ty n = If (NatInBoundOf ty n)
        (() ~ ())
        (TypeError ('Text "Natural " ':<>: 'ShowType n ':<>: 'Text " is out of bounds for " ':<>: 'ShowType ty))
#else
type NatWithinBound ty n = NatInBoundOf ty n ~ 'True
#endif
