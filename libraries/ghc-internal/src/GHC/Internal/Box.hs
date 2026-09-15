{-# LANGUAGE Trustworthy #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_HADDOCK print-explicit-runtime-reps #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
  -- Silence warnings about the boxing data constructors not being exported:
  -- they're internal, but used indirectly via 'Box'/'box'.

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Box
-- License     :  see libraries/ghc-internal/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Box' type, which boxes values of any runtime representation.
--
-- Everything here is wired into the compiler, which desugars the 'box'/'unbox'
-- functions of "GHC.Internal.Prim" in terms of the boxing data types defined
-- in this module. See Note [Boxing constructors] in GHC.Builtin.WiredIn.Types.Box.
--
-- This module must contain nothing but wired-in declarations: no instances,
-- no rules. See Note [Loading instances for wired-in things] in GHC.Iface.Load.
--
-----------------------------------------------------------------------------

module GHC.Internal.Box
  ( Box -- NB: 'BoxTF', 'DictBox' and the boxing data constructors are
        -- deliberately not exported. The API is via the box/unbox functions.
  ) where

import GHC.Internal.Prim
import GHC.Internal.Types
import GHC.Internal.TypeNats ( type (!!) )

-- | @'Box' ty@ is a boxed and lifted type that boxes values of type @ty@.
type Box :: forall (r :: RuntimeRep). TYPE r -> Type
newtype Box (a :: TYPE r) = MkBox (BoxTF r)
type role Box representational

-- | (Internal use only)
--
-- @BoxTF r@ is the type of a box holding a value of representation @r@.
--
-- See Note [Boxing constructors] in GHC.Builtin.WiredIn.Types.Box.
type BoxTF :: RuntimeRep -> Type
type family BoxTF r where
  -- No source-written equations: this is a wired-in type family.
  -- The equations are provided by GHC.Builtin.WiredIn.Types.Box.axBox.

-- Boxing data constructors: see Note [Boxing constructors] in GHC.Builtin.WiredIn.Types.Box.

-- | 'DictBox' provides a simple way to wrap up a (lifted) constraint as a type.
data DictBox c where
  MkDictBox :: c => DictBox c

-- BoxedRep
-- LiftedRep: no box needed. BoxTF LiftedRep = Any @Type.
data BoxUnlifted = BoxUnlifted (Any @UnliftedType)

-- IntRep
data BoxInt   = BoxInt   Int#
data BoxInt8  = BoxInt8  Int8#
data BoxInt16 = BoxInt16 Int16#
data BoxInt32 = BoxInt32 Int32#
data BoxInt64 = BoxInt64 Int64#

-- WordRep
data BoxWord   = BoxWord   Word#
data BoxWord8  = BoxWord8  Word8#
data BoxWord16 = BoxWord16 Word16#
data BoxWord32 = BoxWord32 Word32#
data BoxWord64 = BoxWord64 Word64#

-- FloatRep/DoubleRep
data BoxFloat  = BoxFloat  Float#
data BoxDouble = BoxDouble Double#

-- AddrRep
data BoxAddr = BoxAddr Addr#

-- VecRep
  -- 128 bits
data BoxVec16Int8  = BoxVec16Int8  Int8X16#
data BoxVec16Word8 = BoxVec16Word8 Word8X16#
data BoxVec8Int16  = BoxVec8Int16  Int16X8#
data BoxVec8Word16 = BoxVec8Word16 Word16X8#
data BoxVec4Int32  = BoxVec4Int32  Int32X4#
data BoxVec4Word32 = BoxVec4Word32 Word32X4#
data BoxVec4Float  = BoxVec4Float  FloatX4#
data BoxVec2Int64  = BoxVec2Int64  Int64X2#
data BoxVec2Word64 = BoxVec2Word64 Word64X2#
data BoxVec2Double = BoxVec2Double DoubleX2#
  -- 256 bits
data BoxVec32Int8   = BoxVec32Int8   Int8X32#
data BoxVec32Word8  = BoxVec32Word8  Word8X32#
data BoxVec16Int16  = BoxVec16Int16  Int16X16#
data BoxVec16Word16 = BoxVec16Word16 Word16X16#
data BoxVec8Int32   = BoxVec8Int32   Int32X8#
data BoxVec8Word32  = BoxVec8Word32  Word32X8#
data BoxVec8Float   = BoxVec8Float   FloatX8#
data BoxVec4Int64   = BoxVec4Int64   Int64X4#
data BoxVec4Word64  = BoxVec4Word64  Word64X4#
data BoxVec4Double  = BoxVec4Double  DoubleX4#
  -- 512 bits
data BoxVec64Int8   = BoxVec64Int8   Int8X64#
data BoxVec64Word8  = BoxVec64Word8  Word8X64#
data BoxVec32Int16  = BoxVec32Int16  Int16X32#
data BoxVec32Word16 = BoxVec32Word16 Word16X32#
data BoxVec16Int32  = BoxVec16Int32  Int32X16#
data BoxVec16Word32 = BoxVec16Word32 Word32X16#
data BoxVec16Float  = BoxVec16Float  FloatX16#
data BoxVec8Int64   = BoxVec8Int64   Int64X8#
data BoxVec8Word64  = BoxVec8Word64  Word64X8#
data BoxVec8Double  = BoxVec8Double  DoubleX8#

-- TupleRep: boxed recursively, using boxed tuples.
-- SumRep: boxed recursively, using 'BoxSum'.
-- See [Boxing TupleRep and SumRep] in GHC.Builtin.WiredIn.Types.Box.

-- | (Internal use only)
--
-- 'BoxSum' is used to box unboxed sums.
--
-- See [Boxing TupleRep and SumRep] in GHC.Builtin.WiredIn.Types.Box.
type BoxSum :: [RuntimeRep] -> Type
data BoxSum rs

  -- Two remarks:
  --
  --  1. 'BoxSum' has exactly 64 data constructors, as that's the most
  --     alternatives that an unboxed sum can have (see GHC.Settings.Constants.mAX_SUM_SIZE).
  --  2. Ideally it would be defined as a GADT:
  --
  --       data BoxSum rs where
  --         MkBoxSum0 :: BoxTF r0 -> BoxSum (r0 : rs)
  --         MkBoxSum1 :: BoxTF r1 -> BoxSum (r0 : r1 : rs)
  --
  --     ... but that would require wiring in a GADT, which is difficult.
  --     Instead, we define (!!) as a wired-in type family and use that.

  = MkBoxSum0  (BoxTF (rs !! 0))
  | MkBoxSum1  (BoxTF (rs !! 1))
  | MkBoxSum2  (BoxTF (rs !! 2))
  | MkBoxSum3  (BoxTF (rs !! 3))
  | MkBoxSum4  (BoxTF (rs !! 4))
  | MkBoxSum5  (BoxTF (rs !! 5))
  | MkBoxSum6  (BoxTF (rs !! 6))
  | MkBoxSum7  (BoxTF (rs !! 7))
  | MkBoxSum8  (BoxTF (rs !! 8))
  | MkBoxSum9  (BoxTF (rs !! 9))
  | MkBoxSum10 (BoxTF (rs !! 10))
  | MkBoxSum11 (BoxTF (rs !! 11))
  | MkBoxSum12 (BoxTF (rs !! 12))
  | MkBoxSum13 (BoxTF (rs !! 13))
  | MkBoxSum14 (BoxTF (rs !! 14))
  | MkBoxSum15 (BoxTF (rs !! 15))
  | MkBoxSum16 (BoxTF (rs !! 16))
  | MkBoxSum17 (BoxTF (rs !! 17))
  | MkBoxSum18 (BoxTF (rs !! 18))
  | MkBoxSum19 (BoxTF (rs !! 19))
  | MkBoxSum20 (BoxTF (rs !! 20))
  | MkBoxSum21 (BoxTF (rs !! 21))
  | MkBoxSum22 (BoxTF (rs !! 22))
  | MkBoxSum23 (BoxTF (rs !! 23))
  | MkBoxSum24 (BoxTF (rs !! 24))
  | MkBoxSum25 (BoxTF (rs !! 25))
  | MkBoxSum26 (BoxTF (rs !! 26))
  | MkBoxSum27 (BoxTF (rs !! 27))
  | MkBoxSum28 (BoxTF (rs !! 28))
  | MkBoxSum29 (BoxTF (rs !! 29))
  | MkBoxSum30 (BoxTF (rs !! 30))
  | MkBoxSum31 (BoxTF (rs !! 31))
  | MkBoxSum32 (BoxTF (rs !! 32))
  | MkBoxSum33 (BoxTF (rs !! 33))
  | MkBoxSum34 (BoxTF (rs !! 34))
  | MkBoxSum35 (BoxTF (rs !! 35))
  | MkBoxSum36 (BoxTF (rs !! 36))
  | MkBoxSum37 (BoxTF (rs !! 37))
  | MkBoxSum38 (BoxTF (rs !! 38))
  | MkBoxSum39 (BoxTF (rs !! 39))
  | MkBoxSum40 (BoxTF (rs !! 40))
  | MkBoxSum41 (BoxTF (rs !! 41))
  | MkBoxSum42 (BoxTF (rs !! 42))
  | MkBoxSum43 (BoxTF (rs !! 43))
  | MkBoxSum44 (BoxTF (rs !! 44))
  | MkBoxSum45 (BoxTF (rs !! 45))
  | MkBoxSum46 (BoxTF (rs !! 46))
  | MkBoxSum47 (BoxTF (rs !! 47))
  | MkBoxSum48 (BoxTF (rs !! 48))
  | MkBoxSum49 (BoxTF (rs !! 49))
  | MkBoxSum50 (BoxTF (rs !! 50))
  | MkBoxSum51 (BoxTF (rs !! 51))
  | MkBoxSum52 (BoxTF (rs !! 52))
  | MkBoxSum53 (BoxTF (rs !! 53))
  | MkBoxSum54 (BoxTF (rs !! 54))
  | MkBoxSum55 (BoxTF (rs !! 55))
  | MkBoxSum56 (BoxTF (rs !! 56))
  | MkBoxSum57 (BoxTF (rs !! 57))
  | MkBoxSum58 (BoxTF (rs !! 58))
  | MkBoxSum59 (BoxTF (rs !! 59))
  | MkBoxSum60 (BoxTF (rs !! 60))
  | MkBoxSum61 (BoxTF (rs !! 61))
  | MkBoxSum62 (BoxTF (rs !! 62))
  | MkBoxSum63 (BoxTF (rs !! 63))
