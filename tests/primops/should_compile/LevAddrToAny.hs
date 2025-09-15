{-# LANGUAGE MagicHash, TypeApplications, UnliftedDatatypes, KindSignatures, UnboxedTuples #-}

module LevAddrToAny where

import GHC.Exts
  ( Int#
  , Int (I#)
  , addrToAny#
  , Addr#
  , ByteArray#
  , UnliftedType
  , proxy#
  )

import Data.Proxy (Proxy (..))
import Data.Kind (Type)


addrToAnyLifted :: forall (x :: Type). Addr# -> x
addrToAnyLifted addr = case addrToAny# addr of (# x #) -> x

addrToAnyUnlifted :: forall (x :: UnliftedType). Addr# -> x
addrToAnyUnlifted addr = case addrToAny# addr of (# x #) -> x

addrToUnit :: Addr# -> ()
addrToUnit = addrToAnyLifted @()

addrToInt :: Addr# -> Int
addrToInt = addrToAnyLifted @Int

type UnlUnit :: UnliftedType
data UnlUnit = UnlUnit

addrToUnlUnit :: Addr# -> UnlUnit
addrToUnlUnit = addrToAnyUnlifted

type Box :: UnliftedType -> Type
data Box a = Box a

addrToBoxedUnlUnit :: Addr# -> Box UnlUnit
addrToBoxedUnlUnit a = Box (addrToAnyUnlifted a)

addrToBoxByteArr :: Addr# -> Box ByteArray#
addrToBoxByteArr a = Box (addrToAnyUnlifted a)
