{-# LANGUAGE CPP #-}

-- | This is where we define a mapping from Uniques to their associated
-- known-key Names for things associated with tuples and sums. We use this
-- mapping while deserializing known-key Names in interface file symbol tables,
-- which are encoded as their Unique. See Note [Symbol table representation of
-- names] for details.
--

module KnownUniques
    ( -- * Looking up known-key names
      knownUniqueName

      -- * Getting the 'Unique's of 'Name's
      -- ** Anonymous sums
    , mkSumTyConUnique
    , mkSumDataConUnique
      -- ** Tuples
      -- *** Vanilla
    , mkTupleTyConUnique
    , mkTupleDataConUnique
      -- *** Constraint
    , mkCTupleTyConUnique
    , mkCTupleDataConUnique
    ) where

#include "HsVersions.h"

import TysWiredIn
import TyCon
import DataCon
import Id
import BasicTypes
import Outputable
import Unique
import Name
import Util

import Data.Bits
import Data.Maybe

-- | Get the 'Name' associated with a known-key 'Unique'.
knownUniqueName :: Unique -> Maybe Name
knownUniqueName u =
    case tag of
      'z' -> Just $ getUnboxedSumName n
      '4' -> Just $ getTupleTyConName Boxed n
      '5' -> Just $ getTupleTyConName Unboxed n
      '7' -> Just $ getTupleDataConName Boxed n
      '8' -> Just $ getTupleDataConName Unboxed n
      'k' -> Just $ getCTupleTyConName n
      'm' -> Just $ getCTupleDataConUnique n
      _   -> Nothing
  where
    (tag, n) = unpkUnique u

--------------------------------------------------
-- Anonymous sums
--
-- Sum arities start from 2. The encoding is a bit funny: we break up the
-- integral part into bitfields for the arity, an alternative index (which is
-- taken to be 0xff in the case of the TyCon), and, in the case of a datacon, a
-- tag (used to identify the sum's TypeRep binding).
--
-- This layout is chosen to remain compatible with the usual unique allocation
-- for wired-in data constructors described in Unique.hs
--
-- TyCon for sum of arity k:
--   00000000 kkkkkkkk 11111100

-- TypeRep of TyCon for sum of arity k:
--   00000000 kkkkkkkk 11111101
--
-- DataCon for sum of arity k and alternative n (zero-based):
--   00000000 kkkkkkkk nnnnnn00
--
-- TypeRep for sum DataCon of arity k and alternative n (zero-based):
--   00000000 kkkkkkkk nnnnnn10

mkSumTyConUnique :: Arity -> Unique
mkSumTyConUnique arity =
    ASSERT(arity < 0xff)
    mkUnique 'z' (arity `shiftL` 8 .|. 0xfc)

mkSumDataConUnique :: ConTagZ -> Arity -> Unique
mkSumDataConUnique alt arity
  | alt >= arity
  = panic ("mkSumDataConUnique: " ++ show alt ++ " >= " ++ show arity)
  | otherwise
  = mkUnique 'z' (arity `shiftL` 8 + alt `shiftL` 2) {- skip the tycon -}

getUnboxedSumName :: Int -> Name
getUnboxedSumName n
  | n .&. 0xfc == 0xfc
  = case tag of
      0x0 -> tyConName $ sumTyCon arity
      0x1 -> getRep $ sumTyCon arity
      _   -> pprPanic "getUnboxedSumName: invalid tag" (ppr tag)
  | tag == 0x0
  = dataConName $ sumDataCon (alt + 1) arity
  | tag == 0x2
  = getRep $ promoteDataCon $ sumDataCon (alt + 1) arity
  | otherwise
  = pprPanic "getUnboxedSumName" (ppr n)
  where
    arity = n `shiftR` 8
    alt = (n .&. 0xff) `shiftR` 2
    tag = 0x3 .&. n
    getRep tycon =
        fromMaybe (pprPanic "getUnboxedSumName" (ppr tycon))
        $ tyConRepName_maybe tycon

-- Note [Uniques for tuple type and data constructors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Wired-in type constructor keys occupy *two* slots:
--    * u: the TyCon itself
--    * u+1: the TyConRepName of the TyCon
--
-- Wired-in tuple data constructor keys occupy *three* slots:
--    * u: the DataCon itself
--    * u+1: its worker Id
--    * u+2: the TyConRepName of the promoted TyCon

--------------------------------------------------
-- Constraint tuples

mkCTupleTyConUnique :: Arity -> Unique
mkCTupleTyConUnique a = mkUnique 'k' (2*a)

mkCTupleDataConUnique :: Arity -> Unique
mkCTupleDataConUnique a = mkUnique 'm' (3*a)

getCTupleTyConName :: Int -> Name
getCTupleTyConName n =
    case n `divMod` 2 of
      (arity, 0) -> cTupleTyConName arity
      (arity, 1) -> mkPrelTyConRepName $ cTupleTyConName arity
      _          -> panic "getCTupleTyConName: impossible"

getCTupleDataConUnique :: Int -> Name
getCTupleDataConUnique n =
    case n `divMod` 3 of
      (arity,  0) -> cTupleDataConName arity
      (_arity, 1) -> panic "getCTupleDataConName: no worker"
      (arity,  2) -> mkPrelTyConRepName $ cTupleDataConName arity
      _           -> panic "getCTupleDataConName: impossible"

--------------------------------------------------
-- Normal tuples

mkTupleDataConUnique :: Boxity -> Arity -> Unique
mkTupleDataConUnique Boxed          a = mkUnique '7' (3*a)    -- may be used in C labels
mkTupleDataConUnique Unboxed        a = mkUnique '8' (3*a)

mkTupleTyConUnique :: Boxity -> Arity -> Unique
mkTupleTyConUnique Boxed           a  = mkUnique '4' (2*a)
mkTupleTyConUnique Unboxed         a  = mkUnique '5' (2*a)

getTupleTyConName :: Boxity -> Int -> Name
getTupleTyConName boxity n =
    case n `divMod` 2 of
      (arity, 0) -> tyConName $ tupleTyCon boxity arity
      (arity, 1) -> fromMaybe (panic "getTupleTyConName")
                    $ tyConRepName_maybe $ tupleTyCon boxity arity
      _          -> panic "getTupleTyConName: impossible"

getTupleDataConName :: Boxity -> Int -> Name
getTupleDataConName boxity n =
    case n `divMod` 3 of
      (arity, 0) -> dataConName $ tupleDataCon boxity arity
      (arity, 1) -> idName $ dataConWorkId $ tupleDataCon boxity arity
      (arity, 2) -> fromMaybe (panic "getTupleDataCon")
                    $ tyConRepName_maybe $ promotedTupleDataCon boxity arity
      _          -> panic "getTupleDataConName: impossible"
