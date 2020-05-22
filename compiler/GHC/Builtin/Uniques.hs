{-# LANGUAGE CPP #-}

-- | This is where we define a mapping from Uniques to their associated
-- known-key Names for things associated with tuples and sums. We use this
-- mapping while deserializing known-key Names in interface file symbol tables,
-- which are encoded as their Unique. See Note [Symbol table representation of
-- names] for details.
--

module GHC.Builtin.Uniques
    ( -- * Looking up known-key names
      knownUniqueName
    , knownUniqueTyThing
    , KnownUniqueLookup(..)

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

import GHC.Prelude

import GHC.Builtin.Types
import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Types.Basic
import GHC.Utils.Outputable
import GHC.Types.Unique
import GHC.Types.Name
import GHC.Utils.Misc
import GHC.Core.TyCo.Rep
import {-# SOURCE #-} GHC.Core.ConLike

import Data.Bits
import Data.Maybe

data KnownUniqueLookup = KnownUniqueWiredIn TyThing | KnownUniqueName Name

getNameFromKnownUnique :: KnownUniqueLookup -> Name
getNameFromKnownUnique k =
  case k of
    KnownUniqueWiredIn t -> getName t
    KnownUniqueName n -> n

-- | Get the 'Name' associated with a known-key 'Unique'.
--
-- Get the 'TyThing' for wired in names, otherwise just the 'Name'
knownUniqueTyThing :: Unique -> Maybe KnownUniqueLookup
knownUniqueTyThing u =
    case tag of
      'z' -> Just $ getUnboxedSumTyThing n
      '4' -> Just $ getTupleTyConTyThing Boxed n
      '5' -> Just $ getTupleTyConTyThing Unboxed n
      '7' -> Just $ getTupleDataConTyThing Boxed n
      '8' -> Just $ getTupleDataConTyThing Unboxed n
      'k' -> Just $ KnownUniqueName $ getCTupleTyConName n
      'm' -> Just $ KnownUniqueName $ getCTupleDataConName n
      _   -> Nothing
  where
    (tag, n) = unpkUnique u

knownUniqueName :: Unique -> Maybe Name
knownUniqueName u = getNameFromKnownUnique <$> knownUniqueTyThing u


--------------------------------------------------
-- Anonymous sums
--
-- Sum arities start from 2. The encoding is a bit funny: we break up the
-- integral part into bitfields for the arity, an alternative index (which is
-- taken to be 0xff in the case of the TyCon), and, in the case of a datacon, a
-- tag (used to identify the sum's TypeRep binding).
--
-- This layout is chosen to remain compatible with the usual unique allocation
-- for wired-in data constructors described in GHC.Types.Unique
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
    ASSERT(arity < 0x3f) -- 0x3f since we only have 6 bits to encode the
                         -- alternative
    mkUnique 'z' (arity `shiftL` 8 .|. 0xfc)

mkSumDataConUnique :: ConTagZ -> Arity -> Unique
mkSumDataConUnique alt arity
  | alt >= arity
  = panic ("mkSumDataConUnique: " ++ show alt ++ " >= " ++ show arity)
  | otherwise
  = mkUnique 'z' (arity `shiftL` 8 + alt `shiftL` 2) {- skip the tycon -}

getUnboxedSumTyThing :: Int -> KnownUniqueLookup
getUnboxedSumTyThing n
  | n .&. 0xfc == 0xfc
  = case tag of
      0x0 -> KnownUniqueWiredIn $ ATyCon $ sumTyCon arity
      0x1 -> KnownUniqueName $ getRep $ sumTyCon arity
      _   -> pprPanic "getUnboxedSumName: invalid tag" (ppr tag)
  | tag == 0x0
  = KnownUniqueWiredIn $ AConLike (RealDataCon (sumDataCon (alt + 1) arity))
  | tag == 0x1
  = KnownUniqueWiredIn $ AnId $ dataConWrapId $ sumDataCon (alt + 1) arity
  | tag == 0x2
  = KnownUniqueWiredIn $ ATyCon $ promoteDataCon $ sumDataCon (alt + 1) arity
  | otherwise
  = pprPanic "getUnboxedSumName" (ppr n)
  where
    arity = n `shiftR` 8
    alt = (n .&. 0xfc) `shiftR` 2
    tag = 0x3 .&. n
    getRep tycon =
        fromMaybe (pprPanic "getUnboxedSumName(getRep)" (ppr tycon))
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

getCTupleDataConName :: Int -> Name
getCTupleDataConName n =
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

getTupleTyConTyThing :: Boxity -> Int -> KnownUniqueLookup
getTupleTyConTyThing boxity n =
    case n `divMod` 2 of
      (arity, 0) -> KnownUniqueWiredIn $ ATyCon $ tupleTyCon boxity arity
      (arity, 1) -> KnownUniqueName
                    $ fromMaybe (panic "getTupleTyConName")
                    $ tyConRepName_maybe $ tupleTyCon boxity arity
      _          -> panic "getTupleTyConName: impossible"

getTupleDataConTyThing :: Boxity -> Int -> KnownUniqueLookup
getTupleDataConTyThing boxity n =
    case n `divMod` 3 of
      (arity, 0) -> KnownUniqueWiredIn $ AConLike (RealDataCon (tupleDataCon boxity arity))
      (arity, 1) -> KnownUniqueWiredIn $ AnId $ dataConWorkId $ tupleDataCon boxity arity
      (arity, 2) -> KnownUniqueName
                    $ fromMaybe (panic "getTupleDataCon")
                    $ tyConRepName_maybe $ promotedTupleDataCon boxity arity
      _          -> panic "getTupleDataConName: impossible"
