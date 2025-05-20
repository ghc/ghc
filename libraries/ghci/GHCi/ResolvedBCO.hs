{-# LANGUAGE RecordWildCards, DeriveGeneric, GeneralizedNewtypeDeriving,
    BangPatterns, CPP, MagicHash, FlexibleInstances, FlexibleContexts,
    TypeApplications, ScopedTypeVariables, UnboxedTuples, UndecidableInstances #-}
module GHCi.ResolvedBCO
  ( ResolvedBCO(..)
  , ResolvedBCOPtr(..)
  , isLittleEndian
  , BCOByteArray(..)
  , mkBCOByteArray
  ) where

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Data.SizedSeq
import GHCi.RemoteTypes
import GHCi.BreakArray

import Data.Binary
import GHC.Generics

import Foreign.Storable
import GHC.Exts
import Data.Array.Base (IArray, UArray(..))


#include "MachDeps.h"

isLittleEndian :: Bool
#if defined(WORDS_BIGENDIAN)
isLittleEndian = False
#else
isLittleEndian = True
#endif

-- -----------------------------------------------------------------------------
-- ResolvedBCO

-- | A 'ResolvedBCO' is one in which all the 'Name' references have been
-- resolved to actual addresses or 'RemoteHValues'.
--
data ResolvedBCO
   = ResolvedBCO {
        resolvedBCOIsLE   :: Bool,
        resolvedBCOArity  :: {-# UNPACK #-} !Int,
        resolvedBCOInstrs :: BCOByteArray Word16,       -- ^ insns
        resolvedBCOBitmap :: BCOByteArray Word,         -- ^ bitmap
        resolvedBCOLits   :: BCOByteArray Word,
          -- ^ non-ptrs - subword sized entries still take up a full (host) word
        resolvedBCOPtrs   :: (SizedSeq ResolvedBCOPtr), -- ^ ptrs
        resolvedBCOIsCaseCont :: !Bool                  -- ^ See Note [Case continuation BCOs]
   }
   deriving (Generic, Show)

-- | Wrapper for a 'ByteArray#'.
-- The phantom type tells what elements are stored in the 'ByteArray#'.
-- Creating a 'ByteArray#' can be achieved using 'UArray''s API,
-- where the underlying 'ByteArray#' can be unpacked.
data BCOByteArray a
  = BCOByteArray {
        getBCOByteArray :: !ByteArray#
  }

fromBCOByteArray :: forall a . Storable a => BCOByteArray a -> UArray Int a
fromBCOByteArray (BCOByteArray ba#) = UArray 0 (n - 1) n ba#
  where
    len# = sizeofByteArray# ba#
    n = (I# len#) `div` sizeOf (undefined :: a)

mkBCOByteArray :: UArray Int a -> BCOByteArray a
mkBCOByteArray (UArray _ _ _ arr) = BCOByteArray arr

instance Show (BCOByteArray Word16) where
  showsPrec _ _ = showString "BCOByteArray Word16"

instance Show (BCOByteArray Word) where
  showsPrec _ _ = showString "BCOByteArray Word"

-- | The Binary instance for ResolvedBCOs.
--
-- Note, that we do encode the endianness, however there is no support for mixed
-- endianness setups.  This is primarily to ensure that ghc and iserv share the
-- same endianness.
instance Binary ResolvedBCO where
  put ResolvedBCO{..} = do
    put resolvedBCOIsLE
    put resolvedBCOArity
    put resolvedBCOInstrs
    put resolvedBCOBitmap
    put resolvedBCOLits
    put resolvedBCOPtrs
    put resolvedBCOIsCaseCont
  get = ResolvedBCO <$> get <*> get <*> get <*> get <*> get <*> get <*> get

-- See Note [BCOByteArray serialization]
instance (Binary a, Storable a, IArray UArray a) => Binary (BCOByteArray a) where
  put = put . fromBCOByteArray
  get = mkBCOByteArray <$> get


data ResolvedBCOPtr
  = ResolvedBCORef {-# UNPACK #-} !Int
      -- ^ reference to the Nth BCO in the current set
  | ResolvedBCOPtr {-# UNPACK #-} !(RemoteRef HValue)
      -- ^ reference to a previously created BCO
  | ResolvedBCOStaticPtr {-# UNPACK #-} !(RemotePtr ())
      -- ^ reference to a static ptr
  | ResolvedBCOPtrBCO ResolvedBCO
      -- ^ a nested BCO
  | ResolvedBCOPtrBreakArray {-# UNPACK #-} !(RemoteRef BreakArray)
      -- ^ Resolves to the MutableArray# inside the BreakArray
  deriving (Generic, Show)

instance Binary ResolvedBCOPtr

-- Note [BCOByteArray serialization]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- !12142 changed some BCO blob types from UArray to
-- BCOByteArray(ByteArray#) to save a little space. Unfortunately, a
-- nasty serialization bug has surfaced since then. It happens when we
-- need to pass BCOByteArray between host/target with mismatching word
-- sizes. When 32-bit iserv receives a `BCOByteArray Word` from 64-bit
-- host GHC, it would parse the buffer assuming each Word=Word32, even
-- if host GHC assumes each Word=Word64, and of course it's horribly
-- wrong!
--
-- The root issue here is the usage of platform sized integer types in
-- BCO (and any messages we pass between ghc/iserv really), we should
-- do what we already do for RemotePtr: always use Word64 instead of
-- Word. But that takes much more work, and there's an easier
-- mitigation: keep BCOByteArray as ByteArray#, but serialize it as
-- UArray, given the Binary instances are independent of platform word
-- size and endianness, so each Word/Int is always serialized as
-- 64-bit big-endian Word64/Int64, and the entire UArray is serialized
-- as a list (length+elements).
--
-- Since we erase the metadata in UArray, we need to find a way to
-- calculate the item count by dividing the ByteArray# length with
-- element size. The element size comes from Storable's sizeOf method,
-- thus the addition of Storable constraint.
