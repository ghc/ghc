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

#include "MachDeps.h"

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Data.SmallArray
import GHCi.RemoteTypes
import GHCi.BreakArray

#if SIZEOF_HSWORD == 4
import Control.Monad
import Data.Array.Base (foldrArray, listArray)
import Data.ByteString.Builder.Extra
import Foreign.Storable
#endif

import Data.Binary (Binary(..))
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Short (ShortByteString(..))
import Data.Word
import GHC.Generics

import GHC.Exts
import Data.Array.Base (UArray(..))
import qualified GHC.Exts.Heap as Heap

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
        resolvedBCOIsLE   :: !Bool,
        resolvedBCOArity  :: {-# UNPACK #-} !Int,
        resolvedBCOInstrs :: !(BCOByteArray Word16),       -- ^ insns
        resolvedBCOBitmap :: !(BCOByteArray Word),         -- ^ bitmap
        resolvedBCOLits   :: !(BCOByteArray Word),
          -- ^ non-ptrs - subword sized entries still take up a full (host) word
        resolvedBCOPtrs   :: !(SmallArray ResolvedBCOPtr)  -- ^ ptrs
   }
   -- | A resolved static constructor
   -- See Note [Static constructors in Bytecode]
   | ResolvedStaticCon {
        resolvedBCOIsLE          :: !Bool,
        resolvedStaticConInfoPtr :: !(RemotePtr Heap.StgInfoTable),
        resolvedStaticConArity   :: {-# UNPACK #-} !Word,
        -- ^ how many words are used for the payload of the static constructor
        -- (size of ptrs and (packed) non-ptrs combined)
        resolvedStaticConLits    :: !(BCOByteArray Word),
        -- ^ Notably, sub-word non-ptr arguments and padding have already been
        -- packed into full words, and this array only stores the full final
        -- words to write as the constructor payload.
        --
        -- This is opposed to what we do for BCO literals, where we keep
        -- sub-word literals as full words. For static constructors, the layout
        -- must match exactly what the NCG also expects, so we must pack
        -- sub-words accordingly for compatibility between interpreted and
        -- compiled code.
        resolvedStaticConPtrs       :: !(SmallArray ResolvedBCOPtr),
        resolvedStaticConIsUnlifted :: !Bool
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

#if SIZEOF_HSWORD == 4
fromBCOByteArray :: forall a . Storable a => BCOByteArray a -> UArray Int a
fromBCOByteArray (BCOByteArray ba#) = UArray 0 (n - 1) n ba#
  where
    len# = sizeofByteArray# ba#
    n = (I# len#) `div` sizeOf (undefined :: a)
#endif

mkBCOByteArray :: UArray Int a -> BCOByteArray a
mkBCOByteArray (UArray _ _ _ arr) = BCOByteArray arr

-- | Directly serialize 'BCOByteArray' payload without iterating over
-- individual elements, assuming 'BCOByteArray' element type is a
-- fixed-width type like 'Word16' that doesn't depend on host word
-- size. See Note [BCOByteArray serialization] for more explanation.
unsafePutFixedWidthBCOByteArray :: BCOByteArray a -> Put
unsafePutFixedWidthBCOByteArray (BCOByteArray ba#) = put $ SBS ba#

unsafeGetFixedWidthBCOByteArray :: Get (BCOByteArray a)
unsafeGetFixedWidthBCOByteArray = (\(SBS ba#) -> BCOByteArray ba#) <$> get

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
    putWord8 0
    put resolvedBCOIsLE
    put resolvedBCOArity
    put resolvedBCOInstrs
    put resolvedBCOBitmap
    put resolvedBCOLits
    put resolvedBCOPtrs
  put ResolvedStaticCon{..} = do
    putWord8 1
    put resolvedBCOIsLE
    put resolvedStaticConInfoPtr
    put resolvedStaticConArity
    put resolvedStaticConLits
    put resolvedStaticConPtrs
    put resolvedStaticConIsUnlifted
  get = do
    t <- getWord8
    case t of
      0 -> ResolvedBCO <$> get <*> get <*> get <*> get <*> get <*> get
      1 -> ResolvedStaticCon <$> get <*> get <*> get <*> get <*> get <*> get
      _ -> error "Binary ResolvedBCO: invalid byte"

-- See Note [BCOByteArray serialization]
instance Binary (BCOByteArray Word16) where
  put = unsafePutFixedWidthBCOByteArray
  get = unsafeGetFixedWidthBCOByteArray

-- Word size depends on host, which is tricky when host/target word
-- sizes differ. We always serialize `BCOByteArray Word` as
-- `BCOByteArray Word64`.
instance Binary (BCOByteArray Word) where
#if SIZEOF_HSWORD == 8
  -- 64-bit fast path. `BCOByteArray` is directly serialized via the
  -- `Binary ShortByteString` instance, which serializes the `Int`
  -- bytelength first (via `Int64` transparently), then copies the
  -- buffer.
  put = unsafePutFixedWidthBCOByteArray

  get = unsafeGetFixedWidthBCOByteArray
#else
  -- 32-bit slow path. Pretend it's a `BCOByteArray Word64` and handle
  -- the bytelength & buffer elements directly.
  --
  -- Regarding endianness: the bytelength is serialized via the
  -- `Binary Int` instance, which is serialized as `Int64` via
  -- big-endian. The payload follows host-endianness. This doesn't
  -- work when host/target has different endianness, but we don't
  -- support that setup yet anyway.
  put ba32@(BCOByteArray ba32#) =
    put len64 *>
    putBuilder
      (foldrArray (\w32 acc -> word64Host (fromIntegral w32) <> acc) mempty arr32)
    where
      len32# = sizeofByteArray# ba32#
      len64 = I# len32# * 2
      arr32 = fromBCOByteArray ba32

  get = do
    len64 <- get
    let len = len64 `div` 8
    w32s <- replicateM len (fromIntegral <$> getWord64host)
    pure $ mkBCOByteArray $ listArray (0, len - 1) w32s
#endif

data ResolvedBCOPtr
  = ResolvedBCORef {-# UNPACK #-} !Int
      -- ^ reference to the Nth BCO in the current set of BCOs and
      -- lifted static constructors
  | ResolvedBCOPtr {-# UNPACK #-} !(RemoteRef HValue)
      -- ^ reference to a previously created BCO
  | ResolvedBCOStaticPtr {-# UNPACK #-} !(RemotePtr ())
      -- ^ reference to a static ptr
  | ResolvedBCOPtrBCO ResolvedBCO
      -- ^ a nested BCO
  | ResolvedBCOPtrBreakArray {-# UNPACK #-} !(RemoteRef BreakArray)
      -- ^ Resolves to the MutableArray# inside the BreakArray
  | ResolvedStaticConRef {-# UNPACK #-} !Int
      -- ^ reference to the Nth static constructor in the current set of BCOs
      -- and lifted static constructors
  | ResolvedUnliftedStaticConRef {-# UNPACK #-} !Int
      -- ^ reference to the Nth unlifted static constructor in the current set
      -- of exclusively unlifted static constructors
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
-- Word.
--
-- When we serialize `BCOByteArray Word16`, element is fixed width on
-- 32/64-bit host, so we can directly serialize the buffer per se. For
-- `BCOByteArray Word`, we must always serialize it as `BCOByteArray
-- Word64`, and hence it has fast-path/slow-path decided at
-- compile-time, see comments of `instance Binary (BCOByteArray Word)`
-- for explanation. These are the only two `Binary` instances we ever
-- use, so to avoid unnecessary complexity, we're fine with flexible
-- instances here, instead of generalizing to any element type that
-- may be fixed-width or not.
--
-- Since we erase the metadata in UArray, we need to find a way to
-- calculate the item count by dividing the ByteArray# length with
-- element size. The element size comes from Storable's sizeOf method,
-- thus the addition of Storable constraint.
