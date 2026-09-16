{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeAbstractions #-}

module GHCi.ResolvedBCO
  ( ResolvedBCO(..)
  , ResolvedBCOPtr(..)
  , isLittleEndian
  , BCOByteArray(..)
  , mkBCOByteArray
  ) where

#include "MachDeps.h"

#if SIZEOF_HSWORD != 4 && SIZEOF_HSWORD != 8
#error Bytecode serialization works only on 32- and 64-bit platforms.
#endif

import Prelude -- See note [Why do we import Prelude here?]

import GHCi.RemoteTypes
import GHCi.BreakArray

import GHC.Exts (Int (I#), ByteArray#, sizeofByteArray#)
import qualified GHC.Exts.Heap as Heap

import Data.Word (Word16)
import Data.ByteString.Short (ShortByteString (SBS), toShort)
import Data.Array.Base (UArray (UArray))
import Data.Binary.Builder (fromShortByteString)
import Data.Binary.Put (Put, putWord8, putWord64le, putBuilder)
import Data.Binary.Get (Get, getWord8, getWord64le, getByteString)
import Data.Binary (Binary (put, get))
import Foreign.Storable (Storable, sizeOf)
import GHC.Data.SmallArray (SmallArray)
import GHC.Generics (Generic)

#if defined(WORDS_BIGENDIAN)
import Data.Binary.Put (putWord16le)
import Data.Binary.Get (getWord16le)
#endif

#if defined(WORDS_BIGENDIAN) || SIZEOF_HSWORD == 4
import Control.Monad (replicateM)
import Data.Array.Base (IArray, listArray, numElements, elems)
#endif

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

#if defined(WORDS_BIGENDIAN) || SIZEOF_HSWORD == 4

fromBCOByteArray :: Storable a => BCOByteArray a -> UArray Int a
fromBCOByteArray @a (BCOByteArray ba#) = UArray 0 (n - 1) n ba#
  where
    len# = sizeofByteArray# ba#
    n = (I# len#) `div` sizeOf (undefined :: a)

#endif

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

#if defined(WORDS_BIGENDIAN) || SIZEOF_HSWORD == 4

-- | Serialize a 'BCOByteArray', not writing the payload verbatim but
--   serializing the individual elements. This can be used with any host
--   platform and element type.
putBCOByteArrayPortably :: (Storable a, IArray UArray a)
                        => (a -> Put)
                           -- ^ The serializer to use for each element
                        -> (BCOByteArray a -> Put)
                           -- ^ The 'BCOByteArray' serializer
putBCOByteArrayPortably @a putElement bcoByteArray
  = putWord64le (fromIntegral size) <> foldMap putElement (elems array)
  where

  array :: UArray Int a
  array = fromBCOByteArray bcoByteArray

  size :: Int
  size = numElements array

-- | Deserialize a 'BCOByteArray', not reading the payload verbatim but
--   deserializing the individual elements. This can be used with any host
--   platform and element type.
getBCOByteArrayPortably :: IArray UArray a
                        => Get a
                           -- ^ The deserializer to use for each element
                        -> Get (BCOByteArray a)
                           -- ^ The 'BCOByteArray' deserializer
getBCOByteArrayPortably getElement = do
  size <- fromIntegral <$> getWord64le :: Get Int
  elements <- replicateM size getElement
  return (mkBCOByteArray (listArray (0, pred size) elements))

#endif

#if !defined(WORDS_BIGENDIAN)

-- | Serialize a 'BCOByteArray', writing the payload verbatim instead of
--   serializing the individual elements. This must only be used when the host
--   platform uses little endian and the bitsize of the element type of the
--   array is the one that is used for the serialized form of elements.
putBCOByteArrayDirectly :: Storable a => BCOByteArray a -> Put
putBCOByteArrayDirectly @a (BCOByteArray byteArray#)
  = putWord64le (fromIntegral size) <>
    putBuilder (fromShortByteString (SBS byteArray#))
  where

  size :: Int
  size = I# (sizeofByteArray# byteArray#) `div` sizeOf (undefined :: a)

-- | Deserialize a 'BCOByteArray', reading the payload verbatim instead of
--   deserializing the individual elements. This must only be used when the host
--   platform uses little endian and the bitsize of the element type of the
--   array is the one that is used for the serialized form of elements.
getBCOByteArrayDirectly :: Storable a => Get (BCOByteArray a)
getBCOByteArrayDirectly @a = do
  size <- fromIntegral <$> getWord64le :: Get Int
  SBS byteArray# <- toShort <$> getByteString (size * sizeOf (undefined :: a))
    -- Beware that there is no overflow check for the byte count computation.
  return (BCOByteArray byteArray#)

#endif

-- See [BCOByteArray serialization].
instance Binary (BCOByteArray Word16) where

#if defined(WORDS_BIGENDIAN)
  put = putBCOByteArrayPortably putWord16le
  get = getBCOByteArrayPortably getWord16le
#else
  put = putBCOByteArrayDirectly
  get = getBCOByteArrayDirectly
#endif

-- See [BCOByteArray serialization].
instance Binary (BCOByteArray Word) where

#if defined(WORDS_BIGENDIAN) || SIZEOF_HSWORD == 4
  put = putBCOByteArrayPortably (putWord64le . fromIntegral)
  get = getBCOByteArrayPortably (fromIntegral <$> getWord64le)
#else
  put = putBCOByteArrayDirectly
  get = getBCOByteArrayDirectly
#endif

-- Note [BCOByteArray serialization]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The serialization of a 'BCOByteArray' value consists of the serialization of
-- the number of elements followed by the serializations of the individual
-- elements. The serialization format is platform-independent, thanks to the
-- following specifics:
--
--   * All natural numbers are encoded using little endian, independently of the
--     host’s endianness.
--   * Array sizes are encoded using 64 bits, independently of the host’s native
--     bit size.
--   * 'Word' values are encoded using 64 bits, independently of the host’s
--     native bit size.
--
-- Whenever the encoding of the payload matches its representation in main
-- memory, the whole payload is transferred verbatim (`putBCOByteArrayDirectly`,
-- `getBCOByteArrayDirectly`). Otherwise, the elements are serialized or
-- deserialized individually (`putBCOByteArrayPortably`,
-- `getBCOByteArrayPortably`).

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
