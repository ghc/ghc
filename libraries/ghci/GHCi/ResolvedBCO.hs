{-# LANGUAGE RecordWildCards, DeriveGeneric, GeneralizedNewtypeDeriving,
    BangPatterns, CPP, MagicHash, FlexibleInstances, FlexibleContexts,
    TypeApplications, ScopedTypeVariables, UnboxedTuples #-}
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
import Data.Binary.Put (putBuilder)
import GHC.Generics

import Foreign.Ptr
import Data.Array.Byte
import qualified Data.Binary.Get.Internal as Binary
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Internal as BB
import GHC.Exts
import Data.Array.Base (UArray(..))

import GHC.IO

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
        resolvedBCOInstrs :: BCOByteArray Word16,       -- insns
        resolvedBCOBitmap :: BCOByteArray Word,         -- bitmap
        resolvedBCOLits   :: BCOByteArray Word,         -- non-ptrs
        resolvedBCOPtrs   :: (SizedSeq ResolvedBCOPtr)  -- ptrs
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
  get = ResolvedBCO <$> get <*> get <*> get <*> get <*> get <*> get

instance Binary (BCOByteArray a) where
  put = putBCOByteArray
  get = decodeBCOByteArray


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

-- --------------------------------------------------------
-- Serialisers for 'BCOByteArray'
-- --------------------------------------------------------

putBCOByteArray :: BCOByteArray a -> Put
putBCOByteArray (BCOByteArray bar) = do
  put (I# (sizeofByteArray# bar))
  putBuilder $ byteArrayBuilder bar

decodeBCOByteArray :: Get (BCOByteArray a)
decodeBCOByteArray = do
  n <- get
  getByteArray n

byteArrayBuilder :: ByteArray# -> BB.Builder
byteArrayBuilder arr# = BB.builder $ go 0 (I# (sizeofByteArray# arr#))
  where
    go :: Int -> Int -> BB.BuildStep a -> BB.BuildStep a
    go !inStart !inEnd k (BB.BufferRange outStart outEnd)
      -- There is enough room in this output buffer to write all remaining array
      -- contents
      | inRemaining <= outRemaining = do
          copyByteArrayToAddr arr# inStart outStart inRemaining
          k (BB.BufferRange (outStart `plusPtr` inRemaining) outEnd)
      -- There is only enough space for a fraction of the remaining contents
      | otherwise = do
          copyByteArrayToAddr arr# inStart outStart outRemaining
          let !inStart' = inStart + outRemaining
          return $! BB.bufferFull 1 outEnd (go inStart' inEnd k)
      where
        inRemaining  = inEnd - inStart
        outRemaining = outEnd `minusPtr` outStart

    copyByteArrayToAddr :: ByteArray# -> Int -> Ptr a -> Int -> IO ()
    copyByteArrayToAddr src# (I# src_off#) (Ptr dst#) (I# len#) =
        IO $ \s -> case copyByteArrayToAddr# src# src_off# dst# len# s of
                     s' -> (# s', () #)

getByteArray :: Int -> Get (BCOByteArray a)
getByteArray nbytes@(I# nbytes#) = do
    let !(MutableByteArray arr#) = unsafeDupablePerformIO $
          IO $ \s -> case newByteArray# nbytes# s of
                (# s', mbar #) -> (# s', MutableByteArray mbar #)
    let go 0 _ = return ()
        go !remaining !off = do
            Binary.readNWith n $ \ptr ->
              copyAddrToByteArray ptr arr# off n
            go (remaining - n) (off + n)
          where n = min chunkSize remaining
    go nbytes 0
    return $! unsafeDupablePerformIO $
      IO $ \s -> case unsafeFreezeByteArray# arr# s of
          (# s', bar #) -> (# s', BCOByteArray bar #)
  where
    chunkSize = 10*1024

    copyAddrToByteArray :: Ptr a -> MutableByteArray# RealWorld
                        -> Int -> Int -> IO ()
    copyAddrToByteArray (Ptr src#) dst# (I# dst_off#) (I# len#) =
        IO $ \s -> case copyAddrToByteArray# src# dst# dst_off# len# s of
                     s' -> (# s', () #)
