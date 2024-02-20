{-# LANGUAGE RecordWildCards, DeriveGeneric, GeneralizedNewtypeDeriving,
    BangPatterns, CPP, MagicHash, FlexibleInstances, FlexibleContexts,
    TypeApplications, ScopedTypeVariables #-}
module GHCi.ResolvedBCO
  ( ResolvedBCO(..)
  , ResolvedBCOPtr(..)
  , isLittleEndian
  , BCOByteArray(..)
  ) where

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Base (ByteArray#, sizeofByteArray#, Int (I#))
import GHC.Data.SizedSeq
import GHCi.RemoteTypes
import GHCi.BreakArray

import qualified Data.Array.IO.Internals as A
import qualified Data.Array.Base as A

import Data.Array.Base(UArray(UArray))
import Data.Binary
import GHC.Generics
import GHCi.BinaryArray


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
        resolvedBCOBitmap :: BCOByteArray Word64,       -- bitmap
        resolvedBCOLits   :: BCOByteArray Word64,       -- non-ptrs
        resolvedBCOPtrs   :: (SizedSeq ResolvedBCOPtr)  -- ptrs
   }
   deriving (Generic, Show)

data BCOByteArray a = BCOByteArray { getBCOByteArray :: ByteArray# }

instance Show (BCOByteArray Word16) where
  showsPrec n = showsPrec n . bcoByteArrayToUArray

instance Show (BCOByteArray Word64) where
  showsPrec n = showsPrec n . bcoByteArrayToUArray

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
  get = ResolvedBCO
        <$> get <*> get <*> get <*> get <*> get <*> get

instance Binary (BCOByteArray Word16) where
  put = putBCOByteArray
  get = decodeBCOByteArray

instance Binary (BCOByteArray Word64) where
  put = putBCOByteArray
  get = decodeBCOByteArray

bcoByteArrayToUArray :: BCOByteArray a -> UArray Int a
bcoByteArrayToUArray (BCOByteArray arr#) =
  let n# = sizeofByteArray# arr#
      n = I# n#
  in UArray 0 (n - 1) n arr#

putBCOByteArray :: BCOByteArray a -> Put
putBCOByteArray =
  putArray . bcoByteArrayToUArray

decodeBCOByteArray :: forall a. A.MArray A.IOUArray a IO => Get (BCOByteArray a)
decodeBCOByteArray = do
  !(UArray _ _ _ arr#) <- getArray :: Get (UArray Int a)
  pure $ BCOByteArray arr#


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
