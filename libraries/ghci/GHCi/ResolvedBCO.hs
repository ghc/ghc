{-# LANGUAGE RecordWildCards, DeriveGeneric, GeneralizedNewtypeDeriving,
    BangPatterns, CPP #-}
module GHCi.ResolvedBCO
  ( ResolvedBCO(..)
  , ResolvedBCOPtr(..)
  , isLittleEndian
  ) where

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Data.SizedSeq
import GHCi.RemoteTypes
import GHCi.BreakArray

import Data.Array.Unboxed
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
-- Note, all arrays are zero-indexed (we assume this when
-- serializing/deserializing)
data ResolvedBCO
   = ResolvedBCO {
        resolvedBCOIsLE   :: Bool,
        resolvedBCOArity  :: {-# UNPACK #-} !Int,
        resolvedBCOInstrs :: UArray Int Word16,         -- insns
        resolvedBCOBitmap :: UArray Int Word64,         -- bitmap
        resolvedBCOLits   :: UArray Int Word64,         -- non-ptrs
        resolvedBCOPtrs   :: (SizedSeq ResolvedBCOPtr)  -- ptrs
   }
   deriving (Generic, Show)

-- | The Binary instance for ResolvedBCOs.
--
-- Note, that we do encode the endianness, however there is no support for mixed
-- endianness setups.  This is primarily to ensure that ghc and iserv share the
-- same endianness.
instance Binary ResolvedBCO where
  put ResolvedBCO{..} = do
    put resolvedBCOIsLE
    put resolvedBCOArity
    putArray resolvedBCOInstrs
    putArray resolvedBCOBitmap
    putArray resolvedBCOLits
    put resolvedBCOPtrs
  get = ResolvedBCO
        <$> get <*> get <*> getArray <*> getArray <*> getArray <*> get

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
