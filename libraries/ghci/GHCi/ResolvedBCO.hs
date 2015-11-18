{-# LANGUAGE RecordWildCards, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module GHCi.ResolvedBCO
  ( ResolvedBCO(..)
  , ResolvedBCOPtr(..)
  ) where

import SizedSeq
import GHCi.RemoteTypes

import Data.Array.Unboxed
import Data.Binary
import GHC.Generics

-- -----------------------------------------------------------------------------
-- ResolvedBCO

-- A ResolvedBCO is one in which all the Name references have been
-- resolved to actual addresses or RemoteHValues.

data ResolvedBCO
   = ResolvedBCO {
        resolvedBCOArity  :: Int,
        resolvedBCOInstrs :: UArray Int Word16,         -- insns
        resolvedBCOBitmap :: UArray Int Word,           -- bitmap
        resolvedBCOLits   :: UArray Int Word,           -- non-ptrs
        resolvedBCOPtrs   :: (SizedSeq ResolvedBCOPtr)  -- ptrs
   }
   deriving (Generic, Show)

instance Binary ResolvedBCO

data ResolvedBCOPtr
  = ResolvedBCORef Int
      -- ^ reference to the Nth BCO in the current set
  | ResolvedBCOPtr HValueRef
      -- ^ reference to a previously created BCO
  | ResolvedBCOStaticPtr RemotePtr
      -- ^ reference to a static ptr
  | ResolvedBCOPtrBCO ResolvedBCO
      -- ^ a nested BCO
  | ResolvedBCOPtrLocal HValue
      -- ^ something local, cannot be serialized
  deriving (Generic, Show)

-- Manual Binary instance is needed because we cannot serialize
-- ResolvedBCOPtrLocal.  This will go away once we have support for
-- remote breakpoints.
instance Binary ResolvedBCOPtr where
  put (ResolvedBCORef a) = putWord8 0 >> put a
  put (ResolvedBCOPtr a) = putWord8 1 >> put a
  put (ResolvedBCOStaticPtr a) = putWord8 2 >> put a
  put (ResolvedBCOPtrBCO a) = putWord8 3 >> put a
  put (ResolvedBCOPtrLocal _) =
    error "Cannot serialize a local pointer.  Use -fno-external-interpreter?"

  get = do
    w <- getWord8
    case w of
      0 -> ResolvedBCORef <$> get
      1 -> ResolvedBCOPtr <$> get
      2 -> ResolvedBCOStaticPtr <$> get
      _ -> ResolvedBCOPtrBCO <$> get
