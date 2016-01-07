{-# LANGUAGE RecordWildCards, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module GHCi.ResolvedBCO
  ( ResolvedBCO(..)
  , ResolvedBCOPtr(..)
  ) where

import SizedSeq
import GHCi.RemoteTypes
import GHCi.BreakArray

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
  | ResolvedBCOPtr (RemoteRef HValue)
      -- ^ reference to a previously created BCO
  | ResolvedBCOStaticPtr (RemotePtr ())
      -- ^ reference to a static ptr
  | ResolvedBCOPtrBCO ResolvedBCO
      -- ^ a nested BCO
  | ResolvedBCOPtrBreakArray (RemoteRef BreakArray)
      -- ^ Resolves to the MutableArray# inside the BreakArray
  deriving (Generic, Show)

instance Binary ResolvedBCOPtr
