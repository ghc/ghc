{-# LANGUAGE RecordWildCards, DeriveGeneric, GeneralizedNewtypeDeriving,
    BangPatterns #-}
module GHCi.ResolvedBCO
  ( ResolvedBCO(..)
  , ResolvedBCOPtr(..)
  ) where

import SizedSeq
import GHCi.RemoteTypes
import GHCi.BreakArray

import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.Base
import Data.Binary
import GHC.Generics

-- -----------------------------------------------------------------------------
-- ResolvedBCO

-- A A ResolvedBCO is one in which all the Name references have been
-- resolved to actual addresses or RemoteHValues.
--
-- Note, all arrays are zero-indexed (we assume this when
-- serializing/deserializing)
data ResolvedBCO
   = ResolvedBCO {
        resolvedBCOArity  :: {-# UNPACK #-} !Int,
        resolvedBCOInstrs :: UArray Int Word,           -- insns
        resolvedBCOBitmap :: UArray Int Word,           -- bitmap
        resolvedBCOLits   :: UArray Int Word,           -- non-ptrs
        resolvedBCOPtrs   :: (SizedSeq ResolvedBCOPtr)  -- ptrs
   }
   deriving (Generic, Show)

instance Binary ResolvedBCO where
  put ResolvedBCO{..} = do
    put resolvedBCOArity
    putArray resolvedBCOInstrs
    putArray resolvedBCOBitmap
    putArray resolvedBCOLits
    put resolvedBCOPtrs
  get = ResolvedBCO <$> get <*> getArray <*> getArray <*> getArray <*> get

-- Specialized versions of the binary get/put for UArray Int Word.
-- This saves a bit of time and allocation over using the default
-- get/put, because we get specialisd code and also avoid serializing
-- the bounds.
putArray :: UArray Int Word -> Put
putArray a@(UArray _ _ n _) = do
  put n
  mapM_ put (elems a)

getArray :: Get (UArray Int Word)
getArray = do
  n  <- get
  xs <- gets n []
  return $! mkArray n xs
 where
  gets 0 xs = return xs
  gets n xs = do
    x <- get
    gets (n-1) (x:xs)

  mkArray :: Int -> [Word] -> UArray Int Word
  mkArray n0 xs0 = runST $ do
    !marr <- newArray (0,n0-1) 0
    let go 0 _ = return ()
        go _ [] = error "mkArray"
        go n (x:xs) = do
          let n' = n-1
          unsafeWrite marr n' x
          go n' xs
    go n0 xs0
    unsafeFreezeSTUArray marr

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
