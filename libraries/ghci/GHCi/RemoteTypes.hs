{-# LANGUAGE CPP, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module GHCi.RemoteTypes
  ( RemotePtr(..), toRemotePtr, fromRemotePtr
  , HValue(..)
  , HValueRef, mkHValueRef, localHValueRef, freeHValueRef
  , ForeignHValue, mkForeignHValue, withForeignHValue
  , unsafeForeignHValueToHValueRef, finalizeForeignHValue
  ) where

import Data.Word
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import Data.Binary
import GHC.Exts
import GHC.ForeignPtr

-- -----------------------------------------------------------------------------
-- RemotePtr

-- Static pointers only; don't use this for heap-resident pointers.
-- Instead use HValueRef.

#include "MachDeps.h"
#if SIZEOF_HSINT == 4
newtype RemotePtr = RemotePtr Word32
#elif SIZEOF_HSINT == 8
newtype RemotePtr = RemotePtr Word64
#endif

toRemotePtr :: Ptr a -> RemotePtr
toRemotePtr p = RemotePtr (fromIntegral (ptrToWordPtr p))

fromRemotePtr :: RemotePtr -> Ptr a
fromRemotePtr (RemotePtr p) = wordPtrToPtr (fromIntegral p)

deriving instance Show RemotePtr
deriving instance Binary RemotePtr

-- -----------------------------------------------------------------------------
-- HValueRef

newtype HValue = HValue Any

instance Show HValue where
  show _ = "<HValue>"

newtype HValueRef = HValueRef RemotePtr
  deriving (Show, Binary)

-- | Make a reference to a local HValue that we can send remotely.
-- This reference will keep the value that it refers to alive until
-- 'freeHValueRef' is called.
mkHValueRef :: HValue -> IO HValueRef
mkHValueRef (HValue hv) = do
  sp <- newStablePtr hv
  return $! HValueRef (toRemotePtr (castStablePtrToPtr sp))

-- | Convert an HValueRef to an HValue.  Should only be used if the HValue
-- originated in this process.
localHValueRef :: HValueRef -> IO HValue
localHValueRef (HValueRef w) = do
  p <- deRefStablePtr (castPtrToStablePtr (fromRemotePtr w))
  return (HValue p)

-- | Release an HValueRef that originated in this process
freeHValueRef :: HValueRef -> IO ()
freeHValueRef (HValueRef w) =
  freeStablePtr (castPtrToStablePtr (fromRemotePtr w))

-- | An HValueRef with a finalizer
newtype ForeignHValue = ForeignHValue (ForeignPtr ())

-- | Create a 'ForeignHValue' from an 'HValueRef'.  The finalizer
-- should arrange to call 'freeHValueRef' on the 'HValueRef'.  (since
-- this function needs to be called in the process that created the
-- 'HValueRef', it cannot be called directly from the finalizer).
mkForeignHValue :: HValueRef -> IO () -> IO ForeignHValue
mkForeignHValue (HValueRef hvref) finalizer =
  ForeignHValue <$> newForeignPtr (fromRemotePtr hvref) finalizer

-- | Use a 'ForeignHValue'
withForeignHValue :: ForeignHValue -> (HValueRef -> IO a) -> IO a
withForeignHValue (ForeignHValue fp) f =
   withForeignPtr fp (f . HValueRef . toRemotePtr)

unsafeForeignHValueToHValueRef :: ForeignHValue -> HValueRef
unsafeForeignHValueToHValueRef (ForeignHValue fp) =
  HValueRef (toRemotePtr (unsafeForeignPtrToPtr fp))

finalizeForeignHValue :: ForeignHValue -> IO ()
finalizeForeignHValue (ForeignHValue fp) = finalizeForeignPtr fp
