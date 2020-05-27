{-# LANGUAGE CPP, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

-- |
-- Types for referring to remote objects in Remote GHCi.  For more
-- details, see Note [External GHCi pointers] in compiler/GHC/Runtime/Interpreter.hs
--
-- For details on Remote GHCi, see Note [Remote GHCi] in
-- compiler/GHC/Runtime/Interpreter.hs.
--
module GHCi.RemoteTypes
  ( RemotePtr(..), toRemotePtr, fromRemotePtr, castRemotePtr
  , HValue(..)
  , RemoteRef, mkRemoteRef, localRef, freeRemoteRef
  , HValueRef, toHValueRef
  , ForeignRef, mkForeignRef, withForeignRef
  , ForeignHValue
  , unsafeForeignRefToRemoteRef, finalizeForeignRef
  ) where

import Prelude -- See note [Why do we import Prelude here?]
import Control.DeepSeq
import Data.Word
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import Data.Binary
import Unsafe.Coerce
import GHC.Exts
import GHC.ForeignPtr

-- -----------------------------------------------------------------------------
-- RemotePtr

-- Static pointers only; don't use this for heap-resident pointers.
-- Instead use HValueRef. We will fix the remote pointer to be 64 bits. This
-- should cover 64 and 32bit systems, and permits the exchange of remote ptrs
-- between machines of different word size. For example, when connecting to
-- an iserv instance on a different architecture with different word size via
-- -fexternal-interpreter.
newtype RemotePtr a = RemotePtr Word64

toRemotePtr :: Ptr a -> RemotePtr a
toRemotePtr p = RemotePtr (fromIntegral (ptrToWordPtr p))

fromRemotePtr :: RemotePtr a -> Ptr a
fromRemotePtr (RemotePtr p) = wordPtrToPtr (fromIntegral p)

castRemotePtr :: RemotePtr a -> RemotePtr b
castRemotePtr (RemotePtr a) = RemotePtr a

deriving instance Show (RemotePtr a)
deriving instance Binary (RemotePtr a)
deriving instance NFData (RemotePtr a)

-- -----------------------------------------------------------------------------
-- HValueRef

newtype HValue = HValue Any

instance Show HValue where
  show _ = "<HValue>"

-- | A reference to a remote value.  These are allocated and freed explicitly.
newtype RemoteRef a = RemoteRef (RemotePtr ())
  deriving (Show, Binary)

-- We can discard type information if we want
toHValueRef :: RemoteRef a -> RemoteRef HValue
toHValueRef = unsafeCoerce

-- For convenience
type HValueRef = RemoteRef HValue

-- | Make a reference to a local value that we can send remotely.
-- This reference will keep the value that it refers to alive until
-- 'freeRemoteRef' is called.
mkRemoteRef :: a -> IO (RemoteRef a)
mkRemoteRef a = do
  sp <- newStablePtr a
  return $! RemoteRef (toRemotePtr (castStablePtrToPtr sp))

-- | Convert an HValueRef to an HValue.  Should only be used if the HValue
-- originated in this process.
localRef :: RemoteRef a -> IO a
localRef (RemoteRef w) =
  deRefStablePtr (castPtrToStablePtr (fromRemotePtr w))

-- | Release an HValueRef that originated in this process
freeRemoteRef :: RemoteRef a -> IO ()
freeRemoteRef (RemoteRef w) =
  freeStablePtr (castPtrToStablePtr (fromRemotePtr w))

-- | An HValueRef with a finalizer
newtype ForeignRef a = ForeignRef (ForeignPtr ())

instance NFData (ForeignRef a) where
  rnf x = x `seq` ()

type ForeignHValue = ForeignRef HValue

-- | Create a 'ForeignRef' from a 'RemoteRef'.  The finalizer
-- should arrange to call 'freeHValueRef' on the 'HValueRef'.  (since
-- this function needs to be called in the process that created the
-- 'HValueRef', it cannot be called directly from the finalizer).
mkForeignRef :: RemoteRef a -> IO () -> IO (ForeignRef a)
mkForeignRef (RemoteRef hvref) finalizer =
  ForeignRef <$> newForeignPtr (fromRemotePtr hvref) finalizer

-- | Use a 'ForeignHValue'
withForeignRef :: ForeignRef a -> (RemoteRef a -> IO b) -> IO b
withForeignRef (ForeignRef fp) f =
   withForeignPtr fp (f . RemoteRef . toRemotePtr)

unsafeForeignRefToRemoteRef :: ForeignRef a -> RemoteRef a
unsafeForeignRefToRemoteRef (ForeignRef fp) =
  RemoteRef (toRemotePtr (unsafeForeignPtrToPtr fp))

finalizeForeignRef :: ForeignRef a -> IO ()
finalizeForeignRef (ForeignRef fp) = finalizeForeignPtr fp
