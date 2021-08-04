-- |
-- Module      : Data.ByteArray.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.ByteArray.Types
    ( ByteArrayAccess(..)
    , ByteArray(..)
    ) where

import           Foreign.Ptr
import           Data.Monoid

#ifdef WITH_BYTESTRING_SUPPORT
import qualified Data.ByteString as Bytestring (length)
import qualified Data.ByteString.Internal as Bytestring
import           Foreign.ForeignPtr (withForeignPtr)
#endif

import           Data.Memory.PtrMethods (memCopy)


#ifdef WITH_BASEMENT_SUPPORT

import           Data.Proxy (Proxy(..))
import           Data.Word (Word8)

import qualified Basement.Types.OffsetSize as Base
import qualified Basement.UArray as Base
import qualified Basement.String as Base (String, toBytes, Encoding(UTF8))
import qualified Basement.PrimType as Base (primSizeInBytes)

import qualified Basement.UArray.Mutable as BaseMutable (withMutablePtrHint)
import qualified Basement.Block as Block
import qualified Basement.Block.Mutable as Block

import           Basement.Nat
import qualified Basement.Sized.Block as BlockN

#endif

import Prelude hiding (length)

-- | Class to Access size properties and data of a ByteArray
class ByteArrayAccess ba where
    -- | Return the length in bytes of a bytearray
    length        :: ba -> Int
    -- | Allow to use using a pointer
    withByteArray :: ba -> (Ptr p -> IO a) -> IO a
    -- | Copy the data of a bytearray to a ptr
    copyByteArrayToPtr :: ba -> Ptr p -> IO ()
    copyByteArrayToPtr a dst = withByteArray a $ \src -> memCopy (castPtr dst) src (length a)

-- | Class to allocate new ByteArray of specific size
class (Eq ba, Ord ba, Monoid ba, ByteArrayAccess ba) => ByteArray ba where
    -- | allocate `n` bytes and perform the given operation
    allocRet  :: Int
                -- ^ number of bytes to allocate. i.e. might not match the
                -- size of the given type `ba`.
              -> (Ptr p -> IO a)
              -> IO (a, ba)

#ifdef WITH_BYTESTRING_SUPPORT
instance ByteArrayAccess Bytestring.ByteString where
    length = Bytestring.length
    withByteArray (Bytestring.PS fptr off _) f = withForeignPtr fptr $ \ptr -> f $! (ptr `plusPtr` off)

instance ByteArray Bytestring.ByteString where
    allocRet sz f = do
        fptr <- Bytestring.mallocByteString sz
        r    <- withForeignPtr fptr (f . castPtr)
        return (r, Bytestring.PS fptr 0 sz)
#endif

#ifdef WITH_BASEMENT_SUPPORT

baseBlockRecastW8 :: Base.PrimType ty => Block.Block ty -> Block.Block Word8
baseBlockRecastW8 = Block.unsafeCast -- safe with Word8 destination

instance Base.PrimType ty => ByteArrayAccess (Block.Block ty) where
    length a = let Base.CountOf i = Block.length (baseBlockRecastW8 a) in i
    withByteArray a f = Block.withPtr (baseBlockRecastW8 a) (f . castPtr)
    copyByteArrayToPtr ba dst = do
        mb <- Block.unsafeThaw (baseBlockRecastW8 ba)
        Block.copyToPtr mb 0 (castPtr dst) (Block.length $ baseBlockRecastW8 ba)

instance (KnownNat n, Base.PrimType ty, Base.Countable ty n) => ByteArrayAccess (BlockN.BlockN n ty) where
    length a = let Base.CountOf i = BlockN.lengthBytes a in i
    withByteArray a f = BlockN.withPtr a (f . castPtr)
    copyByteArrayToPtr bna = copyByteArrayToPtr (BlockN.toBlock bna)

baseUarrayRecastW8 :: Base.PrimType ty => Base.UArray ty -> Base.UArray Word8
baseUarrayRecastW8 = Base.recast

instance Base.PrimType ty => ByteArrayAccess (Base.UArray ty) where
    length a = let Base.CountOf i = Base.length (baseUarrayRecastW8 a) in i
    withByteArray a f = Base.withPtr (baseUarrayRecastW8 a) (f . castPtr)
    copyByteArrayToPtr ba dst = Base.copyToPtr ba (castPtr dst)

instance ByteArrayAccess Base.String where
    length str = let Base.CountOf i = Base.length bytes in i
      where
        -- the Foundation's length return a number of elements not a number of
        -- bytes. For @ByteArrayAccess@, because we are using an @Int@, we
        -- didn't see that we were returning the wrong @CountOf@.
        bytes = Base.toBytes Base.UTF8 str
    withByteArray s f = withByteArray (Base.toBytes Base.UTF8 s) f

instance (Ord ty, Base.PrimType ty) => ByteArray (Block.Block ty) where
    allocRet sz f = do
        mba <- Block.new $ sizeRecastBytes sz Proxy
        a   <- Block.withMutablePtrHint True False mba (f . castPtr)
        ba  <- Block.unsafeFreeze mba
        return (a, ba)

instance (Ord ty, Base.PrimType ty) => ByteArray (Base.UArray ty) where
    allocRet sz f = do
        mba <- Base.new $ sizeRecastBytes sz Proxy
        a   <- BaseMutable.withMutablePtrHint True False mba (f . castPtr)
        ba  <- Base.unsafeFreeze mba
        return (a, ba)

sizeRecastBytes :: Base.PrimType ty => Int -> Proxy ty -> Base.CountOf ty
sizeRecastBytes w p = Base.CountOf $
    let (q,r) = w `Prelude.quotRem` szTy
     in q + (if r == 0 then 0 else 1)
  where !(Base.CountOf szTy) = Base.primSizeInBytes p
{-# INLINE [1] sizeRecastBytes #-}

#endif
