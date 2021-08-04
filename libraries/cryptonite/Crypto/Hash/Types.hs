-- |
-- Module      : Crypto.Hash.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Crypto hash types definitions
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Crypto.Hash.Types
    ( HashAlgorithm(..)
    , HashAlgorithmPrefix(..)
    , Context(..)
    , Digest(..)
    ) where

import           Crypto.Internal.Imports
import           Crypto.Internal.ByteArray (ByteArrayAccess, Bytes)
import qualified Crypto.Internal.ByteArray as B
import           Control.Monad.ST
import           Data.Char (digitToInt, isHexDigit)
import           Foreign.Ptr (Ptr)
import           Basement.Block (Block, unsafeFreeze)
import           Basement.Block.Mutable (MutableBlock, new, unsafeWrite)
import           Basement.NormalForm (deepseq)
import           Basement.Types.OffsetSize (CountOf(..), Offset(..))
import           GHC.TypeLits (Nat)
import           Data.Data (Data)

-- | Class representing hashing algorithms.
--
-- The interface presented here is update in place
-- and lowlevel. the Hash module takes care of
-- hidding the mutable interface properly.
class HashAlgorithm a where
    -- | Associated type for the block size of the hash algorithm
    type HashBlockSize a :: Nat
    -- | Associated type for the digest size of the hash algorithm
    type HashDigestSize a :: Nat
    -- | Associated type for the internal context size of the hash algorithm
    type HashInternalContextSize a :: Nat

    -- | Get the block size of a hash algorithm
    hashBlockSize           :: a -> Int
    -- | Get the digest size of a hash algorithm
    hashDigestSize          :: a -> Int
    -- | Get the size of the context used for a hash algorithm
    hashInternalContextSize :: a -> Int
    --hashAlgorithmFromProxy  :: Proxy a -> a

    -- | Initialize a context pointer to the initial state of a hash algorithm
    hashInternalInit     :: Ptr (Context a) -> IO ()
    -- | Update the context with some raw data
    hashInternalUpdate   :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()
    -- | Finalize the context and set the digest raw memory to the right value
    hashInternalFinalize :: Ptr (Context a) -> Ptr (Digest a) -> IO ()

-- | Hashing algorithms with a constant-time implementation.
class HashAlgorithm a => HashAlgorithmPrefix a where
    -- | Update the context with the first N bytes of a buffer and finalize this
    -- context.  The code path executed is independent from N and depends only
    -- on the complete buffer length.
    hashInternalFinalizePrefix :: Ptr (Context a)
                               -> Ptr Word8 -> Word32
                               -> Word32
                               -> Ptr (Digest a)
                               -> IO ()

{-
hashContextGetAlgorithm :: HashAlgorithm a => Context a -> a
hashContextGetAlgorithm = undefined
-}

-- | Represent a context for a given hash algorithm.
--
-- This type is an instance of 'ByteArrayAccess' for debugging purpose. Internal
-- layout is architecture dependent, may contain uninitialized data fragments,
-- and change in future versions.  The bytearray should not be used as input to
-- cryptographic algorithms.
newtype Context a = Context Bytes
    deriving (ByteArrayAccess,NFData)

-- | Represent a digest for a given hash algorithm.
--
-- This type is an instance of 'ByteArrayAccess' from package
-- <https://hackage.haskell.org/package/memory memory>.
-- Module "Data.ByteArray" provides many primitives to work with those values
-- including conversion to other types.
--
-- Creating a digest from a bytearray is also possible with function
-- 'Crypto.Hash.digestFromByteString'.
newtype Digest a = Digest (Block Word8)
    deriving (Eq,Ord,ByteArrayAccess, Data)

instance NFData (Digest a) where
    rnf (Digest u) = u `deepseq` ()

instance Show (Digest a) where
    show (Digest bs) = map (toEnum . fromIntegral)
                     $ B.unpack (B.convertToBase B.Base16 bs :: Bytes)

instance HashAlgorithm a => Read (Digest a) where
    readsPrec _ str = runST $ do mut <- new (CountOf len)
                                 loop mut len str
      where
        len = hashDigestSize (undefined :: a)

        loop :: MutableBlock Word8 s -> Int -> String -> ST s [(Digest a, String)]
        loop mut 0   cs          = (\b -> [(Digest b, cs)]) <$> unsafeFreeze mut
        loop _   _   []          = return []
        loop _   _   [_]         = return []
        loop mut n   (c:(d:ds))
            | not (isHexDigit c) = return []
            | not (isHexDigit d) = return []
            | otherwise          = do
                let w8 = fromIntegral $ digitToInt c * 16 + digitToInt d
                unsafeWrite mut (Offset $ len - n) w8
                loop mut (n - 1) ds
