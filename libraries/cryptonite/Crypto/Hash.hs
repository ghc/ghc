-- |
-- Module      : Crypto.Hash
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Generalized cryptographic hash interface, that you can use with cryptographic hash
-- algorithm that belong to the HashAlgorithm type class.
--
-- > import Crypto.Hash
-- >
-- > sha1 :: ByteString -> Digest SHA1
-- > sha1 = hash
-- >
-- > hexSha3_512 :: ByteString -> String
-- > hexSha3_512 bs = show (hash bs :: Digest SHA3_512)
--
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns        #-}
module Crypto.Hash
    (
    -- * Types
      Context
    , Digest
    -- * Functions
    , digestFromByteString
    -- * Hash methods parametrized by algorithm
    , hashInitWith
    , hashWith
    , hashPrefixWith
    -- * Hash methods
    , hashInit
    , hashUpdates
    , hashUpdate
    , hashFinalize
    , hashFinalizePrefix
    , hashBlockSize
    , hashDigestSize
    , hash
    , hashPrefix
    , hashlazy
    -- * Hash algorithms
    , module Crypto.Hash.Algorithms
    ) where

import           Basement.Types.OffsetSize (CountOf (..))
import           Basement.Block (Block, unsafeFreeze)
import           Basement.Block.Mutable (copyFromPtr, new)
import           Crypto.Internal.Compat (unsafeDoIO)
import           Crypto.Hash.Types
import           Crypto.Hash.Algorithms
import           Foreign.Ptr (Ptr, plusPtr)
import           Crypto.Internal.ByteArray (ByteArrayAccess)
import qualified Crypto.Internal.ByteArray as B
import qualified Data.ByteString.Lazy as L
import           Data.Word (Word8)
import           Data.Int (Int32)

-- | Hash a strict bytestring into a digest.
hash :: (ByteArrayAccess ba, HashAlgorithm a) => ba -> Digest a
hash bs = hashFinalize $ hashUpdate hashInit bs

-- | Hash the first N bytes of a bytestring, with code path independent from N.
hashPrefix :: (ByteArrayAccess ba, HashAlgorithmPrefix a) => ba -> Int -> Digest a
hashPrefix = hashFinalizePrefix hashInit

-- | Hash a lazy bytestring into a digest.
hashlazy :: HashAlgorithm a => L.ByteString -> Digest a
hashlazy lbs = hashFinalize $ hashUpdates hashInit (L.toChunks lbs)

-- | Initialize a new context for this hash algorithm
hashInit :: forall a . HashAlgorithm a => Context a
hashInit = Context $ B.allocAndFreeze (hashInternalContextSize (undefined :: a)) $ \(ptr :: Ptr (Context a)) ->
    hashInternalInit ptr

-- | run hashUpdates on one single bytestring and return the updated context.
hashUpdate :: (ByteArrayAccess ba, HashAlgorithm a) => Context a -> ba -> Context a
hashUpdate ctx b
    | B.null b  = ctx
    | otherwise = hashUpdates ctx [b]

-- | Update the context with a list of strict bytestring,
-- and return a new context with the updates.
hashUpdates :: forall a ba . (HashAlgorithm a, ByteArrayAccess ba)
            => Context a
            -> [ba]
            -> Context a
hashUpdates c l
    | null ls   = c
    | otherwise = Context $ B.copyAndFreeze c $ \(ctx :: Ptr (Context a)) ->
        mapM_ (\b -> B.withByteArray b (processBlocks ctx (B.length b))) ls
  where
    ls = filter (not . B.null) l
    -- process the data in 2GB chunks to fit in uint32_t and Int on 32 bit systems
    processBlocks ctx bytesLeft dataPtr
        | bytesLeft == 0 = return ()
        | otherwise = do
            hashInternalUpdate ctx dataPtr (fromIntegral actuallyProcessed)
            processBlocks ctx (bytesLeft - actuallyProcessed) (dataPtr `plusPtr` actuallyProcessed)
        where
            actuallyProcessed = min bytesLeft (fromIntegral (maxBound :: Int32))

-- | Finalize a context and return a digest.
hashFinalize :: forall a . HashAlgorithm a
             => Context a
             -> Digest a
hashFinalize !c =
    Digest $ B.allocAndFreeze (hashDigestSize (undefined :: a)) $ \(dig :: Ptr (Digest a)) -> do
        ((!_) :: B.Bytes) <- B.copy c $ \(ctx :: Ptr (Context a)) -> hashInternalFinalize ctx dig
        return ()

-- | Update the context with the first N bytes of a bytestring and return the
-- digest.  The code path is independent from N but much slower than a normal
-- 'hashUpdate'.  The function can be called for the last bytes of a message, in
-- order to exclude a variable padding, without leaking the padding length.  The
-- begining of the message, never impacted by the padding, should preferably go
-- through 'hashUpdate' for better performance.
hashFinalizePrefix :: forall a ba . (HashAlgorithmPrefix a, ByteArrayAccess ba)
                   => Context a
                   -> ba
                   -> Int
                   -> Digest a
hashFinalizePrefix !c b len =
    Digest $ B.allocAndFreeze (hashDigestSize (undefined :: a)) $ \(dig :: Ptr (Digest a)) -> do
        ((!_) :: B.Bytes) <- B.copy c $ \(ctx :: Ptr (Context a)) ->
            B.withByteArray b $ \d ->
                hashInternalFinalizePrefix ctx d (fromIntegral $ B.length b) (fromIntegral len) dig
        return ()

-- | Initialize a new context for a specified hash algorithm
hashInitWith :: HashAlgorithm alg => alg -> Context alg
hashInitWith _ = hashInit

-- | Run the 'hash' function but takes an explicit hash algorithm parameter
hashWith :: (ByteArrayAccess ba, HashAlgorithm alg) => alg -> ba -> Digest alg
hashWith _ = hash

-- | Run the 'hashPrefix' function but takes an explicit hash algorithm parameter
hashPrefixWith :: (ByteArrayAccess ba, HashAlgorithmPrefix alg) => alg -> ba -> Int -> Digest alg
hashPrefixWith _ = hashPrefix

-- | Try to transform a bytearray into a Digest of specific algorithm.
--
-- If the digest is not the right size for the algorithm specified, then
-- Nothing is returned.
digestFromByteString :: forall a ba . (HashAlgorithm a, ByteArrayAccess ba) => ba -> Maybe (Digest a)
digestFromByteString = from undefined
  where
        from :: a -> ba -> Maybe (Digest a)
        from alg bs
            | B.length bs == (hashDigestSize alg) = Just $ Digest $ unsafeDoIO $ copyBytes bs
            | otherwise                           = Nothing

        copyBytes :: ba -> IO (Block Word8)
        copyBytes ba = do
            muArray <- new count
            B.withByteArray ba $ \ptr -> copyFromPtr ptr muArray 0 count
            unsafeFreeze muArray
          where
            count = CountOf (B.length ba)
