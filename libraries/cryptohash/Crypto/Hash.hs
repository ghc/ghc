{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
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
module Crypto.Hash
    (
    -- * Types
      HashAlgorithm(..)
    , HashFunctionBS
    , HashFunctionLBS
    , Context
    , Digest
    -- * Functions
    , digestToByteString
    , digestToHexByteString
    , hash
    , hashlazy
    , hashUpdate
    , hashInitAlg
    -- * hash algorithms
    , H.MD2(..)
    , H.MD4(..)
    , H.MD5(..)
    , H.SHA1(..)
    , H.SHA224(..)
    , H.SHA256(..)
    , H.SHA384(..)
    , H.SHA512(..)
    , H.RIPEMD160(..)
    , H.Tiger(..)
    , H.SHA3_224(..)
    , H.SHA3_256(..)
    , H.SHA3_384(..)
    , H.SHA3_512(..)
    , H.Skein256_224(..)
    , H.Skein256_256(..)
    , H.Skein512_224(..)
    , H.Skein512_256(..)
    , H.Skein512_384(..)
    , H.Skein512_512(..)
    , H.Whirlpool(..)
    -- * MAC algorithms
    , HMAC(..)
    , hmac
    , hmacAlg
    ) where

import Crypto.Hash.Types
import Data.ByteString (ByteString)
import Data.Byteable
import Data.Bits (xor)
import qualified Data.ByteString as B
import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteString.Lazy as L

import qualified "cryptonite" Crypto.Hash as H

-- | Alias to a single pass hash function that operate on a strict bytestring
type HashFunctionBS a = ByteString -> Digest a

-- | Alias to a single pass hash function that operate on a lazy bytestring
type HashFunctionLBS a = L.ByteString -> Digest a

-- | run hashUpdates on one single bytestring and return the updated context.
hashUpdate :: HashAlgorithm a => Context a -> ByteString -> Context a
hashUpdate ctx b = hashUpdates ctx [b]

-- | Hash a strict bytestring into a digest.
hash :: HashAlgorithm a => ByteString -> Digest a
hash bs = hashFinalize $ hashUpdate hashInit bs

-- | Hash a lazy bytestring into a digest.
hashlazy :: HashAlgorithm a => L.ByteString -> Digest a
hashlazy lbs = hashFinalize $ hashUpdates hashInit (L.toChunks lbs)

-- | Return the hexadecimal (base16) bytestring of the digest
digestToHexByteString :: Digest a -> ByteString
digestToHexByteString = B.convertToBase B.Base16 . toBytes

-- | Class representing hashing algorithms.
--
-- The hash algorithm is built over 3 primitives:
--
-- * init     : create a new hashing context
--
-- * updates  : update the hashing context with some strict bytestrings
--              and return the new context
--
-- * finalize : finalize the context into a digest
--
class HashAlgorithm a where
    -- | Block size in bytes the hash algorithm operates on
    hashBlockSize :: Context a -> Int

    -- | Initialize a new context for this hash algorithm
    hashInit     :: Context a

    -- | Update the context with a list of strict bytestring,
    -- and return a new context with the updates.
    hashUpdates  :: Context a -> [ByteString] -> Context a

    -- | Finalize a context and return a digest.
    hashFinalize :: Context a -> Digest a

    -- | Try to convert a binary digest bytestring to a digest.
    digestFromByteString :: ByteString -> Maybe (Digest a)


#define DEFINE_INSTANCE(NAME, MODULENAME, BLOCKSIZE) \
instance HashAlgorithm H.NAME where \
    { hashInit = Context $ H.hashInit \
    ; hashBlockSize ~(Context _) = BLOCKSIZE \
    ; hashUpdates (Context c) bs = Context $ H.hashUpdates c bs \
    ; hashFinalize (Context c) = Digest $ H.hashFinalize c \
    ; digestFromByteString bs = Digest `fmap` H.digestFromByteString bs \
    };

#define DEFINE_INSTANCE_LEN(NAME, MODULENAME, LEN, BLOCKSIZE) \
instance HashAlgorithm H.NAME where \
    { hashInit = Context $ H.hashInit \
    ; hashBlockSize ~(Context _) = BLOCKSIZE \
    ; hashUpdates (Context c) bs = Context $ H.hashUpdates c bs \
    ; hashFinalize (Context c) = Digest $ H.hashFinalize c \
    ; digestFromByteString bs = Digest `fmap` H.digestFromByteString bs \
    };

-- | MD2 cryptographic hash
DEFINE_INSTANCE(MD2, MD2, 16)
-- | MD4 cryptographic hash
DEFINE_INSTANCE(MD4, MD4, 64)
-- | MD5 cryptographic hash
DEFINE_INSTANCE(MD5, MD5, 64)
-- | SHA1 cryptographic hash
DEFINE_INSTANCE(SHA1, SHA1, 64)
-- | SHA224 cryptographic hash
DEFINE_INSTANCE(SHA224, SHA224, 64)
-- | SHA256 cryptographic hash
DEFINE_INSTANCE(SHA256, SHA256, 64)
-- | SHA384 cryptographic hash
DEFINE_INSTANCE(SHA384, SHA384, 128)
-- | SHA512 cryptographic hash
DEFINE_INSTANCE(SHA512, SHA512, 128)

-- | RIPEMD160 cryptographic hash
DEFINE_INSTANCE(RIPEMD160, RIPEMD160, 64)
-- | Whirlpool cryptographic hash
DEFINE_INSTANCE(Whirlpool, Whirlpool, 64)
-- | Tiger cryptographic hash
DEFINE_INSTANCE(Tiger, Tiger, 64)

-- | SHA3 (224 bits version) cryptographic hash
DEFINE_INSTANCE_LEN(SHA3_224, SHA3, 224, 144)
-- | SHA3 (256 bits version) cryptographic hash
DEFINE_INSTANCE_LEN(SHA3_256, SHA3, 256, 136)
-- | SHA3 (384 bits version) cryptographic hash
DEFINE_INSTANCE_LEN(SHA3_384, SHA3, 384, 104)
-- | SHA3 (512 bits version) cryptographic hash
DEFINE_INSTANCE_LEN(SHA3_512, SHA3, 512, 72)

-- | Skein256 (224 bits version) cryptographic hash
DEFINE_INSTANCE_LEN(Skein256_224, Skein256, 224, 32)
-- | Skein256 (256 bits version) cryptographic hash
DEFINE_INSTANCE_LEN(Skein256_256, Skein256, 256, 32)

-- | Skein512 (224 bits version) cryptographic hash
DEFINE_INSTANCE_LEN(Skein512_224, Skein512, 224, 64)
-- | Skein512 (256 bits version) cryptographic hash
DEFINE_INSTANCE_LEN(Skein512_256, Skein512, 256, 64)
-- | Skein512 (384 bits version) cryptographic hash
DEFINE_INSTANCE_LEN(Skein512_384, Skein512, 384, 64)
-- | Skein512 (512 bits version) cryptographic hash
DEFINE_INSTANCE_LEN(Skein512_512, Skein512, 512, 64)

-- | Initialize a new context for a specified hash algorithm
hashInitAlg :: HashAlgorithm alg => alg -> Context alg
hashInitAlg _ = hashInit

-- | Represent an HMAC that is a phantom type with the hash used to produce the mac.
--
-- The Eq instance is constant time.
newtype HMAC a = HMAC { hmacGetDigest :: Digest a }

instance Byteable (HMAC a) where
    toBytes (HMAC b) = toBytes b

instance Eq (HMAC a) where
    (HMAC b1) == (HMAC b2) = constEqBytes (toBytes b1) (toBytes b2)

-- | compute a MAC using the supplied hashing function
hmac :: HashAlgorithm a
     => ByteString       -- ^ Secret key
     -> ByteString       -- ^ Message to MAC
     -> HMAC a
hmac secret msg = doHMAC hashInit
  where doHMAC :: HashAlgorithm a => Context a -> HMAC a
        doHMAC ctxInit = HMAC $ hashF $ B.append opad (toBytes $ hashF $ B.append ipad msg)
          where opad = B.map (xor 0x5c) k'
                ipad = B.map (xor 0x36) k'

                k'  = B.append kt pad
                kt  = if B.length secret > fromIntegral blockSize then toBytes (hashF secret) else secret
                pad = B.replicate (fromIntegral blockSize - B.length kt) 0
                hashF = hashFinalize . hashUpdate ctxInit
                blockSize = hashBlockSize ctxInit

-- | compute a HMAC using a specified algorithm
hmacAlg :: HashAlgorithm a
        => a           -- ^ the hash algorithm the actual value is unused.
        -> ByteString  -- ^ Secret key
        -> ByteString  -- ^ Message to MAC
        -> HMAC a
hmacAlg _ secret msg = hmac secret msg
