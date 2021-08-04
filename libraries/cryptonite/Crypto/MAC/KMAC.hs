-- |
-- Module      : Crypto.MAC.KMAC
-- License     : BSD-style
-- Maintainer  : Olivier Chéron <olivier.cheron@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
-- Provide the KMAC (Keccak Message Authentication Code) algorithm, derived from
-- the SHA-3 base algorithm Keccak and defined in NIST SP800-185.
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.MAC.KMAC
    ( HashSHAKE
    , kmac
    , KMAC(..)
    -- * Incremental
    , Context
    , initialize
    , update
    , updates
    , finalize
    ) where

import qualified Crypto.Hash as H
import           Crypto.Hash.SHAKE (HashSHAKE(..))
import           Crypto.Hash.Types (HashAlgorithm(..), Digest(..))
import qualified Crypto.Hash.Types as H
import           Crypto.Internal.Builder
import           Crypto.Internal.Imports
import           Foreign.Ptr (Ptr)
import           Data.Bits (shiftR)
import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray as B


-- cSHAKE

cshakeInit :: forall a name string prefix . (HashSHAKE a, ByteArrayAccess name, ByteArrayAccess string, ByteArrayAccess prefix)
           => name -> string -> prefix -> H.Context a
cshakeInit n s p = H.Context $ B.allocAndFreeze c $ \(ptr :: Ptr (H.Context a)) -> do
    hashInternalInit ptr
    B.withByteArray b $ \d -> hashInternalUpdate ptr d (fromIntegral $ B.length b)
    B.withByteArray p $ \d -> hashInternalUpdate ptr d (fromIntegral $ B.length p)
  where
    c = hashInternalContextSize (undefined :: a)
    w = hashBlockSize (undefined :: a)
    x = encodeString n <> encodeString s
    b = buildAndFreeze (bytepad x w) :: B.Bytes

cshakeUpdate :: (HashSHAKE a, ByteArrayAccess ba)
             => H.Context a -> ba -> H.Context a
cshakeUpdate = H.hashUpdate

cshakeUpdates :: (HashSHAKE a, ByteArrayAccess ba)
              => H.Context a -> [ba] -> H.Context a
cshakeUpdates = H.hashUpdates

cshakeFinalize :: forall a suffix . (HashSHAKE a, ByteArrayAccess suffix)
               => H.Context a -> suffix -> Digest a
cshakeFinalize !c s =
    Digest $ B.allocAndFreeze (hashDigestSize (undefined :: a)) $ \dig -> do
        ((!_) :: B.Bytes) <- B.copy c $ \(ctx :: Ptr (H.Context a)) -> do
            B.withByteArray s $ \d ->
                hashInternalUpdate ctx d (fromIntegral $ B.length s)
            cshakeInternalFinalize ctx dig
        return ()


-- KMAC

-- | Represent a KMAC that is a phantom type with the hash used to produce the
-- mac.
--
-- The Eq instance is constant time.  No Show instance is provided, to avoid
-- printing by mistake.
newtype KMAC a = KMAC { kmacGetDigest :: Digest a }
    deriving (ByteArrayAccess,NFData)

instance Eq (KMAC a) where
    (KMAC b1) == (KMAC b2) = B.constEq b1 b2

-- | Compute a KMAC using the supplied customization string and key.
kmac :: (HashSHAKE a, ByteArrayAccess string, ByteArrayAccess key, ByteArrayAccess ba)
     => string -> key -> ba -> KMAC a
kmac str key msg = finalize $ updates (initialize str key) [msg]

-- | Represent an ongoing KMAC state, that can be appended with 'update' and
-- finalized to a 'KMAC' with 'finalize'.
newtype Context a = Context (H.Context a)

-- | Initialize a new incremental KMAC context with the supplied customization
-- string and key.
initialize :: forall a string key . (HashSHAKE a, ByteArrayAccess string, ByteArrayAccess key)
           => string -> key -> Context a
initialize str key = Context $ cshakeInit n str p
  where
    n = B.pack [75,77,65,67] :: B.Bytes  -- "KMAC"
    w = hashBlockSize (undefined :: a)
    p = buildAndFreeze (bytepad (encodeString key) w) :: B.ScrubbedBytes

-- | Incrementally update a KMAC context.
update :: (HashSHAKE a, ByteArrayAccess ba) => Context a -> ba -> Context a
update (Context ctx) = Context . cshakeUpdate ctx

-- | Incrementally update a KMAC context with multiple inputs.
updates :: (HashSHAKE a, ByteArrayAccess ba) => Context a -> [ba] -> Context a
updates (Context ctx) = Context . cshakeUpdates ctx

-- | Finalize a KMAC context and return the KMAC.
finalize :: forall a . HashSHAKE a => Context a -> KMAC a
finalize (Context ctx) = KMAC $ cshakeFinalize ctx suffix
  where
    l = cshakeOutputLength (undefined :: a)
    suffix = buildAndFreeze (rightEncode l) :: B.Bytes


-- Utilities

bytepad :: Builder -> Int -> Builder
bytepad x w = prefix <> x <> zero padLen
  where
    prefix = leftEncode w
    padLen = (w - builderLength prefix - builderLength x) `mod` w

encodeString :: ByteArrayAccess bin => bin -> Builder
encodeString s = leftEncode (8 * B.length s) <> bytes s

leftEncode :: Int -> Builder
leftEncode x = byte len <> digits
  where
    digits = i2osp x
    len    = fromIntegral (builderLength digits)

rightEncode :: Int -> Builder
rightEncode x = digits <> byte len
  where
    digits = i2osp x
    len    = fromIntegral (builderLength digits)

i2osp :: Int -> Builder
i2osp i | i >= 256  = i2osp (shiftR i 8) <> byte (fromIntegral i)
        | otherwise = byte (fromIntegral i)
