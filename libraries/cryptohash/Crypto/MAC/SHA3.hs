-- |
-- Module      : Crypto.MAC.SHA3
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- provide a simple SHA3 MAC mechanism with
--
-- > mac = hash(key|message)
--
module Crypto.MAC.SHA3
    ( MAC512(..)
    , MAC384(..)
    , MAC256(..)
    , MAC224(..)
    , mac512
    , mac384
    , mac256
    , mac224
    ) where

import Data.Byteable
import Data.ByteString (ByteString)
import Crypto.Hash

-- | SHA3_512 MAC
data MAC512 = MAC512 { getDigest512 :: Digest SHA3_512 }

instance Byteable MAC512 where
    toBytes (MAC512 b) = toBytes b
instance Eq MAC512 where
    (MAC512 b1) == (MAC512 b2) = constEqBytes (toBytes b1) (toBytes b2)

-- | SHA3_384 MAC
data MAC384 = MAC384 { getDigest384 :: Digest SHA3_384 }

instance Byteable MAC384 where
    toBytes (MAC384 b) = toBytes b
instance Eq MAC384 where
    (MAC384 b1) == (MAC384 b2) = constEqBytes (toBytes b1) (toBytes b2)

-- | SHA3_256 MAC
data MAC256 = MAC256 { getDigest256 :: Digest SHA3_256 }

instance Byteable MAC256 where
    toBytes (MAC256 b) = toBytes b
instance Eq MAC256 where
    (MAC256 b1) == (MAC256 b2) = constEqBytes (toBytes b1) (toBytes b2)

-- | SHA3_224 MAC
data MAC224 = MAC224 { getDigest224 :: Digest SHA3_224 }

instance Byteable MAC224 where
    toBytes (MAC224 b) = toBytes b
instance Eq MAC224 where
    (MAC224 b1) == (MAC224 b2) = constEqBytes (toBytes b1) (toBytes b2)

-- | compute a MAC using a simple SHA3_512 key|msg
mac512 :: ByteString -- ^ secret
       -> ByteString -- ^ message
       -> MAC512
mac512 secret msg = MAC512 $ hashFinalize $ hashUpdates hashInit [secret,msg]

-- | compute a MAC using a simple SHA3_384 key|msg
mac384 :: ByteString -- ^ secret
       -> ByteString -- ^ message
       -> MAC384
mac384 secret msg = MAC384 $ hashFinalize $ hashUpdates hashInit [secret,msg]

-- | compute a MAC using a simple SHA3_256 key|msg
mac256 :: ByteString -- ^ secret
       -> ByteString -- ^ message
       -> MAC256
mac256 secret msg = MAC256 $ hashFinalize $ hashUpdates hashInit [secret,msg]

-- | compute a MAC using a simple SHA3_224 key|msg
mac224 :: ByteString -- ^ secret
       -> ByteString -- ^ message
       -> MAC224
mac224 secret msg = MAC224 $ hashFinalize $ hashUpdates hashInit [secret,msg]
