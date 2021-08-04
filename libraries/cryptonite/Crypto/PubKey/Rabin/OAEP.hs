-- |
-- Module      : Crypto.PubKey.Rabin.OAEP
-- License     : BSD-style
-- Maintainer  : Carlos Rodriguez-Vega <crodveg@yahoo.es>
-- Stability   : experimental
-- Portability : unknown
--
-- OAEP padding scheme.
-- See <http://en.wikipedia.org/wiki/Optimal_asymmetric_encryption_padding>.
--
module Crypto.PubKey.Rabin.OAEP
    ( OAEPParams(..)
    , defaultOAEPParams
    , pad
    , unpad
    ) where
        
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Bits (xor)

import           Crypto.Hash
import           Crypto.Internal.ByteArray (ByteArrayAccess, ByteArray)
import qualified Crypto.Internal.ByteArray as B (convert)
import           Crypto.PubKey.MaskGenFunction
import           Crypto.PubKey.Internal (and')
import           Crypto.PubKey.Rabin.Types

-- | Parameters for OAEP padding.
data OAEPParams hash seed output = OAEPParams
    { oaepHash       :: hash                            -- ^ hash function to use
    , oaepMaskGenAlg :: MaskGenAlgorithm seed output    -- ^ mask Gen algorithm to use
    , oaepLabel      :: Maybe ByteString                -- ^ optional label prepended to message
    }

-- | Default Params with a specified hash function.
defaultOAEPParams :: (ByteArrayAccess seed, ByteArray output, HashAlgorithm hash)
                  => hash
                  -> OAEPParams hash seed output
defaultOAEPParams hashAlg =
    OAEPParams { oaepHash       = hashAlg
               , oaepMaskGenAlg = mgf1 hashAlg
               , oaepLabel      = Nothing
               }

-- | Pad a message using OAEP.
pad :: HashAlgorithm hash
    => ByteString                               -- ^ Seed
    -> OAEPParams hash ByteString ByteString    -- ^ OAEP params to use
    -> Int                                      -- ^ size of public key in bytes
    -> ByteString                               -- ^ Message pad
    -> Either Error ByteString
pad seed oaep k msg
    | k < 2*hashLen+2          = Left InvalidParameters
    | B.length seed /= hashLen = Left InvalidParameters
    | mLen > k - 2*hashLen-2   = Left MessageTooLong
    | otherwise                = Right em
    where -- parameters
        mLen       = B.length msg
        mgf        = oaepMaskGenAlg oaep
        labelHash  = hashWith (oaepHash oaep) (maybe B.empty id $ oaepLabel oaep)
        hashLen    = hashDigestSize (oaepHash oaep)
        -- put fields
        ps         = B.replicate (k - mLen - 2*hashLen - 2) 0
        db         = B.concat [B.convert labelHash, ps, B.singleton 0x1, msg]
        dbmask     = mgf seed (k - hashLen - 1)
        maskedDB   = B.pack $ B.zipWith xor db dbmask
        seedMask   = mgf maskedDB hashLen
        maskedSeed = B.pack $ B.zipWith xor seed seedMask
        em         = B.concat [B.singleton 0x0, maskedSeed, maskedDB]

-- | Un-pad a OAEP encoded message.
unpad :: HashAlgorithm hash
      => OAEPParams hash ByteString ByteString  -- ^ OAEP params to use
      -> Int                                    -- ^ size of public key in bytes
      -> ByteString                             -- ^ encoded message (not encrypted)
      -> Either Error ByteString
unpad oaep k em
    | paddingSuccess = Right msg
    | otherwise      = Left MessageNotRecognized
    where -- parameters
        mgf        = oaepMaskGenAlg oaep
        labelHash  = B.convert $ hashWith (oaepHash oaep) (maybe B.empty id $ oaepLabel oaep)
        hashLen    = hashDigestSize (oaepHash oaep)
        -- getting em's fields
        (pb, em0)  = B.splitAt 1 em
        (maskedSeed, maskedDB) = B.splitAt hashLen em0
        seedMask   = mgf maskedDB hashLen
        seed       = B.pack $ B.zipWith xor maskedSeed seedMask
        dbmask     = mgf seed (k - hashLen - 1)
        db         = B.pack $ B.zipWith xor maskedDB dbmask
        -- getting db's fields
        (labelHash', db1) = B.splitAt hashLen db
        (_, db2)   = B.break (/= 0) db1
        (ps1, msg) = B.splitAt 1 db2

        paddingSuccess = and' [ labelHash' == labelHash -- no need for constant eq
                              , ps1        == B.replicate 1 0x1
                              , pb         == B.replicate 1 0x0
                              ]
