-- |
-- Module      : Crypto.PubKey.RSA.PSS
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
module Crypto.PubKey.RSA.PSS
    ( PSSParams(..)
    , defaultPSSParams
    , defaultPSSParamsSHA1
    -- * Sign and verify functions
    , signWithSalt
    , signDigestWithSalt
    , sign
    , signDigest
    , signSafer
    , signDigestSafer
    , verify
    , verifyDigest
    ) where

import           Crypto.Random.Types
import           Crypto.PubKey.RSA.Types
import           Crypto.PubKey.RSA.Prim
import           Crypto.PubKey.RSA (generateBlinder)
import           Crypto.PubKey.MaskGenFunction
import           Crypto.Hash
import           Crypto.Number.Basic (numBits)
import           Data.Bits (xor, shiftR, (.&.))
import           Data.Word

import           Crypto.Internal.ByteArray (ByteArrayAccess, ByteArray)
import qualified Crypto.Internal.ByteArray as B (convert, eq)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

-- | Parameters for PSS signature/verification.
data PSSParams hash seed output = PSSParams
    { pssHash         :: hash             -- ^ Hash function to use
    , pssMaskGenAlg   :: MaskGenAlgorithm seed output -- ^ Mask Gen algorithm to use
    , pssSaltLength   :: Int              -- ^ Length of salt. need to be <= to hLen.
    , pssTrailerField :: Word8            -- ^ Trailer field, usually 0xbc
    }

-- | Default Params with a specified hash function
defaultPSSParams :: (ByteArrayAccess seed, ByteArray output, HashAlgorithm hash)
                 => hash
                 -> PSSParams hash seed output
defaultPSSParams hashAlg =
    PSSParams { pssHash         = hashAlg
              , pssMaskGenAlg   = mgf1 hashAlg
              , pssSaltLength   = hashDigestSize hashAlg
              , pssTrailerField = 0xbc
              }

-- | Default Params using SHA1 algorithm.
defaultPSSParamsSHA1 :: PSSParams SHA1 ByteString ByteString
defaultPSSParamsSHA1 = defaultPSSParams SHA1

-- | Sign using the PSS parameters and the salt explicitely passed as parameters.
--
-- the function ignore SaltLength from the PSS Parameters
signDigestWithSalt :: HashAlgorithm hash
                   => ByteString    -- ^ Salt to use
                   -> Maybe Blinder -- ^ optional blinder to use
                   -> PSSParams hash ByteString ByteString -- ^ PSS Parameters to use
                   -> PrivateKey    -- ^ RSA Private Key
                   -> Digest hash   -- ^ Message digest
                   -> Either Error ByteString
signDigestWithSalt salt blinder params pk digest
    | emLen < hashLen + saltLen + 2 = Left InvalidParameters
    | otherwise                     = Right $ dp blinder pk em
    where k        = private_size pk
          emLen    = if emTruncate pubBits then k - 1 else k
          mHash    = B.convert digest
          dbLen    = emLen - hashLen - 1
          saltLen  = B.length salt
          hashLen  = hashDigestSize (pssHash params)
          pubBits  = numBits (private_n pk)
          m'       = B.concat [B.replicate 8 0,mHash,salt]
          h        = B.convert $ hashWith (pssHash params) m'
          db       = B.concat [B.replicate (dbLen - saltLen - 1) 0,B.singleton 1,salt]
          dbmask   = pssMaskGenAlg params h dbLen
          maskedDB = B.pack $ normalizeToKeySize pubBits $ B.zipWith xor db dbmask
          em       = B.concat [maskedDB, h, B.singleton (pssTrailerField params)]

-- | Sign using the PSS parameters and the salt explicitely passed as parameters.
--
-- the function ignore SaltLength from the PSS Parameters
signWithSalt :: HashAlgorithm hash
             => ByteString    -- ^ Salt to use
             -> Maybe Blinder -- ^ optional blinder to use
             -> PSSParams hash ByteString ByteString -- ^ PSS Parameters to use
             -> PrivateKey    -- ^ RSA Private Key
             -> ByteString    -- ^ Message to sign
             -> Either Error ByteString
signWithSalt salt blinder params pk m = signDigestWithSalt salt blinder params pk mHash
    where mHash    = hashWith (pssHash params) m

-- | Sign using the PSS Parameters
sign :: (HashAlgorithm hash, MonadRandom m)
     => Maybe Blinder   -- ^ optional blinder to use
     -> PSSParams hash ByteString ByteString -- ^ PSS Parameters to use
     -> PrivateKey      -- ^ RSA Private Key
     -> ByteString      -- ^ Message to sign
     -> m (Either Error ByteString)
sign blinder params pk m = do
    salt <- getRandomBytes (pssSaltLength params)
    return (signWithSalt salt blinder params pk m)

-- | Sign using the PSS Parameters
signDigest :: (HashAlgorithm hash, MonadRandom m)
           => Maybe Blinder   -- ^ optional blinder to use
           -> PSSParams hash ByteString ByteString -- ^ PSS Parameters to use
           -> PrivateKey      -- ^ RSA Private Key
           -> Digest hash     -- ^ Message digest
           -> m (Either Error ByteString)
signDigest blinder params pk digest = do
    salt <- getRandomBytes (pssSaltLength params)
    return (signDigestWithSalt salt blinder params pk digest)

-- | Sign using the PSS Parameters and an automatically generated blinder.
signSafer :: (HashAlgorithm hash, MonadRandom m)
          => PSSParams hash ByteString ByteString -- ^ PSS Parameters to use
          -> PrivateKey     -- ^ private key
          -> ByteString     -- ^ message to sign
          -> m (Either Error ByteString)
signSafer params pk m = do
    blinder <- generateBlinder (private_n pk)
    sign (Just blinder) params pk m

-- | Sign using the PSS Parameters and an automatically generated blinder.
signDigestSafer :: (HashAlgorithm hash, MonadRandom m)
                => PSSParams hash ByteString ByteString -- ^ PSS Parameters to use
                -> PrivateKey     -- ^ private key
                -> Digest hash    -- ^ message digst
                -> m (Either Error ByteString)
signDigestSafer params pk digest = do
    blinder <- generateBlinder (private_n pk)
    signDigest (Just blinder) params pk digest

-- | Verify a signature using the PSS Parameters
verify :: HashAlgorithm hash
       => PSSParams hash ByteString ByteString
                     -- ^ PSS Parameters to use to verify,
                     --   this need to be identical to the parameters when signing
       -> PublicKey  -- ^ RSA Public Key
       -> ByteString -- ^ Message to verify
       -> ByteString -- ^ Signature
       -> Bool
verify params pk m = verifyDigest params pk mHash
  where mHash     = hashWith (pssHash params) m

-- | Verify a signature using the PSS Parameters
verifyDigest :: HashAlgorithm hash
             => PSSParams hash ByteString ByteString
                            -- ^ PSS Parameters to use to verify,
                            --   this need to be identical to the parameters when signing
             -> PublicKey   -- ^ RSA Public Key
             -> Digest hash -- ^ Digest to verify
             -> ByteString  -- ^ Signature
             -> Bool
verifyDigest params pk digest s
    | B.length s /= k                     = False
    | B.any (/= 0) pre                    = False
    | B.last em /= pssTrailerField params = False
    | B.any (/= 0) ps0                    = False
    | b1 /= B.singleton 1                 = False
    | otherwise                           = B.eq h h'
        where -- parameters
              hashLen   = hashDigestSize (pssHash params)
              mHash     = B.convert digest
              k         = public_size pk
              emLen     = if emTruncate pubBits then k - 1 else k
              dbLen     = emLen - hashLen - 1
              pubBits   = numBits (public_n pk)
              -- unmarshall fields
              (pre, em) = B.splitAt (k - emLen) (ep pk s) -- drop 0..1 byte
              maskedDB  = B.take dbLen em
              h         = B.take hashLen $ B.drop (B.length maskedDB) em
              dbmask    = pssMaskGenAlg params h dbLen
              db        = B.pack $ normalizeToKeySize pubBits $ B.zipWith xor maskedDB dbmask
              (ps0,z)   = B.break (== 1) db
              (b1,salt) = B.splitAt 1 z
              m'        = B.concat [B.replicate 8 0,mHash,salt]
              h'        = hashWith (pssHash params) m'

-- When the modulus has bit length 1 modulo 8 we drop the first byte.
emTruncate :: Int -> Bool
emTruncate bits = ((bits-1) .&. 0x7) == 0

normalizeToKeySize :: Int -> [Word8] -> [Word8]
normalizeToKeySize _    []     = [] -- very unlikely
normalizeToKeySize bits (x:xs) = x .&. mask : xs
    where mask = if sh > 0 then 0xff `shiftR` (8-sh) else 0xff
          sh   = (bits-1) .&. 0x7

