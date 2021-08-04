-- |
-- Module      : Crypto.KDF.Argon2
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Argon2 hashing function (P-H-C winner)
--
-- Recommended to use this module qualified
--
-- File started from Argon2.hs, from Oliver Charles
-- at https://github.com/ocharles/argon2
--
module Crypto.KDF.Argon2
    (
      Options(..)
    , TimeCost
    , MemoryCost
    , Parallelism
    , Variant(..)
    , Version(..)
    , defaultOptions
    -- * Hashing function
    , hash
    ) where

import           Crypto.Internal.ByteArray (ByteArray, ByteArrayAccess)
import qualified Crypto.Internal.ByteArray as B
import           Crypto.Error
import           Control.Monad (when)
import           Data.Word
import           Foreign.C
import           Foreign.Ptr

-- | Which variant of Argon2 to use. You should choose the variant that is most
-- applicable to your intention to hash inputs.
data Variant =
      Argon2d  -- ^ Argon2d is faster than Argon2i and uses data-depending memory access,
               -- which makes it suitable for cryptocurrencies and applications with no
               -- threats from side-channel timing attacks.
    | Argon2i  -- ^ Argon2i uses data-independent memory access, which is preferred
               -- for password hashing and password-based key derivation. Argon2i
               -- is slower as it makes more passes over the memory to protect from
               -- tradeoff attacks.
    | Argon2id -- ^ Argon2id is a hybrid of Argon2i and Argon2d, using a combination
               -- of data-depending and data-independent memory accesses, which gives
               -- some of Argon2i's resistance to side-channel cache timing attacks
               -- and much of Argon2d's resistance to GPU cracking attacks
    deriving (Eq,Ord,Read,Show,Enum,Bounded)

-- | Which version of Argon2 to use
data Version = Version10 | Version13
    deriving (Eq,Ord,Read,Show,Enum,Bounded)

-- | The time cost, which defines the amount of computation realized and therefore the execution time, given in number of iterations.
--
-- 'FFI.ARGON2_MIN_TIME' <= 'hashIterations' <= 'FFI.ARGON2_MAX_TIME'
type TimeCost = Word32

-- | The memory cost, which defines the memory usage, given in kibibytes.
--
-- max 'FFI.ARGON2_MIN_MEMORY' (8 * 'hashParallelism') <= 'hashMemory' <= 'FFI.ARGON2_MAX_MEMORY'
type MemoryCost = Word32

-- | A parallelism degree, which defines the number of parallel threads.
--
-- 'FFI.ARGON2_MIN_LANES' <= 'hashParallelism' <= 'FFI.ARGON2_MAX_LANES' && 'FFI.ARGON_MIN_THREADS' <= 'hashParallelism' <= 'FFI.ARGON2_MAX_THREADS'
type Parallelism = Word32

-- | Parameters that can be adjusted to change the runtime performance of the
-- hashing.
data Options = Options
    { iterations  :: !TimeCost
    , memory      :: !MemoryCost
    , parallelism :: !Parallelism
    , variant     :: !Variant     -- ^ Which variant of Argon2 to use.
    , version     :: !Version     -- ^ Which version of Argon2 to use.
    }
    deriving (Eq,Ord,Read,Show)

saltMinLength :: Int
saltMinLength = 8

outputMinLength :: Int
outputMinLength = 4

-- specification allows up to 2^32-1 but this is too big for a signed Int
-- on a 32-bit architecture, so we limit tag length to 2^31-1 bytes
outputMaxLength :: Int
outputMaxLength = 0x7fffffff

defaultOptions :: Options
defaultOptions =
    Options { iterations  = 1
            , memory      = 2 ^ (17 :: Int)
            , parallelism = 4
            , variant     = Argon2i
            , version     = Version13
            }

hash :: (ByteArrayAccess password, ByteArrayAccess salt, ByteArray out)
     => Options
     -> password
     -> salt
     -> Int
     -> CryptoFailable out
hash options password salt outLen
    | saltLen < saltMinLength  = CryptoFailed CryptoError_SaltTooSmall
    | outLen < outputMinLength = CryptoFailed CryptoError_OutputLengthTooSmall
    | outLen > outputMaxLength = CryptoFailed CryptoError_OutputLengthTooBig
    | otherwise                = CryptoPassed $ B.allocAndFreeze outLen $ \out -> do
        res <- B.withByteArray password $ \pPass ->
               B.withByteArray salt     $ \pSalt ->
                    argon2_hash (iterations options)
                                (memory options)
                                (parallelism options)
                                pPass
                                (csizeOfInt passwordLen)
                                pSalt
                                (csizeOfInt saltLen)
                                out
                                (csizeOfInt outLen)
                                (cOfVariant $ variant options)
                                (cOfVersion $ version options)
        when (res /= 0) $ error "argon2: hash: internal error"
  where
    saltLen = B.length salt
    passwordLen = B.length password

data Pass
data Salt
data HashOut

type CVariant = CInt -- valid value is 0 (Argon2d), 1 (Argon2i) and 2 (Argon2id)
type CVersion = CInt -- valid value is 0x10, 0x13

cOfVersion :: Version -> CVersion
cOfVersion Version10 = 0x10
cOfVersion Version13 = 0x13

cOfVariant :: Variant -> CVariant
cOfVariant Argon2d  = 0
cOfVariant Argon2i  = 1
cOfVariant Argon2id = 2

csizeOfInt :: Int -> CSize
csizeOfInt = fromIntegral

foreign import ccall unsafe "cryptonite_argon2_hash"
    argon2_hash :: Word32 -> Word32 -> Word32
                -> Ptr Pass -> CSize
                -> Ptr Salt -> CSize
                -> Ptr HashOut -> CSize
                -> CVariant
                -> CVersion
                -> IO CInt
