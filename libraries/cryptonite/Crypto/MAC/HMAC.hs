-- |
-- Module      : Crypto.MAC.HMAC
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Provide the HMAC (Hash based Message Authentification Code) base algorithm.
-- <http://en.wikipedia.org/wiki/HMAC>
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.MAC.HMAC
    ( hmac
    , HMAC(..)
    -- * Incremental
    , Context(..)
    , initialize
    , update
    , updates
    , finalize
    ) where

import           Crypto.Hash hiding (Context)
import qualified Crypto.Hash as Hash (Context)
import           Crypto.Hash.IO
import           Crypto.Internal.ByteArray (ScrubbedBytes, ByteArrayAccess)
import qualified Crypto.Internal.ByteArray as B
import           Data.Memory.PtrMethods
import           Crypto.Internal.Compat

-- | Represent an HMAC that is a phantom type with the hash used to produce the mac.
--
-- The Eq instance is constant time.  No Show instance is provided, to avoid
-- printing by mistake.
newtype HMAC a = HMAC { hmacGetDigest :: Digest a }
    deriving (ByteArrayAccess)

instance Eq (HMAC a) where
    (HMAC b1) == (HMAC b2) = B.constEq b1 b2

-- | compute a MAC using the supplied hashing function
hmac :: (ByteArrayAccess key, ByteArrayAccess message, HashAlgorithm a)
     => key     -- ^ Secret key
     -> message -- ^ Message to MAC
     -> HMAC a
hmac secret msg = finalize $ updates (initialize secret) [msg]

-- | Represent an ongoing HMAC state, that can be appended with 'update'
-- and finalize to an HMAC with 'hmacFinalize'
data Context hashalg = Context !(Hash.Context hashalg) !(Hash.Context hashalg)

-- | Initialize a new incremental HMAC context
initialize :: (ByteArrayAccess key, HashAlgorithm a)
           => key       -- ^ Secret key
           -> Context a
initialize secret = unsafeDoIO (doHashAlg undefined)
  where
        doHashAlg :: HashAlgorithm a => a -> IO (Context a)
        doHashAlg alg = do
            !withKey <- case B.length secret `compare` blockSize of
                            EQ -> return $ B.withByteArray secret
                            LT -> do key <- B.alloc blockSize $ \k -> do
                                        memSet k 0 blockSize
                                        B.withByteArray secret $ \s -> memCopy k s (B.length secret)
                                     return $ B.withByteArray (key :: ScrubbedBytes)
                            GT -> do
                                -- hash the secret key
                                ctx <- hashMutableInitWith alg
                                hashMutableUpdate ctx secret
                                digest <- hashMutableFinalize ctx
                                hashMutableReset ctx
                                -- pad it if necessary
                                if digestSize < blockSize
                                    then do
                                        key <- B.alloc blockSize $ \k -> do
                                            memSet k 0 blockSize
                                            B.withByteArray digest $ \s -> memCopy k s (B.length digest)
                                        return $ B.withByteArray (key :: ScrubbedBytes)
                                    else
                                       return $ B.withByteArray digest
            (inner, outer) <- withKey $ \keyPtr ->
                (,) <$> B.alloc blockSize (\p -> memXorWith p 0x36 keyPtr blockSize)
                    <*> B.alloc blockSize (\p -> memXorWith p 0x5c keyPtr blockSize)
            return $ Context (hashUpdates initCtx [outer :: ScrubbedBytes])
                             (hashUpdates initCtx [inner :: ScrubbedBytes])
          where 
                blockSize  = hashBlockSize alg
                digestSize = hashDigestSize alg
                initCtx    = hashInitWith alg
{-# NOINLINE initialize #-}

-- | Incrementally update a HMAC context
update :: (ByteArrayAccess message, HashAlgorithm a)
       => Context a  -- ^ Current HMAC context
       -> message    -- ^ Message to append to the MAC
       -> Context a  -- ^ Updated HMAC context
update (Context octx ictx) msg =
    Context octx (hashUpdate ictx msg)

-- | Increamentally update a HMAC context with multiple inputs
updates :: (ByteArrayAccess message, HashAlgorithm a)
        => Context a -- ^ Current HMAC context
        -> [message] -- ^ Messages to append to the MAC
        -> Context a -- ^ Updated HMAC context
updates (Context octx ictx) msgs =
    Context octx (hashUpdates ictx msgs)

-- | Finalize a HMAC context and return the HMAC.
finalize :: HashAlgorithm a
         => Context a
         -> HMAC a
finalize (Context octx ictx) =
    HMAC $ hashFinalize $ hashUpdates octx [hashFinalize ictx]
