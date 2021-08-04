-- |
-- Module      : Crypto.Hash.IO
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Generalized impure cryptographic hash interface
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Crypto.Hash.IO
    ( HashAlgorithm(..)
    , MutableContext
    , hashMutableInit
    , hashMutableInitWith
    , hashMutableUpdate
    , hashMutableFinalize
    , hashMutableReset
    ) where

import           Crypto.Hash.Types
import qualified Crypto.Internal.ByteArray as B
import           Foreign.Ptr

-- | A Mutable hash context
--
-- This type is an instance of 'B.ByteArrayAccess' for debugging purpose.
-- Internal layout is architecture dependent, may contain uninitialized data
-- fragments, and change in future versions.  The bytearray should not be used
-- as input to cryptographic algorithms.
newtype MutableContext a = MutableContext B.Bytes
    deriving (B.ByteArrayAccess)

-- | Create a new mutable hash context.
--
-- the algorithm used is automatically determined from the return constraint.
hashMutableInit :: HashAlgorithm alg => IO (MutableContext alg)
hashMutableInit = doInit undefined B.alloc
  where
        doInit :: HashAlgorithm a => a -> (Int -> (Ptr (Context a) -> IO ()) -> IO B.Bytes) -> IO (MutableContext a)
        doInit alg alloc = MutableContext `fmap` alloc (hashInternalContextSize alg) hashInternalInit

-- | Create a new mutable hash context.
--
-- The algorithm is explicitely passed as parameter
hashMutableInitWith :: HashAlgorithm alg => alg -> IO (MutableContext alg)
hashMutableInitWith _ = hashMutableInit

-- | Update a mutable hash context in place
hashMutableUpdate :: (B.ByteArrayAccess ba, HashAlgorithm a) => MutableContext a -> ba -> IO ()
hashMutableUpdate mc dat = doUpdate mc (B.withByteArray mc)
  where doUpdate :: HashAlgorithm a => MutableContext a -> ((Ptr (Context a) -> IO ()) -> IO ()) -> IO ()
        doUpdate _ withCtx =
            withCtx             $ \ctx ->
            B.withByteArray dat $ \d   ->
                hashInternalUpdate ctx d (fromIntegral $ B.length dat)

-- | Finalize a mutable hash context and compute a digest
hashMutableFinalize :: forall a . HashAlgorithm a => MutableContext a -> IO (Digest a)
hashMutableFinalize mc = do
    b <- B.alloc (hashDigestSize (undefined :: a)) $ \dig -> B.withByteArray mc $ \(ctx :: Ptr (Context a)) -> hashInternalFinalize ctx dig
    return $ Digest b

-- | Reset the mutable context to the initial state of the hash
hashMutableReset :: HashAlgorithm a => MutableContext a -> IO ()
hashMutableReset mc = doReset mc (B.withByteArray mc)
  where
    doReset :: HashAlgorithm a => MutableContext a -> ((Ptr (Context a) -> IO ()) -> IO ()) -> IO ()
    doReset _ withCtx = withCtx hashInternalInit
