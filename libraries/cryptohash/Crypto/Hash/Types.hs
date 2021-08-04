{-# LANGUAGE PackageImports #-}
-- |
-- Module      : Crypto.Hash.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Crypto hash types definitions
--
module Crypto.Hash.Types
    ( Context(..)
    , Digest(..)
    -- * deprecated
    , contextToByteString
    , digestToByteString
    )
    where

import Data.ByteString (ByteString)
import Data.Byteable
import qualified Data.ByteArray as B (convert)
import qualified "cryptonite" Crypto.Hash as H

-- | Represent a context for a given hash algorithm.
newtype Context a = Context (H.Context a)

instance Byteable (Context a) where
    toBytes (Context ctx) = B.convert ctx

--- | return the binary bytestring. deprecated use toBytes.
contextToByteString :: Context a -> ByteString
contextToByteString = toBytes

-- | Represent a digest for a given hash algorithm.
newtype Digest a = Digest (H.Digest a)
    deriving (Eq,Ord)

instance Byteable (Digest a) where
    toBytes (Digest dig) = B.convert dig

-- | return the binary bytestring. deprecated use toBytes.
{-# DEPRECATED digestToByteString "use toBytes from byteable:Data.Byteable" #-}
digestToByteString :: Digest a -> ByteString
digestToByteString = toBytes

instance Show (Digest a) where
    show (Digest dig) = show dig
