{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Crypto.Hash.Internal
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
module Crypto.Hash.Internal
    ( unsafeDoIO
    , digestToByteString
    , digestToByteStringWitness
    ) where

import System.IO.Unsafe
import Data.ByteArray (convert)
import "cryptonite" Crypto.Hash
import Data.ByteString (ByteString)

-- | perform io for hashes that do allocation and ffi.
-- unsafeDupablePerformIO is used when possible as the
-- computation is pure and the output is directly linked
-- to the input. we also do not modify anything after it has
-- been returned to the user.
unsafeDoIO :: IO a -> a
#if __GLASGOW_HASKELL__ > 704
unsafeDoIO = unsafeDupablePerformIO
#else
unsafeDoIO = unsafePerformIO
#endif

digestToByteString :: HashAlgorithm h => Digest h -> ByteString
digestToByteString = convert

digestToByteStringWitness :: HashAlgorithm h => h -> Digest h -> ByteString
digestToByteStringWitness _ = convert

