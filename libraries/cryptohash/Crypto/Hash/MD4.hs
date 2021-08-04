{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Crypto.Hash.MD4
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A module containing MD4 bindings
--
module Crypto.Hash.MD4
    ( Ctx(..)

    -- * Incremental hashing Functions
    , init     -- :: Ctx
    , update   -- :: Ctx -> ByteString -> Ctx
    , updates  -- :: Ctx -> [ByteString] -> Ctx
    , finalize -- :: Ctx -> ByteString

    -- * Single Pass hashing
    , hash     -- :: ByteString -> ByteString
    , hashlazy -- :: ByteString -> ByteString
    ) where

import Prelude hiding (init)
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)
import Crypto.Hash.Internal (digestToByteString, digestToByteStringWitness)

import qualified "cryptonite" Crypto.Hash as H

-- | MD4 Context
newtype Ctx = Ctx (H.Context H.MD4)

-- | init a context
init :: Ctx
init = Ctx H.hashInit

-- | update a context with a bytestring
update :: Ctx -> ByteString -> Ctx
update (Ctx ctx) d = Ctx $ H.hashUpdate ctx d

-- | updates a context with multiples bytestring
updates :: Ctx -> [ByteString] -> Ctx
updates (Ctx ctx) d =
    Ctx $ H.hashUpdates ctx d

-- | finalize the context into a digest bytestring
finalize :: Ctx -> ByteString
finalize (Ctx ctx) = digestToByteString $ H.hashFinalize ctx

-- | hash a strict bytestring into a digest bytestring
hash :: ByteString -> ByteString
hash d = digestToByteStringWitness H.MD4 $ H.hash d

-- | hash a lazy bytestring into a digest bytestring
hashlazy :: L.ByteString -> ByteString
hashlazy l = digestToByteStringWitness H.MD4 $ H.hashlazy l
