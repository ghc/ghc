-- |
-- Module      : Crypto.Internal.Builder
-- License     : BSD-style
-- Maintainer  : Olivier Chéron <olivier.cheron@gmail.com>
-- Stability   : stable
-- Portability : Good
--
-- Delaying and merging ByteArray allocations.  This is similar to module
-- "Data.ByteArray.Pack" except the total length is computed automatically based
-- on what is appended.
--
{-# LANGUAGE BangPatterns #-}
module Crypto.Internal.Builder
    ( Builder
    , buildAndFreeze
    , builderLength
    , byte
    , bytes
    , zero
    ) where

import           Data.ByteArray (ByteArray, ByteArrayAccess)
import qualified Data.ByteArray as B
import           Data.Memory.PtrMethods (memSet)

import           Foreign.Ptr (Ptr, plusPtr)
import           Foreign.Storable (poke)

import           Crypto.Internal.Imports hiding (empty)

data Builder =  Builder !Int (Ptr Word8 -> IO ())  -- size and initializer

instance Semigroup Builder where
    (Builder s1 f1) <> (Builder s2 f2) = Builder (s1 + s2) f
      where f p = f1 p >> f2 (p `plusPtr` s1)

builderLength :: Builder -> Int
builderLength (Builder s _) = s

buildAndFreeze :: ByteArray ba => Builder -> ba
buildAndFreeze (Builder s f) = B.allocAndFreeze s f

byte :: Word8 -> Builder
byte !b = Builder 1 (`poke` b)

bytes :: ByteArrayAccess ba => ba -> Builder
bytes bs = Builder (B.length bs) (B.copyByteArrayToPtr bs)

zero :: Int -> Builder
zero s = if s > 0 then Builder s (\p -> memSet p 0 s) else empty

empty :: Builder
empty = Builder 0 (const $ return ())
