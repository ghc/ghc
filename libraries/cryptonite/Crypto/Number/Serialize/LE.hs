-- |
-- Module      : Crypto.Number.Serialize.LE
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- Fast serialization primitives for integer (little endian)
{-# LANGUAGE BangPatterns #-}
module Crypto.Number.Serialize.LE
    ( i2osp
    , os2ip
    , i2ospOf
    , i2ospOf_
    ) where

import           Crypto.Number.Basic
import           Crypto.Internal.Compat (unsafeDoIO)
import qualified Crypto.Internal.ByteArray as B
import qualified Crypto.Number.Serialize.Internal.LE as Internal

-- | @os2ip@ converts a byte string into a positive integer.
os2ip :: B.ByteArrayAccess ba => ba -> Integer
os2ip bs = unsafeDoIO $ B.withByteArray bs (\p -> Internal.os2ip p (B.length bs))

-- | @i2osp@ converts a positive integer into a byte string.
--
-- The first byte is LSB (least significant byte); the last byte is the MSB (most significant byte)
i2osp :: B.ByteArray ba => Integer -> ba
i2osp 0 = B.allocAndFreeze 1  (\p -> Internal.i2osp 0 p 1 >> return ())
i2osp m = B.allocAndFreeze sz (\p -> Internal.i2osp m p sz >> return ())
  where
        !sz = numBytes m

-- | Just like 'i2osp', but takes an extra parameter for size.
-- If the number is too big to fit in @len@ bytes, 'Nothing' is returned
-- otherwise the number is padded with 0 to fit the @len@ required.
{-# INLINABLE i2ospOf #-}
i2ospOf :: B.ByteArray ba => Int -> Integer -> Maybe ba
i2ospOf len m
    | len <= 0  = Nothing
    | m < 0     = Nothing
    | sz > len  = Nothing
    | otherwise = Just $ B.unsafeCreate len (\p -> Internal.i2ospOf m p len >> return ())
  where
        !sz = numBytes m

-- | Just like 'i2ospOf' except that it doesn't expect a failure: i.e.
-- an integer larger than the number of output bytes requested.
--
-- For example if you just took a modulo of the number that represent
-- the size (example the RSA modulo n).
i2ospOf_ :: B.ByteArray ba => Int -> Integer -> ba
i2ospOf_ len = maybe (error "i2ospOf_: integer is larger than expected") id . i2ospOf len
