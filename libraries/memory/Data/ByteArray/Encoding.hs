-- |
-- Module      : Data.ByteArray.Encoding
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Base conversions for 'ByteArray'.
--
module Data.ByteArray.Encoding
    ( convertToBase
    , convertFromBase
    , Base(..)
    ) where

import           Data.ByteArray.Types
import qualified Data.ByteArray.Types        as B
import qualified Data.ByteArray.Methods      as B
import           Data.Memory.Internal.Compat
import           Data.Memory.Encoding.Base16
import           Data.Memory.Encoding.Base32
import           Data.Memory.Encoding.Base64

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.ByteString

-- | The different bases that can be used.
--
-- See <http://tools.ietf.org/html/rfc4648 RFC4648> for details.
-- In particular, Base64 can be standard or
-- <http://tools.ietf.org/html/rfc4648#section-5 URL-safe>. URL-safe
-- encoding is often used in other specifications without
-- <http://tools.ietf.org/html/rfc4648#section-3.2 padding> characters.
--
-- <https://www.ietf.org/rfc/rfc2045.txt RFC 2045>
-- defines a separate Base64 encoding, which is not supported. This format
-- requires a newline at least every 76 encoded characters, which works around
-- limitations of older email programs that could not handle long lines.
-- Be aware that other languages, such as Ruby, encode the RFC 2045 version
-- by default. To decode their ouput, remove all newlines before decoding.
--
-- ==== Examples
--
-- A quick example to show the differences:
--
-- >>> let input = "Is 3 > 2?" :: ByteString
-- >>> let convertedTo base = convertToBase base input :: ByteString
-- >>> convertedTo Base16
-- "49732033203e20323f"
-- >>> convertedTo Base32
-- "JFZSAMZAHYQDEPY="
-- >>> convertedTo Base64
-- "SXMgMyA+IDI/"
-- >>> convertedTo Base64URLUnpadded
-- "SXMgMyA-IDI_"
-- >>> convertedTo Base64OpenBSD
-- "QVKeKw.8GBG9"
--
data Base = Base16            -- ^ similar to hexadecimal
          | Base32
          | Base64            -- ^ standard Base64
          | Base64URLUnpadded -- ^ unpadded URL-safe Base64
          | Base64OpenBSD     -- ^ Base64 as used in OpenBSD password encoding (such as bcrypt)
          deriving (Show,Eq)

-- | Encode some bytes to the equivalent representation in a specific 'Base'.
--
-- ==== Examples
--
-- Convert a 'ByteString' to base-64:
--
-- >>> convertToBase Base64 ("foobar" :: ByteString) :: ByteString
-- "Zm9vYmFy"
--
convertToBase :: (ByteArrayAccess bin, ByteArray bout) => Base -> bin -> bout
convertToBase base b = case base of
    Base16 -> doConvert (binLength * 2) toHexadecimal
    Base32 -> let (q,r)  = binLength `divMod` 5
                  outLen = 8 * (if r == 0 then q else q + 1)
               in doConvert outLen toBase32
    Base64 -> doConvert base64Length toBase64
    -- Base64URL         -> doConvert base64Length (toBase64URL True)
    Base64URLUnpadded -> doConvert base64UnpaddedLength (toBase64URL False)
    Base64OpenBSD     -> doConvert base64UnpaddedLength toBase64OpenBSD
  where
    binLength = B.length b

    base64Length = let (q,r) = binLength `divMod` 3
                    in 4 * (if r == 0 then q else q+1)

    base64UnpaddedLength = let (q,r) = binLength `divMod` 3
                            in 4 * q + (if r == 0 then 0 else r+1)
    doConvert l f =
        B.unsafeCreate l $ \bout ->
        B.withByteArray b     $ \bin  ->
            f bout bin binLength

-- | Try to decode some bytes from the equivalent representation in a specific 'Base'.
--
-- ==== Examples
--
-- Successfully convert from base-64 to a 'ByteString':
--
-- >>> convertFromBase Base64 ("Zm9vYmFy" :: ByteString) :: Either String ByteString
-- Right "foobar"
--
-- Trying to decode invalid data will return an error string:
--
-- >>> convertFromBase Base64 ("!!!" :: ByteString) :: Either String ByteString
-- Left "base64: input: invalid length"
--
convertFromBase :: (ByteArrayAccess bin, ByteArray bout) => Base -> bin -> Either String bout
convertFromBase Base16 b
    | odd (B.length b) = Left "base16: input: invalid length"
    | otherwise        = unsafeDoIO $ do
        (ret, out) <-
            B.allocRet (B.length b `div` 2) $ \bout ->
            B.withByteArray b               $ \bin  ->
                fromHexadecimal bout bin (B.length b)
        case ret of
            Nothing  -> return $ Right out
            Just ofs -> return $ Left ("base16: input: invalid encoding at offset: " ++ show ofs)
convertFromBase Base32 b = unsafeDoIO $
    withByteArray b $ \bin -> do
        mDstLen <- unBase32Length bin (B.length b)
        case mDstLen of
            Nothing     -> return $ Left "base32: input: invalid length"
            Just dstLen -> do
                (ret, out) <- B.allocRet dstLen $ \bout -> fromBase32 bout bin (B.length b)
                case ret of
                    Nothing  -> return $ Right out
                    Just ofs -> return $ Left ("base32: input: invalid encoding at offset: " ++ show ofs)
convertFromBase Base64 b = unsafeDoIO $
    withByteArray b $ \bin -> do
        mDstLen <- unBase64Length bin (B.length b)
        case mDstLen of
            Nothing     -> return $ Left "base64: input: invalid length"
            Just dstLen -> do
                (ret, out) <- B.allocRet dstLen $ \bout -> fromBase64 bout bin (B.length b)
                case ret of
                    Nothing  -> return $ Right out
                    Just ofs -> return $ Left ("base64: input: invalid encoding at offset: " ++ show ofs)
convertFromBase Base64URLUnpadded b = unsafeDoIO $
    withByteArray b $ \bin ->
        case unBase64LengthUnpadded (B.length b) of
            Nothing     -> return $ Left "base64URL unpadded: input: invalid length"
            Just dstLen -> do
                (ret, out) <- B.allocRet dstLen $ \bout -> fromBase64URLUnpadded bout bin (B.length b)
                case ret of
                    Nothing  -> return $ Right out
                    Just ofs -> return $ Left ("base64URL unpadded: input: invalid encoding at offset: " ++ show ofs)
convertFromBase Base64OpenBSD b = unsafeDoIO $
    withByteArray b $ \bin ->
        case unBase64LengthUnpadded (B.length b) of
            Nothing     -> return $ Left "base64 unpadded: input: invalid length"
            Just dstLen -> do
                (ret, out) <- B.allocRet dstLen $ \bout -> fromBase64OpenBSD bout bin (B.length b)
                case ret of
                    Nothing  -> return $ Right out
                    Just ofs -> return $ Left ("base64 unpadded: input: invalid encoding at offset: " ++ show ofs)

