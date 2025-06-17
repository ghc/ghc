{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
    UnliftedFFITypes, GHC2021 #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Data.Text.Encoding
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts,
--               (c) 2008, 2009 Tom Harper
--               (c) 2021 Andrew Lelechenko
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : portable
--
-- Functions for converting 'Text' values to and from 'ByteString',
-- using several standard encodings.
--
-- To gain access to a much larger family of encodings, use the
-- <http://hackage.haskell.org/package/text-icu text-icu package>.

module Data.Text.Encoding
    (
    encodeUtf8BuilderEscaped
    ) where

import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)

import Control.Exception (evaluate, try)
import Control.Monad.ST (runST, ST)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Short.Internal as SBS
import Data.Text.Encoding.Error (OnDecodeError, UnicodeException, strictDecode, lenientDecode)
import Data.Text.Internal (Text(..), safe, empty, append)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import Data.Text.Internal.Unsafe.Char (unsafeWrite)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word8)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (poke, peekByteOff)
import GHC.Exts (byteArrayContents#, unsafeCoerce#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(PlainPtr))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Internal as B hiding (empty, append)
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Builder.Prim.Internal as BP
import Data.Text.Internal.Encoding.Utf8 (utf8DecodeStart, utf8DecodeContinue, DecoderResult(..))
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Encoding.Fusion as E
import qualified Data.Text.Internal.Fusion as F
import Data.Text.Internal.ByteStringCompat



-- | Encode text using UTF-8 encoding and escape the ASCII characters using
-- a 'BP.BoundedPrim'.
--
-- Use this function is to implement efficient encoders for text-based formats
-- like JSON or HTML.
--
-- @since 1.1.0.0
{-# INLINE encodeUtf8BuilderEscaped #-}
-- TODO: Extend documentation with references to source code in @blaze-html@
-- or @aeson@ that uses this function.
encodeUtf8BuilderEscaped :: BP.BoundedPrim Word8 -> Text -> B.Builder
encodeUtf8BuilderEscaped be =
    -- manual eta-expansion to ensure inlining works as expected
    \txt -> B.builder (mkBuildstep txt)
  where
    bound = max 4 $ BP.sizeBound be

    mkBuildstep (Text arr off len) !k =
        outerLoop off
      where
        iend = off + len

        outerLoop !i0 !br@(B.BufferRange op0 ope)
          | i0 >= iend       = k br
          | outRemaining > 0 = goPartial (i0 + min outRemaining inpRemaining)
          -- TODO: Use a loop with an integrated bound's check if outRemaining
          -- is smaller than 8, as this will save on divisions.
          | otherwise        = return $ B.bufferFull bound op0 (outerLoop i0)
          where
            outRemaining = (ope `minusPtr` op0) `quot` bound
            inpRemaining = iend - i0

            goPartial !iendTmp = go i0 op0
              where
                go !i !op
                  | i < iendTmp = do
                    let w = A.unsafeIndex arr i
                    if w < 0x80
                      then BP.runB be w op >>= go (i + 1)
                      else poke op w >> go (i + 1) (op `plusPtr` 1)
                  | otherwise = outerLoop i (B.BufferRange op ope)

