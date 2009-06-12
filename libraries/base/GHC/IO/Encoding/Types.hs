{-# OPTIONS_GHC -fno-implicit-prelude -funbox-strict-fields #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding.Types
-- Copyright   :  (c) The University of Glasgow, 2008-2009
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Types for text encoding/decoding
--
-----------------------------------------------------------------------------

module GHC.IO.Encoding.Types (
    BufferCodec(..),
    TextEncoding(..),
    TextEncoder, TextDecoder,
    EncodeBuffer, DecodeBuffer,
  ) where

import GHC.Base
import GHC.Word
import GHC.IO
import GHC.IO.Buffer

-- -----------------------------------------------------------------------------
-- Text encoders/decoders

data BufferCodec from to = BufferCodec {
  encode :: Buffer from -> Buffer to -> IO (Buffer from, Buffer to),
   -- ^ The @encode@ function translates elements of the buffer @from@
   -- to the buffer @to@.  It should translate as many elements as possible
   -- given the sizes of the buffers, including translating zero elements
   -- if there is either not enough room in @to@, or @from@ does not
   -- contain a complete multibyte sequence.
   -- 
   -- @encode@ should raise an exception if, and only if, @from@
   -- begins with an illegal sequence, or the first element of @from@
   -- is not representable in the encoding of @to@.  That is, if any
   -- elements can be successfully translated before an error is
   -- encountered, then @encode@ should translate as much as it can
   -- and not throw an exception.  This behaviour is used by the IO
   -- library in order to report translation errors at the point they
   -- actually occur, rather than when the buffer is translated.
   --
  close  :: IO ()
   -- ^ Resources associated with the encoding may now be released.
   -- The @encode@ function may not be called again after calling
   -- @close@.
 }

type DecodeBuffer = Buffer Word8 -> Buffer Char
                  -> IO (Buffer Word8, Buffer Char)

type EncodeBuffer = Buffer Char -> Buffer Word8
                  -> IO (Buffer Char, Buffer Word8)

type TextDecoder = BufferCodec Word8 CharBufElem
type TextEncoder = BufferCodec CharBufElem Word8

-- | A 'TextEncoding' is a specification of a conversion scheme
-- between sequences of bytes and sequences of Unicode characters.
--
-- For example, UTF-8 is an encoding of Unicode characters into a sequence
-- of bytes.  The 'TextEncoding' for UTF-8 is 'utf_8'.
data TextEncoding
  = TextEncoding  {
	mkTextDecoder :: IO TextDecoder,
	mkTextEncoder :: IO TextEncoder
  }
