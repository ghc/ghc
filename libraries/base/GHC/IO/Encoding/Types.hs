{-# OPTIONS_GHC -XNoImplicitPrelude -funbox-strict-fields #-}
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
-- import GHC.IO
import GHC.IO.Buffer

-- -----------------------------------------------------------------------------
-- Text encoders/decoders

data BufferCodec from to state = BufferCodec {
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
  close  :: IO (),
   -- ^ Resources associated with the encoding may now be released.
   -- The @encode@ function may not be called again after calling
   -- @close@.

  getState :: IO state,
   -- ^ Return the current state of the codec.
   --
   -- Many codecs are not stateful, and in these case the state can be
   -- represented as '()'.  Other codecs maintain a state.  For
   -- example, UTF-16 recognises a BOM (byte-order-mark) character at
   -- the beginning of the input, and remembers thereafter whether to
   -- use big-endian or little-endian mode.  In this case, the state
   -- of the codec would include two pieces of information: whether we
   -- are at the beginning of the stream (the BOM only occurs at the
   -- beginning), and if not, whether to use the big or little-endian
   -- encoding.

  setState :: state -> IO()
   -- restore the state of the codec using the state from a previous
   -- call to 'getState'.
 }

type DecodeBuffer = Buffer Word8 -> Buffer Char
                  -> IO (Buffer Word8, Buffer Char)

type EncodeBuffer = Buffer Char -> Buffer Word8
                  -> IO (Buffer Char, Buffer Word8)

type TextDecoder state = BufferCodec Word8 CharBufElem state
type TextEncoder state = BufferCodec CharBufElem Word8 state

-- | A 'TextEncoding' is a specification of a conversion scheme
-- between sequences of bytes and sequences of Unicode characters.
--
-- For example, UTF-8 is an encoding of Unicode characters into a sequence
-- of bytes.  The 'TextEncoding' for UTF-8 is 'utf8'.
data TextEncoding
  = forall dstate estate . TextEncoding  {
	mkTextDecoder :: IO (TextDecoder dstate),
	mkTextEncoder :: IO (TextEncoder estate)
  }
