{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

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
    CodeBuffer, EncodeBuffer, DecodeBuffer,
    CodingProgress(..)
  ) where

import GHC.Base
import GHC.Word
import GHC.Show
-- import GHC.IO
import GHC.IO.Buffer

-- -----------------------------------------------------------------------------
-- Text encoders/decoders

data BufferCodec from to state = BufferCodec {
  encode :: CodeBuffer from to,
   -- ^ The @encode@ function translates elements of the buffer @from@
   -- to the buffer @to@.  It should translate as many elements as possible
   -- given the sizes of the buffers, including translating zero elements
   -- if there is either not enough room in @to@, or @from@ does not
   -- contain a complete multibyte sequence.
   --
   -- If multiple CodingProgress returns are possible, OutputUnderflow must be
   -- preferred to InvalidSequence. This allows GHC's IO library to assume that
   -- if we observe InvalidSequence there is at least a single element available
   -- in the output buffer.
   --
   -- The fact that as many elements as possible are translated is used by the IO
   -- library in order to report translation errors at the point they
   -- actually occur, rather than when the buffer is translated.
  
  recover :: Buffer from -> Buffer to -> IO (Buffer from, Buffer to),
   -- ^ The @recover@ function is used to continue decoding
   -- in the presence of invalid or unrepresentable sequences. This includes
   -- both those detected by @encode@ returning @InvalidSequence@ and those
   -- that occur because the input byte sequence appears to be truncated.
   --
   -- Progress will usually be made by skipping the first element of the @from@
   -- buffer. This function should only be called if you are certain that you
   -- wish to do this skipping and if the @to@ buffer has at least one element
   -- of free space. Because this function deals with decoding failure, it assumes
   -- that the from buffer has at least one element.
   --
   -- @recover@ may raise an exception rather than skipping anything.
   --
   -- Currently, some implementations of @recover@ may mutate the input buffer.
   -- In particular, this feature is used to implement transliteration.
   --
   -- @since 4.4.0.0
  
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

  setState :: state -> IO ()
   -- restore the state of the codec using the state from a previous
   -- call to 'getState'.
 }

type CodeBuffer from to = Buffer from -> Buffer to -> IO (CodingProgress, Buffer from, Buffer to)
type DecodeBuffer = CodeBuffer Word8 Char
type EncodeBuffer = CodeBuffer Char Word8

type TextDecoder state = BufferCodec Word8 CharBufElem state
type TextEncoder state = BufferCodec CharBufElem Word8 state

-- | A 'TextEncoding' is a specification of a conversion scheme
-- between sequences of bytes and sequences of Unicode characters.
--
-- For example, UTF-8 is an encoding of Unicode characters into a sequence
-- of bytes.  The 'TextEncoding' for UTF-8 is 'utf8'.
data TextEncoding
  = forall dstate estate . TextEncoding  {
        textEncodingName :: String,
                   -- ^ a string that can be passed to 'mkTextEncoding' to
                   -- create an equivalent 'TextEncoding'.
        mkTextDecoder :: IO (TextDecoder dstate),
                   -- ^ Creates a means of decoding bytes into characters: the result must not
                   -- be shared between several byte sequences or simultaneously across threads
        mkTextEncoder :: IO (TextEncoder estate)
                   -- ^ Creates a means of encode characters into bytes: the result must not
                   -- be shared between several character sequences or simultaneously across threads
  }

instance Show TextEncoding where
  -- | Returns the value of 'textEncodingName'
  show te = textEncodingName te

-- | @since 4.4.0.0
data CodingProgress = InputUnderflow  -- ^ Stopped because the input contains insufficient available elements,
                                      -- or all of the input sequence has been sucessfully translated.
                    | OutputUnderflow -- ^ Stopped because the output contains insufficient free elements
                    | InvalidSequence -- ^ Stopped because there are sufficient free elements in the output
                                      -- to output at least one encoded ASCII character, but the input contains
                                      -- an invalid or unrepresentable sequence
                    deriving (Eq, Show)

