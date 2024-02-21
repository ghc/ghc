{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , NondecreasingIndentation
           , UnboxedTuples
           , MagicHash
  #-}
{-# OPTIONS_GHC  -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.IO.Encoding.Latin1
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Single-byte encodings that map directly to Unicode code points.
--
-- Portions Copyright   : (c) Tom Harper 2008-2009,
--                        (c) Bryan O'Sullivan 2009,
--                        (c) Duncan Coutts 2009
--
-----------------------------------------------------------------------------

module GHC.Internal.IO.Encoding.Latin1 (
  latin1, mkLatin1,
  latin1_checked, mkLatin1_checked,
  ascii, mkAscii,
  latin1_decode,
  ascii_decode,
  latin1_encode,
  latin1_checked_encode,
  ascii_encode,
  ) where

import GHC.Internal.Base
import GHC.Internal.Real
import GHC.Internal.Num
-- import GHC.Internal.IO
import GHC.Internal.IO.Buffer
import GHC.Internal.IO.Encoding.Failure
import GHC.Internal.IO.Encoding.Types

-- -----------------------------------------------------------------------------
-- Latin1

latin1 :: TextEncoding
latin1 = mkLatin1 ErrorOnCodingFailure

-- | @since base-4.4.0.0
mkLatin1 :: CodingFailureMode -> TextEncoding
mkLatin1 cfm = TextEncoding { textEncodingName = "ISO-8859-1",
                              mkTextDecoder = latin1_DF cfm,
                              mkTextEncoder = latin1_EF cfm }

latin1_DF :: CodingFailureMode -> IO (TextDecoder ())
latin1_DF cfm =
  return (BufferCodec# {
             encode#   = latin1_decode,
             recover#  = recoverDecode# cfm,
             close#    = return (),
             getState# = return (),
             setState# = const $ return ()
          })

latin1_EF :: CodingFailureMode -> IO (TextEncoder ())
latin1_EF cfm =
  return (BufferCodec# {
             encode#   = latin1_encode,
             recover#  = recoverEncode# cfm,
             close#    = return (),
             getState# = return (),
             setState# = const $ return ()
          })

latin1_checked :: TextEncoding
latin1_checked = mkLatin1_checked ErrorOnCodingFailure

-- | @since base-4.4.0.0
mkLatin1_checked :: CodingFailureMode -> TextEncoding
mkLatin1_checked cfm = TextEncoding { textEncodingName = "ISO-8859-1",
                                      mkTextDecoder = latin1_DF cfm,
                                      mkTextEncoder = latin1_checked_EF cfm }

latin1_checked_EF :: CodingFailureMode -> IO (TextEncoder ())
latin1_checked_EF cfm =
  return (BufferCodec# {
             encode#   = latin1_checked_encode,
             recover#  = recoverEncode# cfm,
             close#    = return (),
             getState# = return (),
             setState# = const $ return ()
          })

-- -----------------------------------------------------------------------------
-- ASCII

-- | @since base-4.9.0.0
ascii :: TextEncoding
ascii = mkAscii ErrorOnCodingFailure

-- | @since base-4.9.0.0
mkAscii :: CodingFailureMode -> TextEncoding
mkAscii cfm = TextEncoding { textEncodingName = "ASCII",
                             mkTextDecoder = ascii_DF cfm,
                             mkTextEncoder = ascii_EF cfm }

ascii_DF :: CodingFailureMode -> IO (TextDecoder ())
ascii_DF cfm =
  return (BufferCodec# {
             encode#   = ascii_decode,
             recover#  = recoverDecode# cfm,
             close#    = return (),
             getState# = return (),
             setState# = const $ return ()
          })

ascii_EF :: CodingFailureMode -> IO (TextEncoder ())
ascii_EF cfm =
  return (BufferCodec# {
             encode#   = ascii_encode,
             recover#  = recoverEncode# cfm,
             close#    = return (),
             getState# = return (),
             setState# = const $ return ()
          })



-- -----------------------------------------------------------------------------
-- The actual decoders and encoders

-- TODO: Eliminate code duplication between the checked and unchecked
-- versions of the decoder or encoder (but don't change the Core!)

latin1_decode :: DecodeBuffer#
latin1_decode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
  st
 = let
       loop :: Int -> Int -> DecodingBuffer#
       loop !ir !ow st0
         | ow >= os = done OutputUnderflow ir ow st0
         | ir >= iw = done InputUnderflow ir ow st0
         | otherwise = do
              let !(# st1, c0 #) = unIO (readWord8Buf iraw ir) st0
                  !(# st2, ow' #) = unIO (writeCharBuf oraw ow (unsafeChr (fromIntegral c0))) st1
              loop (ir+1) ow' st2

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       {-# NOINLINE done #-}
       done :: CodingProgress -> Int -> Int -> DecodingBuffer#
       done why !ir !ow st' =
         let !ri = if ir == iw then input{ bufL=0, bufR=0 } else input{ bufL=ir }
             !ro = output{ bufR=ow }
         in  (# st', why, ri, ro #)
    in
    loop ir0 ow0 st

ascii_decode :: DecodeBuffer#
ascii_decode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
  st
 = let
       loop :: Int -> Int -> DecodingBuffer#
       loop !ir !ow st0
         | ow >= os = done OutputUnderflow ir ow st0
         | ir >= iw = done InputUnderflow ir ow st0
         | otherwise = do
              let !(# st1, c0 #) = unIO (readWord8Buf iraw ir) st0
              if c0 > 0x7f then invalid st1 else do
                let !(# st2, ow' #) = unIO (writeCharBuf oraw ow (unsafeChr (fromIntegral c0))) st1
                loop (ir+1) ow' st2
         where
           invalid :: DecodingBuffer#
           invalid st' = done InvalidSequence ir ow st'

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       {-# NOINLINE done #-}
       done :: CodingProgress -> Int -> Int -> DecodingBuffer#
       done why !ir !ow st' =
         let !ri = if ir == iw then input{ bufL=0, bufR=0 } else input{ bufL=ir }
             !ro = output{ bufR=ow }
         in  (# st', why, ri, ro #)
    in
    loop ir0 ow0 st

latin1_encode :: EncodeBuffer#
latin1_encode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
  st
 = let
      {-# NOINLINE done #-}
      done :: CodingProgress -> Int -> Int -> EncodingBuffer#
      done why !ir !ow st' =
        let !ri = if ir == iw then input{ bufL=0, bufR=0 } else input{ bufL=ir }
            !ro = output{ bufR=ow }
        in  (# st', why, ri, ro #)
      loop :: Int -> Int -> EncodingBuffer#
      loop !ir !ow st0
        | ow >= os = done OutputUnderflow ir ow st0
        | ir >= iw = done InputUnderflow ir ow st0
        | otherwise = do
           let !(# st1, (c,ir') #) = unIO (readCharBuf iraw ir) st0
               !(# st2, () #) = unIO (writeWord8Buf oraw ow (fromIntegral (ord c))) st1
           loop ir' (ow+1) st2
    in
    loop ir0 ow0 st

latin1_checked_encode :: EncodeBuffer#
latin1_checked_encode input output
 = single_byte_checked_encode 0xff input output

ascii_encode :: EncodeBuffer#
ascii_encode input output
 = single_byte_checked_encode 0x7f input output

single_byte_checked_encode :: Int -> EncodeBuffer#
single_byte_checked_encode max_legal_char
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
  st
 = let
      {-# NOINLINE done #-}
      done :: CodingProgress -> Int -> Int -> EncodingBuffer#
      done why !ir !ow st' =
        let !ri = if ir == iw then input{ bufL=0, bufR=0 } else input{ bufL=ir }
            !ro = output{ bufR=ow }
        in  (# st', why, ri, ro #)
      loop :: Int -> Int -> EncodingBuffer#
      loop !ir !ow st0
        | ow >= os = done OutputUnderflow ir ow st0
        | ir >= iw = done InputUnderflow ir ow st0
        | otherwise = do
           let !(# st1, (c,ir') #) = unIO (readCharBuf iraw ir) st0
           if ord c > max_legal_char then invalid st1 else do
             let !(# st2, () #) = unIO (writeWord8Buf oraw ow (fromIntegral (ord c))) st1
             loop ir' (ow+1) st2
        where
          invalid :: EncodingBuffer#
          invalid st' = done InvalidSequence ir ow st'
    in
    loop ir0 ow0 st
{-# INLINE single_byte_checked_encode #-}
