{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , NondecreasingIndentation
  #-}
{-# OPTIONS_GHC  -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding.Latin1
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

module GHC.IO.Encoding.Latin1 (
  latin1, mkLatin1,
  latin1_checked, mkLatin1_checked,
  ascii, mkAscii,
  latin1_decode,
  ascii_decode,
  latin1_encode,
  latin1_checked_encode,
  ascii_encode,
  ) where

import GHC.Base
import GHC.Real
import GHC.Num
-- import GHC.IO
import GHC.IO.Buffer
import GHC.IO.Encoding.Failure
import GHC.IO.Encoding.Types

-- -----------------------------------------------------------------------------
-- Latin1

latin1 :: TextEncoding
latin1 = mkLatin1 ErrorOnCodingFailure

-- | @since 4.4.0.0
mkLatin1 :: CodingFailureMode -> TextEncoding
mkLatin1 cfm = TextEncoding { textEncodingName = "ISO8859-1",
                              mkTextDecoder = latin1_DF cfm,
                              mkTextEncoder = latin1_EF cfm }

latin1_DF :: CodingFailureMode -> IO (TextDecoder ())
latin1_DF cfm =
  return (BufferCodec {
             encode   = latin1_decode,
             recover  = recoverDecode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

latin1_EF :: CodingFailureMode -> IO (TextEncoder ())
latin1_EF cfm =
  return (BufferCodec {
             encode   = latin1_encode,
             recover  = recoverEncode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

latin1_checked :: TextEncoding
latin1_checked = mkLatin1_checked ErrorOnCodingFailure

-- | @since 4.4.0.0
mkLatin1_checked :: CodingFailureMode -> TextEncoding
mkLatin1_checked cfm = TextEncoding { textEncodingName = "ISO8859-1(checked)",
                                      mkTextDecoder = latin1_DF cfm,
                                      mkTextEncoder = latin1_checked_EF cfm }

latin1_checked_EF :: CodingFailureMode -> IO (TextEncoder ())
latin1_checked_EF cfm =
  return (BufferCodec {
             encode   = latin1_checked_encode,
             recover  = recoverEncode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

-- -----------------------------------------------------------------------------
-- ASCII

-- | @since 4.9.0.0
ascii :: TextEncoding
ascii = mkAscii ErrorOnCodingFailure

-- | @since 4.9.0.0
mkAscii :: CodingFailureMode -> TextEncoding
mkAscii cfm = TextEncoding { textEncodingName = "ASCII",
                             mkTextDecoder = ascii_DF cfm,
                             mkTextEncoder = ascii_EF cfm }

ascii_DF :: CodingFailureMode -> IO (TextDecoder ())
ascii_DF cfm =
  return (BufferCodec {
             encode   = ascii_decode,
             recover  = recoverDecode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

ascii_EF :: CodingFailureMode -> IO (TextEncoder ())
ascii_EF cfm =
  return (BufferCodec {
             encode   = ascii_encode,
             recover  = recoverEncode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })



-- -----------------------------------------------------------------------------
-- The actual decoders and encoders

-- TODO: Eliminate code duplication between the checked and unchecked
-- versions of the decoder or encoder (but don't change the Core!)

latin1_decode :: DecodeBuffer
latin1_decode 
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let 
       loop !ir !ow
         | ow >= os = done OutputUnderflow ir ow
         | ir >= iw = done InputUnderflow ir ow
         | otherwise = do
              c0 <- readWord8Buf iraw ir
              ow' <- writeCharBuf oraw ow (unsafeChr (fromIntegral c0))
              loop (ir+1) ow'

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       done why !ir !ow = return (why,
                                  if ir == iw then input{ bufL=0, bufR=0 }
                                              else input{ bufL=ir },
                                  output{ bufR=ow })
    in
    loop ir0 ow0

ascii_decode :: DecodeBuffer
ascii_decode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
       loop !ir !ow
         | ow >= os = done OutputUnderflow ir ow
         | ir >= iw = done InputUnderflow ir ow
         | otherwise = do
              c0 <- readWord8Buf iraw ir
              if c0 > 0x7f then invalid else do
              ow' <- writeCharBuf oraw ow (unsafeChr (fromIntegral c0))
              loop (ir+1) ow'
         where
           invalid = done InvalidSequence ir ow

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       done why !ir !ow = return (why,
                                  if ir == iw then input{ bufL=0, bufR=0 }
                                              else input{ bufL=ir },
                                  output{ bufR=ow })
    in
    loop ir0 ow0

latin1_encode :: EncodeBuffer
latin1_encode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
      done why !ir !ow = return (why,
                                 if ir == iw then input{ bufL=0, bufR=0 }
                                             else input{ bufL=ir },
                                 output{ bufR=ow })
      loop !ir !ow
        | ow >= os = done OutputUnderflow ir ow
        | ir >= iw = done InputUnderflow ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           writeWord8Buf oraw ow (fromIntegral (ord c))
           loop ir' (ow+1)
    in
    loop ir0 ow0

latin1_checked_encode :: EncodeBuffer
latin1_checked_encode input output
 = single_byte_checked_encode 0xff input output

ascii_encode :: EncodeBuffer
ascii_encode input output
 = single_byte_checked_encode 0x7f input output

single_byte_checked_encode :: Int -> EncodeBuffer
single_byte_checked_encode max_legal_char
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
      done why !ir !ow = return (why,
                                 if ir == iw then input{ bufL=0, bufR=0 }
                                             else input{ bufL=ir },
                                 output{ bufR=ow })
      loop !ir !ow
        | ow >= os = done OutputUnderflow ir ow
        | ir >= iw = done InputUnderflow ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           if ord c > max_legal_char then invalid else do
           writeWord8Buf oraw ow (fromIntegral (ord c))
           loop ir' (ow+1)
        where
           invalid = done InvalidSequence ir ow
    in
    loop ir0 ow0
{-# INLINE single_byte_checked_encode #-}
