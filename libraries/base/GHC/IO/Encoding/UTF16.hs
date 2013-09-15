{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , NondecreasingIndentation
           , MagicHash
  #-}
{-# OPTIONS_GHC  -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding.UTF16
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- UTF-16 Codecs for the IO library
--
-- Portions Copyright   : (c) Tom Harper 2008-2009,
--                        (c) Bryan O'Sullivan 2009,
--                        (c) Duncan Coutts 2009
--
-----------------------------------------------------------------------------

module GHC.IO.Encoding.UTF16 (
  utf16, mkUTF16,
  utf16_decode,
  utf16_encode,

  utf16be, mkUTF16be,
  utf16be_decode,
  utf16be_encode,

  utf16le, mkUTF16le,
  utf16le_decode,
  utf16le_encode,
  ) where

import GHC.Base
import GHC.Real
import GHC.Num
-- import GHC.IO
import GHC.IO.Buffer
import GHC.IO.Encoding.Failure
import GHC.IO.Encoding.Types
import GHC.Word
import Data.Bits
import Data.Maybe
import GHC.IORef

-- -----------------------------------------------------------------------------
-- The UTF-16 codec: either UTF16BE or UTF16LE with a BOM

utf16  :: TextEncoding
utf16 = mkUTF16 ErrorOnCodingFailure

mkUTF16 :: CodingFailureMode -> TextEncoding
mkUTF16 cfm =  TextEncoding { textEncodingName = "UTF-16",
                              mkTextDecoder = utf16_DF cfm,
                              mkTextEncoder = utf16_EF cfm }

utf16_DF :: CodingFailureMode -> IO (TextDecoder (Maybe DecodeBuffer))
utf16_DF cfm = do
  seen_bom <- newIORef Nothing
  return (BufferCodec {
             encode   = utf16_decode seen_bom,
             recover  = recoverDecode cfm,
             close    = return (),
             getState = readIORef seen_bom,
             setState = writeIORef seen_bom
          })

utf16_EF :: CodingFailureMode -> IO (TextEncoder Bool)
utf16_EF cfm = do
  done_bom <- newIORef False
  return (BufferCodec {
             encode   = utf16_encode done_bom,
             recover  = recoverEncode cfm,
             close    = return (),
             getState = readIORef done_bom,
             setState = writeIORef done_bom
          })

utf16_encode :: IORef Bool -> EncodeBuffer
utf16_encode done_bom input
  output@Buffer{ bufRaw=oraw, bufL=_, bufR=ow, bufSize=os }
 = do
  b <- readIORef done_bom
  if b then utf16_native_encode input output
       else if os - ow < 2
               then return (OutputUnderflow,input,output)
               else do
                    writeIORef done_bom True
                    writeWord8Buf oraw ow     bom1
                    writeWord8Buf oraw (ow+1) bom2
                    utf16_native_encode input output{ bufR = ow+2 }

utf16_decode :: IORef (Maybe DecodeBuffer) -> DecodeBuffer
utf16_decode seen_bom
  input@Buffer{  bufRaw=iraw, bufL=ir, bufR=iw,  bufSize=_  }
  output
 = do
   mb <- readIORef seen_bom
   case mb of
     Just decode -> decode input output
     Nothing ->
       if iw - ir < 2 then return (InputUnderflow,input,output) else do
       c0 <- readWord8Buf iraw ir
       c1 <- readWord8Buf iraw (ir+1)
       case () of
        _ | c0 == bomB && c1 == bomL -> do
               writeIORef seen_bom (Just utf16be_decode)
               utf16be_decode input{ bufL= ir+2 } output
          | c0 == bomL && c1 == bomB -> do
               writeIORef seen_bom (Just utf16le_decode)
               utf16le_decode input{ bufL= ir+2 } output
          | otherwise -> do
               writeIORef seen_bom (Just utf16_native_decode)
               utf16_native_decode input output


bomB, bomL, bom1, bom2 :: Word8
bomB = 0xfe
bomL = 0xff

-- choose UTF-16BE by default for UTF-16 output
utf16_native_decode :: DecodeBuffer
utf16_native_decode = utf16be_decode

utf16_native_encode :: EncodeBuffer
utf16_native_encode = utf16be_encode

bom1 = bomB
bom2 = bomL

-- -----------------------------------------------------------------------------
-- UTF16LE and UTF16BE

utf16be :: TextEncoding
utf16be = mkUTF16be ErrorOnCodingFailure

mkUTF16be :: CodingFailureMode -> TextEncoding
mkUTF16be cfm = TextEncoding { textEncodingName = "UTF-16BE",
                               mkTextDecoder = utf16be_DF cfm,
                               mkTextEncoder = utf16be_EF cfm }

utf16be_DF :: CodingFailureMode -> IO (TextDecoder ())
utf16be_DF cfm =
  return (BufferCodec {
             encode   = utf16be_decode,
             recover  = recoverDecode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

utf16be_EF :: CodingFailureMode -> IO (TextEncoder ())
utf16be_EF cfm =
  return (BufferCodec {
             encode   = utf16be_encode,
             recover  = recoverEncode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

utf16le :: TextEncoding
utf16le = mkUTF16le ErrorOnCodingFailure

mkUTF16le :: CodingFailureMode -> TextEncoding
mkUTF16le cfm = TextEncoding { textEncodingName = "UTF16-LE",
                               mkTextDecoder = utf16le_DF cfm,
                               mkTextEncoder = utf16le_EF cfm }

utf16le_DF :: CodingFailureMode -> IO (TextDecoder ())
utf16le_DF cfm =
  return (BufferCodec {
             encode   = utf16le_decode,
             recover  = recoverDecode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

utf16le_EF :: CodingFailureMode -> IO (TextEncoder ())
utf16le_EF cfm =
  return (BufferCodec {
             encode   = utf16le_encode,
             recover  = recoverEncode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })


utf16be_decode :: DecodeBuffer
utf16be_decode 
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let 
       loop !ir !ow
         | ow >= os     = done OutputUnderflow ir ow
         | ir >= iw     = done InputUnderflow ir ow
         | ir + 1 == iw = done InputUnderflow ir ow
         | otherwise = do
              c0 <- readWord8Buf iraw ir
              c1 <- readWord8Buf iraw (ir+1)
              let x1 = fromIntegral c0 `shiftL` 8 + fromIntegral c1
              if validate1 x1
                 then do ow' <- writeCharBuf oraw ow (unsafeChr (fromIntegral x1))
                         loop (ir+2) ow'
                 else if iw - ir < 4 then done InputUnderflow ir ow else do
                      c2 <- readWord8Buf iraw (ir+2)
                      c3 <- readWord8Buf iraw (ir+3)
                      let x2 = fromIntegral c2 `shiftL` 8 + fromIntegral c3
                      if not (validate2 x1 x2) then invalid else do
                      ow' <- writeCharBuf oraw ow (chr2 x1 x2)
                      loop (ir+4) ow'
         where
           invalid = done InvalidSequence ir ow

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       done why !ir !ow = return (why,
                                  if ir == iw then input{ bufL=0, bufR=0 }
                                              else input{ bufL=ir },
                                  output{ bufR=ow })
    in
    loop ir0 ow0

utf16le_decode :: DecodeBuffer
utf16le_decode 
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let 
       loop !ir !ow
         | ow >= os     = done OutputUnderflow ir ow
         | ir >= iw     = done InputUnderflow ir ow
         | ir + 1 == iw = done InputUnderflow ir ow
         | otherwise = do
              c0 <- readWord8Buf iraw ir
              c1 <- readWord8Buf iraw (ir+1)
              let x1 = fromIntegral c1 `shiftL` 8 + fromIntegral c0
              if validate1 x1
                 then do ow' <- writeCharBuf oraw ow (unsafeChr (fromIntegral x1))
                         loop (ir+2) ow'
                 else if iw - ir < 4 then done InputUnderflow ir ow else do
                      c2 <- readWord8Buf iraw (ir+2)
                      c3 <- readWord8Buf iraw (ir+3)
                      let x2 = fromIntegral c3 `shiftL` 8 + fromIntegral c2
                      if not (validate2 x1 x2) then invalid else do
                      ow' <- writeCharBuf oraw ow (chr2 x1 x2)
                      loop (ir+4) ow'
         where
           invalid = done InvalidSequence ir ow

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       done why !ir !ow = return (why,
                                  if ir == iw then input{ bufL=0, bufR=0 }
                                              else input{ bufL=ir },
                                  output{ bufR=ow })
    in
    loop ir0 ow0

utf16be_encode :: EncodeBuffer
utf16be_encode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let 
      done why !ir !ow = return (why,
                                 if ir == iw then input{ bufL=0, bufR=0 }
                                             else input{ bufL=ir },
                                 output{ bufR=ow })
      loop !ir !ow
        | ir >= iw     =  done InputUnderflow ir ow
        | os - ow < 2  =  done OutputUnderflow ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           case ord c of
             x | x < 0x10000 -> if isSurrogate c then done InvalidSequence ir ow else do
                    writeWord8Buf oraw ow     (fromIntegral (x `shiftR` 8))
                    writeWord8Buf oraw (ow+1) (fromIntegral x)
                    loop ir' (ow+2)
               | otherwise -> do
                    if os - ow < 4 then done OutputUnderflow ir ow else do
                    let 
                         n1 = x - 0x10000
                         c1 = fromIntegral (n1 `shiftR` 18 + 0xD8)
                         c2 = fromIntegral (n1 `shiftR` 10)
                         n2 = n1 .&. 0x3FF
                         c3 = fromIntegral (n2 `shiftR` 8 + 0xDC)
                         c4 = fromIntegral n2
                    --
                    writeWord8Buf oraw ow     c1
                    writeWord8Buf oraw (ow+1) c2
                    writeWord8Buf oraw (ow+2) c3
                    writeWord8Buf oraw (ow+3) c4
                    loop ir' (ow+4)
    in
    loop ir0 ow0

utf16le_encode :: EncodeBuffer
utf16le_encode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
      done why !ir !ow = return (why,
                                 if ir == iw then input{ bufL=0, bufR=0 }
                                             else input{ bufL=ir },
                                 output{ bufR=ow })
      loop !ir !ow
        | ir >= iw     =  done InputUnderflow ir ow
        | os - ow < 2  =  done OutputUnderflow ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           case ord c of
             x | x < 0x10000 -> if isSurrogate c then done InvalidSequence ir ow else do
                    writeWord8Buf oraw ow     (fromIntegral x)
                    writeWord8Buf oraw (ow+1) (fromIntegral (x `shiftR` 8))
                    loop ir' (ow+2)
               | otherwise ->
                    if os - ow < 4 then done OutputUnderflow ir ow else do
                    let 
                         n1 = x - 0x10000
                         c1 = fromIntegral (n1 `shiftR` 18 + 0xD8)
                         c2 = fromIntegral (n1 `shiftR` 10)
                         n2 = n1 .&. 0x3FF
                         c3 = fromIntegral (n2 `shiftR` 8 + 0xDC)
                         c4 = fromIntegral n2
                    --
                    writeWord8Buf oraw ow     c2
                    writeWord8Buf oraw (ow+1) c1
                    writeWord8Buf oraw (ow+2) c4
                    writeWord8Buf oraw (ow+3) c3
                    loop ir' (ow+4)
    in
    loop ir0 ow0

chr2 :: Word16 -> Word16 -> Char
chr2 (W16# a#) (W16# b#) = C# (chr# (upper# +# lower# +# 0x10000#))
    where
      !x# = word2Int# a#
      !y# = word2Int# b#
      !upper# = uncheckedIShiftL# (x# -# 0xD800#) 10#
      !lower# = y# -# 0xDC00#
{-# INLINE chr2 #-}

validate1    :: Word16 -> Bool
validate1 x1 = (x1 >= 0 && x1 < 0xD800) || x1 > 0xDFFF
{-# INLINE validate1 #-}

validate2       ::  Word16 -> Word16 -> Bool
validate2 x1 x2 = x1 >= 0xD800 && x1 <= 0xDBFF &&
                  x2 >= 0xDC00 && x2 <= 0xDFFF
{-# INLINE validate2 #-}

