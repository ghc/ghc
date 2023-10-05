{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , NondecreasingIndentation
           , MagicHash
           , UnboxedTuples
  #-}
{-# OPTIONS_GHC  -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding.UTF32
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- UTF-32 Codecs for the IO library
--
-- Portions Copyright   : (c) Tom Harper 2008-2009,
--                        (c) Bryan O'Sullivan 2009,
--                        (c) Duncan Coutts 2009
--
-----------------------------------------------------------------------------

module GHC.IO.Encoding.UTF32 (
  utf32, mkUTF32,
  utf32_decode,
  utf32_encode,

  utf32be, mkUTF32be,
  utf32be_decode,
  utf32be_encode,

  utf32le, mkUTF32le,
  utf32le_decode,
  utf32le_encode,
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
import GHC.IORef

-- -----------------------------------------------------------------------------
-- The UTF-32 codec: either UTF-32BE or UTF-32LE with a BOM

utf32  :: TextEncoding
utf32 = mkUTF32 ErrorOnCodingFailure

-- | @since 4.4.0.0
mkUTF32 :: CodingFailureMode -> TextEncoding
mkUTF32 cfm = TextEncoding { textEncodingName = "UTF-32",
                             mkTextDecoder = utf32_DF cfm,
                             mkTextEncoder = utf32_EF cfm }

utf32_DF :: CodingFailureMode -> IO (TextDecoder (Maybe DecodeBuffer#))
utf32_DF cfm = do
  seen_bom <- newIORef Nothing
  return (BufferCodec# {
             encode#   = utf32_decode seen_bom,
             recover#  = recoverDecode# cfm,
             close#    = return (),
             getState# = readIORef seen_bom,
             setState# = writeIORef seen_bom
          })

utf32_EF :: CodingFailureMode -> IO (TextEncoder Bool)
utf32_EF cfm = do
  done_bom <- newIORef False
  return (BufferCodec# {
             encode#   = utf32_encode done_bom,
             recover#  = recoverEncode# cfm,
             close#    = return (),
             getState# = readIORef done_bom,
             setState# = writeIORef done_bom
          })

utf32_encode :: IORef Bool -> EncodeBuffer#
utf32_encode done_bom input
  output@Buffer{ bufRaw=oraw, bufL=_, bufR=ow, bufSize=os }
  st0
 = do
  let !(# st1, b #) = unIO (readIORef done_bom) st0
  if b then utf32_native_encode input output st1
       else if os - ow < 4
               then (# st1,OutputUnderflow,input,output #)
               else do
               let !(# st2, () #) = unIO (writeIORef done_bom True) st1
                   !(# st3, () #) = unIO (writeWord8Buf oraw ow     bom0) st2
                   !(# st4, () #) = unIO (writeWord8Buf oraw (ow+1) bom1) st3
                   !(# st5, () #) = unIO (writeWord8Buf oraw (ow+2) bom2) st4
                   !(# st6, () #) = unIO (writeWord8Buf oraw (ow+3) bom3) st5
               utf32_native_encode input output{ bufR = ow+4 } st6

utf32_decode :: IORef (Maybe DecodeBuffer#) -> DecodeBuffer#
utf32_decode seen_bom
  input@Buffer{  bufRaw=iraw, bufL=ir, bufR=iw,  bufSize=_  }
  output
  st0
 = do
   let !(# st1, mb #) = unIO (readIORef seen_bom) st0
   case mb of
     Just decode -> decode input output st1
     Nothing ->
       if iw - ir < 4 then (# st1,InputUnderflow,input,output #) else do
       let !(# st2, c0 #) = unIO (readWord8Buf iraw  ir   ) st1
           !(# st3, c1 #) = unIO (readWord8Buf iraw (ir+1)) st2
           !(# st4, c2 #) = unIO (readWord8Buf iraw (ir+2)) st3
           !(# st5, c3 #) = unIO (readWord8Buf iraw (ir+3)) st4
       case () of
        _ | c0 == bom0 && c1 == bom1 && c2 == bom2 && c3 == bom3 ->
               let !(# st6, () #) = unIO (writeIORef seen_bom (Just utf32be_decode)) st5
               in utf32be_decode input{ bufL= ir+4 } output st6
        _ | c0 == bom3 && c1 == bom2 && c2 == bom1 && c3 == bom0 ->
               let !(# st6, () #) = unIO (writeIORef seen_bom (Just utf32le_decode)) st5
               in utf32le_decode input{ bufL= ir+4 } output st6
          | otherwise ->
               let !(# st6, () #) = unIO (writeIORef seen_bom (Just utf32_native_decode)) st5
               in utf32_native_decode input output st6


bom0, bom1, bom2, bom3 :: Word8
bom0 = 0
bom1 = 0
bom2 = 0xfe
bom3 = 0xff

-- choose UTF-32BE by default for UTF-32 output
utf32_native_decode :: DecodeBuffer#
utf32_native_decode = utf32be_decode

utf32_native_encode :: EncodeBuffer#
utf32_native_encode = utf32be_encode

-- -----------------------------------------------------------------------------
-- UTF32LE and UTF32BE

utf32be :: TextEncoding
utf32be = mkUTF32be ErrorOnCodingFailure

-- | @since 4.4.0.0
mkUTF32be :: CodingFailureMode -> TextEncoding
mkUTF32be cfm = TextEncoding { textEncodingName = "UTF-32BE",
                               mkTextDecoder = utf32be_DF cfm,
                               mkTextEncoder = utf32be_EF cfm }

utf32be_DF :: CodingFailureMode -> IO (TextDecoder ())
utf32be_DF cfm =
  return (BufferCodec# {
             encode#   = utf32be_decode,
             recover#  = recoverDecode# cfm,
             close#    = return (),
             getState# = return (),
             setState# = const $ return ()
          })

utf32be_EF :: CodingFailureMode -> IO (TextEncoder ())
utf32be_EF cfm =
  return (BufferCodec# {
             encode#   = utf32be_encode,
             recover#  = recoverEncode# cfm,
             close#    = return (),
             getState# = return (),
             setState# = const $ return ()
          })


utf32le :: TextEncoding
utf32le = mkUTF32le ErrorOnCodingFailure

-- | @since 4.4.0.0
mkUTF32le :: CodingFailureMode -> TextEncoding
mkUTF32le cfm = TextEncoding { textEncodingName = "UTF-32LE",
                               mkTextDecoder = utf32le_DF cfm,
                               mkTextEncoder = utf32le_EF cfm }

utf32le_DF :: CodingFailureMode -> IO (TextDecoder ())
utf32le_DF cfm =
  return (BufferCodec# {
             encode#   = utf32le_decode,
             recover#  = recoverDecode# cfm,
             close#    = return (),
             getState# = return (),
             setState# = const $ return ()
          })

utf32le_EF :: CodingFailureMode -> IO (TextEncoder ())
utf32le_EF cfm =
  return (BufferCodec# {
             encode#   = utf32le_encode,
             recover#  = recoverEncode# cfm,
             close#    = return (),
             getState# = return (),
             setState# = const $ return ()
          })


utf32be_decode :: DecodeBuffer#
utf32be_decode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
  st
 = let
       loop :: Int -> Int -> DecodingBuffer#
       loop !ir !ow st0
         | ow >= os    = done OutputUnderflow ir ow st0
         | iw - ir < 4 = done InputUnderflow  ir ow st0
         | otherwise = do
              let !(# st1, c0 #) = unIO (readWord8Buf iraw ir    ) st0
                  !(# st2, c1 #) = unIO (readWord8Buf iraw (ir+1)) st1
                  !(# st3, c2 #) = unIO (readWord8Buf iraw (ir+2)) st2
                  !(# st4, c3 #) = unIO (readWord8Buf iraw (ir+3)) st3
              let x1 = chr4 c0 c1 c2 c3
              if not (validate x1) then invalid st4 else do
              let !(# st5, ow' #) = unIO (writeCharBuf oraw ow x1) st4
              loop (ir+4) ow' st5
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

utf32le_decode :: DecodeBuffer#
utf32le_decode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
  st
 = let
       loop :: Int -> Int -> DecodingBuffer#
       loop !ir !ow st0
         | ow >= os    = done OutputUnderflow ir ow st0
         | iw - ir < 4 = done InputUnderflow  ir ow st0
         | otherwise = do
              let !(# st1, c0 #) = unIO (readWord8Buf iraw ir    ) st0
                  !(# st2, c1 #) = unIO (readWord8Buf iraw (ir+1)) st1
                  !(# st3, c2 #) = unIO (readWord8Buf iraw (ir+2)) st2
                  !(# st4, c3 #) = unIO (readWord8Buf iraw (ir+3)) st3
              let x1 = chr4 c3 c2 c1 c0
              if not (validate x1) then invalid st4 else do
              let !(# st5, ow' #) = unIO (writeCharBuf oraw ow x1) st4
              loop (ir+4) ow' st5
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

utf32be_encode :: EncodeBuffer#
utf32be_encode
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
        | ir >= iw    = done InputUnderflow  ir ow st0
        | os - ow < 4 = done OutputUnderflow ir ow st0
        | otherwise = do
           let !(# st1, (c,ir') #) = unIO (readCharBuf iraw ir) st0
           if isSurrogate c then done InvalidSequence ir ow st1 else do
             let (c0,c1,c2,c3) = ord4 c
                 !(# st2, () #) = unIO (writeWord8Buf oraw ow     c0) st1
                 !(# st3, () #) = unIO (writeWord8Buf oraw (ow+1) c1) st2
                 !(# st4, () #) = unIO (writeWord8Buf oraw (ow+2) c2) st3
                 !(# st5, () #) = unIO (writeWord8Buf oraw (ow+3) c3) st4
             loop ir' (ow+4) st5
    in
    loop ir0 ow0 st

utf32le_encode :: EncodeBuffer#
utf32le_encode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
  st
 = let
      done :: CodingProgress -> Int -> Int -> EncodingBuffer#
      done why !ir !ow st' =
        let !ri = if ir == iw then input{ bufL=0, bufR=0 } else input{ bufL=ir }
            !ro = output{ bufR=ow }
        in  (# st', why, ri, ro #)
      loop :: Int -> Int -> EncodingBuffer#
      loop !ir !ow st0
        | ir >= iw    = done InputUnderflow  ir ow st0
        | os - ow < 4 = done OutputUnderflow ir ow st0
        | otherwise = do
           let !(# st1, (c,ir') #) = unIO (readCharBuf iraw ir) st0
           if isSurrogate c then done InvalidSequence ir ow st1 else do
             let (c0,c1,c2,c3) = ord4 c
                 !(# st2, () #) = unIO (writeWord8Buf oraw ow     c3) st1
                 !(# st3, () #) = unIO (writeWord8Buf oraw (ow+1) c2) st2
                 !(# st4, () #) = unIO (writeWord8Buf oraw (ow+2) c1) st3
                 !(# st5, () #) = unIO (writeWord8Buf oraw (ow+3) c0) st4
             loop ir' (ow+4) st5
    in
    loop ir0 ow0 st

chr4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
    C# (chr# (z1# +# z2# +# z3# +# z4#))
    where
      !y1# = word2Int# (word8ToWord# x1#)
      !y2# = word2Int# (word8ToWord# x2#)
      !y3# = word2Int# (word8ToWord# x3#)
      !y4# = word2Int# (word8ToWord# x4#)
      !z1# = uncheckedIShiftL# y1# 24#
      !z2# = uncheckedIShiftL# y2# 16#
      !z3# = uncheckedIShiftL# y3# 8#
      !z4# = y4#
{-# INLINE chr4 #-}

ord4 :: Char -> (Word8,Word8,Word8,Word8)
ord4 c = (fromIntegral (x `shiftR` 24),
          fromIntegral (x `shiftR` 16),
          fromIntegral (x `shiftR` 8),
          fromIntegral x)
  where
    x = ord c
{-# INLINE ord4 #-}


validate    :: Char -> Bool
validate c = (x1 >= 0x0 && x1 < 0xD800) || (x1 > 0xDFFF && x1 <= 0x10FFFF)
   where x1 = ord c
{-# INLINE validate #-}
