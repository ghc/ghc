{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , NondecreasingIndentation
           , MagicHash
           , UnboxedTuples
  #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding.UTF8
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- UTF-8 Codec for the IO library
--
-- This is one of several UTF-8 implementations provided by GHC; see Note
-- [GHC's many UTF-8 implementations] in "GHC.Encoding.UTF8" for an
-- overview.
--
-- Portions Copyright   : (c) Tom Harper 2008-2009,
--                        (c) Bryan O'Sullivan 2009,
--                        (c) Duncan Coutts 2009
--
-----------------------------------------------------------------------------

module GHC.IO.Encoding.UTF8 (
  utf8, mkUTF8,
  utf8_bom, mkUTF8_bom
  ) where

import GHC.Base
import GHC.Real
import GHC.Num
import GHC.IORef
-- import GHC.IO
import GHC.IO.Buffer
import GHC.IO.Encoding.Failure
import GHC.IO.Encoding.Types
import GHC.Word
import Data.Bits

utf8 :: TextEncoding
utf8 = mkUTF8 ErrorOnCodingFailure

-- | @since 4.4.0.0
mkUTF8 :: CodingFailureMode -> TextEncoding
mkUTF8 cfm = TextEncoding { textEncodingName = "UTF-8",
                            mkTextDecoder = utf8_DF cfm,
                            mkTextEncoder = utf8_EF cfm }


utf8_DF :: CodingFailureMode -> IO (TextDecoder ())
utf8_DF cfm =
  return (BufferCodec# {
             encode#   = utf8_decode,
             recover#  = recoverDecode# cfm,
             close#    = return (),
             getState# = return (),
             setState# = const $ return ()
          })

utf8_EF :: CodingFailureMode -> IO (TextEncoder ())
utf8_EF cfm =
  return (BufferCodec# {
             encode#   = utf8_encode,
             recover#  = recoverEncode# cfm,
             close#    = return (),
             getState# = return (),
             setState# = const $ return ()
          })

utf8_bom :: TextEncoding
utf8_bom = mkUTF8_bom ErrorOnCodingFailure

mkUTF8_bom :: CodingFailureMode -> TextEncoding
mkUTF8_bom cfm = TextEncoding { textEncodingName = "UTF-8BOM",
                                mkTextDecoder = utf8_bom_DF cfm,
                                mkTextEncoder = utf8_bom_EF cfm }

utf8_bom_DF :: CodingFailureMode -> IO (TextDecoder Bool)
utf8_bom_DF cfm = do
   ref <- newIORef True
   return (BufferCodec# {
             encode#   = utf8_bom_decode ref,
             recover#  = recoverDecode# cfm,
             close#    = return (),
             getState# = readIORef ref,
             setState# = writeIORef ref
          })

utf8_bom_EF :: CodingFailureMode -> IO (TextEncoder Bool)
utf8_bom_EF cfm = do
   ref <- newIORef True
   return (BufferCodec# {
             encode#   = utf8_bom_encode ref,
             recover#  = recoverEncode# cfm,
             close#    = return (),
             getState# = readIORef ref,
             setState# = writeIORef ref
          })

utf8_bom_decode :: IORef Bool -> DecodeBuffer#
utf8_bom_decode ref
  input@Buffer{  bufRaw=iraw, bufL=ir, bufR=iw,  bufSize=_  }
  output
  st0
 = do
   let !(# st1, first #) = unIO (readIORef ref) st0
   if not first
      then utf8_decode input output st1
      else do
       let no_bom = let !(# st', () #) = unIO (writeIORef ref False) st1 in utf8_decode input output st'
       if iw - ir < 1 then (# st1,InputUnderflow,input,output #) else do
       let !(# st2, c0 #) = unIO (readWord8Buf iraw ir) st1
       if (c0 /= bom0) then no_bom else do
       if iw - ir < 2 then (# st2,InputUnderflow,input,output #) else do
       let !(# st3, c1 #) = unIO (readWord8Buf iraw (ir+1)) st2
       if (c1 /= bom1) then no_bom else do
       if iw - ir < 3 then (# st3,InputUnderflow,input,output #) else do
       let !(# st4, c2 #) = unIO (readWord8Buf iraw (ir+2)) st3
       if (c2 /= bom2) then no_bom else do
       -- found a BOM, ignore it and carry on
       let !(# st5, () #) = unIO (writeIORef ref False) st4
       utf8_decode input{ bufL = ir + 3 } output st5

utf8_bom_encode :: IORef Bool -> EncodeBuffer#
utf8_bom_encode ref input
  output@Buffer{ bufRaw=oraw, bufL=_, bufR=ow, bufSize=os }
  st0
 = do
  let !(# st1, b #) = unIO (readIORef ref) st0
  if not b then utf8_encode input output st1
           else if os - ow < 3
                  then (# st1,OutputUnderflow,input,output #)
                  else do
                    let !(# st2, () #) = unIO (writeIORef ref False)           st1
                        !(# st3, () #) = unIO (writeWord8Buf oraw ow     bom0) st2
                        !(# st4, () #) = unIO (writeWord8Buf oraw (ow+1) bom1) st3
                        !(# st5, () #) = unIO (writeWord8Buf oraw (ow+2) bom2) st4
                    utf8_encode input output{ bufR = ow+3 } st5

bom0, bom1, bom2 :: Word8
bom0 = 0xef
bom1 = 0xbb
bom2 = 0xbf

utf8_decode :: DecodeBuffer#
utf8_decode
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
              case c0 of
                _ | c0 <= 0x7f -> do
                           let !(# st2, ow' #) = unIO (writeCharBuf oraw ow (unsafeChr (fromIntegral c0))) st1
                           loop (ir+1) ow' st2
                  | c0 >= 0xc0 && c0 <= 0xc1 -> invalid st1 -- Overlong forms
                  | c0 >= 0xc2 && c0 <= 0xdf ->
                           if iw - ir < 2 then done InputUnderflow ir ow st1 else do
                           let !(# st2, c1 #) = unIO (readWord8Buf iraw (ir+1)) st1
                           if (c1 < 0x80 || c1 >= 0xc0) then invalid st2 else do
                           let !(# st3, ow' #) = unIO (writeCharBuf oraw ow (chr2 c0 c1)) st2
                           loop (ir+2) ow' st3
                  | c0 >= 0xe0 && c0 <= 0xef ->
                      case iw - ir of
                        1 -> done InputUnderflow ir ow st1
                        2 -> do -- check for an error even when we don't have
                                -- the full sequence yet (#3341)
                           let !(# st2, c1 #) = unIO (readWord8Buf iraw (ir+1)) st1
                           if not (validate3 c0 c1 0x80)
                              then invalid st2 else done InputUnderflow ir ow st2
                        _ -> do
                           let !(# st2, c1 #) = unIO (readWord8Buf iraw (ir+1)) st1
                           let !(# st3, c2 #) = unIO (readWord8Buf iraw (ir+2)) st2
                           if not (validate3 c0 c1 c2) then invalid st3 else do
                           let !(# st4, ow' #) = unIO (writeCharBuf oraw ow (chr3 c0 c1 c2)) st3
                           loop (ir+3) ow' st4
                  | c0 >= 0xf0 ->
                      case iw - ir of
                        1 -> done InputUnderflow ir ow st1
                        2 -> do -- check for an error even when we don't have
                                -- the full sequence yet (#3341)
                           let !(# st2, c1 #) = unIO (readWord8Buf iraw (ir+1)) st1
                           if not (validate4 c0 c1 0x80 0x80)
                              then invalid st2 else done InputUnderflow ir ow st2
                        3 -> do
                           let !(# st2, c1 #) = unIO (readWord8Buf iraw (ir+1)) st1
                               !(# st3, c2 #) = unIO (readWord8Buf iraw (ir+2)) st2
                           if not (validate4 c0 c1 c2 0x80)
                              then invalid st3 else done InputUnderflow ir ow st3
                        _ -> do
                           let !(# st2, c1 #) = unIO (readWord8Buf iraw (ir+1)) st1
                               !(# st3, c2 #) = unIO (readWord8Buf iraw (ir+2)) st2
                               !(# st4, c3 #) = unIO (readWord8Buf iraw (ir+3)) st3
                           if not (validate4 c0 c1 c2 c3) then invalid st4 else do
                           let !(# st5, ow' #) = unIO (writeCharBuf oraw ow (chr4 c0 c1 c2 c3)) st4
                           loop (ir+4) ow' st5
                  | otherwise ->
                           invalid st1
         where
           invalid :: DecodingBuffer#
           invalid st' = done InvalidSequence ir ow st'

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       {-# NOINLINE done #-}
       done :: CodingProgress -> Int -> Int -> DecodingBuffer#
       done why !ir !ow st' =
         let !ri = if ir == iw then input{ bufL = 0, bufR = 0} else input{ bufL = ir }
             !ro = output { bufR = ow }
         in (# st', why, ri, ro #)
   in
   loop ir0 ow0 st

utf8_encode :: EncodeBuffer#
utf8_encode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
  st
 = let
      {-# NOINLINE done #-}
      done :: CodingProgress -> Int -> Int -> EncodingBuffer#
      done why !ir !ow st' =
        let !ri = if ir == iw then input{ bufL = 0, bufR = 0 } else input{ bufL = ir }
            !ro = output{ bufR = ow }
        in  (# st', why, ri, ro #)
      loop :: Int -> Int -> EncodingBuffer#
      loop !ir !ow st0
        | ow >= os = done OutputUnderflow ir ow st0
        | ir >= iw = done InputUnderflow ir ow st0
        | otherwise = do
           let !(# st1, (c,ir') #) = unIO (readCharBuf iraw ir) st0
           case ord c of
             x | x <= 0x7F   -> do
                     let !(# st2, () #) = unIO (writeWord8Buf oraw ow (fromIntegral x)) st1
                     loop ir' (ow+1) st2
               | x <= 0x07FF ->
                    if os - ow < 2 then done OutputUnderflow ir ow st1 else do
                    let (c1,c2) = ord2 c
                        !(# st2, () #) = unIO (writeWord8Buf oraw ow     c1) st1
                        !(# st3, () #) = unIO (writeWord8Buf oraw (ow+1) c2) st2
                    loop ir' (ow+2) st3
               | x <= 0xFFFF -> if isSurrogate c then done InvalidSequence ir ow st1 else do
                    if os - ow < 3 then done OutputUnderflow ir ow st1 else do
                    let (c1,c2,c3) = ord3 c
                        !(# st2, () #) = unIO (writeWord8Buf oraw ow     c1) st1
                        !(# st3, () #) = unIO (writeWord8Buf oraw (ow+1) c2) st2
                        !(# st4, () #) = unIO (writeWord8Buf oraw (ow+2) c3) st3
                    loop ir' (ow+3) st4
               | otherwise -> do
                    if os - ow < 4 then done OutputUnderflow ir ow st1 else do
                    let (c1,c2,c3,c4) = ord4 c
                        !(# st2, () #) = unIO (writeWord8Buf oraw ow     c1) st1
                        !(# st3, () #) = unIO (writeWord8Buf oraw (ow+1) c2) st2
                        !(# st4, () #) = unIO (writeWord8Buf oraw (ow+2) c3) st3
                        !(# st5, () #) = unIO (writeWord8Buf oraw (ow+3) c4) st4
                    loop ir' (ow+4) st5
   in
   loop ir0 ow0 st

-- -----------------------------------------------------------------------------
-- UTF-8 primitives, lifted from Data.Text.Fusion.Utf8

ord2   :: Char -> (Word8,Word8)
ord2 c = assert (n >= 0x80 && n <= 0x07ff) (x1,x2)
    where
      n  = ord c
      x1 = fromIntegral $ (n `shiftR` 6) + 0xC0
      x2 = fromIntegral $ (n .&. 0x3F)   + 0x80

ord3   :: Char -> (Word8,Word8,Word8)
ord3 c = assert (n >= 0x0800 && n <= 0xffff) (x1,x2,x3)
    where
      n  = ord c
      x1 = fromIntegral $ (n `shiftR` 12) + 0xE0
      x2 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x3 = fromIntegral $ (n .&. 0x3F) + 0x80

ord4   :: Char -> (Word8,Word8,Word8,Word8)
ord4 c = assert (n >= 0x10000) (x1,x2,x3,x4)
    where
      n  = ord c
      x1 = fromIntegral $ (n `shiftR` 18) + 0xF0
      x2 = fromIntegral $ ((n `shiftR` 12) .&. 0x3F) + 0x80
      x3 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x4 = fromIntegral $ (n .&. 0x3F) + 0x80

chr2       :: Word8 -> Word8 -> Char
chr2 (W8# x1#) (W8# x2#) = C# (chr# (z1# +# z2#))
    where
      !y1# = word2Int# (word8ToWord# x1#)
      !y2# = word2Int# (word8ToWord# x2#)
      !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
      !z2# = y2# -# 0x80#
{-# INLINE chr2 #-}

chr3          :: Word8 -> Word8 -> Word8 -> Char
chr3 (W8# x1#) (W8# x2#) (W8# x3#) = C# (chr# (z1# +# z2# +# z3#))
    where
      !y1# = word2Int# (word8ToWord# x1#)
      !y2# = word2Int# (word8ToWord# x2#)
      !y3# = word2Int# (word8ToWord# x3#)
      !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
      !z3# = y3# -# 0x80#
{-# INLINE chr3 #-}

chr4             :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
    C# (chr# (z1# +# z2# +# z3# +# z4#))
    where
      !y1# = word2Int# (word8ToWord# x1#)
      !y2# = word2Int# (word8ToWord# x2#)
      !y3# = word2Int# (word8ToWord# x3#)
      !y4# = word2Int# (word8ToWord# x4#)
      !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
      !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
      !z4# = y4# -# 0x80#
{-# INLINE chr4 #-}

between :: Word8                -- ^ byte to check
        -> Word8                -- ^ lower bound
        -> Word8                -- ^ upper bound
        -> Bool
between x y z = x >= y && x <= z
{-# INLINE between #-}

validate3          :: Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate3 #-}
validate3 x1 x2 x3 = validate3_1 ||
                     validate3_2 ||
                     validate3_3 ||
                     validate3_4
  where
    validate3_1 = (x1 == 0xE0) &&
                  between x2 0xA0 0xBF &&
                  between x3 0x80 0xBF
    validate3_2 = between x1 0xE1 0xEC &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF
    validate3_3 = x1 == 0xED &&
                  between x2 0x80 0x9F &&
                  between x3 0x80 0xBF
    validate3_4 = between x1 0xEE 0xEF &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF

validate4             :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate4 #-}
validate4 x1 x2 x3 x4 = validate4_1 ||
                        validate4_2 ||
                        validate4_3
  where
    validate4_1 = x1 == 0xF0 &&
                  between x2 0x90 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_2 = between x1 0xF1 0xF3 &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_3 = x1 == 0xF4 &&
                  between x2 0x80 0x8F &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
