{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , NondecreasingIndentation
           , MagicHash
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
  return (BufferCodec {
             encode   = utf8_decode,
             recover  = recoverDecode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

utf8_EF :: CodingFailureMode -> IO (TextEncoder ())
utf8_EF cfm =
  return (BufferCodec {
             encode   = utf8_encode,
             recover  = recoverEncode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
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
   return (BufferCodec {
             encode   = utf8_bom_decode ref,
             recover  = recoverDecode cfm,
             close    = return (),
             getState = readIORef ref,
             setState = writeIORef ref
          })

utf8_bom_EF :: CodingFailureMode -> IO (TextEncoder Bool)
utf8_bom_EF cfm = do
   ref <- newIORef True
   return (BufferCodec {
             encode   = utf8_bom_encode ref,
             recover  = recoverEncode cfm,
             close    = return (),
             getState = readIORef ref,
             setState = writeIORef ref
          })

utf8_bom_decode :: IORef Bool -> DecodeBuffer
utf8_bom_decode ref
  input@Buffer{  bufRaw=iraw, bufL=ir, bufR=iw,  bufSize=_  }
  output
 = do
   first <- readIORef ref
   if not first
      then utf8_decode input output
      else do
       let no_bom = do writeIORef ref False; utf8_decode input output
       if iw - ir < 1 then return (InputUnderflow,input,output) else do
       c0 <- readWord8Buf iraw ir
       if (c0 /= bom0) then no_bom else do
       if iw - ir < 2 then return (InputUnderflow,input,output) else do
       c1 <- readWord8Buf iraw (ir+1)
       if (c1 /= bom1) then no_bom else do
       if iw - ir < 3 then return (InputUnderflow,input,output) else do
       c2 <- readWord8Buf iraw (ir+2)
       if (c2 /= bom2) then no_bom else do
       -- found a BOM, ignore it and carry on
       writeIORef ref False
       utf8_decode input{ bufL = ir + 3 } output

utf8_bom_encode :: IORef Bool -> EncodeBuffer
utf8_bom_encode ref input
  output@Buffer{ bufRaw=oraw, bufL=_, bufR=ow, bufSize=os }
 = do
  b <- readIORef ref
  if not b then utf8_encode input output
           else if os - ow < 3
                  then return (OutputUnderflow,input,output)
                  else do
                    writeIORef ref False
                    writeWord8Buf oraw ow     bom0
                    writeWord8Buf oraw (ow+1) bom1
                    writeWord8Buf oraw (ow+2) bom2
                    utf8_encode input output{ bufR = ow+3 }

bom0, bom1, bom2 :: Word8
bom0 = 0xef
bom1 = 0xbb
bom2 = 0xbf

utf8_decode :: DecodeBuffer
utf8_decode 
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let 
       loop !ir !ow
         | ow >= os = done OutputUnderflow ir ow
         | ir >= iw = done InputUnderflow ir ow
         | otherwise = do
              c0 <- readWord8Buf iraw ir
              case c0 of
                _ | c0 <= 0x7f -> do 
                           ow' <- writeCharBuf oraw ow (unsafeChr (fromIntegral c0))
                           loop (ir+1) ow'
                  | c0 >= 0xc0 && c0 <= 0xc1 -> invalid -- Overlong forms
                  | c0 >= 0xc2 && c0 <= 0xdf ->
                           if iw - ir < 2 then done InputUnderflow ir ow else do
                           c1 <- readWord8Buf iraw (ir+1)
                           if (c1 < 0x80 || c1 >= 0xc0) then invalid else do
                           ow' <- writeCharBuf oraw ow (chr2 c0 c1)
                           loop (ir+2) ow'
                  | c0 >= 0xe0 && c0 <= 0xef ->
                      case iw - ir of
                        1 -> done InputUnderflow ir ow
                        2 -> do -- check for an error even when we don't have
                                -- the full sequence yet (#3341)
                           c1 <- readWord8Buf iraw (ir+1)
                           if not (validate3 c0 c1 0x80) 
                              then invalid else done InputUnderflow ir ow
                        _ -> do
                           c1 <- readWord8Buf iraw (ir+1)
                           c2 <- readWord8Buf iraw (ir+2)
                           if not (validate3 c0 c1 c2) then invalid else do
                           ow' <- writeCharBuf oraw ow (chr3 c0 c1 c2)
                           loop (ir+3) ow'
                  | c0 >= 0xf0 ->
                      case iw - ir of
                        1 -> done InputUnderflow ir ow
                        2 -> do -- check for an error even when we don't have
                                -- the full sequence yet (#3341)
                           c1 <- readWord8Buf iraw (ir+1)
                           if not (validate4 c0 c1 0x80 0x80)
                              then invalid else done InputUnderflow ir ow
                        3 -> do
                           c1 <- readWord8Buf iraw (ir+1)
                           c2 <- readWord8Buf iraw (ir+2)
                           if not (validate4 c0 c1 c2 0x80)
                              then invalid else done InputUnderflow ir ow
                        _ -> do
                           c1 <- readWord8Buf iraw (ir+1)
                           c2 <- readWord8Buf iraw (ir+2)
                           c3 <- readWord8Buf iraw (ir+3)
                           if not (validate4 c0 c1 c2 c3) then invalid else do
                           ow' <- writeCharBuf oraw ow (chr4 c0 c1 c2 c3)
                           loop (ir+4) ow'
                  | otherwise ->
                           invalid
         where
           invalid = done InvalidSequence ir ow

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       done why !ir !ow = return (why,
                                  if ir == iw then input{ bufL=0, bufR=0 }
                                              else input{ bufL=ir },
                                  output{ bufR=ow })
   in
   loop ir0 ow0

utf8_encode :: EncodeBuffer
utf8_encode
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
           case ord c of
             x | x <= 0x7F   -> do
                    writeWord8Buf oraw ow (fromIntegral x)
                    loop ir' (ow+1)
               | x <= 0x07FF ->
                    if os - ow < 2 then done OutputUnderflow ir ow else do
                    let (c1,c2) = ord2 c
                    writeWord8Buf oraw ow     c1
                    writeWord8Buf oraw (ow+1) c2
                    loop ir' (ow+2)
               | x <= 0xFFFF -> if isSurrogate c then done InvalidSequence ir ow else do
                    if os - ow < 3 then done OutputUnderflow ir ow else do
                    let (c1,c2,c3) = ord3 c
                    writeWord8Buf oraw ow     c1
                    writeWord8Buf oraw (ow+1) c2
                    writeWord8Buf oraw (ow+2) c3
                    loop ir' (ow+3)
               | otherwise -> do
                    if os - ow < 4 then done OutputUnderflow ir ow else do
                    let (c1,c2,c3,c4) = ord4 c
                    writeWord8Buf oraw ow     c1
                    writeWord8Buf oraw (ow+1) c2
                    writeWord8Buf oraw (ow+2) c3
                    writeWord8Buf oraw (ow+3) c4
                    loop ir' (ow+4)
   in
   loop ir0 ow0

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
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
      !z2# = y2# -# 0x80#
{-# INLINE chr2 #-}

chr3          :: Word8 -> Word8 -> Word8 -> Char
chr3 (W8# x1#) (W8# x2#) (W8# x3#) = C# (chr# (z1# +# z2# +# z3#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !y3# = word2Int# x3#
      !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
      !z3# = y3# -# 0x80#
{-# INLINE chr3 #-}

chr4             :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
    C# (chr# (z1# +# z2# +# z3# +# z4#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !y3# = word2Int# x3#
      !y4# = word2Int# x4#
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

