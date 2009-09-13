{-# LANGUAGE BangPatterns #-}
module GHC.IO.Encoding.CodePage(
#if !defined(mingw32_HOST_OS)
 ) where
#else
                        codePageEncoding,
                        localeEncoding
                            ) where

import GHC.Base
import GHC.Num
import GHC.Enum
import GHC.Word
import GHC.IO (unsafePerformIO)
import GHC.IO.Encoding.Types
import GHC.IO.Buffer
import GHC.IO.Exception
import Data.Bits
import Data.Maybe
import Data.List (lookup)

import GHC.IO.Encoding.CodePage.Table

import GHC.IO.Encoding.Latin1 (latin1)
import GHC.IO.Encoding.UTF8 (utf8)
import GHC.IO.Encoding.UTF16 (utf16le, utf16be)
import GHC.IO.Encoding.UTF32 (utf32le, utf32be)

-- note CodePage = UInt which might not work on Win64.  But the Win32 package
-- also has this issue.
getCurrentCodePage :: IO Word32
getCurrentCodePage = do
    conCP <- getConsoleCP
    if conCP > 0
        then return conCP
        else getACP

-- Since the Win32 package depends on base, we have to import these ourselves:
foreign import stdcall unsafe "windows.h GetConsoleCP"
    getConsoleCP :: IO Word32

foreign import stdcall unsafe "windows.h GetACP"
    getACP :: IO Word32

{-# NOINLINE localeEncoding #-}
localeEncoding :: TextEncoding
localeEncoding = unsafePerformIO $ fmap codePageEncoding getCurrentCodePage
    

codePageEncoding :: Word32 -> TextEncoding
codePageEncoding 65001 = utf8
codePageEncoding 1200 = utf16le
codePageEncoding 1201 = utf16be
codePageEncoding 12000 = utf32le
codePageEncoding 12001 = utf32be
codePageEncoding cp = maybe latin1 buildEncoding (lookup cp codePageMap)

buildEncoding :: CodePageArrays -> TextEncoding
buildEncoding SingleByteCP {decoderArray = dec, encoderArray = enc}
  = TextEncoding {
    mkTextDecoder = return $ simpleCodec
        $ decodeFromSingleByte dec
    , mkTextEncoder = return $ simpleCodec $ encodeToSingleByte enc
    }

simpleCodec :: (Buffer from -> Buffer to -> IO (Buffer from, Buffer to))
                -> BufferCodec from to ()
simpleCodec f = BufferCodec {encode = f, close = return (), getState = return (),
                                    setState = return }

decodeFromSingleByte :: ConvArray Char -> DecodeBuffer
decodeFromSingleByte convArr
    input@Buffer  { bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
    output@Buffer { bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
  = let
        done !ir !ow = return (if ir==iw then input{ bufL=0, bufR=0}
                                            else input{ bufL=ir},
                                    output {bufR=ow})
        loop !ir !ow
            | ow >= os  || ir >= iw     = done ir ow
            | otherwise = do
                b <- readWord8Buf iraw ir
                let c = lookupConv convArr b
                if c=='\0' && b /= 0 then invalid else do
                ow' <- writeCharBuf oraw ow c
                loop (ir+1) ow'
          where
            invalid = if ir > ir0 then done ir ow else ioe_decodingError
    in loop ir0 ow0

encodeToSingleByte :: CompactArray Char Word8 -> EncodeBuffer
encodeToSingleByte CompactArray { encoderMax = maxChar,
                         encoderIndices = indices,
                         encoderValues = values }
    input@Buffer{ bufRaw=iraw, bufL=ir0, bufR=iw, bufSize=_ }
    output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
  = let
        done !ir !ow = return (if ir==iw then input { bufL=0, bufR=0 }
                                            else input { bufL=ir },
                                output {bufR=ow})
        loop !ir !ow
            | ow >= os || ir >= iw  = done ir ow
            | otherwise = do
                (c,ir') <- readCharBuf iraw ir
                case lookupCompact maxChar indices values c of
                    Nothing -> invalid
                    Just 0 | c /= '\0' -> invalid
                    Just b -> do
                        writeWord8Buf oraw ow b
                        loop ir' (ow+1)
            where
                invalid = if ir > ir0 then done ir ow else ioe_encodingError
    in
    loop ir0 ow0

ioe_decodingError :: IO a
ioe_decodingError = ioException
    (IOError Nothing InvalidArgument "codePageEncoding"
        "invalid code page byte sequence" Nothing Nothing)

ioe_encodingError :: IO a
ioe_encodingError = ioException
    (IOError Nothing InvalidArgument "codePageEncoding"
        "character is not in the code page" Nothing Nothing)


--------------------------------------------
-- Array access functions

-- {-# INLINE lookupConv #-}
lookupConv :: ConvArray Char -> Word8 -> Char
lookupConv a = indexChar a . fromEnum

{-# INLINE lookupCompact #-}
lookupCompact :: Char -> ConvArray Int -> ConvArray Word8 -> Char -> Maybe Word8
lookupCompact maxVal indexes values x
    | x > maxVal = Nothing
    | otherwise = Just $ indexWord8 values $ j + (i .&. mask)
  where
    i = fromEnum x
    mask = (1 `shiftL` n) - 1
    k = i `shiftR` n
    j = indexInt indexes k
    n = blockBitSize

{-# INLINE indexInt #-}
indexInt :: ConvArray Int -> Int -> Int
indexInt (ConvArray p) (I# i) = I# (indexInt16OffAddr# p i)

{-# INLINE indexWord8 #-}
indexWord8 :: ConvArray Word8 -> Int -> Word8
indexWord8 (ConvArray p) (I# i) = W8# (indexWord8OffAddr# p i)

{-# INLINE indexChar #-}
indexChar :: ConvArray Char -> Int -> Char
indexChar (ConvArray p) (I# i) = C# (chr# (indexInt16OffAddr# p i))

#endif
