{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, BangPatterns, NoImplicitPrelude,
             NondecreasingIndentation, MagicHash #-}

module GHC.IO.Encoding.CodePage(
#if defined(mingw32_HOST_OS)
                        codePageEncoding, mkCodePageEncoding,
                        localeEncoding, mkLocaleEncoding
#endif
                            ) where

#if !defined(mingw32_HOST_OS)
import GHC.Base () -- Build ordering
#else
import GHC.Base
import GHC.Show
import GHC.Num
import GHC.Enum
import GHC.Word
import GHC.IO (unsafePerformIO)
import GHC.IO.Encoding.Failure
import GHC.IO.Encoding.Types
import GHC.IO.Buffer
import Data.Bits
import Data.Maybe
import Data.List (lookup)

import qualified GHC.IO.Encoding.CodePage.API as API
import GHC.IO.Encoding.CodePage.Table

import GHC.IO.Encoding.UTF8 (mkUTF8)
import GHC.IO.Encoding.UTF16 (mkUTF16le, mkUTF16be)
import GHC.IO.Encoding.UTF32 (mkUTF32le, mkUTF32be)

#ifdef mingw32_HOST_OS
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

-- note CodePage = UInt which might not work on Win64.  But the Win32 package
-- also has this issue.
getCurrentCodePage :: IO Word32
getCurrentCodePage = do
    conCP <- getConsoleCP
    if conCP > 0
        then return conCP
        else getACP

-- Since the Win32 package depends on base, we have to import these ourselves:
foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleCP"
    getConsoleCP :: IO Word32

foreign import WINDOWS_CCONV unsafe "windows.h GetACP"
    getACP :: IO Word32

{-# NOINLINE currentCodePage #-}
currentCodePage :: Word32
currentCodePage = unsafePerformIO getCurrentCodePage

localeEncoding :: TextEncoding
localeEncoding = mkLocaleEncoding ErrorOnCodingFailure

mkLocaleEncoding :: CodingFailureMode -> TextEncoding
mkLocaleEncoding cfm = mkCodePageEncoding cfm currentCodePage


codePageEncoding :: Word32 -> TextEncoding
codePageEncoding = mkCodePageEncoding ErrorOnCodingFailure

mkCodePageEncoding :: CodingFailureMode -> Word32 -> TextEncoding
mkCodePageEncoding cfm 65001 = mkUTF8 cfm
mkCodePageEncoding cfm 1200 = mkUTF16le cfm
mkCodePageEncoding cfm 1201 = mkUTF16be cfm
mkCodePageEncoding cfm 12000 = mkUTF32le cfm
mkCodePageEncoding cfm 12001 = mkUTF32be cfm
mkCodePageEncoding cfm cp = maybe (API.mkCodePageEncoding cfm cp) (buildEncoding cfm cp) (lookup cp codePageMap)

buildEncoding :: CodingFailureMode -> Word32 -> CodePageArrays -> TextEncoding
buildEncoding cfm cp SingleByteCP {decoderArray = dec, encoderArray = enc}
  = TextEncoding {
      textEncodingName = "CP" ++ show cp
    , mkTextDecoder = return $ simpleCodec (recoverDecode cfm) $ decodeFromSingleByte dec
    , mkTextEncoder = return $ simpleCodec (recoverEncode cfm) $ encodeToSingleByte enc
    }

simpleCodec :: (Buffer from -> Buffer to -> IO (Buffer from, Buffer to))
            -> (Buffer from -> Buffer to -> IO (CodingProgress, Buffer from, Buffer to))
                -> BufferCodec from to ()
simpleCodec r f = BufferCodec {
    encode = f,
    recover = r,
    close = return (),
    getState = return (),
    setState = return
  }

decodeFromSingleByte :: ConvArray Char -> DecodeBuffer
decodeFromSingleByte convArr
    input@Buffer  { bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
    output@Buffer { bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
  = let
        done why !ir !ow = return (why,
                                   if ir==iw then input{ bufL=0, bufR=0}
                                             else input{ bufL=ir},
                                   output {bufR=ow})
        loop !ir !ow
            | ow >= os  = done OutputUnderflow ir ow
            | ir >= iw  = done InputUnderflow ir ow
            | otherwise = do
                b <- readWord8Buf iraw ir
                let c = lookupConv convArr b
                if c=='\0' && b /= 0 then invalid else do
                ow' <- writeCharBuf oraw ow c
                loop (ir+1) ow'
          where
            invalid = done InvalidSequence ir ow
    in loop ir0 ow0

encodeToSingleByte :: CompactArray Char Word8 -> EncodeBuffer
encodeToSingleByte CompactArray { encoderMax = maxChar,
                         encoderIndices = indices,
                         encoderValues = values }
    input@Buffer{ bufRaw=iraw, bufL=ir0, bufR=iw, bufSize=_ }
    output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
  = let
        done why !ir !ow = return (why,
                                   if ir==iw then input { bufL=0, bufR=0 }
                                             else input { bufL=ir },
                                   output {bufR=ow})
        loop !ir !ow
            | ow >= os  = done OutputUnderflow ir ow
            | ir >= iw  = done InputUnderflow ir ow
            | otherwise = do
                (c,ir') <- readCharBuf iraw ir
                case lookupCompact maxChar indices values c of
                    Nothing -> invalid
                    Just 0 | c /= '\0' -> invalid
                    Just b -> do
                        writeWord8Buf oraw ow b
                        loop ir' (ow+1)
            where
                invalid = done InvalidSequence ir ow
    in
    loop ir0 ow0


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

