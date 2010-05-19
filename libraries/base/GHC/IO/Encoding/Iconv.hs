{-# OPTIONS_GHC -XNoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding.Iconv
-- Copyright   :  (c) The University of Glasgow, 2008-2009
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- This module provides text encoding/decoding using iconv
--
-----------------------------------------------------------------------------

-- #hide
module GHC.IO.Encoding.Iconv (
#if !defined(mingw32_HOST_OS)
   mkTextEncoding,
   latin1,
   utf8, 
   utf16, utf16le, utf16be,
   utf32, utf32le, utf32be,
   localeEncoding
#endif
 ) where

#include "MachDeps.h"
#include "HsBaseConfig.h"

#if !defined(mingw32_HOST_OS)

import Foreign
import Foreign.C
import Data.Maybe
import GHC.Base
import GHC.IO.Buffer
import GHC.IO.Encoding.Types
import GHC.Num
import GHC.Show
import GHC.Real
import System.Posix.Internals

c_DEBUG_DUMP :: Bool
c_DEBUG_DUMP = False

iconv_trace :: String -> IO ()
iconv_trace s
 | c_DEBUG_DUMP = puts s
 | otherwise    = return ()

puts :: String -> IO ()
puts s = do _ <- withCStringLen (s ++ "\n") $ \(p, len) ->
                     c_write 1 (castPtr p) (fromIntegral len)
            return ()

-- -----------------------------------------------------------------------------
-- iconv encoders/decoders

{-# NOINLINE latin1 #-}
latin1 :: TextEncoding
latin1 = unsafePerformIO (mkTextEncoding "Latin1")

{-# NOINLINE utf8 #-}
utf8 :: TextEncoding
utf8 = unsafePerformIO (mkTextEncoding "UTF8")

{-# NOINLINE utf16 #-}
utf16 :: TextEncoding
utf16 = unsafePerformIO (mkTextEncoding "UTF16")

{-# NOINLINE utf16le #-}
utf16le :: TextEncoding
utf16le = unsafePerformIO (mkTextEncoding "UTF16LE")

{-# NOINLINE utf16be #-}
utf16be :: TextEncoding
utf16be = unsafePerformIO (mkTextEncoding "UTF16BE")

{-# NOINLINE utf32 #-}
utf32 :: TextEncoding
utf32 = unsafePerformIO (mkTextEncoding "UTF32")

{-# NOINLINE utf32le #-}
utf32le :: TextEncoding
utf32le = unsafePerformIO (mkTextEncoding "UTF32LE")

{-# NOINLINE utf32be #-}
utf32be :: TextEncoding
utf32be = unsafePerformIO (mkTextEncoding "UTF32BE")

{-# NOINLINE localeEncoding #-}
localeEncoding :: TextEncoding
localeEncoding = unsafePerformIO $ do
   -- Use locale_charset() or nl_langinfo(CODESET) to get the encoding
   -- if we have either of them.
   cstr <- c_localeEncoding
   r <- peekCString cstr
   mkTextEncoding r

-- We hope iconv_t is a storable type.  It should be, since it has at least the
-- value -1, which is a possible return value from iconv_open.
type IConv = CLong -- ToDo: (#type iconv_t)

foreign import ccall unsafe "hs_iconv_open"
    hs_iconv_open :: CString -> CString -> IO IConv

foreign import ccall unsafe "hs_iconv_close"
    hs_iconv_close :: IConv -> IO CInt

foreign import ccall unsafe "hs_iconv"
    hs_iconv :: IConv -> Ptr CString -> Ptr CSize -> Ptr CString -> Ptr CSize
	  -> IO CSize

foreign import ccall unsafe "localeEncoding"
    c_localeEncoding :: IO CString

haskellChar :: String
#ifdef WORDS_BIGENDIAN
haskellChar | charSize == 2 = "UTF-16BE"
            | otherwise     = "UTF-32BE"
#else
haskellChar | charSize == 2 = "UTF-16LE"
            | otherwise     = "UTF-32LE"
#endif

char_shift :: Int
char_shift | charSize == 2 = 1
           | otherwise     = 2

mkTextEncoding :: String -> IO TextEncoding
mkTextEncoding charset = do
  return (TextEncoding { 
		mkTextDecoder = newIConv charset haskellChar iconvDecode,
		mkTextEncoder = newIConv haskellChar charset iconvEncode})

newIConv :: String -> String
   -> (IConv -> Buffer a -> Buffer b -> IO (Buffer a, Buffer b))
   -> IO (BufferCodec a b ())
newIConv from to fn =
  withCString from $ \ from_str ->
  withCString to   $ \ to_str -> do
    iconvt <- throwErrnoIfMinus1 "mkTextEncoding" $ hs_iconv_open to_str from_str
    let iclose = throwErrnoIfMinus1_ "Iconv.close" $ hs_iconv_close iconvt
    return BufferCodec{
                encode = fn iconvt,
                close  = iclose,
                -- iconv doesn't supply a way to save/restore the state
                getState = return (),
                setState = const $ return ()
                }

iconvDecode :: IConv -> Buffer Word8 -> Buffer CharBufElem
	     -> IO (Buffer Word8, Buffer CharBufElem)
iconvDecode iconv_t ibuf obuf = iconvRecode iconv_t ibuf 0 obuf char_shift

iconvEncode :: IConv -> Buffer CharBufElem -> Buffer Word8
	     -> IO (Buffer CharBufElem, Buffer Word8)
iconvEncode iconv_t ibuf obuf = iconvRecode iconv_t ibuf char_shift obuf 0

iconvRecode :: IConv -> Buffer a -> Int -> Buffer b -> Int 
  -> IO (Buffer a, Buffer b)
iconvRecode iconv_t
  input@Buffer{  bufRaw=iraw, bufL=ir, bufR=iw, bufSize=_  }  iscale
  output@Buffer{ bufRaw=oraw, bufL=_,  bufR=ow, bufSize=os }  oscale
  = do
    iconv_trace ("haskelChar=" ++ show haskellChar)
    iconv_trace ("iconvRecode before, input=" ++ show (summaryBuffer input))
    iconv_trace ("iconvRecode before, output=" ++ show (summaryBuffer output))
    withRawBuffer iraw $ \ piraw -> do
    withRawBuffer oraw $ \ poraw -> do
    with (piraw `plusPtr` (ir `shiftL` iscale)) $ \ p_inbuf -> do
    with (poraw `plusPtr` (ow `shiftL` oscale)) $ \ p_outbuf -> do
    with (fromIntegral ((iw-ir) `shiftL` iscale)) $ \ p_inleft -> do
    with (fromIntegral ((os-ow) `shiftL` oscale)) $ \ p_outleft -> do
      res <- hs_iconv iconv_t p_inbuf p_inleft p_outbuf p_outleft
      new_inleft  <- peek p_inleft
      new_outleft <- peek p_outleft
      let 
	  new_inleft'  = fromIntegral new_inleft `shiftR` iscale
	  new_outleft' = fromIntegral new_outleft `shiftR` oscale
	  new_input  
            | new_inleft == 0  = input { bufL = 0, bufR = 0 }
	    | otherwise        = input { bufL = iw - new_inleft' }
	  new_output = output{ bufR = os - new_outleft' }
      iconv_trace ("iconv res=" ++ show res)
      iconv_trace ("iconvRecode after,  input=" ++ show (summaryBuffer new_input))
      iconv_trace ("iconvRecode after,  output=" ++ show (summaryBuffer new_output))
      if (res /= -1)
	then do -- all input translated
	   return (new_input, new_output)
	else do
      errno <- getErrno
      case errno of
	e |  e == eINVAL 
          || (e == e2BIG || e == eILSEQ) && new_inleft' /= (iw-ir) -> do
            iconv_trace ("iconv ignoring error: " ++ show (errnoToIOError "iconv" e Nothing Nothing))
		-- Output overflow is relatively harmless, unless
		-- we made no progress at all.  
                --
                -- Similarly, we ignore EILSEQ unless we converted no
                -- characters.  Sometimes iconv reports EILSEQ for a
                -- character in the input even when there is no room
                -- in the output; in this case we might be about to
                -- change the encoding anyway, so the following bytes
                -- could very well be in a different encoding.
                -- This also helps with pinpointing EILSEQ errors: we
                -- don't report it until the rest of the characters in
                -- the buffer have been drained.
            return (new_input, new_output)

	_other -> 
		throwErrno "iconvRecoder" 
			-- illegal sequence, or some other error

#endif /* !mingw32_HOST_OS */
