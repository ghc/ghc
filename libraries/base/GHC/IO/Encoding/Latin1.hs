{-# OPTIONS_GHC  -XNoImplicitPrelude -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
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
-- UTF-32 Codecs for the IO library
--
-- Portions Copyright   : (c) Tom Harper 2008-2009,
--                        (c) Bryan O'Sullivan 2009,
--                        (c) Duncan Coutts 2009
--
-----------------------------------------------------------------------------

module GHC.IO.Encoding.Latin1 (
  latin1,
  latin1_checked,
  latin1_decode,
  latin1_encode,
  latin1_checked_encode,
  ) where

import GHC.Base
import GHC.Real
import GHC.Num
-- import GHC.IO
import GHC.IO.Exception
import GHC.IO.Buffer
import GHC.IO.Encoding.Types
import Data.Maybe

-- -----------------------------------------------------------------------------
-- Latin1

latin1 :: TextEncoding
latin1 = TextEncoding { mkTextDecoder = latin1_DF,
                        mkTextEncoder = latin1_EF }

latin1_DF :: IO (TextDecoder ())
latin1_DF =
  return (BufferCodec {
             encode   = latin1_decode,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

latin1_EF :: IO (TextEncoder ())
latin1_EF =
  return (BufferCodec {
             encode   = latin1_encode,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

latin1_checked :: TextEncoding
latin1_checked = TextEncoding { mkTextDecoder = latin1_DF,
                                mkTextEncoder = latin1_checked_EF }

latin1_checked_EF :: IO (TextEncoder ())
latin1_checked_EF =
  return (BufferCodec {
             encode   = latin1_checked_encode,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })


latin1_decode :: DecodeBuffer
latin1_decode 
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let 
       loop !ir !ow
         | ow >= os || ir >= iw =  done ir ow
         | otherwise = do
              c0 <- readWord8Buf iraw ir
              ow' <- writeCharBuf oraw ow (unsafeChr (fromIntegral c0))
              loop (ir+1) ow'

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       done !ir !ow = return (if ir == iw then input{ bufL=0, bufR=0 }
                                          else input{ bufL=ir },
                         output{ bufR=ow })
    in
    loop ir0 ow0

latin1_encode :: EncodeBuffer
latin1_encode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
      done !ir !ow = return (if ir == iw then input{ bufL=0, bufR=0 }
                                         else input{ bufL=ir },
                             output{ bufR=ow })
      loop !ir !ow
        | ow >= os || ir >= iw =  done ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           writeWord8Buf oraw ow (fromIntegral (ord c))
           loop ir' (ow+1)
    in
    loop ir0 ow0

latin1_checked_encode :: EncodeBuffer
latin1_checked_encode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
      done !ir !ow = return (if ir == iw then input{ bufL=0, bufR=0 }
                                         else input{ bufL=ir },
                             output{ bufR=ow })
      loop !ir !ow
        | ow >= os || ir >= iw =  done ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           if ord c > 0xff then invalid else do
           writeWord8Buf oraw ow (fromIntegral (ord c))
           loop ir' (ow+1)
        where
           invalid = if ir > ir0 then done ir ow else ioe_encodingError
    in
    loop ir0 ow0

ioe_encodingError :: IO a
ioe_encodingError = ioException
     (IOError Nothing InvalidArgument "latin1_checked_encode"
          "character is out of range for this encoding" Nothing Nothing)
