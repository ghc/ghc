{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Fast write-buffered Handles
--
-- (c) The University of Glasgow 2005-2006
--
-- This is a simple abstraction over Handles that offers very fast write
-- buffering, but without the thread safety that Handles provide.  It's used
-- to save time in Pretty.printDoc.
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module BufWrite (
	BufHandle(..),
	newBufHandle,
	bPutChar,
	bPutStr,
	bPutFS,
	bPutFZS,
	bPutLitString,
	bFlush,
  ) where

#include "HsVersions.h"

import FastString
import FastTypes
import FastMutInt

import Control.Monad	( when )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Data.Char	( ord )
import Foreign
import Foreign.C.String
import System.IO

-- -----------------------------------------------------------------------------

data BufHandle = BufHandle {-#UNPACK#-}!(Ptr Word8)
			   {-#UNPACK#-}!FastMutInt
			   Handle

newBufHandle :: Handle -> IO BufHandle
newBufHandle hdl = do
  ptr <- mallocBytes buf_size
  r <- newFastMutInt
  writeFastMutInt r 0
  return (BufHandle ptr r hdl)

buf_size :: Int
buf_size = 8192

#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined

bPutChar :: BufHandle -> Char -> IO ()
STRICT2(bPutChar)
bPutChar b@(BufHandle buf r hdl) c = do
  i <- readFastMutInt r
  if (i >= buf_size)
	then do hPutBuf hdl buf buf_size
		writeFastMutInt r 0
		bPutChar b c
	else do pokeElemOff buf i (fromIntegral (ord c) :: Word8)
		writeFastMutInt r (i+1)

bPutStr :: BufHandle -> String -> IO ()
STRICT2(bPutStr)
bPutStr (BufHandle buf r hdl) str = do
  i <- readFastMutInt r
  loop str i
  where loop _ i | i `seq` False = undefined
	loop "" i = do writeFastMutInt r i; return ()
	loop (c:cs) i
	   | i >= buf_size = do
		hPutBuf hdl buf buf_size
		loop (c:cs) 0
	   | otherwise = do
		pokeElemOff buf i (fromIntegral (ord c))
		loop cs (i+1)
  
bPutFS :: BufHandle -> FastString -> IO ()
bPutFS b fs = bPutBS b $ fastStringToByteString fs

bPutFZS :: BufHandle -> FastZString -> IO ()
bPutFZS b fs = bPutBS b $ fastZStringToByteString fs

bPutBS :: BufHandle -> ByteString -> IO ()
bPutBS b bs = BS.unsafeUseAsCStringLen bs $ bPutCStringLen b

bPutCStringLen :: BufHandle -> CStringLen -> IO ()
bPutCStringLen b@(BufHandle buf r hdl) cstr@(ptr, len) = do
  i <- readFastMutInt r
  if (i + len) >= buf_size
        then do hPutBuf hdl buf i
                writeFastMutInt r 0
                if (len >= buf_size)
                    then hPutBuf hdl ptr len
                    else bPutCStringLen b cstr
        else do
                copyBytes (buf `plusPtr` i) ptr len
                writeFastMutInt r (i + len)

bPutLitString :: BufHandle -> LitString -> FastInt -> IO ()
bPutLitString b@(BufHandle buf r hdl) a len_ = a `seq` do
  let len = iBox len_
  i <- readFastMutInt r
  if (i+len) >= buf_size
	then do hPutBuf hdl buf i
		writeFastMutInt r 0
		if (len >= buf_size) 
		    then hPutBuf hdl a len
		    else bPutLitString b a len_
	else do
		copyBytes (buf `plusPtr` i) a len
		writeFastMutInt r (i+len)

bFlush :: BufHandle -> IO ()
bFlush (BufHandle buf r hdl) = do
  i <- readFastMutInt r
  when (i > 0) $ hPutBuf hdl buf i
  free buf
  return ()

#if 0
myPutBuf s hdl buf i = 
  modifyIOError (\e -> ioeSetErrorString e (ioeGetErrorString e ++ ':':s ++ " (" ++ show buf ++ "," ++ show i ++ ")")) $

  hPutBuf hdl buf i
#endif
