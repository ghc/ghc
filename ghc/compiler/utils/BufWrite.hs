-----------------------------------------------------------------------------
--
-- Fast write-buffered Handles
--
-- (c) The University of Glasgow 2005
--
-- This is a simple abstraction over Handles that offers very fast write
-- buffering, but without the thread safety that Handles provide.  It's used
-- to save time in Pretty.printDoc.
--
-----------------------------------------------------------------------------

module BufWrite (
	BufHandle(..),
	newBufHandle,
	bPutChar,
	bPutStr,
	bPutFS,
	bPutLitString,
	bFlush,
  ) where

#include "HsVersions.h"

import FastString
import FastMutInt
import Panic		( panic )

import Monad		( when )
import Char		( ord )
import Foreign
import IO

import GHC.IOBase	( IO(..) )
import System.IO	( hPutBuf )
import GHC.Ptr		( Ptr(..) )

import GLAEXTS		( Int(..), Int#, Addr# )

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

buf_size = 8192 :: Int

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
bPutStr b@(BufHandle buf r hdl) str = do
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
bPutFS b@(BufHandle buf r hdl) fs@(FastString _ len _ fp _) =
 withForeignPtr fp $ \ptr -> do
  i <- readFastMutInt r
  if (i + len) >= buf_size
	then do hPutBuf hdl buf i
		writeFastMutInt r 0
		if (len >= buf_size) 
		    then hPutBuf hdl ptr len
		    else bPutFS b fs
	else do
		copyBytes (buf `plusPtr` i) ptr len
		writeFastMutInt r (i+len)
bPutFS _ _ = panic "bPutFS"

bPutLitString :: BufHandle -> Addr# -> Int# -> IO ()
bPutLitString b@(BufHandle buf r hdl) a# len# = do
  let len = I# len#
  i <- readFastMutInt r
  if (i+len) >= buf_size
	then do hPutBuf hdl buf i
		writeFastMutInt r 0
		if (len >= buf_size) 
		    then hPutBuf hdl (Ptr a#) len
		    else bPutLitString b a# len#
	else do
		copyBytes (buf `plusPtr` i) (Ptr a#) len
		writeFastMutInt r (i+len)

bFlush :: BufHandle -> IO ()
bFlush b@(BufHandle buf r hdl) = do
  i <- readFastMutInt r
  when (i > 0) $ hPutBuf hdl buf i
  free buf
  return ()

#if 0
myPutBuf s hdl buf i = 
  modifyIOError (\e -> ioeSetErrorString e (ioeGetErrorString e ++ ':':s ++ " (" ++ show buf ++ "," ++ show i ++ ")")) $

  hPutBuf hdl buf i
#endif
