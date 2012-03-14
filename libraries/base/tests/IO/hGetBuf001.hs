-- !!! Testing hGetBuf(NonBlocking), hPutBuf(NonBlocking)

import System.Posix
import System.IO
import Control.Concurrent
import Foreign
import Foreign.C
import System.Exit
import Control.Exception
import Control.Monad


main = do
  -- test should run quickly, but arrange to kill it if it hangs for any reason:
  main_t <- myThreadId
  forkIO $ do
	threadDelay 10000000
	throwTo main_t (ErrorCall "killed")

  zipWithM_ ($) 
	  [ f rbuf wbuf
	    | f <- [hGetBufTest, hGetBufNBTest, hGetBufSomeTest],
	      rbuf <- [buf1,buf2,buf3],
	      wbuf <- [buf1,buf2,buf3]
	    ]
	  [1..]

msg = "hello!"
msg_length = length msg

buf1 = NoBuffering
buf2 = BlockBuffering (Just 5)
buf3 = BlockBuffering (Just 10)

-- chosen to be larger than buf2 & smaller than buf3, so that we exercise
-- all code paths:
read_size = 8 :: Int

-- ----------------------------------------------------------------------------

-- hGetBuf/hPutBuf:
--   - test that it always reads all the data that is available
--     (with buffer size <, =, > message size).
--   - test that at the EOF, it returns a short read.
--   - the writing end is using hPutBuf, with various buffer sizes, and
--     doing an hFlush at the end of each write.

hGetBufTest rbuf wbuf n = do
  (read,write) <- createPipe
  hread <- fdToHandle read
  hwrite <- fdToHandle write
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  finished <- newEmptyMVar
  hSetBuffering hread rbuf
  hSetBuffering hwrite wbuf
  forkIO (readProc m1 m2 finished hread)
  writeProc m1 m2 hwrite
  takeMVar finished
  putStrLn ("test " ++ show n ++ " OK")


readProc :: MVar () -> MVar () -> MVar () -> Handle -> IO ()
readProc m1 m2 finished h = do
  buf <- mallocBytes 20
  let
    loop 0 = return ()
    loop n = do putMVar m2 (); takeMVar m1
		r <- hGetBuf h buf msg_length
		if (r /= msg_length) 
			then do hPutStr stderr ("error: " ++ show r)
			        exitFailure
			else do s <- peekCStringLen (buf,r)
			        hPutStr stdout (show n ++ " ")
			        loop (n-1)
  loop 100
  hPutStr stdout "\n"
  putMVar m2 (); takeMVar m1
  r <- hGetBuf h buf read_size -- EOF, should get short read
  s <- peekCStringLen (buf,r)
  putStrLn ("got " ++ show r ++  ": " ++ s)
  r <- hGetBuf h buf read_size -- EOF, should get zero-length read
  s <- peekCStringLen (buf,r)
  putStrLn ("got " ++ show r ++  ": " ++ s)
  hClose h
  putMVar finished ()

writeProc :: MVar () -> MVar () -> Handle -> IO ()
writeProc m1 m2 h = do
  let
    loop 0 = return ()
    loop n =
	withCStringLen msg  $ \ (s,len) -> do
	  takeMVar m2
	  hPutBuf h s len
	  hFlush h
	  putMVar m1 ()
	  loop (n-1)

  loop 100
  takeMVar m2
  withCString "end" $ \s -> do
    hPutBuf h s 3
    putMVar m1 ()
  hClose h

-- -----------------------------------------------------------------------------
-- hGetBufNonBlocking:

hGetBufNBTest rbuf wbuf n = do
  (read,write) <- createPipe
  hread <- fdToHandle read
  hwrite <- fdToHandle write
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  finished <- newEmptyMVar
  hSetBuffering hread rbuf
  hSetBuffering hwrite wbuf
  forkIO (readProcNB m1 m2 finished hread)
  writeProcNB m1 m2 hwrite
  takeMVar finished
  putStrLn ("test " ++ show n ++ " OK")


readProcNB :: MVar () -> MVar () -> MVar () -> Handle -> IO ()
readProcNB m1 m2 finished h = do
  buf <- mallocBytes 20

  -- first, test that we can do a non-blocking read:
  r <- hGetBufNonBlocking h buf read_size
  s <- peekCStringLen (buf,r)
  putStrLn ("got " ++ show r ++  ": " ++ s)

  let
    loop 0 = return ()
    loop n = do putMVar m2 (); takeMVar m1
		r <- hGetBufNonBlocking h buf read_size
		if (r /= msg_length) 
			then do hPutStr stderr ("error: " ++ show r)
			        exitFailure
			else do s <- peekCStringLen (buf,r)
			        hPutStr stdout (show n ++ " ")
			        loop (n-1)
  loop 100
  hPutStr stdout "\n"
  putMVar m2 (); takeMVar m1
  r <- hGetBufNonBlocking h buf read_size -- EOF, should get short read
  s <- peekCStringLen (buf,r)
  putStrLn ("got " ++ show r ++  ": " ++ s)
  r <- hGetBufNonBlocking h buf read_size -- EOF, should get zero-length read
  s <- peekCStringLen (buf,r)
  putStrLn ("got " ++ show r ++  ": " ++ s)
  hClose h
  putMVar finished ()

writeProcNB :: MVar () -> MVar () -> Handle -> IO ()
writeProcNB m1 m2 h = do
  let
    loop 0 = return ()
    loop n =
	withCStringLen msg  $ \ (s,len) -> do
	  takeMVar m2
	  hPutBufNonBlocking h s len
	  hFlush h
	  putMVar m1 ()
	  loop (n-1)

  loop 100
  takeMVar m2
  withCString "end" $ \s -> do
    hPutBuf h s 3
    hFlush h
    putMVar m1 ()
  hClose h

-- -----------------------------------------------------------------------------
-- hGetBufSome:

hGetBufSomeTest rbuf wbuf n = do
  (read,write) <- createPipe
  hread <- fdToHandle read
  hwrite <- fdToHandle write
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  finished <- newEmptyMVar
  hSetBuffering hread rbuf
  hSetBuffering hwrite wbuf
  forkIO (readProcSome m1 m2 finished hread)
  writeProcNB m1 m2 hwrite
  takeMVar finished
  putStrLn ("test " ++ show n ++ " OK")


readProcSome :: MVar () -> MVar () -> MVar () -> Handle -> IO ()
readProcSome m1 m2 finished h = do
  buf <- mallocBytes 20

  let
    loop 0 = return ()
    loop n = do putMVar m2 (); takeMVar m1
		r <- hGetBufSome h buf read_size
		if (r /= msg_length) 
			then do hPutStr stderr ("error: " ++ show r)
			        exitFailure
			else do s <- peekCStringLen (buf,r)
			        hPutStr stdout (show n ++ " ")
			        loop (n-1)
  loop 100
  hPutStr stdout "\n"
  putMVar m2 (); takeMVar m1
  r <- hGetBufSome h buf read_size -- EOF, should get short read
  s <- peekCStringLen (buf,r)
  putStrLn ("got " ++ show r ++  ": " ++ s)
  r <- hGetBufSome h buf read_size -- EOF, should get zero-length read
  s <- peekCStringLen (buf,r)
  putStrLn ("got " ++ show r ++  ": " ++ s)
  hClose h
  putMVar finished ()
