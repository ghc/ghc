-- !!! Testing hGetBuf(NonBlocking), hPutBuf(NonBlocking)

import System.Posix
import System.IO
import Control.Concurrent
import Foreign
import Foreign.C
import System.Exit
import Control.Exception


main = do
  -- test should run quickly, but arrange to kill it if it hangs for any reason:
  main_t <- myThreadId
  forkIO $ do
	threadDelay 2000000
	throwTo main_t (ErrorCall "killed")

  hGetBufTest 1 NoBuffering 		   NoBuffering
  hGetBufTest 2 (BlockBuffering (Just 5))  NoBuffering
  hGetBufTest 3 (BlockBuffering (Just 10)) NoBuffering
  hGetBufTest 4 NoBuffering 		   (BlockBuffering (Just 5))
  hGetBufTest 5 (BlockBuffering (Just 5))  (BlockBuffering (Just 5))
  hGetBufTest 6 (BlockBuffering (Just 10)) (BlockBuffering (Just 5))
  hGetBufTest 7 NoBuffering 		   (BlockBuffering (Just 10))
  hGetBufTest 8 (BlockBuffering (Just 5))  (BlockBuffering (Just 10))
  hGetBufTest 9 (BlockBuffering (Just 10)) (BlockBuffering (Just 10))

  hGetBufNBTest 11 NoBuffering 		      NoBuffering
  hGetBufNBTest 12 (BlockBuffering (Just 5))  NoBuffering
  hGetBufNBTest 13 (BlockBuffering (Just 10)) NoBuffering
  hGetBufNBTest 14 NoBuffering 		      (BlockBuffering (Just 5))
  hGetBufNBTest 15 (BlockBuffering (Just 5))  (BlockBuffering (Just 5))
  hGetBufNBTest 16 (BlockBuffering (Just 10)) (BlockBuffering (Just 5))
  hGetBufNBTest 17 NoBuffering 		      (BlockBuffering (Just 10))
  hGetBufNBTest 18 (BlockBuffering (Just 5))  (BlockBuffering (Just 10))
  hGetBufNBTest 19 (BlockBuffering (Just 10)) (BlockBuffering (Just 10))

-- ----------------------------------------------------------------------------

-- hGetBuf/hPutBuf:
--   - test that it always reads all the data that is available
--     (with buffer size <, =, > message size).
--   - test that at the EOF, it returns a short read.
--   - the writing end is using hPutBuf, with various buffer sizes, and
--     doing an hFlush at the end of each write.

hGetBufTest n rbuf wbuf = do
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
		r <- hGetBuf h buf 5
		if (r /= 5) 
			then do hPutStr stderr ("error: " ++ show r)
			        exitFailure
			else do s <- peekCStringLen (buf,r)
			        hPutStr stdout (show n ++ " ")
			        loop (n-1)
  loop 100
  hPutStr stdout "\n"
  putMVar m2 (); takeMVar m1
  r <- hGetBuf h buf 20 -- EOF, should get short read
  s <- peekCStringLen (buf,r)
  putStrLn ("got " ++ show r ++  ": " ++ s)
  r <- hGetBuf h buf 20 -- EOF, should get zero-length read
  s <- peekCStringLen (buf,r)
  putStrLn ("got " ++ show r ++  ": " ++ s)
  hClose h
  putMVar finished ()

writeProc :: MVar () -> MVar () -> Handle -> IO ()
writeProc m1 m2 h = do
  let
    loop 0 = return ()
    loop n =
	withCString "hello"  $ \ s -> do
	  takeMVar m2
	  hPutBuf h s 5
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

hGetBufNBTest n rbuf wbuf = do
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
  r <- hGetBufNonBlocking h buf 20
  s <- peekCStringLen (buf,r)
  putStrLn ("got " ++ show r ++  ": " ++ s)

  let
    loop 0 = return ()
    loop n = do putMVar m2 (); takeMVar m1
		r <- hGetBufNonBlocking h buf 20
		if (r /= 5) 
			then do hPutStr stderr ("error: " ++ show r)
			        exitFailure
			else do s <- peekCStringLen (buf,r)
			        hPutStr stdout (show n ++ " ")
			        loop (n-1)
  loop 100
  hPutStr stdout "\n"
  putMVar m2 (); takeMVar m1
  r <- hGetBufNonBlocking h buf 20 -- EOF, should get short read
  s <- peekCStringLen (buf,r)
  putStrLn ("got " ++ show r ++  ": " ++ s)
  r <- hGetBufNonBlocking h buf 20 -- EOF, should get zero-length read
  s <- peekCStringLen (buf,r)
  putStrLn ("got " ++ show r ++  ": " ++ s)
  hClose h
  putMVar finished ()

writeProcNB :: MVar () -> MVar () -> Handle -> IO ()
writeProcNB m1 m2 h = do
  let
    loop 0 = return ()
    loop n =
	withCString "hello"  $ \ s -> do
	  takeMVar m2
	  hPutBufNonBlocking h s 5
	  hFlush h
	  putMVar m1 ()
	  loop (n-1)

  loop 100
  takeMVar m2
  withCString "end" $ \s -> do
    hPutBuf h s 3
    putMVar m1 ()
  hClose h
