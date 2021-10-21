import Control.Concurrent
import Control.Exception

-- version of conc015 using mask in place of the old deprecated
-- block/unblock.

-- test blocking & unblocking of async exceptions.

-- the first exception "foo" should be caught by the "caught1" handler,
-- since async exceptions are blocked outside this handler.

-- the second exception "bar" should be caught by the outer "caught2" handler,
-- (i.e. this tests that async exceptions are properly unblocked after
-- being blocked).

main = do
  main_thread <- myThreadId
  print =<< getMaskingState
  m <- newEmptyMVar
  m2 <- newEmptyMVar
  forkIO (do takeMVar m
             throwTo main_thread (ErrorCall "foo")
             throwTo main_thread (ErrorCall "bar")
             putMVar m2 ()
         )
  ( do
    mask $ \restore -> do
        putMVar m ()
        print =<< getMaskingState
        sum [1..100000] `seq` -- give 'foo' a chance to be raised
          (restore (myDelay 500000)
                `Control.Exception.catch`
                    \e -> putStrLn ("caught1: " ++ show (e::SomeExceptionWithLocation)))

    threadDelay 10000
    takeMVar m2
   )
    `Control.Exception.catch`
       \e -> do print =<< getMaskingState
                putStrLn ("caught2: " ++ show (e::SomeExceptionWithLocation))

-- compensate for the fact that threadDelay is non-interruptible
-- on Windows with the threaded RTS in 6.6.
myDelay usec = do
  m <- newEmptyMVar
  forkIO $ do threadDelay usec; putMVar m ()
  takeMVar m
