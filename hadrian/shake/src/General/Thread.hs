{-# LANGUAGE ScopedTypeVariables #-}

-- | A bit like 'Fence', but not thread safe and optimised for avoiding taking the fence
module General.Thread(
    withThreadsBoth,
    withThreadSlave,
    allocateThread,
    Thread, newThreadFinally, stopThreads
    ) where

import General.Cleanup
import Data.Hashable
import Control.Concurrent.Extra
import Control.Exception
import General.Extra
import Control.Monad.Extra


data Thread = Thread ThreadId (Barrier ())

instance Eq Thread where
    Thread a _ == Thread b _ = a == b

instance Hashable Thread where
    hashWithSalt salt (Thread a _) = hashWithSalt salt a

-- | The inner thread is unmasked even if you started masked.
newThreadFinally :: IO a -> (Thread -> Either SomeException a -> IO ()) -> IO Thread
newThreadFinally act cleanup = do
    bar <- newBarrier
    t <- mask_ $ forkIOWithUnmask $ \unmask -> flip finally (signalBarrier bar ()) $ do
        res <- try $ unmask act
        me <- myThreadId
        cleanup (Thread me bar) res
    pure $ Thread t bar


stopThreads :: [Thread] -> IO ()
stopThreads threads = do
    -- if a thread is in a masked action, killing it may take some time, so kill them in parallel
    bars <- sequence [do forkIO $ killThread t; pure bar | Thread t bar <- threads]
    mapM_ waitBarrier bars


-- Run both actions. If either throws an exception, both threads
-- are killed and an exception reraised.
-- Not called much, so simplicity over performance (2 threads).
withThreadsBoth :: IO a -> IO b -> IO (a, b)
withThreadsBoth act1 act2 = do
    bar1 <- newBarrier
    bar2 <- newBarrier
    parent <- myThreadId
    ignore <- newVar False
    mask $ \unmask -> do
        t1 <- forkIOWithUnmask $ \unmask -> do
            res1 :: Either SomeException a <- try $ unmask act1
            unlessM (readVar ignore) $ whenLeft res1 $ throwTo parent
            signalBarrier bar1 res1
        t2 <- forkIOWithUnmask $ \unmask -> do
            res2 :: Either SomeException b <- try $ unmask act2
            unlessM (readVar ignore) $ whenLeft res2 $ throwTo parent
            signalBarrier bar2 res2
        res :: Either SomeException (a,b) <- try $ unmask $ do
            Right v1 <- waitBarrier bar1
            Right v2 <- waitBarrier bar2
            pure (v1,v2)
        writeVar ignore True
        killThread t1
        forkIO $ killThread t2
        waitBarrier bar1
        waitBarrier bar2
        either throwIO pure res


-- | Run an action in a separate thread.
--   After the first action terminates, the thread will be killed.
--   If the action raises an exception it will be rethrown on the parent thread.
withThreadSlave :: IO () -> IO a -> IO a
withThreadSlave slave act = withCleanup $ \cleanup -> do
    allocateThread cleanup slave
    act


-- | Run the given action in a separate thread.
--   On cleanup, the thread will be killed before continuing.
--   If the action raises an exception it will be rethrown on the parent thread.
allocateThread :: Cleanup -> IO () -> IO ()
allocateThread cleanup act = do
    bar <- newBarrier
    parent <- myThreadId
    ignore <- newVar False
    void $ allocate cleanup
        (mask_ $ forkIOWithUnmask $ \unmask -> do
            res :: Either SomeException () <- try $ unmask act
            unlessM (readVar ignore) $ whenLeft res $ throwTo parent
            signalBarrier bar ()
        )
        (\t -> do writeVar ignore True; killThread t; waitBarrier bar)
