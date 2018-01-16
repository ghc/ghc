{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Distribution.Client.Compat.Semaphore
    ( QSem
    , newQSem
    , waitQSem
    , signalQSem
    ) where

import Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, retry,
                               writeTVar)
import Control.Exception (mask_, onException)
import Control.Monad (join, unless)
import Data.Typeable (Typeable)

-- | 'QSem' is a quantity semaphore in which the resource is aqcuired
-- and released in units of one. It provides guaranteed FIFO ordering
-- for satisfying blocked `waitQSem` calls.
--
data QSem = QSem !(TVar Int) !(TVar [TVar Bool]) !(TVar [TVar Bool])
  deriving (Eq, Typeable)

newQSem :: Int -> IO QSem
newQSem i = atomically $ do
  q <- newTVar i
  b1 <- newTVar []
  b2 <- newTVar []
  return (QSem q b1 b2)

waitQSem :: QSem -> IO ()
waitQSem s@(QSem q _b1 b2) =
  mask_ $ join $ atomically $ do
        -- join, because if we need to block, we have to add a TVar to
        -- the block queue.
        -- mask_, because we need a chance to set up an exception handler
        -- after the join returns.
     v <- readTVar q
     if v == 0
        then do b <- newTVar False
                ys <- readTVar b2
                writeTVar b2 (b:ys)
                return (wait b)
        else do writeTVar q $! v - 1
                return (return ())
  where
    --
    -- very careful here: if we receive an exception, then we need to
    --  (a) write True into the TVar, so that another signalQSem doesn't
    --      try to wake up this thread, and
    --  (b) if the TVar is *already* True, then we need to do another
    --      signalQSem to avoid losing a unit of the resource.
    --
    -- The 'wake' function does both (a) and (b), so we can just call
    -- it here.
    --
    wait t =
      flip onException (wake s t) $
      atomically $ do
        b <- readTVar t
        unless b retry


wake :: QSem -> TVar Bool -> IO ()
wake s x = join $ atomically $ do
      b <- readTVar x
      if b then return (signalQSem s)
           else do writeTVar x True
                   return (return ())

{-
 property we want:

   bracket waitQSem (\_ -> signalQSem) (\_ -> ...)

 never loses a unit of the resource.
-}

signalQSem :: QSem -> IO ()
signalQSem s@(QSem q b1 b2) =
  mask_ $ join $ atomically $ do
      -- join, so we don't force the reverse inside the txn
      -- mask_ is needed so we don't lose a wakeup
    v <- readTVar q
    if v /= 0
       then do writeTVar q $! v + 1
               return (return ())
       else do xs <- readTVar b1
               checkwake1 xs
  where
    checkwake1 [] = do
      ys <- readTVar b2
      checkwake2 ys
    checkwake1 (x:xs) = do
      writeTVar b1 xs
      return (wake s x)

    checkwake2 [] = do
      writeTVar q 1
      return (return ())
    checkwake2 ys = do
      let (z:zs) = reverse ys
      writeTVar b1 zs
      writeTVar b2 []
      return (wake s z)
