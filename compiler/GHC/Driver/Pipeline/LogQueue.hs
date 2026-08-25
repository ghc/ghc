{-# LANGUAGE DerivingVia #-}
module GHC.Driver.Pipeline.LogQueue ( LogQueue(..)
                                  , newLogQueue
                                  , finishLogQueue
                                  , writeLogQueue
                                  , parLogAction
                                  , printLogs

                                  , LogQueueQueue(..)
                                  , initLogQueue
                                  , allLogQueues
                                  , newLogQueueQueue

                                  , logThread
                                  ) where

import GHC.Prelude
import Control.Concurrent
import Data.IORef
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Utils.Logger
import qualified Data.IntMap as IM
import Control.Concurrent.STM
import Control.Monad

-- LogQueue Abstraction

-- | A 'LogQueue' is used to accumulate compilation messages.
--
-- This allows compilation output to be reported to the user without
-- interleaving concurrent messages (garbled text).
data LogQueue =
  LogQueue
    { logQueueMessages  :: !(IORef [Maybe (MessageClass, SrcSpan, SDoc, LogFlags)])
       -- ^ All logged messages, in reverse chronological order (later messages
       -- appearing nearer the start of the list), with 'Nothing' denoting the
       -- end of the message queue.
       --
       -- A typical message queue will look like:
       --
       -- > <ignored_data> : Nothing : Just msg_9 : Just msg_8 : ... : Just msg_1 : []
    , logQueueSemaphore :: !(MVar ())
    }

newLogQueue :: IO LogQueue
newLogQueue = do
  mqueue <- newIORef []
  sem <- newMVar ()
  return (LogQueue mqueue sem)

finishLogQueue :: LogQueue -> IO ()
finishLogQueue lq = do
  writeLogQueueInternal lq Nothing


writeLogQueue :: LogQueue -> (MessageClass,SrcSpan,SDoc, LogFlags) -> IO ()
writeLogQueue lq msg = do
  writeLogQueueInternal lq (Just msg)

-- | Internal helper for writing log messages
writeLogQueueInternal :: LogQueue -> Maybe (MessageClass,SrcSpan,SDoc, LogFlags) -> IO ()
writeLogQueueInternal (LogQueue ref sem) msg = do
    atomicModifyIORef' ref $ \msgs -> (msg:msgs,())
    _ <- tryPutMVar sem ()
    return ()

-- The log_action callback that is used to synchronize messages from a
-- worker thread.
parLogAction :: LogQueue -> LogAction
parLogAction log_queue log_flags !msgClass !srcSpan !msg =
    writeLogQueue log_queue (msgClass,srcSpan,msg, log_flags)

-- | Print each message from the log queue using the given logger.
--
-- Blocks until the queue has been finished with 'finishLogQueue'.
printLogs :: Logger -> LogQueue -> IO ()
printLogs !logger (LogQueue ref sem) = read_msgs
  where read_msgs = do
            takeMVar sem
            msgs <- atomicModifyIORef' ref $ \xs -> ([], reverse xs)
            print_loop msgs

        print_loop [] = read_msgs
        print_loop (x:xs) = case x of
            Just (msgClass,srcSpan,msg,flags) -> do
                logMsg (setLogFlags logger flags) msgClass srcSpan msg
                print_loop xs
            -- Exit the loop once we encounter the end marker.
            Nothing -> return ()

-- The LogQueueQueue abstraction

data LogQueueQueue = LogQueueQueue Int (IM.IntMap LogQueue)

newLogQueueQueue :: LogQueueQueue
newLogQueueQueue = LogQueueQueue 1 IM.empty

addToQueueQueue
  :: Int -- ^ 1-indexed position in which to add
  -> LogQueue
  -> LogQueueQueue
  -> LogQueueQueue
addToQueueQueue i lq (LogQueueQueue n im) = LogQueueQueue n (IM.insert i lq im)

-- | Hand a log queue to the log thread, to be printed at the given position.
--
-- Positions must be contiguous: the log thread prints position @n@ only after
-- every position below @n@ is done.
initLogQueue
  :: TVar LogQueueQueue
  -> Int -- ^ position in the 'LogQueueQueue' in which to insert the 'LogQueue'
  -> LogQueue
  -> STM ()
initLogQueue lqq i lq = modifyTVar lqq (addToQueueQueue i lq)

-- | Return all items in the queue in ascending order
allLogQueues :: LogQueueQueue -> [LogQueue]
allLogQueues (LogQueueQueue _n im) = IM.elems im

dequeueLogQueueQueue :: LogQueueQueue -> Maybe (LogQueue, LogQueueQueue)
dequeueLogQueueQueue (LogQueueQueue n lqq) = case IM.minViewWithKey lqq of
                                                Just ((k, v), lqq') | k == n -> Just (v, LogQueueQueue (n + 1) lqq')
                                                _ -> Nothing

logThread :: Logger -> TVar Bool -- Signal that no more new logs will be added, clear the queue and exit
                    -> TVar LogQueueQueue -- Queue for logs
                    -> IO (IO ())
logThread logger stopped lqq_var = do
  finished_var <- newEmptyMVar
  _ <- forkIO $ print_logs *> putMVar finished_var ()
  return (takeMVar finished_var)
  where
    finish = mapM (printLogs logger)

    print_logs = join $ atomically $ do
      lqq <- readTVar lqq_var
      case dequeueLogQueueQueue lqq of
        Just (lq, lqq') -> do
          writeTVar lqq_var lqq'
          return (printLogs logger lq *> print_logs)
        Nothing -> do
          -- No log to print, check if we are finished.
          stopped <- readTVar stopped
          if not stopped then retry
                         else return (finish (allLogQueues lqq))
