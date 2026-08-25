{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
module GHC.Driver.Pipeline.LogQueue ( LogQueue(..)
                                  , newLogQueue
                                  , finishLogQueue
                                  , writeLogQueue
                                  , parLogAction
                                  , printLogs

                                  , withLogPrinter
                                  ) where

import GHC.Prelude
import Control.Concurrent
import Control.Exception ( finally )
import Data.Foldable ( for_ )
import Data.IORef
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Utils.Concurrent.Scope ( scoped, fork, activeCount )
import GHC.Utils.Logger
import Control.Concurrent.STM

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

-- Printing log queues as they are being written

-- | Run an action that can submit log queues for printing.
--
-- The log queues are printed concurrently with the action, one after the other,
-- in the order in which they were submitted.
--
-- Does not return until every submitted log queue has been printed, whether
-- the action returns or throws an exception. All of the submitted log queues
-- must have been finished (with 'finishLogQueue') by the time the action ends.
--
-- An exception arising from printing is rethrown.
withLogPrinter
  :: Logger
  -> ( ( LogQueue -> IO () ) -> IO a )
     -- ^ the action, given how to submit a log queue for printing
  -> IO a
withLogPrinter logger action =
  scoped \ scope -> do
    -- 'Nothing' denotes the end of the queue.
    queue <- newTQueueIO @( Maybe LogQueue )
    let
      print_queued :: IO ()
      print_queued = do
        next <- atomically $ readTQueue queue
        for_ next \ lq -> do
          printLogs logger lq
          print_queued
    fork scope print_queued
    action ( atomically . writeTQueue queue . Just )
      `finally` do
        atomically $ writeTQueue queue Nothing
        -- Wait for the remaining log queues to be printed.
        atomically $ activeCount scope >>= check . ( == 0 )
