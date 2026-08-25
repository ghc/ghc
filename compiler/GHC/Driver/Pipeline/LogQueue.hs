{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
module GHC.Driver.Pipeline.LogQueue ( LogQueue(..)
                                  , newLogQueue
                                  , finishLogQueue
                                  , writeLogQueue
                                  , parLogAction
                                  , printLogs

                                  , SubmitLogQueueAction
                                  , withLogPrinter
                                  ) where

import GHC.Prelude
import Control.Exception ( finally )
import Control.Monad ( unless, when )
import Data.Foldable ( for_ )
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
--
-- A log queue is /finished/ (with 'finishLogQueue') once nothing more will be
-- logged to it: 'printLogs' prints the messages of a queue as they arrive, until
-- the queue is finished.
data LogQueue =
  LogQueue
    { logQueueMessages :: !(TQueue (MessageClass, SrcSpan, SDoc, LogFlags))
      -- ^ messages not yet printed, in chronological order
    , logQueueFinished :: !(TVar Bool)
      -- ^ whether the queue is finished
    }

newLogQueue :: IO LogQueue
newLogQueue = LogQueue <$> newTQueueIO <*> newTVarIO False

-- | Finish the log queue: messages written afterwards are not recorded.
finishLogQueue :: LogQueue -> IO ()
finishLogQueue lq = atomically $ writeTVar (logQueueFinished lq) True

writeLogQueue :: LogQueue -> (MessageClass,SrcSpan,SDoc, LogFlags) -> IO ()
writeLogQueue (LogQueue msgs finished) msg = atomically do
  is_finished <- readTVar finished
  -- TODO: messages written after the queue has been finished are dropped.
  -- This can happen when a thunk that captured the logger is forced later,
  -- e.g. lazy interface typechecking ('GHC.Tc.Utils.Monad.forkM') logging
  -- with -ddump-if-trace.
  unless is_finished $ writeTQueue msgs msg

-- The log_action callback that is used to synchronize messages from a
-- worker thread.
parLogAction :: LogQueue -> LogAction
parLogAction log_queue log_flags !msgClass !srcSpan !msg =
    writeLogQueue log_queue (msgClass,srcSpan,msg, log_flags)

-- | Print each message from the log queue using the given logger.
--
-- Blocks until the queue has been finished with 'finishLogQueue'.
printLogs :: Logger -> LogQueue -> IO ()
printLogs !logger (LogQueue msgs_var finished_var) = loop
  where
    loop = do
      (msgs, finished) <- atomically do
        msgs <- flushTQueue msgs_var
        finished <- readTVar finished_var
        -- Wait until there is something to print, or the queue is finished.
        when (null msgs && not finished) retry
        return (msgs, finished)
      for_ msgs \ (msgClass, srcSpan, msg, flags) ->
        logMsg (setLogFlags logger flags) msgClass srcSpan msg
      unless finished loop

-- Printing log queues as they are being written

-- | Submit a log queue for printing.
--
-- Does not use any interruptible operations.
type SubmitLogQueueAction = LogQueue -> IO ()

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
  -> ( SubmitLogQueueAction -> IO a )
     -- ^ the action, which can submit log queues for printing
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
