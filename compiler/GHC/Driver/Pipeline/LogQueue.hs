{-# LANGUAGE DerivingVia #-}
module GHC.Driver.Pipeline.LogQueue ( LogQueue(..)
                                  , newLogQueue
                                  , finishLogQueue
                                  , writeLogQueue
                                  , parLogAction

                                  , LogQueueQueue(..)
                                  , initLogQueue
                                  , allLogQueues
                                  , newLogQueueQueue

                                  , logThread
                                  ) where

import GHC.Prelude
import Control.Concurrent
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Utils.Logger
import qualified Data.IntMap as IM
import Control.Concurrent.STM
import Control.Monad

-- LogQueue Abstraction

-- | Each module is given a unique 'LogQueue' to redirect compilation messages
-- to.
--
-- Log queues are printed one at a time, in order, so that the output of
-- different modules does not interleave. The messages of the current queue are
-- printed as they arrive, and the next queue is only started once the current
-- one is /finished/ (with 'finishLogQueue'), i.e. once its module is done
-- logging.
data LogQueue = LogQueue { logQueueId :: !Int
                         , logQueueMessages :: !(TQueue (MessageClass, SrcSpan, SDoc, LogFlags))
                           -- ^ messages not yet printed, in chronological order
                         , logQueueFinished :: !(TVar Bool)
                           -- ^ whether the queue is finished
                         }

newLogQueue :: Int -> IO LogQueue
newLogQueue n = LogQueue n <$> newTQueueIO <*> newTVarIO False

-- | Finish the log queue: messages written afterwards are not recorded.
finishLogQueue :: LogQueue -> IO ()
finishLogQueue lq = atomically $ writeTVar (logQueueFinished lq) True

writeLogQueue :: LogQueue -> (MessageClass,SrcSpan,SDoc, LogFlags) -> IO ()
writeLogQueue (LogQueue _n msgs finished) msg = atomically $ do
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

-- Print each message from the log_queue using the global logger
--
-- Returns once the queue has been finished and all of its messages printed.
printLogs :: Logger -> LogQueue -> IO ()
printLogs !logger (LogQueue _n msgs_var finished_var) = loop
  where
    loop = do
      (msgs, finished) <- atomically $ do
        msgs <- flushTQueue msgs_var
        finished <- readTVar finished_var
        -- Wait until there is something to print, or the queue is finished.
        when (null msgs && not finished) retry
        return (msgs, finished)
      forM_ msgs $ \ (msgClass, srcSpan, msg, flags) ->
        logMsg (setLogFlags logger flags) msgClass srcSpan msg
      unless finished loop

-- The LogQueueQueue abstraction

data LogQueueQueue = LogQueueQueue Int (IM.IntMap LogQueue)

newLogQueueQueue :: LogQueueQueue
newLogQueueQueue = LogQueueQueue 1 IM.empty

addToQueueQueue :: LogQueue -> LogQueueQueue -> LogQueueQueue
addToQueueQueue lq (LogQueueQueue n im) = LogQueueQueue n (IM.insert (logQueueId lq) lq im)

initLogQueue :: TVar LogQueueQueue -> LogQueue -> STM ()
initLogQueue lqq lq = modifyTVar lqq (addToQueueQueue lq)

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
