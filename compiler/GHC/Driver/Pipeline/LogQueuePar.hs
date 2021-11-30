{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
module GHC.Driver.Pipeline.LogQueuePar ( LogQueue(..)
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
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative
import GHC.Utils.Outputable
import System.IO
import Data.Function
import qualified Data.Set as S
import Data.Coerce

-- LogQueue Abstraction

-- | Each module is given a unique 'LogQueue' to redirect compilation messages
-- to. A 'Nothing' value contains the result of compilation, and denotes the
-- end of the message queue.
data LogQueue = LogQueue { logQueueId :: !Int
                         , logQueueChan :: TChan (Maybe (MessageClass, SrcSpan, SDoc, LogFlags))
                         }

instance Ord LogQueue where
  compare = compare `on` logQueueId

instance Eq LogQueue where
  (==) = (==) `on` logQueueId

newLogQueue :: Int -> IO LogQueue
newLogQueue n = do
  mqueue <- newTChanIO
  return (LogQueue n mqueue)

finishLogQueue :: LogQueue -> IO ()
finishLogQueue lq = do
  writeLogQueueInternal lq Nothing


writeLogQueue :: LogQueue -> (MessageClass,SrcSpan,SDoc, LogFlags) -> IO ()
writeLogQueue lq msg = do
  writeLogQueueInternal lq (Just msg)

-- | Internal helper for writing log messages
writeLogQueueInternal :: LogQueue -> Maybe (MessageClass,SrcSpan,SDoc, LogFlags) -> IO ()
writeLogQueueInternal (LogQueue _n chan) msg = do
  atomically $ writeTChan chan msg

-- The log_action callback that is used to synchronize messages from a
-- worker thread.
parLogAction :: LogQueue -> LogAction
parLogAction log_queue log_flags !msgClass !srcSpan !msg =
    writeLogQueue log_queue (msgClass,srcSpan,msg, log_flags)

-- The LogQueueQueue abstraction

newtype LogQueueQueue = LogQueueQueue (S.Set LogQueue)

newLogQueueQueue :: LogQueueQueue
newLogQueueQueue = LogQueueQueue S.empty

addToQueueQueue :: LogQueue -> LogQueueQueue -> LogQueueQueue
addToQueueQueue lq (LogQueueQueue im) = LogQueueQueue (S.insert lq im)

initLogQueue :: TVar LogQueueQueue -> LogQueue -> STM ()
initLogQueue lqq lq = modifyTVar lqq (addToQueueQueue lq)

-- | Return all items in the queue in ascending order
allLogQueues :: LogQueueQueue -> [LogQueue]
allLogQueues (LogQueueQueue qs) = S.toList qs

logThread :: Int -> Int
              -> Logger -> TVar Bool -- Signal that no more new logs will be added, clear the queue and exit
                    -> TVar LogQueueQueue -- Queue for logs
                    -> IO (IO ())
logThread n_jobs total_modules logger stopped lqq_var = do
  finished_var <- newEmptyMVar
  _ <- forkIO $ do
     putStr "\ESC[?25l"
     hFlush stdout
     print_logs 0 *> putMVar finished_var ()
     putStr "\ESC[?25h"
     hFlush stdout
  return (takeMVar finished_var)
  where
    finish _active completed [] = output 0 completed (MCOutput, noSrcSpan, GHC.Utils.Outputable.empty, defaultLogFlags)
    finish active completed (LogQueue _n chan :lqq) =
      let loop = join $ atomically $ do
            empty <- isEmptyTChan chan
            if empty
              then return $ finish (active - 1) (completed + 1) lqq
              else do
                v <- readTChan chan
                case v of
                  Nothing -> do
                    return (finish (active - 1) (completed + 1) lqq)
                  Just msg -> return (output active completed msg >> loop)
      in loop


    readWithId l@(LogQueue _ chan) = (l,) <$> readTChan chan

    print_logs completed = join $ atomically $ do
      lqq <- readTVar lqq_var
      let active = allLogQueues lqq
      -- No log to print, check if we are finished.
      stopped <- readTVar stopped
      if stopped
        then return (finish (length active) completed active)
        else do
          (id, msg) <- foldr (<|>) retry (map readWithId active)
          case msg of
            Nothing -> do
              modifyTVar lqq_var (coerce $ S.delete id)
              return (print_logs (completed + 1))
            Just msg -> return (output (length active) completed msg >> print_logs completed)


    formatLine :: Int -> Int -> SDoc -> SDoc
    formatLine current_caps finished  msg =
      brackets ( int finished <> text "/" <> int total_modules <+> int (min current_caps n_jobs) <> text "/" <> int n_jobs ) <+> msg

    output :: Int -> Int -> (MessageClass, SrcSpan, SDoc, LogFlags) -> IO ()
    output n_active completed (cls, pos, msg, lf) = do
      -- 1. Clear current line
      putStr "\ESC[2K\ESC[1G"
      hFlush stdout
      line_msg <- case cls of
        MCOutput  -> return GHC.Utils.Outputable.empty
        _ -> logMsg (setLogFlags logger lf) cls pos msg >> return GHC.Utils.Outputable.empty
      defaultLogActionHPutStrDoc lf False stdout (formatLine n_active completed line_msg)

