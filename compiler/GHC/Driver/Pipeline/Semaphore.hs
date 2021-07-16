{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingVia #-}
module GHC.Driver.Pipeline.Semaphore where

import GHC.Prelude
import qualified GHC.Data.DependentMap as M
import Control.Concurrent
import GHC.Driver.Hooks
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Reader
import Control.Monad
import Data.IORef
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Utils.Logger
import qualified Control.Monad.Catch as MC
import GHC.Utils.Outputable
import GHC.Utils.Trace

data ActionStatus = Waiting | Running | Finished

data ActionResult a = ActionResult { actionResult :: MVar a -- Where the result will end up
--                                   , actionLog    :: LogQueue -- Where the action can write messages to
                                   , killAction   :: IO () -- How to kill the running action
                                   , actionName   :: SDoc  -- For debugging
                                   }

waitResult :: ActionResult a -> IO a
waitResult ar = do
  pprTraceM "WAITING" (actionName ar)
  rs <- readMVar (actionResult ar)
  pprTraceM "UNBLOCKED" (actionName ar)
  return rs

mkAction :: SDoc -> QSem -> IO a -> IO (ActionResult a)
mkAction name par_sem act = do
  res_var <- newEmptyMVar
  -- MP: There used to be a forkIOWithUnmask here, but there was not a corresponding
  -- mask so unmasking was a no-op.
  r <- forkIO $ do
        pprTraceM "RUNNING" name
        r <- act
        putMVar res_var r

  return $ ActionResult res_var (killThread r) name

type ActionMap f = M.DependentMap f ActionResult

emptyActionMap :: ActionMap f
emptyActionMap  = M.emptyDepMap

killAllActions :: ActionMap f -> IO ()
killAllActions =
  MC.uninterruptibleMask_ . sequence_ . M.elemsDepMap getKill
  where
    getKill :: ActionResult a -> IO ()
    getKill = killAction

queueAction :: (Outputable (f a), M.GOrd f) => MVar (ActionMap f)
                                            -> QSem
                                            -> f a
                                            -> IO a
                                            -> IO a
queueAction act_var par_sem key raw_act = do
  join $ modifyMVar act_var (\m ->
    case M.lookupDepMap key m of
      Just a -> do
        return (m, waitResult a)
      Nothing -> do
        pprTraceM "create" (ppr key)
        wrapped_act <- mkAction (ppr key) par_sem raw_act
        return (M.insertDepMap key wrapped_act m, waitResult wrapped_act))

withSem :: QSem -> IO b -> IO b
withSem sem = MC.bracket_ (waitQSem sem) (signalQSem sem)

data SemaphoreReader f = SemaphoreReader { semaphoreHooks :: Hooks
                                         , semaphoreSem   :: QSem
                                         , semaphoreActions :: MVar (ActionMap f)
                                         }

newtype SemaphoreUse f a = SemaphoreUse { runSemaphoreUse :: SemaphoreReader f -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch) via (ReaderT (SemaphoreReader f) IO)

-- LogQueue Abstraction

-- | Each module is given a unique 'LogQueue' to redirect compilation messages
-- to. A 'Nothing' value contains the result of compilation, and denotes the
-- end of the message queue.
data LogQueue = LogQueue !(IORef [Maybe (MessageClass, SrcSpan, SDoc)])
                         !(MVar ())

newLogQueue :: IO LogQueue
newLogQueue = do
  mqueue <- newIORef []
  sem <- newMVar ()
  return (LogQueue mqueue sem)

finishLogQueue :: LogQueue -> IO ()
finishLogQueue lq =
  writeLogQueueInternal lq Nothing


writeLogQueue :: LogQueue -> (MessageClass,SrcSpan,SDoc) -> IO ()
writeLogQueue lq msg =
  writeLogQueueInternal lq (Just msg)

-- | Internal helper for writing log messages
writeLogQueueInternal :: LogQueue -> Maybe (MessageClass,SrcSpan,SDoc) -> IO ()
writeLogQueueInternal (LogQueue ref sem) msg = do
    atomicModifyIORef' ref $ \msgs -> (msg:msgs,())
    _ <- tryPutMVar sem ()
    return ()

-- The log_action callback that is used to synchronize messages from a
-- worker thread.
parLogAction :: LogQueue -> LogAction
parLogAction log_queue _dflags !msgClass !srcSpan !msg =
    writeLogQueue log_queue (msgClass,srcSpan,msg)

-- Print each message from the log_queue using the global logger
printLogs :: Logger -> LogQueue -> IO ()
printLogs !logger (LogQueue ref sem) = read_msgs
  where read_msgs = do
            takeMVar sem
            msgs <- atomicModifyIORef' ref $ \xs -> ([], reverse xs)
            print_loop msgs

        print_loop [] = read_msgs
        print_loop (x:xs) = case x of
            Just (msgClass,srcSpan,msg) -> do
                logMsg logger msgClass srcSpan msg
                print_loop xs
            -- Exit the loop once we encounter the end marker.
            Nothing -> return ()


{-
            forkIOWithUnmask $ \unmask -> do
                liftIO $ label_self $ unwords $ concat
                    [ [ "worker --make thread" ]
                    , case mod of
                        InstantiationNode iuid ->
                          [ "for instantiation of unit"
                          , show $ VirtUnit iuid
                          ]
                        ModuleNode ems ->
                          [ "for module"
                          , show (moduleNameString (ms_mod_name (emsModSummary ems)))
                          ]
                    , ["number"
                      , show mod_idx
                      ]
                    ]

                -- Replace the default logger with one that writes each
                -- message to the module's log_queue. The main thread will
                -- deal with synchronously printing these messages.
                let lcl_logger = pushLogHook (const (parLogAction log_queue)) thread_safe_logger

                -- Use a local TmpFs so that we can clean up intermediate files
                -- in a timely fashion (as soon as compilation for that module
                -- is finished) without having to worry about accidentally
                -- deleting a simultaneous compile's important files.
                lcl_tmpfs <- forkTmpFsFrom tmpfs


                -- Populate the result MVar.
                putMVar mvar res

                -- Write the end marker to the message queue, telling the main
                -- thread that it can stop waiting for messages from this
                -- particular compile.
                writeLogQueue log_queue Nothing

                -- Add the remaining files that weren't cleaned up to the
                -- global TmpFs, for cleanup later.
                mergeTmpFsInto lcl_tmpfs tmpfs

        -- Kill all the workers, masking interrupts (since killThread is
        -- interruptible). XXX: This is not ideal.
        ; killWorkers = MC.uninterruptibleMask_ . mapM_ killThread }
                    -}