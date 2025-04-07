{-# LANGUAGE CPP #-}
module GHC.Driver.MakeAction
  ( MakeAction(..)
  , MakeEnv(..)
  , RunMakeM
  -- * Running the pipelines
  , runAllPipelines
  , runParPipelines
  , runSeqPipelines
  , runPipelines
  -- * Worker limit
  , WorkerLimit(..)
  , mkWorkerLimit
  , runWorkerLimit
  -- * Utility
  , withLoggerHsc
  , withParLog
  , withLocalTmpFS
  , withLocalTmpFSMake
  ) where

import GHC.Prelude
import GHC.Driver.DynFlags

import GHC.Driver.Monad
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.Main
import GHC.Driver.MakeSem

import GHC.Utils.Logger
import GHC.Utils.TmpFs

import Control.Concurrent ( newQSem, waitQSem, signalQSem, ThreadId, killThread, forkIOWithUnmask )
import qualified GHC.Conc as CC
import Control.Concurrent.MVar
import Control.Monad
import qualified Control.Monad.Catch as MC

import GHC.Conc ( getNumProcessors, getNumCapabilities, setNumCapabilities )
import Control.Monad.Trans.Reader
import GHC.Driver.Pipeline.LogQueue
import Control.Concurrent.STM
import Control.Monad.Trans.Maybe

-- Executing the pipelines

mkWorkerLimit :: DynFlags -> IO WorkerLimit
mkWorkerLimit dflags =
  case parMakeCount dflags of
    Nothing -> pure $ num_procs 1
    Just (ParMakeSemaphore h) -> pure (JSemLimit (SemaphoreName h))
    Just ParMakeNumProcessors -> num_procs <$> getNumProcessors
    Just (ParMakeThisMany n) -> pure $ num_procs n
  where
    num_procs x = NumProcessorsLimit (max 1 x)

isWorkerLimitSequential :: WorkerLimit -> Bool
isWorkerLimitSequential (NumProcessorsLimit x) = x <= 1
isWorkerLimitSequential (JSemLimit {})         = False

-- | This describes what we use to limit the number of jobs, either we limit it
-- ourselves to a specific number or we have an external parallelism semaphore
-- limit it for us.
data WorkerLimit
  = NumProcessorsLimit Int
  | JSemLimit
    SemaphoreName
      -- ^ Semaphore name to use
  deriving Eq

-- | Environment used when compiling a module
data MakeEnv = MakeEnv { hsc_env :: !HscEnv -- The basic HscEnv which will be augmented for each module
                       , compile_sem :: !AbstractSem
                       -- Modify the environment for module k, with the supplied logger modification function.
                       -- For -j1, this wrapper doesn't do anything
                       -- For -jn, the wrapper initialised a log queue and then modifies the logger to pipe its output
                       --          into the log queue.
                       , withLogger :: forall a . Int -> ((Logger -> Logger) -> IO a) -> IO a
                       , env_messager :: !(Maybe Messager)
                       , diag_wrapper :: GhcMessage -> AnyGhcDiagnostic
                       }


label_self :: String -> IO ()
label_self thread_name = do
    self_tid <- CC.myThreadId
    CC.labelThread self_tid thread_name


runPipelines :: WorkerLimit -> HscEnv -> (GhcMessage -> AnyGhcDiagnostic) -> Maybe Messager -> [MakeAction] -> IO ()
-- Don't even initialise plugins if there are no pipelines
runPipelines n_job hsc_env diag_wrapper mHscMessager all_pipelines = do
  liftIO $ label_self "main --make thread"
  case n_job of
    NumProcessorsLimit n | n <= 1 -> runSeqPipelines hsc_env diag_wrapper mHscMessager all_pipelines
    _n -> runParPipelines n_job hsc_env diag_wrapper mHscMessager all_pipelines

runSeqPipelines :: HscEnv -> (GhcMessage -> AnyGhcDiagnostic) -> Maybe Messager -> [MakeAction] -> IO ()
runSeqPipelines plugin_hsc_env diag_wrapper mHscMessager all_pipelines =
  let env = MakeEnv { hsc_env = plugin_hsc_env
                    , withLogger = \_ k -> k id
                    , compile_sem = AbstractSem (return ()) (return ())
                    , env_messager = mHscMessager
                    , diag_wrapper = diag_wrapper
                    }
  in runAllPipelines (NumProcessorsLimit 1) env all_pipelines

runNjobsAbstractSem :: Int -> (AbstractSem -> IO a) -> IO a
runNjobsAbstractSem n_jobs action = do
  compile_sem <- newQSem n_jobs
  n_capabilities <- getNumCapabilities
  n_cpus <- getNumProcessors
  let
    asem = AbstractSem (waitQSem compile_sem) (signalQSem compile_sem)
    set_num_caps n = unless (n_capabilities /= 1) $ setNumCapabilities n
    updNumCapabilities =  do
      -- Setting number of capabilities more than
      -- CPU count usually leads to high userspace
      -- lock contention. #9221
      set_num_caps $ min n_jobs n_cpus
    resetNumCapabilities = set_num_caps n_capabilities
  MC.bracket_ updNumCapabilities resetNumCapabilities $ action asem

runWorkerLimit :: WorkerLimit -> (AbstractSem -> IO a) -> IO a
#if defined(wasm32_HOST_ARCH)
runWorkerLimit _ action = do
  lock <- newMVar ()
  action $ AbstractSem (takeMVar lock) (putMVar lock ())
#else
runWorkerLimit worker_limit action = case worker_limit of
    NumProcessorsLimit n_jobs ->
      runNjobsAbstractSem n_jobs action
    JSemLimit sem ->
      runJSemAbstractSem sem action
#endif

-- | Build and run a pipeline
runParPipelines :: WorkerLimit -- ^ How to limit work parallelism
             -> HscEnv         -- ^ The basic HscEnv which is augmented with specific info for each module
             -> (GhcMessage -> AnyGhcDiagnostic)
             -> Maybe Messager   -- ^ Optional custom messager to use to report progress
             -> [MakeAction]  -- ^ The build plan for all the module nodes
             -> IO ()
runParPipelines worker_limit plugin_hsc_env diag_wrapper mHscMessager all_pipelines = do


  -- A variable which we write to when an error has happened and we have to tell the
  -- logging thread to gracefully shut down.
  stopped_var <- newTVarIO False
  -- The queue of LogQueues which actions are able to write to. When an action starts it
  -- will add it's LogQueue into this queue.
  log_queue_queue_var <- newTVarIO newLogQueueQueue
  -- Thread which coordinates the printing of logs
  wait_log_thread <- logThread (hsc_logger plugin_hsc_env) stopped_var log_queue_queue_var


  -- Make the logger thread-safe, in case there is some output which isn't sent via the LogQueue.
  thread_safe_logger <- liftIO $ makeThreadSafe (hsc_logger plugin_hsc_env)
  let thread_safe_hsc_env = plugin_hsc_env { hsc_logger = thread_safe_logger }

  runWorkerLimit worker_limit $ \abstract_sem -> do
    let env = MakeEnv { hsc_env = thread_safe_hsc_env
                      , withLogger = withParLog log_queue_queue_var
                      , compile_sem = abstract_sem
                      , env_messager = mHscMessager
                      , diag_wrapper = diag_wrapper
                      }
    -- Reset the number of capabilities once the upsweep ends.
    runAllPipelines worker_limit env all_pipelines
    atomically $ writeTVar stopped_var True
    wait_log_thread

withLoggerHsc :: Int -> MakeEnv -> (HscEnv -> IO a) -> IO a
withLoggerHsc k MakeEnv{withLogger, hsc_env} cont = do
  withLogger k $ \modifyLogger -> do
    let lcl_logger = modifyLogger (hsc_logger hsc_env)
        hsc_env' = hsc_env { hsc_logger = lcl_logger }
    -- Run continuation with modified logger
    cont hsc_env'

withParLog :: TVar LogQueueQueue -> Int -> ((Logger -> Logger) -> IO b) -> IO b
withParLog lqq_var k cont = do
  let init_log = do
        -- Make a new log queue
        lq <- newLogQueue k
        -- Add it into the LogQueueQueue
        atomically $ initLogQueue lqq_var lq
        return lq
      finish_log lq = liftIO (finishLogQueue lq)
  MC.bracket init_log finish_log $ \lq -> cont (pushLogHook (const (parLogAction lq)))

withLocalTmpFS :: TmpFs -> (TmpFs -> IO a) -> IO a
withLocalTmpFS tmpfs act = do
  let initialiser = do
        liftIO $ forkTmpFsFrom tmpfs
      finaliser tmpfs_local = do
        liftIO $ mergeTmpFsInto tmpfs_local tmpfs
       -- Add remaining files which weren't cleaned up into local tmp fs for
       -- clean-up later.
       -- Clear the logQueue if this node had it's own log queue
  MC.bracket initialiser finaliser act

withLocalTmpFSMake :: MakeEnv -> (MakeEnv -> IO a) -> IO a
withLocalTmpFSMake env k =
  withLocalTmpFS (hsc_tmpfs (hsc_env env)) $ \lcl_tmpfs
    -> k (env { hsc_env = (hsc_env env) { hsc_tmpfs = lcl_tmpfs }})


-- | Run the given actions and then wait for them all to finish.
runAllPipelines :: WorkerLimit -> MakeEnv -> [MakeAction] -> IO ()
runAllPipelines worker_limit env acts = do
  let single_worker = isWorkerLimitSequential worker_limit
      spawn_actions :: IO [ThreadId]
      spawn_actions = if single_worker
        then (:[]) <$> (forkIOWithUnmask $ \unmask -> void $ runLoop (\io -> io unmask) env acts)
        else runLoop forkIOWithUnmask env acts

      kill_actions :: [ThreadId] -> IO ()
      kill_actions tids = mapM_ killThread tids

  MC.bracket spawn_actions kill_actions $ \_ -> do
    mapM_ waitMakeAction acts

-- | Execute each action in order, limiting the amount of parallelism by the given
-- semaphore.
runLoop :: (((forall a. IO a -> IO a) -> IO ()) -> IO a) -> MakeEnv -> [MakeAction] -> IO [a]
runLoop _ _env [] = return []
runLoop fork_thread env (MakeAction act res_var :acts) = do

  -- withLocalTmpFs has to occur outside of fork to remain deterministic
  new_thread <- withLocalTmpFSMake env $ \lcl_env ->
    fork_thread $ \unmask -> (do
            mres <- (unmask $ run_pipeline lcl_env act)
                      `MC.onException` (putMVar res_var Nothing) -- Defensive: If there's an unhandled exception then still signal the failure.
            putMVar res_var mres)
  threads <- runLoop fork_thread env acts
  return (new_thread : threads)
  where
      run_pipeline :: MakeEnv -> RunMakeM a -> IO (Maybe a)
      run_pipeline env p = runMaybeT (runReaderT p env)

type RunMakeM a = ReaderT MakeEnv (MaybeT IO) a

data MakeAction = forall a . MakeAction !(RunMakeM a) !(MVar (Maybe a))

waitMakeAction :: MakeAction -> IO ()
waitMakeAction (MakeAction _ mvar) = () <$ readMVar mvar