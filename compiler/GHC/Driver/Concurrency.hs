{-# LANGUAGE CPP #-}

{-# LANGUAGE BlockArguments #-}

module GHC.Driver.Concurrency
  ( -- * Worker limit and concurrency
    WorkerLimit(..)
  , isWorkerLimitSequential
  , withWorkerLimit
  , Concurrency
  , withConcurrency
    -- * Concurrent worker scheduling
  , ConcurrentWorkerEnv(..)
  , mapConcurrentWorkers
  , concurrentTraversal_DF
  )
  where

import GHC.Prelude

import GHC.Driver.MakeSem
import GHC.Driver.Pipeline.LogQueue
  ( LogQueueQueue, finishLogQueue, initLogQueue, logThread
  , newLogQueue, newLogQueueQueue, parLogAction )
import GHC.Utils.Logger
  ( Logger, makeThreadSafe, pushLogHook )
import GHC.Utils.Panic
  ( panic )
import GHC.Utils.TmpFs
  ( TmpFs, forkTmpFsFrom, mergeTmpFsInto, withLocalTmpFS )

import System.Semaphore
  ( SemaphoreError, SemaphoreIdentifier )

#if defined(wasm32_HOST_ARCH) || defined(javascript_HOST_ARCH)
import Control.Concurrent
  ( ThreadId, forkIOWithUnmask, killThread, myThreadId )
import Control.Concurrent.MVar
  ( MVar, newEmptyMVar, newMVar, putMVar, takeMVar )
import GHC.Conc
  ( labelThread )
#else
import Control.Concurrent
  ( ThreadId, forkIOWithUnmask, killThread, myThreadId
  , newQSem, signalQSem, waitQSem, MVar, takeMVar, putMVar, newEmptyMVar )
import Control.Monad
  ( unless )
import qualified Control.Monad.Catch as MC
import GHC.Conc
  ( getNumCapabilities, getNumProcessors, labelThread, setNumCapabilities )
#endif
import Control.Concurrent.STM
  ( TVar, atomically, check, modifyTVar', newTVarIO, readTVar, writeTVar )
import Control.Exception
  ( AsyncException(ThreadKilled), SomeAsyncException, SomeException
  , finally, fromException, mask, mask_, onException
  , throwIO, try, uninterruptibleMask_ )
import Control.Monad
  ( replicateM )
import Data.Foldable
  ( for_ )
import Data.IORef
  ( IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef )
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- * Worker Limit
--------------------------------------------------------------------------------

-- | A limit on the number of concurrent worker threads.
data WorkerLimit
  -- | Fixed concurrent worker count limit @-jN@
  = NumProcessorsLimit Int
  -- | The concurrent worker count is limited by a @-jsem@ semaphore
  | JSemLimit
      SemaphoreIdentifier
        -- ^ Semaphore identifier (from the @semaphore-compat@ library)
  deriving Eq

isWorkerLimitSequential :: WorkerLimit -> Bool
isWorkerLimitSequential (NumProcessorsLimit x) = x <= 1
isWorkerLimitSequential (JSemLimit {})         = False

runWorkerLimit
  :: (SemaphoreError -> IO ())
     -- ^ report failure when opening the @-jsem@ semaphore
     -- (after which we fall back to running with a single job)
  -> WorkerLimit -> (AbstractSem -> IO a) -> IO a
#if defined(wasm32_HOST_ARCH) || defined(javascript_HOST_ARCH)
runWorkerLimit _report_semaphore_failure _ action = do
  lock <- newMVar ()
  action $ AbstractSem (takeMVar lock) (putMVar lock ())
#else
runWorkerLimit report_semaphore_failure worker_limit action = case worker_limit of
    NumProcessorsLimit n_jobs ->
      runNjobsAbstractSem n_jobs action
    JSemLimit sem_ident ->
      runJSemAbstractSem sem_ident action >>= \case
        Right a -> return a
        Left err -> do
          report_semaphore_failure err
          runNjobsAbstractSem 1 action
#endif

#if !(defined(wasm32_HOST_ARCH) || defined(javascript_HOST_ARCH))
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

#endif

--------------------------------------------------------------------------------
-- * Workers
--------------------------------------------------------------------------------

data Concurrency
  = Serial
  | Concurrent !ConcurrencyEnv

-- | Run an action with the given concurrency control (serial or concurrent).
withConcurrency :: Concurrency -> IO a -> IO a
withConcurrency conc act =
  case conc of
    Serial -> act
    Concurrent ( ConcurrencyEnv { ce_semaphore } ) ->
      withAbstractSem ce_semaphore act

data ConcurrencyEnv =
  ConcurrencyEnv
    { ce_semaphore         :: !AbstractSem
    , ce_log_queue_queue   :: !( TVar LogQueueQueue )
    , ce_next_log_queue_id :: !( IORef Int )
    }

-- | The local environment of a worker thread that may be scheduled concurrently.
data ConcurrentWorkerEnv = ConcurrentWorkerEnv
  { cwe_logger :: !Logger
  , cwe_tmpfs  :: !TmpFs
  }

-- | Run an action with a local 'TmpFs', merging in the resulting temporary file
-- accumulator into the parent afterwards.
workerEnv_withLocalTmpFS :: ConcurrentWorkerEnv -> (ConcurrentWorkerEnv -> IO a) -> IO a
workerEnv_withLocalTmpFS env use =
  withLocalTmpFS (cwe_tmpfs env) \ lcl_tmpfs ->
    use env { cwe_tmpfs = lcl_tmpfs }

-- | Run an action either serially or concurrently based on the provided
-- 'WorkerLimit'.
withWorkerLimit
  :: Logger
  -> TmpFs
  -> (SemaphoreError -> IO ())
     -- ^ report a failure to open the @-jsem@ semaphore
     -- (after which we fall back to running with a single job)
  -> WorkerLimit
  -> (Concurrency -> ConcurrentWorkerEnv -> IO a) -- ^ action to run
  -> IO a
withWorkerLimit logger tmpfs report_semaphore_failure limit action
  | isWorkerLimitSequential limit
  = action Serial $
      ConcurrentWorkerEnv
        { cwe_logger = logger
        , cwe_tmpfs  = tmpfs
        }
  | otherwise
  = do
      safe_logger     <- makeThreadSafe logger
      lqq_var         <- newTVarIO newLogQueueQueue
      stopped_var     <- newTVarIO False
      wait_log_thread <- logThread safe_logger stopped_var lqq_var
      next_logq_var   <- newIORef 1

      let
        stop_logging :: IO ()
        stop_logging = do
          atomically $ writeTVar stopped_var True
          wait_log_thread

        parent_work_env :: ConcurrentWorkerEnv
        parent_work_env =
          ConcurrentWorkerEnv
            { cwe_logger = safe_logger
            , cwe_tmpfs  = tmpfs
            }

      ( `finally` stop_logging ) $
        runWorkerLimit report_semaphore_failure limit \ sem -> do
          let
            conc =
              Concurrent $
                ConcurrencyEnv
                  { ce_semaphore         = sem
                  , ce_log_queue_queue   = lqq_var
                  , ce_next_log_queue_id = next_logq_var
                  }
          action conc parent_work_env

--------------------------------------------------------------------------------
-- * Scheduling concurrent workers
--------------------------------------------------------------------------------

-- | Internal scheduler abstraction with two capabilities:
--
--  - spawn a new worker thread
--  - wait for a worker thread to complete
data Scheduler r = Scheduler
  { spawnWorker :: (ConcurrentWorkerEnv -> IO r) -> IO ()
    -- ^ Spawn one concurrent worker.
    --
    -- The worker does not hold a token of the concurrency semaphore: the
    -- worker action should use 'withConcurrency' around the work whose
    -- concurrency should be limited.
  , awaitWorker :: IO (Either SomeException r)
    -- ^ Wait for one worker to complete.
    --
    -- Will crash if there are no outstanding workers.
  }

-- | Internal implementation of a concurrent worker scheduler.
--
-- Usage of this function requires the following:
--
--  - all spawn/await actions are performed by a single thread,
--  - we never wait for more workers than were spawned,
--  - no worker outlives 'run_schedule'.
run_schedule
  :: forall r a
  .  String
      -- ^ thread label for workers
  -> Concurrency
  -> ConcurrentWorkerEnv
  -> (Scheduler r -> IO a)
        -- ^ worker action
  -> IO a
run_schedule worker_label conc parent_work_env withScheduler =
  case conc of

    Serial -> do
      results_var <- newIORef Seq.empty
      let
        spawnWorker :: (ConcurrentWorkerEnv -> IO r) -> IO ()
        spawnWorker action = do
          res <- try @SomeException $
            workerEnv_withLocalTmpFS parent_work_env action
          case res of
            Left e
              | Just _ <- fromException @SomeAsyncException e
              -> throwIO e
            _ -> modifyIORef' results_var (Seq.|> res)

        awaitWorker :: IO (Either SomeException r)
        awaitWorker =
          readIORef results_var >>= \case
            res Seq.:<| rest -> do
              writeIORef results_var rest
              pure res
            Seq.Empty ->
              panic "run_schedule: no outstanding job"

      withScheduler $ Scheduler { spawnWorker, awaitWorker }

    Concurrent ( ConcurrencyEnv { ce_next_log_queue_id, ce_log_queue_queue } ) -> do
      worker_tids_var <- newTVarIO $ Set.empty @ThreadId
      all_results_vars_var <- newIORef $ Seq.empty @(MVar (Either SomeException r))

      let
        wait_for_workers :: IO ()
        wait_for_workers =
          atomically $
            check . Set.null =<< readTVar worker_tids_var

        cancel_workers :: IO ()
        cancel_workers = do
          uninterruptibleMask_ do
            tids <- atomically $ readTVar worker_tids_var
            for_ tids killThread
          wait_for_workers

        awaitWorker :: IO (Either SomeException r)
        awaitWorker =
          readIORef all_results_vars_var >>= \case
            first_worker_res_var Seq.:<| rest -> do
              writeIORef all_results_vars_var rest
              -- block on the earliest-spawned outstanding worker
              takeMVar first_worker_res_var
            Seq.Empty ->
              panic "run_schedule: no outstanding job"

        spawnWorker :: (ConcurrentWorkerEnv -> IO r) -> IO ()
        spawnWorker action = mask_ do

          worker_res_var <- newEmptyMVar

          -- TmpFs
          lcl_tmpfs <- forkTmpFsFrom (cwe_tmpfs parent_work_env)

          -- LogQueue
          lq <- do
            job_id <- atomicModifyIORef' ce_next_log_queue_id \n -> (n + 1, n)
            lq <- newLogQueue job_id
            atomically $ initLogQueue ce_log_queue_queue lq
            pure lq

          let

            worker_work_env :: ConcurrentWorkerEnv
            worker_work_env =
              parent_work_env
                { cwe_tmpfs  = lcl_tmpfs
                , cwe_logger = pushLogHook (const (parLogAction lq))
                                (cwe_logger parent_work_env)
                }

            -- Run a worker action and record its result.
            run_worker_and_record :: IO r -> IO ()
            run_worker_and_record worker_action = do
              res <- try @SomeException worker_action
              case res of
                Left e
                  -- Worker is being cancelled: don't record anything.
                  | Just ThreadKilled <- fromException e
                  -> pure ()
                _ -> putMVar worker_res_var res

            -- Record that a worker thread is done.
            mark_worker_done :: ThreadId -> IO ()
            mark_worker_done tid =
              uninterruptibleMask_ do
                -- Uninterruptible: the deletion below /must/ occur.
                -- An uninterruptible mask is OK as we only ever block for (GAP) below.
                mergeTmpFsInto lcl_tmpfs $ cwe_tmpfs parent_work_env
                finishLogQueue lq
                atomically do
                  tids <- readTVar worker_tids_var
                  check $ tid `Set.member` tids
                    -- Ensure we never end up with a dead ThreadId in 'worker_tids_var'
                    -- (if the worker thread finishes before the parent thread has
                    -- the time to add its ThreadId to 'worker_tids_var').

                  writeTVar worker_tids_var $ Set.delete tid tids

            run_worker :: (forall b. IO b -> IO b) -> IO ()
            run_worker unmask = do
              tid <- myThreadId
              labelThread tid worker_label
              let
                worker_action :: IO r
                worker_action = unmask $ action worker_work_env

              run_worker_and_record worker_action `finally`
                mark_worker_done tid

          worker_tid <-
            forkIOWithUnmask run_worker
              `onException` finishLogQueue lq
          -- Very short (GAP) between forking the thread and recording its ThreadId.
          atomically $ modifyTVar' worker_tids_var $ Set.insert worker_tid
          modifyIORef' all_results_vars_var (Seq.|> worker_res_var)

      mask \ restore -> do
        result <- restore (withScheduler $ Scheduler { spawnWorker, awaitWorker })
                    `onException` cancel_workers
        restore wait_for_workers `onException` cancel_workers
        pure result

--------------------------------------------------------------------------------
-- * Derived scheduling functionality
--------------------------------------------------------------------------------

-- | Map a worker action over the input list with the given concurrency control.
--
-- Workers run to completion (no early abort); the first exception
-- (in input order) is rethrown at the end.
mapConcurrentWorkers
  :: String -- ^ thread label for workers
  -> Concurrency
  -> ConcurrentWorkerEnv
  -> (ConcurrentWorkerEnv -> a -> IO b)
      -- ^ individual worker action
      --
      -- NB: workers do not hold semaphore tokens by default; use
      -- 'withConcurrency' to acquire one
  -> [a]
  -> IO [b]
mapConcurrentWorkers worker_label conc work_env f xs =
  run_schedule worker_label conc work_env \ scheduler -> do
    for_ xs \ x -> spawnWorker scheduler \ worker_env -> f worker_env x
    results <- replicateM (length xs) (awaitWorker scheduler)
    either throwIO pure (sequence results)

-- | Depth-first traversal with on-the-fly expansion of nodes.
--
-- Each expansion step is handled by a worker thread under the given
-- concurrency control.
--
-- Deterministic: expansions are consumed in the order the nodes were
-- discovered, so the traversal is a function of the node graph alone.
--
-- Fails fast: the first worker exception cancels the outstanding workers and
-- is rethrown.
concurrentTraversal_DF
  :: forall k n r
  .  Ord k
  => String -- ^ thread label for workers
  -> Concurrency
  -> ConcurrentWorkerEnv
  -> Map.Map k r
     -- ^ results known ahead of time (no expansion needed)
  -> [n]
     -- ^ root nodes
  -> (n -> k)
     -- ^ node key from node
  -> (ConcurrentWorkerEnv -> n -> IO (r, [n]))
     -- ^ worker action: expand a node into its result and the children to visit next
     --
     -- NB: workers do not hold semaphore tokens by default; use
     -- 'withConcurrency' to acquire one
  -> IO (Map.Map k r)
concurrentTraversal_DF worker_label conc work_env base_map roots key expand =
  run_schedule worker_label conc work_env \ scheduler -> do
    let
      expand_node :: n -> ConcurrentWorkerEnv -> IO (k, (r, [n]))
      expand_node node worker_env = do
        res <- expand worker_env node
        pure (key node, res)

      go
        :: Map.Map k r -- expanded nodes and their results
        -> Set.Set k   -- nodes currently being expanded
        -> [n]         -- discovered nodes, to expand next
        -> IO (Map.Map k r)
      go !visited !pending (node : worklist)
        | k `Set.member` pending || k `Map.member` visited
        = go visited pending worklist
        | otherwise
        = do spawnWorker scheduler (expand_node node)
             go visited (Set.insert k pending) worklist
        where
          k = key node
      go visited pending []
        | Set.null pending
        = pure visited
        | otherwise
        = awaitWorker scheduler >>= \case
            Left e -> throwIO e
            Right (k, (result, children)) ->
              go (Map.insert k result visited) (Set.delete k pending) children

    go base_map Set.empty roots
