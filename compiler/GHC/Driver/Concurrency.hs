{-# LANGUAGE CPP #-}

{-# LANGUAGE BlockArguments #-}

module GHC.Driver.Concurrency
  ( -- * Worker limit
    WorkerLimit(..)

    -- * Concurrent worker scheduling
  , ConcurrentWorkerEnv(..)
  , WorkerPoolConfig(..)

    -- ** Independent workers
  , mapIndependentWorkers

    -- ** Coordinating workers
  , runCoordinatingWorkers

  )
  where

import GHC.Prelude


import GHC.Driver.MakeSem
import GHC.Driver.Pipeline.LogQueue
  ( LogQueue, finishLogQueue, initLogQueue, logThread
  , newLogQueue, newLogQueueQueue, parLogAction, printLogs )
import GHC.Utils.Logger
  ( Logger, makeThreadSafe, pushLogHook )
import GHC.Utils.Misc
  ( HasDebugCallStack )
import GHC.Utils.Outputable
  ( Outputable(..), text, (<+>) )
import GHC.Utils.Panic
  ( massertPpr, pprPanic )
import GHC.Utils.TmpFs
  ( TmpFs, forkTmpFsFrom, mergeTmpFsInto, withLocalTmpFS )
import qualified GHC.Utils.Concurrent.Scope as Scope

import System.Semaphore
  ( SemaphoreError, SemaphoreIdentifier )

#if defined(wasm32_HOST_ARCH) || defined(javascript_HOST_ARCH)
import Control.Concurrent
  ( myThreadId )
import Control.Concurrent.MVar
  ( newMVar, putMVar, takeMVar )
import GHC.Conc
  ( labelThread )
#else
import Control.Concurrent
  ( myThreadId, newQSem, signalQSem, waitQSem )
import GHC.Conc
  ( getNumCapabilities, getNumProcessors, labelThread, setNumCapabilities )
#endif
import Control.Concurrent.STM
  ( STM, atomically, newTVarIO, writeTVar )
import Control.Exception
  ( SomeAsyncException, SomeException
  , bracket, finally, fromException, mask, onException, throwIO, try )
import Control.Monad
  ( unless )
import Data.Foldable
  ( for_ )
import Data.IORef
  ( IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef )
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

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

-- | Open the semaphore for the given worker limit, returning it together with
-- its release action (which also restores the RTS capability count).
acquireWorkerLimit
  :: (SemaphoreError -> IO ())
     -- ^ report failure when opening the @-jsem@ semaphore
     -- (after which we fall back to running with a single job)
  -> WorkerLimit -> IO (AbstractSem, IO ())
#if defined(wasm32_HOST_ARCH) || defined(javascript_HOST_ARCH)
acquireWorkerLimit _report_semaphore_failure _ = do
  lock <- newMVar ()
  pure (AbstractSem (takeMVar lock) (putMVar lock ()), pure ())
#else
acquireWorkerLimit report_semaphore_failure worker_limit = case worker_limit of
    NumProcessorsLimit n_jobs ->
      acquireNjobsAbstractSem n_jobs
    JSemLimit sem_ident ->
      acquireJSemAbstractSem sem_ident >>= \case
        Right acquired -> pure acquired
        Left err -> do
          report_semaphore_failure err
          acquireNjobsAbstractSem 1
#endif

#if !(defined(wasm32_HOST_ARCH) || defined(javascript_HOST_ARCH))
acquireNjobsAbstractSem :: Int -> IO (AbstractSem, IO ())
acquireNjobsAbstractSem n_jobs = do
  compile_sem <- newQSem n_jobs
  n_capabilities <- getNumCapabilities
  n_cpus <- getNumProcessors
  let
    asem = AbstractSem (waitQSem compile_sem) (signalQSem compile_sem)
    set_num_caps n = unless (n_capabilities /= 1) $ setNumCapabilities n
  -- Setting number of capabilities more than CPU count usually leads to high
  -- userspace lock contention. #9221
  set_num_caps $ min n_jobs n_cpus
  pure ( asem, set_num_caps n_capabilities )
#endif

--------------------------------------------------------------------------------
-- * Workers
--------------------------------------------------------------------------------

data Concurrency
  -- | @-j1@: run serially.
  = Serial
  -- | @-jN@/@-jsem@: run concurrently, with maximum concurrency controlled
  -- by the given abstract semaphore.
  | Concurrent !AbstractSem

-- | __Internal__: run an action with the given concurrency control.
--
-- Deliberately not exported, so that consumers are not burdened with manual
-- concurrency management.
with_concurrency :: Concurrency -> IO a -> IO a
with_concurrency conc act =
  case conc of
    Serial         -> act
    Concurrent sem -> withAbstractSem sem act

-- | The local environment of a worker thread that may be scheduled concurrently.
data ConcurrentWorkerEnv = ConcurrentWorkerEnv
  { cwe_logger :: !Logger
  , cwe_tmpfs  :: !TmpFs
  , cwe_conc   :: !Concurrency
  }

-- | Run an action with a local 'TmpFs', merging in the resulting temporary file
-- accumulator into the parent afterwards.
workerEnv_withLocalTmpFS :: ConcurrentWorkerEnv -> (ConcurrentWorkerEnv -> IO a) -> IO a
workerEnv_withLocalTmpFS env use =
  withLocalTmpFS (cwe_tmpfs env) \ lcl_tmpfs ->
    use env { cwe_tmpfs = lcl_tmpfs }

--------------------------------------------------------------------------------
-- * Monotone data structures
--------------------------------------------------------------------------------

-- | A map that only ever grows, and whose entries are written at most once.
newtype MonotoneMap k v = MonotoneMap ( IORef ( Map.Map k v ) )

newMonotoneMap :: Map.Map k v -> IO ( MonotoneMap k v )
newMonotoneMap initial = MonotoneMap <$> newIORef initial

-- | The outcome of inserting into a 'MonotoneMap'.
data InsertionResult
  -- | The key was absent before the insertion.
  = Inserted
  -- | The key was already present; the container is unchanged.
  | AlreadyPresent

-- | Write an entry into a 'MonotoneMap' unless the key is already present.
insertMonotoneMap :: Ord k => MonotoneMap k v -> k -> v -> IO InsertionResult
insertMonotoneMap ( MonotoneMap ref ) k v =
  atomicModifyIORef' ref \ m ->
    case Map.insertLookupWithKey ( \ _ _ old -> old ) k v m of
      ( Nothing , m' ) -> ( m', Inserted )
      ( Just _  , _  ) -> ( m , AlreadyPresent )

-- | Write a new entry into a 'MonotoneMap'.
--
-- Panics if the entry is already present.
insertMonotoneMap_new
  :: ( HasDebugCallStack, Ord k, Outputable k )
  => MonotoneMap k v -> k -> v -> IO ()
insertMonotoneMap_new  mm k v =
  insertMonotoneMap mm k v >>= \case
    Inserted       -> pure ()
    AlreadyPresent -> pprPanic "monotone map: duplicate key" $ ppr k

-- | The contents of a monotone map.
freezeMonotoneMap :: MonotoneMap k v -> IO ( Map.Map k v )
freezeMonotoneMap ( MonotoneMap ref ) = readIORef ref

--------------------------------------------------------------------------------
-- * Pools of concurrent workers
--------------------------------------------------------------------------------

{- Note [Deterministic concurrent workers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The output of compilation must not depend on how the computation was scheduled,
e.g. the usage of -jN or -jsem.

It would be easy to guarantee determinism by having a single parent thread
consume worker results in a fixed order, but that would cost parallelism: work
discovered by a finished worker could not start until every worker started
before it had also finished.

To solve this problem, GHC.Driver.Concurrency defines two kinds of concurrent
workers:

  - Independent workers ('mapIndependentWorkers'), which cannot wait for one
    another at all.
    The only scheduling operation is quiescence: wait for all workers to
    complete, and only then observe the final state.

  - Coordinating workers ('runCoordinatingWorkers') start by first waiting
    for dependent workers' results.

To achieve determinism, we must guarantee that every worker operation is
insensitive to the precise time it was run. We guarantee this as follows:

  - Workers can only report a result by writing to write-once state: monotone
    maps for independent workers, and each work item's own result variable for
    coordinating workers.

  - Any read of this mutable state happens after the unique write:
      - a coordinating worker only runs once its dependencies are ready,
      - for independent workers, we only read results after quiescence.

  - For log output, each worker's log queue is accumulated by the key the worker
    was spawned with (a pure function of the work item). This ensures that the
    log output is printed in a deterministic order.

The remaining requirement is that all of the workers' observable effects must
be commutable (e.g. filesystem reads, cache reads). In particular, a shared
cache may only be written as memoisation of a pure function of the immutable
environment, so that the answer a worker reads never depends on when another
worker's write occurred.

Failure is quasi-deterministic: whether an exception is reported is deterministic,
but the specific error reported depends on the order in which workers were
scheduled. Consumers that need deterministic exceptions should record failures
as values (as e.g. 'mapIndependentWorkers' does).
-}

-- | The configuration of a pool of concurrent workers.
data WorkerPoolConfig =
  WorkerPoolConfig
    { wp_label  :: !String
      -- ^ thread label for the pool's workers
    , wp_limit  :: !WorkerLimit
      -- ^ the @-jN@/@-jsem@ limit
    , wp_report_semaphore_failure :: !(SemaphoreError -> IO ())
      -- ^ report a failure to open the @-jsem@ semaphore
      -- (after which we fall back to running serially)
    , wp_logger :: !Logger
      -- ^ the parent logger; each worker's own logger is derived from this
    , wp_tmpfs  :: !TmpFs
      -- ^ the parent temporary-file accumulator; each worker's own is
      -- derived from this
    }

-- | The order in which logging should happen when using concurrent workers.
data LogOrder
  -- | Log as we go.
  --
  -- Only valid when workers are spawned in a deterministic order.
  = LogAsWeGo
  -- | Accumulate logs per worker. Once all work is done, sort the logs
  -- before proceeding.
  --
  -- Used when workers may be spawned in a non-deterministic order.
  | SortLogs

-- | Spawn one worker with the given worker key, returning whether the worker
-- was actually spawned.
--
-- May be called from inside another worker.
--
-- An exception escaping the action stops further workers from being spawned,
-- and is rethrown by 'withWorkerPool'.
--
-- See Note [Deterministic concurrent workers].
type SpawnWorker worker_key = worker_key -> ( ConcurrentWorkerEnv -> IO () ) -> IO Bool
  -- NB: the worker is not limited by any concurrency; any concurrent work that
  -- needs such limitation should use 'with_concurrency' on the worker's 'cwe_conc'.

-- | Run an action with a pool of concurrent workers, in which the output of
-- each worker is ordered by its worker key.
--
-- Does not return until every worker the action spawned has finished.
withWorkerPool
  :: forall worker_key a
  .  ( HasDebugCallStack, Ord worker_key, Outputable worker_key )
  => WorkerPoolConfig
  -> LogOrder
  -> ( SpawnWorker worker_key -> IO a )
  -> IO a
withWorkerPool
  ( WorkerPoolConfig { wp_label = worker_label, wp_limit = limit
                     , wp_report_semaphore_failure = report_semaphore_failure
                     , wp_logger = logger, wp_tmpfs = tmpfs } )
  log_order withPool

  | isWorkerLimitSequential limit
  = do
      queued_var <- newIORef $ Seq.empty @( worker_key, ConcurrentWorkerEnv -> IO () )
      logs_var   <- newMonotoneMap $ Map.empty @worker_key @LogQueue

      let
        parent_work_env :: ConcurrentWorkerEnv
        parent_work_env =
          ConcurrentWorkerEnv
            { cwe_logger = logger, cwe_tmpfs = tmpfs, cwe_conc = Serial }

        spawnWorker :: SpawnWorker worker_key
        spawnWorker worker_key action = do
          modifyIORef' queued_var ( Seq.|> ( worker_key, action ) )
          pure True

        run_worker :: worker_key -> ( ConcurrentWorkerEnv -> IO () ) -> IO ()
        run_worker worker_key action = case log_order of
          LogAsWeGo ->
            workerEnv_withLocalTmpFS parent_work_env action
          SortLogs ->
            -- Masked until the finaliser is installed: every log queue must be
            -- finished, or 'print_logs' blocks forever.
            mask \ restore -> do
              -- Use a log queue for consistency with the concurrent case.
              lq <- newLogQueue
              insertMonotoneMap_new logs_var worker_key lq
              let
                worker_work_env :: ConcurrentWorkerEnv
                worker_work_env =
                  parent_work_env { cwe_logger = worker_logger logger lq }
              restore ( workerEnv_withLocalTmpFS worker_work_env action )
                `finally` finishLogQueue lq

        run_queued_workers :: IO ()
        run_queued_workers = do
          next <- atomicModifyIORef' queued_var \ queued ->
            case queued of
              work Seq.:<| rest -> ( rest  , Just work )
              Seq.Empty         -> ( queued, Nothing )
          for_ next \ ( worker_key, action ) ->
            run_worker worker_key action *> run_queued_workers

        print_logs :: IO ()
        print_logs = do
          logs <- freezeMonotoneMap logs_var
          for_ ( Map.elems logs ) $ printLogs logger

      ( `finally` print_logs ) do
        result <- withPool spawnWorker
        run_queued_workers
        pure result

  | otherwise
  = bracket ( acquireWorkerLimit report_semaphore_failure limit ) snd
      \ ( sem, _close_sem ) -> do
      safe_logger <- makeThreadSafe logger
      let
        parent_work_env :: ConcurrentWorkerEnv
        parent_work_env =
          ConcurrentWorkerEnv
            { cwe_logger = safe_logger
            , cwe_tmpfs  = tmpfs
            , cwe_conc   = Concurrent sem
            }

        run_pool
          :: ( worker_key -> LogQueue -> IO () )
          -> IO ()
          -> IO a
        run_pool register_log_queue finaliser =
          ( `finally` finaliser ) $
            Scope.scoped \ scope -> do
              let
                spawnWorker :: SpawnWorker worker_key
                spawnWorker worker_key action =
                  let
                    acquire :: IO (LogQueue, TmpFs)
                    acquire = do
                      lcl_tmpfs <- forkTmpFsFrom tmpfs
                      lq        <- newLogQueue
                      -- Log queue registration must happen last: a subsequent
                      -- synchronous exception would leave the log queue open,
                      -- never finalised.
                      register_log_queue worker_key lq
                      pure (lq, lcl_tmpfs)
                    release :: (LogQueue, TmpFs) -> IO ()
                    release (lq, lcl_tmpfs) = do
                      mergeTmpFsInto lcl_tmpfs tmpfs
                      finishLogQueue lq
                    inner_action :: (LogQueue, TmpFs) -> IO ()
                    inner_action (lq, lcl_tmpfs) = do
                      tid <- myThreadId
                      labelThread tid worker_label
                      action $
                        parent_work_env
                          { cwe_tmpfs  = lcl_tmpfs
                          , cwe_logger = worker_logger safe_logger lq
                          }
                  in
                    Scope.forkIn scope acquire release inner_action

              withPool spawnWorker

      case log_order of

        LogAsWeGo -> do
          lqq_var          <- newTVarIO newLogQueueQueue
          stopped_var      <- newTVarIO False
          wait_log_thread  <- logThread safe_logger stopped_var lqq_var
          next_id_var      <- newIORef @Int 1
          last_spawned_var <- newIORef @(Maybe worker_key) Nothing
          let
            register_log_queue worker_key lq = do
              last_spawned <-
                atomicModifyIORef' last_spawned_var \ last_spawned ->
                  ( Just worker_key, last_spawned )
              massertPpr ( all ( < worker_key ) last_spawned ) $
                text "withWorkerPool: LogAsWeGo workers spawned out of order:"
                  <+> ppr last_spawned <+> text "then" <+> ppr worker_key
              job_id <- atomicModifyIORef' next_id_var \ n -> ( n + 1, n )
              atomically $ initLogQueue lqq_var job_id lq
            finaliser = do
              atomically $ writeTVar stopped_var True
              wait_log_thread
          run_pool register_log_queue finaliser

        SortLogs -> do
          -- Accumulate a log per worker key.
          logs_var <- newMonotoneMap $ Map.empty @worker_key @LogQueue
          let
            register_log_queue worker_key lq =
              insertMonotoneMap_new logs_var worker_key lq
            finaliser = do
              queued <- freezeMonotoneMap logs_var
              for_ ( Map.elems queued ) $ printLogs safe_logger
          run_pool register_log_queue finaliser

  where
    worker_logger :: Logger -> LogQueue -> Logger
    worker_logger parent_logger worker_log_queue =
      pushLogHook ( const ( parLogAction worker_log_queue ) ) parent_logger

--------------------------------------------------------------------------------
-- * Independent workers
--------------------------------------------------------------------------------

-- | Map a worker action over the input list.
--
-- Each list element is handled by its own independent worker running with
-- limited concurrency.
--
-- The result is returned at the end once all workers have finished; workers
-- have no way to wait for one another.
--
-- A worker failing with a synchronous exception does not affect the others:
-- the exception is recorded as that item's result, and the first one (in
-- input order) is rethrown at the end. An asynchronous exception tears the
-- whole pool down.
mapIndependentWorkers
  :: forall a b
  .  HasDebugCallStack
  => WorkerPoolConfig
  -> (ConcurrentWorkerEnv -> a -> IO b)
      -- ^ individual worker action, run with limited concurrency
  -> [a]
  -> IO [b]
mapIndependentWorkers pool_config f xs =
  -- LogAsWeGo: workers are keyed by their position in the input list and
  -- spawned in that same order, so their output can be printed as it is produced.
  do
    results <- newMonotoneMap $ Map.empty @Int @( Either SomeException b )
    withWorkerPool pool_config LogAsWeGo \ spawnWorker ->
      for_ ( zip [ 0 .. ] xs ) \ ( i, x ) ->
        spawnWorker i \ worker_env -> do
          res <- try @SomeException $
            with_concurrency (cwe_conc worker_env) (f worker_env x)
          case res of
            Left e
              -- Take care to avoid swallowing async exceptions.
              | Just _ <- fromException @SomeAsyncException e
              -> throwIO e
            _ -> insertMonotoneMap_new results i res
    all_results <- freezeMonotoneMap results
    massertPpr ( Map.size all_results == length xs ) $
      text "mapIndependentWorkers: missing results"
    either throwIO pure $ sequence $ Map.elems all_results

--------------------------------------------------------------------------------
-- * Coordinating workers
--------------------------------------------------------------------------------

-- | Concurrent traversal of the input list by concurrent workers.
--
-- Workers are co-operative: each item declares when it is ready to run (e.g.
-- every result it depends on has been written), and workers answer via a
-- separate channel (e.g. by writing to a mutable variable).
--
-- Each worker first waits to be ready without limiting concurrency; the work
-- itself then runs with limited concurrency (e.g. holding a semaphore
-- token throughout).
--
-- For every work item, either the worker action runs to completion or the
-- skip action runs.
runCoordinatingWorkers
  :: HasDebugCallStack
  => WorkerPoolConfig
  -> (a -> STM ())
      -- ^ readiness condition
  -> (ConcurrentWorkerEnv -> a -> IO ())
      -- ^ individual worker action, run with limited concurrency
  -> (a -> IO ())
      -- ^ skip action, run whenever the worker does not complete;
      -- must be idempotent and non-blocking
  -> [a]
  -> IO ()
runCoordinatingWorkers pool_config ready worker skip_action xs =
  -- LogAsWeGo: workers are keyed by their position in the input list and
  -- spawned in that same order, so their output can be printed as it is produced.
  withWorkerPool pool_config LogAsWeGo \ spawnWorker ->
    for_ ( zip [ 0 :: Int .. ] xs ) \ ( i, x ) -> do
      started <- spawnWorker i \ worker_env ->
        ( do
            atomically $ ready x
            with_concurrency (cwe_conc worker_env) $
              worker worker_env x
        ) `onException` skip_action x
      unless started $ skip_action x
