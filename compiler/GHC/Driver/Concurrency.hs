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
  , NodeExpander(..)
  , concurrentTraversal
  )
  where

import GHC.Prelude

import GHC.Driver.MakeSem
import GHC.Driver.Pipeline.LogQueue
  ( LogQueue, LogQueueQueue, finishLogQueue, initLogQueue, logThread
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

import System.Semaphore
  ( SemaphoreError, SemaphoreIdentifier )

#if defined(wasm32_HOST_ARCH) || defined(javascript_HOST_ARCH)
import Control.Concurrent
  ( ThreadId, forkIOWithUnmask, killThread, myThreadId )
import Control.Concurrent.MVar
  ( MVar, newMVar, putMVar, takeMVar )
import GHC.Conc
  ( labelThread )
#else
import Control.Concurrent
  ( ThreadId, forkIOWithUnmask, killThread, myThreadId
  , newQSem, signalQSem, waitQSem )
import qualified Control.Monad.Catch as MC
import GHC.Conc
  ( getNumCapabilities, getNumProcessors, labelThread, setNumCapabilities )
#endif
import Control.Concurrent.STM
  ( TVar, atomically, check, modifyTVar', newTVarIO, readTVar, readTVarIO
  , writeTVar )
import Control.Exception
  ( AsyncException(ThreadKilled), SomeAsyncException, SomeException
  , catch, finally, fromException, mask, mask_, onException
  , throwIO, try, uninterruptibleMask_ )
import Control.Monad
  ( unless )
import Data.Foldable
  ( for_, traverse_ )
import Data.IORef
  ( IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef )
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
-- * Monotone data structures
--------------------------------------------------------------------------------

-- | A map that only ever grows, and whose entries are written at most once.
newtype MonotoneMap k v = MonotoneMap ( IORef ( Map.Map k v ) )

newMonotoneMap :: Map.Map k v -> IO ( MonotoneMap k v )
newMonotoneMap initial = MonotoneMap <$> newIORef initial

-- | The outcome of inserting into a 'MonotoneMap' or a 'MonotoneSet'.
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

-- | A set that only ever grows.
newtype MonotoneSet k = MonotoneSet ( IORef ( Set.Set k ) )

newMonotoneSet :: Set.Set k -> IO ( MonotoneSet k )
newMonotoneSet initial = MonotoneSet <$> newIORef initial

-- | Add an element, unless it is already present.
insertMonotoneSet :: Ord k => MonotoneSet k -> k -> IO InsertionResult
insertMonotoneSet ( MonotoneSet ref ) k =
  atomicModifyIORef' ref \ s ->
    if k `Set.member` s
    then ( s              , AlreadyPresent )
    else ( Set.insert k s , Inserted )

--------------------------------------------------------------------------------
-- * Pools of concurrent workers
--------------------------------------------------------------------------------

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

-- | A pool of concurrent workers with a given worker key type, supporting two
-- operations:
--
--  - spawning a new worker,
--  - waiting on all workers to finish.
--
-- See Note [Deterministic concurrent workers].
data WorkerPool worker_key =
  WorkerPool
    { spawnWorker :: worker_key -> ( ConcurrentWorkerEnv -> IO () ) -> IO ()
      -- ^ Spawn one worker with the given worker key.
      --
      -- May be called from inside another worker.
      --
      -- The worker does not hold a token of the concurrency semaphore: the
      -- worker action should use 'withConcurrency' around the work whose
      -- concurrency should be limited.
      --
      -- An exception escaping the action stops further workers from being
      -- spawned, and is rethrown by 'waitForWorkers'.
    , waitForWorkers :: IO ()
      -- ^ Wait until all workers are done, throwing an exception if any
      -- worker failed (which exception is thrown is not deterministic).
    }

-- | Internal implementation of a pool of concurrent workers.
run_pool
  :: forall worker_key a
  .  ( HasDebugCallStack, Ord worker_key, Outputable worker_key )
  => String -- ^ thread label for workers
  -> LogOrder
  -> Concurrency
  -> ConcurrentWorkerEnv
  -> ( WorkerPool worker_key -> IO a )
  -> IO a
run_pool worker_label log_order conc parent_work_env withPool =
  case conc of

    Serial -> do
      queued_var <- newIORef $ Seq.empty @( worker_key, ConcurrentWorkerEnv -> IO () )
      logs_var   <- newMonotoneMap $ Map.empty @worker_key @LogQueue

      let
        spawnWorker :: worker_key -> ( ConcurrentWorkerEnv -> IO () ) -> IO ()
        spawnWorker worker_key action =
          modifyIORef' queued_var ( Seq.|> ( worker_key, action ) )

        run_worker :: worker_key -> ( ConcurrentWorkerEnv -> IO () ) -> IO ()
        run_worker worker_key action = case log_order of
          LogAsWeGo ->
            workerEnv_withLocalTmpFS parent_work_env action
          SortLogs -> do
            -- Use a log queue for consistency with the concurrent case.
            lq <- newLogQueue
            insertMonotoneMap_new logs_var worker_key lq
            let
              worker_work_env :: ConcurrentWorkerEnv
              worker_work_env =
                parent_work_env
                  { cwe_logger = pushLogHook ( const ( parLogAction lq ) )
                                   ( cwe_logger parent_work_env ) }
            workerEnv_withLocalTmpFS worker_work_env action
              `finally` finishLogQueue lq

        waitForWorkers :: IO ()
        waitForWorkers = do
          next <- atomicModifyIORef' queued_var \ queued ->
            case queued of
              work Seq.:<| rest -> ( rest  , Just work )
              Seq.Empty         -> ( queued, Nothing )
          case next of
            Nothing -> pure ()
            Just ( worker_key, action ) ->
              run_worker worker_key action *> waitForWorkers

        print_logs :: IO ()
        print_logs = do
          logs <- freezeMonotoneMap logs_var
          for_ ( Map.elems logs ) $ printLogs ( cwe_logger parent_work_env )

      withPool ( WorkerPool { spawnWorker, waitForWorkers } )
        `finally` print_logs

    Concurrent ( ConcurrencyEnv { ce_next_log_queue_id, ce_log_queue_queue } ) -> do
      worker_tids_var  <- newTVarIO $ Set.empty @ThreadId
      failure_var      <- newTVarIO $ Nothing @SomeException
      logs_var         <- newMonotoneMap $ Map.empty @worker_key @LogQueue
      last_spawned_var <- newIORef $ Nothing @worker_key

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

        -- Record a worker failure, preventing any further work from starting.
        record_failure :: SomeException -> IO ()
        record_failure e =
          atomically $ modifyTVar' failure_var \ failure ->
            case failure of
              Nothing -> Just e
              Just {} -> failure

        waitForWorkers :: IO ()
        waitForWorkers = do
          wait_for_workers
          traverse_ throwIO =<< readTVarIO failure_var

        -- Create the log queue of a worker, ordering it according to the worker key.
        new_worker_log_queue :: worker_key -> IO LogQueue
        new_worker_log_queue worker_key = do
          lq <- newLogQueue
          case log_order of
            LogAsWeGo -> do
              last_spawned <-
                atomicModifyIORef' last_spawned_var \ last_spawned ->
                  ( Just worker_key, last_spawned )
              massertPpr ( all ( < worker_key ) last_spawned ) $
                text "run_pool: LogAsWeGo workers spawned out of order:"
                  <+> ppr last_spawned <+> text "then" <+> ppr worker_key
              job_id <- atomicModifyIORef' ce_next_log_queue_id \ n -> ( n + 1, n )
              atomically $ initLogQueue ce_log_queue_queue job_id lq
            SortLogs ->
              insertMonotoneMap_new logs_var worker_key lq
          pure lq

        -- Hand the log queues over for printing, in worker key order.
        release_queued_logs :: IO ()
        release_queued_logs = do
          queued <- freezeMonotoneMap logs_var
          unless ( Map.null queued ) do
            first_id <-
              atomicModifyIORef' ce_next_log_queue_id \ n ->
                ( n + Map.size queued, n )
            atomically $
              for_ ( zip [ first_id .. ] ( Map.elems queued ) ) \ ( job_id, lq ) ->
                initLogQueue ce_log_queue_queue job_id lq

        spawnWorker :: worker_key -> ( ConcurrentWorkerEnv -> IO () ) -> IO ()
        spawnWorker worker_key action = mask_ do
          failure <- readTVarIO failure_var
          case failure of
            -- A worker has failed: don't start any more work.
            Just {} -> pure ()
            Nothing -> do

              -- TmpFs
              lcl_tmpfs <- forkTmpFsFrom ( cwe_tmpfs parent_work_env )

              -- LogQueue
              lq <- new_worker_log_queue worker_key

              let

                worker_work_env :: ConcurrentWorkerEnv
                worker_work_env =
                  parent_work_env
                    { cwe_tmpfs  = lcl_tmpfs
                    , cwe_logger = pushLogHook ( const ( parLogAction lq ) )
                                    ( cwe_logger parent_work_env )
                    }

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

                handle_worker_exception :: SomeException -> IO ()
                handle_worker_exception e
                  -- Worker is being cancelled: not a failure to report.
                  | Just ThreadKilled <- fromException e
                  = pure ()
                  | otherwise
                  = record_failure e

                run_worker :: ( forall b. IO b -> IO b ) -> IO ()
                run_worker unmask = do
                  tid <- myThreadId
                  labelThread tid worker_label
                  ( unmask ( action worker_work_env )
                      `catch` handle_worker_exception )
                    `finally` mark_worker_done tid

              worker_tid <-
                forkIOWithUnmask run_worker
                  `onException` finishLogQueue lq
              -- Very short (GAP) between forking the thread and recording its ThreadId.
              atomically $ modifyTVar' worker_tids_var $ Set.insert worker_tid

      ( `finally` release_queued_logs ) $
        mask \ restore -> do
          result <- restore ( withPool $ WorkerPool { spawnWorker, waitForWorkers } )
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
  :: forall a b
  .  HasDebugCallStack
  => String -- ^ thread label for workers
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
  -- LogAsWeGo: workers are keyed by their position in the input list and
  -- spawned in that same order, so their output can be printed as it is produced.
  run_pool worker_label LogAsWeGo conc work_env \ pool -> do
    results <- newMonotoneMap $ Map.empty @Int @( Either SomeException b )
    for_ ( zip [ 0 .. ] xs ) \ ( i, x ) ->
      spawnWorker pool i \ worker_env -> do
        res <- try @SomeException $ f worker_env x
        case res of
          Left e
            -- Take care to avoid swallowing async exceptions.
            | Just _ <- fromException @SomeAsyncException e
            -> throwIO e
          _ -> insertMonotoneMap_new results i res
    waitForWorkers pool
    all_results <- freezeMonotoneMap results
    massertPpr ( Map.size all_results == length xs ) $
      text "mapConcurrentWorkers: missing results"
    either throwIO pure $ sequence $ Map.elems all_results

-- | How to expand a node in a graph for 'concurrentTraversal'.
data NodeExpander k n v =
  NodeExpander
    { nodeKey :: n -> k
      -- ^ The identity of a node.
    , expandNode :: ConcurrentWorkerEnv -> n -> IO ( v, [n] )
      -- ^ Expand a node into its result and the children to visit next.
      --
      -- To guarantee determinism, the children must be a pure function of the
      -- input, and IO effects must not observably depend on the order in
      -- which nodes are expanded.
      --
      -- NB: workers do not hold semaphore tokens by default; use
      -- 'withConcurrency' to acquire one
    }

-- | Deterministically traverse a graph whose nodes are discovered as they are
-- expanded.
--
-- Fails fast: once a worker throws an exception, no further work is started,
-- and the exception is rethrown once the outstanding workers finish.
concurrentTraversal
  :: forall k n v
  .  ( HasDebugCallStack, Ord k, Outputable k )
  => String -- ^ thread label for workers
  -> Concurrency
  -> ConcurrentWorkerEnv
  -> NodeExpander k n v
  -> Map.Map k v
     -- ^ results known ahead of time (no expansion needed)
  -> [n]
     -- ^ root nodes
  -> IO ( Map.Map k v )
concurrentTraversal
  worker_label conc work_env
  ( NodeExpander { nodeKey, expandNode } )
  base_map roots
  =
  -- SortLogs: nodes are discovered in an order that depends on the schedule,
  -- so the workers' logs must be ordered before being printed.
  run_pool worker_label SortLogs conc work_env \ pool -> do

    -- The keys whose expansion has been started.
    claims  <- newMonotoneSet $ Map.keysSet base_map
    results <- newMonotoneMap base_map

    let
      discover :: n -> IO ()
      discover node =
        -- Claim the work for this node to avoid any other worker duplicating it.
        insertMonotoneSet claims key >>= \case
          AlreadyPresent -> pure ()
          Inserted ->
            spawnWorker pool key \ worker_env -> do
              ( result, children ) <- expandNode worker_env node
              insertMonotoneMap_new results key result
              for_ children discover
        where
          key = nodeKey node

    for_ roots discover
    waitForWorkers pool
    freezeMonotoneMap results

{- Note [Deterministic concurrent workers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To provide deterministic output when doing graph traversal (as in downsweep)
despite using concurrent workers, we ensure that nothing can observe the order
in which workers do their work:

  1. Any chunk of work is performed at most once: every worker atomically claims
     ownership of the work it is going to do before it starts that work.

  2. Workers report results by writing to a 'MonotoneMap', whose entries are
     written at most once. Other outputs (such as logging output) is accumulated
     in a deterministic order and reported at the end.

This scheme allows us to retain maximum concurrency: it allows new edges to be
discovered by any worker and immediately processed.

For this scheme to provide deterministic output, we require that:

  * The expansion of a node is a pure function of the node.
  * The work itself should not observably depend on when it was run.

Failure is not deterministic: which worker's exception is reported depends on
the schedule. When deterministic error messages are desired, the workers should
return an error value instead (as 'mapConcurrentWorkers' does).
-}
