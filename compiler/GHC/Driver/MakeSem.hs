{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Implementation of a jobserver using system semaphores.
--
--
module GHC.Driver.MakeSem
  ( -- * JSem: parallelism semaphore backed
    -- by a system semaphore (Posix/Windows)
    runJSemAbstractSem

  -- * System semaphores
  , Semaphore, SemaphoreName(..)

  -- * Abstract semaphores
  , AbstractSem(..)
  , withAbstractSem
  )
  where

import GHC.Prelude
import GHC.Conc
import GHC.Data.OrdList
import GHC.IO.Exception
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Json

import System.Semaphore

import Control.Monad
import qualified Control.Monad.Catch as MC
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.Foldable
import Data.Functor
import GHC.Stack
import Debug.Trace

---------------------------------------
-- Semaphore jobserver

-- | A jobserver based off a system 'Semaphore'.
--
-- Keeps track of the pending jobs and resources
-- available from the semaphore.
data Jobserver
  = Jobserver
  { jSemaphore :: !Semaphore
    -- ^ The semaphore which controls available resources
  , jobs :: !(TVar JobResources)
    -- ^ The currently pending jobs, and the resources
    -- obtained from the semaphore
  }

data JobserverOptions
  = JobserverOptions
  { releaseDebounce    :: !Int
     -- ^ Minimum delay, in milliseconds, between acquiring a token
     -- and releasing a token.
  , setNumCapsDebounce :: !Int
    -- ^ Minimum delay, in milliseconds, between two consecutive
    -- calls of 'setNumCapabilities'.
  }

defaultJobserverOptions :: JobserverOptions
defaultJobserverOptions =
  JobserverOptions
    { releaseDebounce    = 1000 -- 1 second
    , setNumCapsDebounce = 1000 -- 1 second
    }

-- | Resources available for running jobs, i.e.
-- tokens obtained from the parallelism semaphore.
data JobResources
  = Jobs
  { tokensOwned :: !Int
    -- ^ How many tokens have been claimed from the semaphore
  , tokensFree  :: !Int
    -- ^ How many tokens are not currently being used
  , jobsWaiting :: !(OrdList (TMVar ()))
    -- ^ Pending jobs waiting on a token, the job will be blocked on the TMVar so putting into
    -- the TMVar will allow the job to continue.
  }

instance Outputable JobResources where
  ppr Jobs{..}
    = text "JobResources" <+>
        ( braces $ hsep
          [ text "owned=" <> ppr tokensOwned
          , text "free=" <> ppr tokensFree
          , text "num_waiting=" <> ppr (length jobsWaiting)
          ] )

-- | Add one new token.
addToken :: JobResources -> JobResources
addToken jobs@( Jobs { tokensOwned = owned, tokensFree = free })
  = jobs { tokensOwned = owned + 1, tokensFree = free + 1 }

-- | Free one token.
addFreeToken :: JobResources -> JobResources
addFreeToken jobs@( Jobs { tokensFree = free })
  = assertPpr (tokensOwned jobs > free)
      (text "addFreeToken:" <+> ppr (tokensOwned jobs) <+> ppr free)
  $ jobs { tokensFree = free + 1 }

-- | Use up one token.
removeFreeToken :: JobResources -> JobResources
removeFreeToken jobs@( Jobs { tokensFree = free })
  = assertPpr (free > 0)
      (text "removeFreeToken:" <+> ppr free)
  $ jobs { tokensFree = free - 1 }

-- | Return one owned token.
removeOwnedToken :: JobResources -> JobResources
removeOwnedToken jobs@( Jobs { tokensOwned = owned })
  = assertPpr (owned > 1)
      (text "removeOwnedToken:" <+> ppr owned)
  $ jobs { tokensOwned = owned - 1 }

-- | Add one new job to the end of the list of pending jobs.
addJob :: TMVar () -> JobResources -> JobResources
addJob job jobs@( Jobs { jobsWaiting = wait })
  = jobs { jobsWaiting = wait `SnocOL` job }

-- | The state of the semaphore job server.
data JobserverState
  = JobserverState
    { jobserverAction  :: !JobserverAction
      -- ^ The current action being performed by the
      -- job server.
    , canChangeNumCaps :: !(TVar Bool)
      -- ^ A TVar that signals whether it has been long
      -- enough since we last changed 'numCapabilities'.
    , canReleaseToken  :: !(TVar Bool)
      -- ^ A TVar that signals whether we last acquired
      -- a token long enough ago that we can now release
      -- a token.
    }
data JobserverAction
  -- | The jobserver is idle: no thread is currently
  -- interacting with the semaphore.
  = Idle
  -- | A thread is waiting for a token on the semaphore.
  | Acquiring
    { activeWaitId   :: WaitId
    , threadFinished :: TMVar (Maybe MC.SomeException) }

-- | Retrieve the 'TMVar' that signals if the current thread has finished,
-- if any thread is currently active in the jobserver.
activeThread_maybe :: JobserverAction -> Maybe (TMVar (Maybe MC.SomeException))
activeThread_maybe Idle                                   = Nothing
activeThread_maybe (Acquiring { threadFinished = tmvar }) = Just tmvar

-- | Whether we should try to acquire a new token from the semaphore:
-- there is a pending job and no free tokens.
guardAcquire :: JobResources -> Bool
guardAcquire ( Jobs { tokensFree, jobsWaiting } )
  = tokensFree == 0 && not (null jobsWaiting)

-- | Whether we should release a token from the semaphore:
-- there are no pending jobs and we can release a token.
guardRelease :: JobResources -> Bool
guardRelease ( Jobs { tokensFree, tokensOwned, jobsWaiting } )
  = null jobsWaiting && tokensFree > 0 && tokensOwned > 1

---------------------------------------
-- Semaphore jobserver implementation

-- | Add one pending job to the jobserver.
--
-- Blocks, waiting on the jobserver to supply a free token.
acquireJob :: TVar JobResources -> IO ()
acquireJob jobs_tvar = do
  (job_tmvar, _jobs0) <- tracedAtomically "acquire" $
    modifyJobResources jobs_tvar \ jobs -> do
      job_tmvar <- newEmptyTMVar
      return ((job_tmvar, jobs), addJob job_tmvar jobs)
  atomically $ takeTMVar job_tmvar

-- | Signal to the job server that one job has completed,
-- releasing its corresponding token.
releaseJob :: TVar JobResources -> IO ()
releaseJob jobs_tvar = do
  tracedAtomically "release" do
    modifyJobResources jobs_tvar \ jobs -> do
      massertPpr (tokensFree jobs < tokensOwned jobs)
        (text "releaseJob: more free jobs than owned jobs!")
      return ((), addFreeToken jobs)


-- | Release all tokens owned from the semaphore (to clean up
-- the jobserver at the end).
cleanupJobserver :: Jobserver -> IO ()
cleanupJobserver (Jobserver { jSemaphore = sem
                            , jobs       = jobs_tvar })
  = do
    Jobs { tokensOwned = owned } <- readTVarIO jobs_tvar
    let toks_to_release = owned - 1
      -- Subtract off the implicit token: whoever spawned the ghc process
      -- in the first place is responsible for that token.
    releaseSemaphore sem toks_to_release

-- | Dispatch the available tokens acquired from the semaphore
-- to the pending jobs in the job server.
dispatchTokens :: JobResources -> STM JobResources
dispatchTokens jobs@( Jobs { tokensFree = toks_free, jobsWaiting = wait } )
  | toks_free > 0
  , next `ConsOL` rest <- wait
  -- There's a pending job and a free token:
  -- pass on the token to that job, and recur.
  = do
      putTMVar next ()
      let jobs' = jobs { tokensFree = toks_free - 1, jobsWaiting = rest }
      dispatchTokens jobs'
  | otherwise
  = return jobs

-- | Update the available resources used from a semaphore, dispatching
-- any newly acquired resources.
--
-- Invariant: if the number of available resources decreases, there
-- must be no pending jobs.
--
-- All modifications should go through this function to ensure the contents
-- of the 'TVar' remains in normal form.
modifyJobResources :: HasCallStack => TVar JobResources
                   -> (JobResources -> STM (a, JobResources))
                   -> STM (a, Maybe JobResources)
modifyJobResources jobs_tvar action = do
  old_jobs  <- readTVar jobs_tvar
  (a, jobs) <- action old_jobs

  -- Check the invariant: if the number of free tokens has decreased,
  -- there must be no pending jobs.
  massertPpr (null (jobsWaiting jobs) || tokensFree jobs >= tokensFree old_jobs) $
    vcat [ text "modiyJobResources: pending jobs but fewer free tokens" ]
  dispatched_jobs <- dispatchTokens jobs
  writeTVar jobs_tvar dispatched_jobs
  return (a, Just dispatched_jobs)


tracedAtomically_ :: String -> STM (Maybe JobResources) -> IO ()
tracedAtomically_ s act = tracedAtomically s (((),) <$> act)

tracedAtomically :: String -> STM (a, Maybe JobResources) -> IO a
tracedAtomically origin act = do
  (a, mjr) <- atomically act
  forM_ mjr $ \ jr -> do
    -- Use the "jsem:" prefix to identify where the write traces are
    traceEventIO ("jsem:" ++ renderJobResources origin jr)
  return a

renderJobResources :: String -> JobResources -> String
renderJobResources origin (Jobs own free pending) = showSDocUnsafe $ renderJSON $
  JSObject [ ("name", JSString origin)
           , ("owned", JSInt own)
           , ("free", JSInt free)
           , ("pending", JSInt (length pending) )
           ]


-- | Spawn a new thread that waits on the semaphore in order to acquire
-- an additional token.
acquireThread :: Jobserver -> IO JobserverAction
acquireThread (Jobserver { jSemaphore = sem, jobs = jobs_tvar }) = do
    threadFinished_tmvar <- newEmptyTMVarIO
    let
      wait_result_action :: Either MC.SomeException Bool -> IO ()
      wait_result_action wait_res =
        tracedAtomically_ "acquire_thread" do
          (r, jb) <- case wait_res of
            Left (e :: MC.SomeException) -> do
              return $ (Just e, Nothing)
            Right success -> do
              if success
                then do
                  modifyJobResources jobs_tvar \ jobs ->
                    return (Nothing, addToken jobs)
                else
                  return (Nothing, Nothing)
          putTMVar threadFinished_tmvar r
          return jb
    wait_id <- forkWaitOnSemaphoreInterruptible sem wait_result_action
    labelThread (waitingThreadId wait_id) "acquire_thread"
    return $ Acquiring { activeWaitId   = wait_id
                       , threadFinished = threadFinished_tmvar }

-- | Spawn a thread to release ownership of one resource from the semaphore,
-- provided we have spare resources and no pending jobs.
releaseThread :: Jobserver -> IO JobserverAction
releaseThread (Jobserver { jSemaphore = sem, jobs = jobs_tvar }) = do
  threadFinished_tmvar <- newEmptyTMVarIO
  MC.mask_ do
    -- Pre-release the resource so that another thread doesn't take control of it
    -- just as we release the lock on the semaphore.
    still_ok_to_release
      <- tracedAtomically "pre_release" $
         modifyJobResources jobs_tvar \ jobs ->
           if guardRelease jobs
               -- TODO: should this also debounce?
           then return (True , removeOwnedToken $ removeFreeToken jobs)
           else return (False, jobs)
    if not still_ok_to_release
    then return Idle
    else do
      tid <- forkIO $ do
        x <- MC.try $ releaseSemaphore sem 1
        tracedAtomically_ "post-release" $ do
          (r, jobs) <- case x of
            Left (e :: MC.SomeException) -> do
              modifyJobResources jobs_tvar \ jobs ->
                return (Just e, addToken jobs)
            Right _ -> do
              return (Nothing, Nothing)
          putTMVar threadFinished_tmvar r
          return jobs
      labelThread tid "release_thread"
      return Idle

-- | When there are pending jobs but no free tokens,
-- spawn a thread to acquire a new token from the semaphore.
--
-- See 'acquireThread'.
tryAcquire :: JobserverOptions
           -> Jobserver
           -> JobserverState
           -> STM (IO JobserverState)
tryAcquire opts js@( Jobserver { jobs = jobs_tvar })
  st@( JobserverState { jobserverAction = Idle } )
  = do
    jobs <- readTVar jobs_tvar
    guard $ guardAcquire jobs
    return do
      action           <- acquireThread js
      -- Set a debounce after acquiring a token.
      can_release_tvar <- registerDelay $ (releaseDebounce opts * 1000)
      return $ st { jobserverAction = action
                  , canReleaseToken = can_release_tvar }
tryAcquire _ _ _ = retry

-- | When there are free tokens and no pending jobs,
-- spawn a thread to release a token from the semamphore.
--
-- See 'releaseThread'.
tryRelease :: Jobserver
           -> JobserverState
           -> STM (IO JobserverState)
tryRelease sjs@( Jobserver { jobs = jobs_tvar } )
  st@( JobserverState
      { jobserverAction = Idle
      , canReleaseToken = can_release_tvar } )
  = do
    jobs <- readTVar jobs_tvar
    guard  $ guardRelease jobs
    can_release <- readTVar can_release_tvar
    guard can_release
    return do
      action <- releaseThread sjs
      return $ st { jobserverAction = action }
tryRelease _ _ = retry

-- | Wait for an active thread to finish. Once it finishes:
--
--  - set the 'JobserverAction' to 'Idle',
--  - update the number of capabilities to reflect the number
--    of owned tokens from the semaphore.
tryNoticeIdle :: JobserverOptions
              -> TVar JobResources
              -> JobserverState
              -> STM (IO JobserverState)
tryNoticeIdle opts jobs_tvar jobserver_state
  | Just threadFinished_tmvar <- activeThread_maybe $ jobserverAction jobserver_state
  = sync_num_caps (canChangeNumCaps jobserver_state) threadFinished_tmvar
  | otherwise
  = retry -- no active thread: wait until jobserver isn't idle
  where
    sync_num_caps :: TVar Bool
                  -> TMVar (Maybe MC.SomeException)
                  -> STM (IO JobserverState)
    sync_num_caps can_change_numcaps_tvar threadFinished_tmvar = do
      mb_ex <- takeTMVar threadFinished_tmvar
      for_ mb_ex MC.throwM
      Jobs { tokensOwned } <- readTVar jobs_tvar
      can_change_numcaps <- readTVar can_change_numcaps_tvar
      guard can_change_numcaps
      return do
        x <- getNumCapabilities
        can_change_numcaps_tvar_2 <-
          if x == tokensOwned
          then return can_change_numcaps_tvar
          else do
            setNumCapabilities tokensOwned
            registerDelay $ (setNumCapsDebounce opts * 1000)
        return $
          jobserver_state
            { jobserverAction  = Idle
            , canChangeNumCaps = can_change_numcaps_tvar_2 }

-- | Try to stop the current thread which is acquiring/releasing resources
-- if that operation is no longer relevant.
tryStopThread :: TVar JobResources
              -> JobserverState
              -> STM (IO JobserverState)
tryStopThread jobs_tvar jsj = do
  case jobserverAction jsj of
    Acquiring { activeWaitId = wait_id } -> do
     jobs <- readTVar jobs_tvar
     guard $ null (jobsWaiting jobs)
     return do
       interruptWaitOnSemaphore wait_id
       return $ jsj { jobserverAction = Idle }
    _ -> retry

-- | Main jobserver loop: acquire/release resources as
-- needed for the pending jobs and available semaphore tokens.
jobserverLoop :: JobserverOptions -> Jobserver -> IO ()
jobserverLoop opts sjs@(Jobserver { jobs = jobs_tvar })
  = do
      true_tvar <- newTVarIO True
      let init_state :: JobserverState
          init_state =
            JobserverState
              { jobserverAction  = Idle
              , canChangeNumCaps = true_tvar
              , canReleaseToken  = true_tvar }
      loop init_state
  where
    loop s = do
      action <- atomically $ asum $ (\x -> x s) <$>
        [ tryRelease    sjs
        , tryAcquire    opts sjs
        , tryNoticeIdle opts jobs_tvar
        , tryStopThread jobs_tvar
        ]
      s <- action
      loop s

-- | Create a new jobserver using the given semaphore handle.
makeJobserver :: SemaphoreName -> IO (AbstractSem, IO ())
makeJobserver sem_name = do
  semaphore <- openSemaphore sem_name
  let
    init_jobs =
      Jobs { tokensOwned = 1
           , tokensFree  = 1
           , jobsWaiting = NilOL
           }
  jobs_tvar <- newTVarIO init_jobs
  let
    opts = defaultJobserverOptions -- TODO: allow this to be configured
    sjs = Jobserver { jSemaphore = semaphore
                    , jobs       = jobs_tvar }
  loop_finished_mvar <- newEmptyMVar
  loop_tid <- forkIOWithUnmask \ unmask -> do
    r <- try $ unmask $ jobserverLoop opts sjs
    putMVar loop_finished_mvar $
      case r of
        Left e
          | Just ThreadKilled <- fromException e
          -> Nothing
          | otherwise
          -> Just e
        Right () -> Nothing
  labelThread loop_tid "job_server"
  let
    acquireSem = acquireJob jobs_tvar
    releaseSem = releaseJob jobs_tvar
    cleanupSem = do
      -- this is interruptible
      cleanupJobserver sjs
      killThread loop_tid
      mb_ex <- takeMVar loop_finished_mvar
      for_ mb_ex MC.throwM

  return (AbstractSem{..}, cleanupSem)

-- | Implement an abstract semaphore using a semaphore 'Jobserver'
-- which queries the system semaphore of the given name for resources.
runJSemAbstractSem :: SemaphoreName         -- ^ the system semaphore to use
                   -> (AbstractSem -> IO a) -- ^ the operation to run
                                            -- which requires a semaphore
                   -> IO a
runJSemAbstractSem sem action = MC.mask \ unmask -> do
  (abs, cleanup) <- makeJobserver sem
  r <- try $ unmask $ action abs
  case r of
    Left (e1 :: MC.SomeException) -> do
      (_ :: Either MC.SomeException ()) <- MC.try cleanup
      MC.throwM e1
    Right x -> cleanup $> x

{- Note [Architecture of the Job Server]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In `-jsem` mode, the amount of parallelism that GHC can use is controlled by a
system semaphore. We take resources from the semaphore when we need them, and
give them back if we don't have enough to do.

A naive implementation would just take and release the semaphore around performing
the action, but this leads to two issues:

* When taking a token in the semaphore, we must call `setNumCapabilities` in order
  to adjust how many capabilities are available for parallel garbage collection.
  This causes unnecessary synchronisations.
* We want to implement a debounce, so that whilst there is pending work in the
  current process we prefer to keep hold of resources from the semaphore.
  This reduces overall memory usage, as there are fewer live GHC processes at once.

Therefore, the obtention of semaphore resources is separated away from the
request for the resource in the driver.

A token from the semaphore is requested using `acquireJob`. This creates a pending
job, which is a MVar that can be filled in to signal that the requested token is ready.

When the job is finished, the token is released by calling `releaseJob`, which just
increases the number of `free` jobs. If there are more pending jobs when the free count
is increased, the token is immediately reused (see `modifyJobResources`).

The `jobServerLoop` interacts with the system semaphore: when there are pending
jobs, `acquireThread` blocks, waiting for a token from the semaphore. Once a
token is obtained, it increases the owned count.

When GHC has free tokens (tokens from the semaphore that it is not using),
no pending jobs, and the debounce has expired, then `releaseThread` will
release tokens back to the global semaphore.

`tryStopThread` attempts to kill threads which are waiting to acquire a resource
when we no longer need it. For example, consider that we attempt to acquire two
tokens, but the first job finishes before we acquire the second token.
This second token is no longer needed, so we should cancel the wait
(as it would not be used to do any work, and not be returned until the debounce).
We only need to kill `acquireJob`, because `releaseJob` never blocks.

Note [Eventlog Messages for jsem]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It can be tricky to verify that the work is shared adequately across different
processes. To help debug this, we output the values of `JobResource` to the
eventlog whenever the global state changes. There are some scripts which can be used
to analyse this output and report statistics about core saturation in the
GitHub repo (https://github.com/mpickering/ghc-jsem-analyse).

-}
