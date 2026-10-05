{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

-- | Implementation of a jobserver using system semaphores.
--
--
module GHC.Driver.MakeSem
  (
#if !(defined(wasm32_HOST_ARCH) || defined(javascript_HOST_ARCH))
    -- * JSem: parallelism semaphore backed
    -- by a system semaphore (Posix/Windows)
    withJobserver,
#endif

  -- * Abstract semaphores
    AbstractSem(..)
  , withAbstractSem
  )
  where

#if defined(wasm32_HOST_ARCH) || defined(javascript_HOST_ARCH)

import System.Semaphore
  ( AbstractSem(..)
  , withAbstractSem
  )

#else

import GHC.Prelude
import GHC.Conc
import GHC.Data.OrdList
import GHC.Utils.Concurrent.Scope
  ( Scope, scoped, fork, forkIn, activeCount, interruptAll )
import GHC.Utils.Outputable
import GHC.Utils.Panic
  ( panic, assertPpr, massertPpr )
import GHC.Utils.Json

import Control.Concurrent.STM
import Control.Exception
  ( SomeException, bracketOnError, finally, mask_, try )
import Control.Monad
  ( guard, void, forM_ )
import Data.Foldable
  ( asum, for_ )
import Debug.Trace
  ( traceEventIO )
import GHC.Stack
  ( HasCallStack )

import System.Semaphore
  ( AbstractSem(..)
  , ClientSemaphore
  , SemaphoreToken
  , releaseSemaphoreToken
  , waitOnSemaphore
  , withAbstractSem
  )

---------------------------------------
-- Semaphore jobserver

-- | A jobserver based off a system 'Semaphore'.
--
-- Keeps track of the pending jobs and resources
-- available from the semaphore.
data Jobserver
  = Jobserver
  { jSemaphore :: !ClientSemaphore
    -- ^ The semaphore which controls available resources
  , jMaxCapabilities :: !Int
    -- ^ Maximum number of capabilities to ever use: a cache of
    -- 'getNumProcessors'
  , jobs :: !(TVar JobResources)
    -- ^ The currently pending jobs, and the resources
    -- obtained from the semaphore
  , jAcquirer :: !Scope
    -- ^ The scope of the acquire thread: at most one thread, waiting on the
    -- semaphore for one token
  }

data JobserverOptions
  = JobserverOptions
  { releaseDebounce    :: !Int
     -- ^ Minimum delay, in milliseconds, between wanting a token
     -- and releasing a token.
  , setNumCapsDebounce :: !Int
    -- ^ Minimum delay, in milliseconds, between two consecutive
    -- calls of 'setNumCapabilities'.
  }

defaultJobserverOptions :: JobserverOptions
defaultJobserverOptions =
  -- NB: semaphore operations cost microseconds (not milliseconds).
  -- The debounce nudges us towards preferring continuing work with a given
  -- GHC instead of bouncing around multiple concurrent GHCs.
  JobserverOptions
    { releaseDebounce    = 10 -- ms
    , setNumCapsDebounce = 10 -- ms
    }

-- | Resources available for running jobs, i.e.
-- tokens obtained from the parallelism semaphore.
data JobResources
  = Jobs
  { extraTokens :: ![SemaphoreToken]
    -- ^ Tokens acquired from the semaphore (not including the implicit token).
  , tokensFree  :: !Int
    -- ^ How many tokens are not currently being used
  , jobsWaiting :: !(OrdList (TMVar ()))
    -- ^ Pending jobs waiting on a token, the job will be blocked on the TMVar so putting into
    -- the TMVar will allow the job to continue.
  }

-- | How many tokens this process owns: the implicit token, plus those
-- acquired from the semaphore.
tokensOwned :: JobResources -> Int
tokensOwned ( Jobs { extraTokens } ) = 1 + length extraTokens

instance Outputable JobResources where
  ppr jobs@Jobs{..}
    = text "JobResources" <+>
        ( braces $ hsep
          [ text "owned=" <> ppr (tokensOwned jobs)
          , text "free=" <> ppr tokensFree
          , text "num_waiting=" <> ppr (length jobsWaiting)
          ] )

-- | Add one newly acquired token.
addToken :: SemaphoreToken -> JobResources -> JobResources
addToken tok jobs@( Jobs { tokensFree = free, extraTokens = toks })
  = jobs { tokensFree = free + 1, extraTokens = tok : toks }

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

-- | Give up one extra token, extracting the 'SemaphoreToken' for release.
removeExtraToken :: JobResources -> (SemaphoreToken, JobResources)
removeExtraToken jobs@( Jobs { extraTokens = toks })
  = case toks of
      t : rest -> (t, jobs { extraTokens = rest })
      []       -> panic "removeExtraToken: no extra tokens"

-- | Add one new job to the end of the list of pending jobs.
addJob :: TMVar () -> JobResources -> JobResources
addJob job jobs@( Jobs { jobsWaiting = wait })
  = jobs { jobsWaiting = wait `SnocOL` job }

-- | Remove a job from the list of pending jobs.
removeJob :: TMVar () -> JobResources -> JobResources
removeJob job jobs@( Jobs { jobsWaiting = wait })
  = jobs { jobsWaiting = fst $ partitionOL ( /= job ) wait }

-- | The state of the semaphore job server.
data JobserverState
  = JobserverState
    { canChangeNumCaps :: !(TVar Bool)
      -- ^ A TVar that signals whether it has been long
      -- enough since we last changed 'numCapabilities'.
    , canReleaseToken  :: !(TVar Bool)
      -- ^ A TVar that signals whether we last wanted a token
      -- long enough ago that we can now release one.
    , numCapsSet       :: !Int
      -- ^ What 'setNumCapabilities' was last given.
    }

-- | Whether we want another token from the semaphore.
tokenWanted :: JobResources -> Bool
tokenWanted ( Jobs { tokensFree, jobsWaiting } )
  = length jobsWaiting > tokensFree

-- | Whether we should release a token back to the semaphore:
-- there are no pending jobs and we have a free extra token.
guardRelease :: JobResources -> Bool
guardRelease ( Jobs { tokensFree, extraTokens, jobsWaiting } )
  = null jobsWaiting && tokensFree > 0 && not (null extraTokens)

---------------------------------------
-- Semaphore jobserver implementation

-- | Add one pending job to the jobserver.
--
-- Blocks, waiting on the jobserver to supply a free token. If the wait is
-- interrupted by an exception, the job is withdrawn.
acquireJob :: TVar JobResources -> IO ()
acquireJob jobs_tvar =
  bracketOnError add_job withdraw_job ( atomically . takeTMVar )
  where
    add_job :: IO ( TMVar () )
    add_job =
      tracedAtomically "acquire" $
        modifyJobResources jobs_tvar \ jobs -> do
          job <- newEmptyTMVar
          return ( job, addJob job jobs )

    withdraw_job :: TMVar () -> IO ()
    withdraw_job job =
      tracedAtomically "acquire_withdrawn" $
        modifyJobResources jobs_tvar \ jobs -> do
          mb_token <- tryTakeTMVar job
          return $ case mb_token of
            Nothing -> ( (), removeJob job jobs )  -- still pending
            Just () -> ( (), addFreeToken jobs )   -- already handed a token

-- | Signal to the job server that one job has completed,
-- releasing its corresponding token.
releaseJob :: TVar JobResources -> IO ()
releaseJob jobs_tvar = do
  tracedAtomically "release" do
    modifyJobResources jobs_tvar \ jobs -> do
      massertPpr (tokensFree jobs < tokensOwned jobs)
        (text "releaseJob: more free jobs than owned jobs!")
      return ((), addFreeToken jobs)

-- | Release every held token, when shutting down the jobserver.
releaseAllHeld :: TVar JobResources -> IO ()
releaseAllHeld jobs_tvar = do
  Jobs { extraTokens = toks } <- readTVarIO jobs_tvar
  forM_ toks $ \t ->
    void $ try @SomeException $ releaseSemaphoreToken t

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
                   -> STM (a, JobResources)
modifyJobResources jobs_tvar action = do
  old_jobs  <- readTVar jobs_tvar
  (a, jobs) <- action old_jobs

  -- Check the invariant: if the number of free tokens has decreased,
  -- there must be no pending jobs.
  massertPpr (null (jobsWaiting jobs) || tokensFree jobs >= tokensFree old_jobs) $
    vcat [ text "modifyJobResources: pending jobs but fewer free tokens" ]
  dispatched_jobs <- dispatchTokens jobs
  writeTVar jobs_tvar dispatched_jobs
  return (a, dispatched_jobs)

tracedAtomically :: String -> STM (a, JobResources) -> IO a
tracedAtomically origin act = do
  (a, jobs) <- atomically act
  -- Use the "jsem:" prefix to identify where the write traces are
  traceEventIO ("jsem:" ++ renderJobResources origin jobs)
  return a

renderJobResources :: String -> JobResources -> String
renderJobResources origin jobs@(Jobs { tokensFree = free, jobsWaiting = pending }) =
  showSDocUnsafe $ renderJSON $
    JSObject [ ("name", JSString origin)
             , ("owned", JSInt (tokensOwned jobs))
             , ("free", JSInt free)
             , ("pending", JSInt (length pending) )
             ]

-- | The body of one acquire thread: wait for one semaphore token and add it
-- to the pool.
acquirerAction :: Jobserver -> IO ()
acquirerAction ( Jobserver { jSemaphore = sem, jobs = jobs_tvar } ) = do
  myThreadId >>= \ tid -> labelThread tid "acquire_thread"
  -- Masked: once the waiter acquires a token, we don't want to lose it on the
  -- way to recording it in our local accounting.
  -- The wait itself remains interruptible.
  mask_ do
    tok <- waitOnSemaphore sem
    tracedAtomically "acquire_thread" $
      modifyJobResources jobs_tvar \ jobs ->
        return ((), addToken tok jobs)

-- | Keep the acquire thread waiting on the semaphore exactly when a token is
-- wanted: start it when it is missing, interrupt it when it is unwanted.
tryAcquire :: JobserverOptions
           -> Jobserver
           -> JobserverState
           -> STM (IO JobserverState)
tryAcquire opts js@( Jobserver { jobs = jobs_tvar, jAcquirer = acquirer } ) st = do
  jobs <- readTVar jobs_tvar
  acquiring <- ( > 0 ) <$> activeCount acquirer
  let tokWanted = tokenWanted jobs
  guard $ acquiring /= tokWanted
  return $
    if tokWanted
    then do
      fork acquirer (acquirerAction js)
      can_release_tvar <- registerDelay $ releaseDebounce opts * 1000
      return $ st { canReleaseToken = can_release_tvar }
    else do
      interruptAll acquirer
      return st

-- | When there is a free extra token, no pending jobs, and the release
-- debounce has expired, give one token back to the semaphore.
tryRelease :: Jobserver
           -> JobserverState
           -> STM (IO JobserverState)
tryRelease ( Jobserver { jobs = jobs_tvar } )
  st@( JobserverState { canReleaseToken = can_release_tvar } ) = do
    jobs <- readTVar jobs_tvar
    guard $ guardRelease jobs
    readTVar can_release_tvar >>= guard
    -- Masked, to avoid removing the token from the local accounting without
    -- actually giving it back to the semaphore.
    return $ mask_ do
      -- Check we still want to release the token (no new work arrived since).
      mb_tok <- tracedAtomically "pre_release" $
        modifyJobResources jobs_tvar \ jobs' ->
          if guardRelease jobs'
          then
            let (tok, jobs'') = removeExtraToken (removeFreeToken jobs')
            in  return (Just tok, jobs'')
          else  return (Nothing , jobs')
      for_ mb_tok releaseSemaphoreToken
      return st

-- | Keep 'setNumCapabilities' in sync with the number of owned tokens
-- (debounced), so that parallel garbage collection uses as many capabilities
-- as there are tokens to run on, up to the number of processors.
trySyncNumCaps :: JobserverOptions
               -> Jobserver
               -> JobserverState
               -> STM (IO JobserverState)
trySyncNumCaps opts ( Jobserver { jobs = jobs_tvar, jMaxCapabilities = max_caps } )
  st@( JobserverState { canChangeNumCaps = can_change_tvar, numCapsSet = prev } ) = do
    jobs <- readTVar jobs_tvar
    -- Using more capabilities than there are processors usually leads to high
    -- userspace lock contention (#9221).
    let num_caps = min (tokensOwned jobs) max_caps
    guard $ num_caps /= prev
    readTVar can_change_tvar >>= guard
    return do
      setNumCapabilities num_caps
      can_change_tvar' <- registerDelay $ setNumCapsDebounce opts * 1000
      return $ st { canChangeNumCaps = can_change_tvar'
                  , numCapsSet       = num_caps }

-- | Main jobserver loop.
jobserverLoop :: JobserverOptions -> Jobserver -> IO ()
jobserverLoop opts js = do
  true_tvar <- newTVarIO True
  num_caps <- getNumCapabilities
  let init_state :: JobserverState
      init_state =
        JobserverState
          { canChangeNumCaps = true_tvar
          , canReleaseToken  = true_tvar
          , numCapsSet       = num_caps }
  loop init_state `finally` setNumCapabilities num_caps
  where
    loop s = do
      action <- atomically $ asum $ (\x -> x s) <$>
        [ tryRelease          js
        , tryAcquire     opts js
        , trySyncNumCaps opts js
        ]
      s <- action
      loop s

-- | Run an action taking an abstract semaphore by using a semaphore 'Jobserver'.
--
-- The jobserver runs for the duration of the action. Once finished, all tokens
-- will have been given back to the semaphore.
--
-- See Note [Architecture of the Job Server].
withJobserver
  :: ClientSemaphore -- ^ the system semaphore (from @-jsem@)
  -> ( AbstractSem -> IO a )
  -> IO a
withJobserver semaphore action = do
  num_processors <- getNumProcessors
  let
    init_jobs =
      Jobs { extraTokens = []
           , tokensFree  = 1
           , jobsWaiting = NilOL
           }
  jobs_tvar <- newTVarIO init_jobs
  let
    opts = defaultJobserverOptions -- TODO: allow this to be configured
    abstract_sem =
      AbstractSem
        { acquireSem = acquireJob jobs_tvar
        , releaseSem = releaseJob jobs_tvar
        }
    run_jobserver :: IO ()
    run_jobserver = do
      myThreadId >>= \ tid -> labelThread tid "job_server"
      scoped \ acquirer ->
        jobserverLoop opts $
          Jobserver { jSemaphore       = semaphore
                    , jMaxCapabilities = num_processors
                    , jobs             = jobs_tvar
                    , jAcquirer        = acquirer
                    }
  scoped \ scope -> do
    forkIn scope (pure ()) (\ () -> releaseAllHeld jobs_tvar) (\ () -> run_jobserver)
    result <- action abstract_sem
    interruptAll scope
    return result

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

The `jobserverLoop` continually tries to reconcile the available work with
the demand for semaphore tokens:

  - A single waiter thread, running while there are pending jobs that require a
    token, and interrupted as soon as we stop having use for a new token.

  - When we have a free token with no pending jobs, we give it back, after
    the release debounce period has expired.

The jobserver only lives for the duration of an action (see `withJobserver`).
We rely on the scoped machinery from GHC.Utils.Concurrent.Scope: the jobserver
loop is interrupted when the action ends, giving all the held semaphore tokens
back.

Note [Eventlog Messages for jsem]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It can be tricky to verify that the work is shared adequately across different
processes. To help debug this, we output the values of `JobResource` to the
eventlog whenever the global state changes. There are some scripts which can be used
to analyse this output and report statistics about core saturation in the
GitHub repo (https://github.com/mpickering/ghc-jsem-analyse).

-}

#endif
