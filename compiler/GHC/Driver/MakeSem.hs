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
    acquireJSemAbstractSem,
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
import GHC.IO.Exception
import GHC.Utils.Concurrent.Scope
  ( Scope, scoped, forkIn, interruptOne, activeCount )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Json

import System.Semaphore
  ( AbstractSem(..)
  , ClientSemaphore
  , SemaphoreError
  , SemaphoreIdentifier
  , SemaphoreToken
  , openSemaphore
  , releaseSemaphoreToken
  , waitOnSemaphore
  , withAbstractSem
  )

import Control.Monad
import qualified Control.Monad.Catch as MC
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.Foldable
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
  { jSemaphore :: !ClientSemaphore
    -- ^ The semaphore which controls available resources
  , jMaxTokens :: !Int
    -- ^ Maximum number of tokens to ever use: a cache of 'getNumProcessors'
  , jobs :: !(TVar JobResources)
    -- ^ The currently pending jobs, and the resources
    -- obtained from the semaphore
  , jAcquirers :: !Scope
    -- ^ The acquire threads: one outstanding semaphore wait each
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

-- | How many outstanding semaphore waits we want: one per pending job that no
-- free token covers, without ever owning more tokens than 'jMaxTokens'.
acquirersWanted :: Int -> JobResources -> Int
acquirersWanted max_tokens jobs@( Jobs { tokensFree, jobsWaiting } )
  = max 0 $ min ( length jobsWaiting - tokensFree )
                ( max_tokens - tokensOwned jobs )

-- | Whether we should release a token back to the semaphore:
-- there are no pending jobs and we have a free extra token.
guardRelease :: JobResources -> Bool
guardRelease ( Jobs { tokensFree, extraTokens, jobsWaiting } )
  = null jobsWaiting && tokensFree > 0 && not (null extraTokens)

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

-- | Release every held token, when shutting down the jobserver.
releaseAllHeld :: TVar JobResources -> IO ()
releaseAllHeld jobs_tvar =
  -- Uninterruptible, to avoid loss of tokens.
  MC.uninterruptibleMask_ do
    Jobs { extraTokens = toks } <- readTVarIO jobs_tvar
    forM_ toks $ \t ->
      void $ MC.try @_ @MC.SomeException (releaseSemaphoreToken t)

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
    vcat [ text "modifyJobResources: pending jobs but fewer free tokens" ]
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
  MC.mask_ do
    tok <- waitOnSemaphore sem
    tracedAtomically_ "acquire_thread" $
      snd <$> modifyJobResources jobs_tvar \ jobs ->
        return ((), addToken tok jobs)

-- | Keep as many acquire threads outstanding as there are tokens wanted:
-- spawn the missing ones, interrupt the surplus ones.
tryAcquire :: JobserverOptions
           -> Jobserver
           -> JobserverState
           -> STM (IO JobserverState)
tryAcquire opts js@( Jobserver { jobs = jobs_tvar, jAcquirers = acquirers } ) st = do
  jobs <- readTVar jobs_tvar
  outstanding <- activeCount acquirers
  let wanted = acquirersWanted (jMaxTokens js) jobs
  guard $ outstanding /= wanted
  return
    if outstanding < wanted
    then do
      _ <- replicateM (wanted - outstanding) $
             forkIn acquirers (pure ()) (const $ pure ()) (\_ -> acquirerAction js)
      can_release_tvar <- registerDelay $ releaseDebounce opts * 1000
      return $ st { canReleaseToken = can_release_tvar }
    else do
      replicateM_ (outstanding - wanted) $ interruptOne acquirers
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
    return $ MC.mask_ do
      -- Check we still want to release the token (no new work arrived since).
      mb_tok <- tracedAtomically "pre_release" $
        modifyJobResources jobs_tvar \ jobs' ->
          if guardRelease jobs'
          then
            let (tok, jobs'') = removeExtraToken (removeFreeToken jobs')
            in  return (Just tok, jobs'')
          else  return (Nothing , jobs')
      for_ mb_tok \ tok ->
        releaseSemaphoreToken tok `MC.onException`
          tracedAtomically_ "release_failed"
            ( snd <$> modifyJobResources jobs_tvar \ jobs' ->
                return ((), addToken tok jobs') )
      return st

-- | Keep 'setNumCapabilities' in sync with the number of owned tokens
-- (debounced), so that parallel garbage collection uses as many capabilities
-- as there are tokens to run on.
trySyncNumCaps :: JobserverOptions
               -> Jobserver
               -> JobserverState
               -> STM (IO JobserverState)
trySyncNumCaps opts ( Jobserver { jobs = jobs_tvar } )
  st@( JobserverState { canChangeNumCaps = can_change_tvar, numCapsSet = prev } ) = do
    jobs <- readTVar jobs_tvar
    let owned = tokensOwned jobs
    guard $ owned /= prev
    readTVar can_change_tvar >>= guard
    return do
      setNumCapabilities owned
      can_change_tvar' <- registerDelay $ setNumCapsDebounce opts * 1000
      return $ st { canChangeNumCaps = can_change_tvar'
                  , numCapsSet       = owned }

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
  loop init_state
  where
    loop s = do
      action <- atomically $ asum $ (\x -> x s) <$>
        [ tryRelease          js
        , tryAcquire     opts js
        , trySyncNumCaps opts js
        ]
      s <- action
      loop s

-- | Create a new jobserver using the given semaphore identifier.
makeJobserver :: SemaphoreIdentifier -> IO (AbstractSem, IO ())
makeJobserver sem_ident = do
  semaphore <- openSemaphore sem_ident >>= either MC.throwM pure
  max_tokens <- getNumProcessors
  let
    init_jobs =
      Jobs { extraTokens = []
           , tokensFree  = 1
           , jobsWaiting = NilOL
           }
  jobs_tvar <- newTVarIO init_jobs
  let
    opts = defaultJobserverOptions -- TODO: allow this to be configured
  loop_finished_mvar <- newEmptyMVar
  loop_tid <- MC.mask_ $ forkIOWithUnmask \ unmask -> do
    r <- try $ unmask $
      scoped \ scope ->
        jobserverLoop opts
          Jobserver { jSemaphore = semaphore
                    , jMaxTokens = max_tokens
                    , jobs       = jobs_tvar
                    , jAcquirers = scope }
    releaseAllHeld jobs_tvar
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
      killThread loop_tid
      mb_ex <- takeMVar loop_finished_mvar
      for_ mb_ex MC.throwM

  return (AbstractSem{..}, cleanupSem)

-- | Open an abstract semaphore backed by a semaphore 'Jobserver', which
-- queries the system semaphore of the given name for resources.
--
-- Returns 'Left' if the system semaphore could not be opened, and otherwise
-- the abstract semaphore together with the action that tears the jobserver
-- down. A 'SemaphoreError' arising after the semaphore was successfully
-- opened is thrown.
acquireJSemAbstractSem
  :: SemaphoreIdentifier -- ^ the semaphore identifier (from @-jsem@)
  -> IO (Either SemaphoreError (AbstractSem, IO ()))
acquireJSemAbstractSem sem_ident =
  MC.try @_ @SemaphoreError (makeJobserver sem_ident)

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

  - One acquire thread per piece of outstanding work waiting for a token.
    When demand drops, these waiter threads are cancelled.

  - When we have a free token with no pending jobs, we give it back, after
    the release debounce period has expired.

Shutdown relies on the the scoped machinery from GHC.Utils.Concurrent.Scope,
releasing all held semaphore tokens before finishing.

Note [Eventlog Messages for jsem]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It can be tricky to verify that the work is shared adequately across different
processes. To help debug this, we output the values of `JobResource` to the
eventlog whenever the global state changes. There are some scripts which can be used
to analyse this output and report statistics about core saturation in the
GitHub repo (https://github.com/mpickering/ghc-jsem-analyse).

-}

#endif
