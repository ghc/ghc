{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}

-- | Structured concurrency in the style of @ki@: a collection of threads within
-- a scope, which does not end until every one of its threads has ended.
--
-- One addition relative to @ki@: 'forkIn' allows allocating a resource in the
-- calling thread while always ensuring the resource is properly released.
module GHC.Utils.Concurrent.Scope
  ( Scope
  , scoped
  , fork
  , forkIn
  , activeCount
  )
  where

import GHC.Prelude

import Control.Concurrent
  ( ThreadId, forkIOWithUnmask, myThreadId )
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
  ( void, when )
import Data.Foldable
  ( for_, traverse_ )
import Data.Maybe
  ( isNothing )
import qualified Data.Set as Set

--------------------------------------------------------------------------------

-- | A scope in which threads can be started, and which does not end until
-- all of its threads have run to completion.
newtype Scope = Scope ( TVar ScopeState )

data ScopeState =
  ScopeState
    { scope_owner    :: !ThreadId
      -- ^ The thread that created the scope.
    , scope_running  :: !Int
      -- ^ The count of active threads within the scope.
      --
      -- NB: separate from 'scope_threads' to protect against race conditions
      -- in between forking a thread and recording its 'ThreadId'.
    , scope_threads  :: !( Set.Set ThreadId )
      -- ^ All threads active within the scope.
      --
      -- Used only to know which threads to interrupt, never directly awaited on.
    , scope_failure  :: !( Maybe SomeException )
      -- ^ The first exception that a thread in the scope failed with (if any).
    , scope_starting :: !Bool
      -- ^ Does the scope still allow starting new threads?
    }

-- | The exception delivered to a scope's threads to stop them.
--
-- Asynchronous, and separate from 'ThreadKilled' so that we can distinguish
-- the two.
data ScopeInterrupt = ScopeInterrupt
  deriving stock Show

-- | Asynchronous, so that worker thread code catching synchronous exceptions
-- does not swallow the interruption.
instance Exception ScopeInterrupt where
  toException   = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Run an action within a new scope, returning once the action has finished
-- and every thread in the scope has ended.
--
-- If the action or a thread in the scope throws an exception, all threads
-- are torn down, after which the exception is then rethrown. An exception
-- thrown by a thread in the scope interrupts the action.
scoped
  :: ( Scope -> IO a )
     -- ^ The action to run within a scope.
     --
     -- The 'Scope' must not be used once 'scoped' has returned.
  -> IO a
scoped body = do
  owner <- myThreadId
  let
    init_state =
      ScopeState
        { scope_owner    = owner
        , scope_running  = 0
        , scope_threads  = Set.empty
        , scope_failure  = Nothing
        , scope_starting = True
        }
  scope <- Scope <$> newTVarIO init_state
  ( body scope <* awaitAll scope )
    `onException` killAll scope

-- | Start a thread in the scope, unless the scope no longer accepts new threads
-- (which happens once we start tearing down the scope, e.g. because one of the
-- threads in the scope has failed), in which case nothing is run.
forkIn
  :: Scope
  -> IO r
      -- ^ acquire a resource for the thread; runs masked on the calling thread
  -> ( r -> IO () )
      -- ^ release the resource (always runs, uninterruptibly, once the resource
      -- has been acquired)
  -> ( r -> IO () )
      -- ^ the thread's action (runs unmasked)
  -> IO ()
forkIn ( Scope state ) acquire release action = mask_ do
  started <-
    atomically do
      st <- readTVar state
      let starting = scope_starting st && isNothing ( scope_failure st )
      when starting $
        writeTVar state $
          st { scope_running = scope_running st + 1 }
      pure starting
  when started do
    -- If acquisition fails, roll the claim back: 'awaitAll' must not count a
    -- thread that never existed.
    r <- acquire `onException` unclaim
    void $
      -- The child starts masked, so its handlers below are installed before
      -- its first interruptible point.
      forkIOWithUnmask
        ( \ unmask -> do
            me <- myThreadId
            -- The thread registers itself, masked, before its first
            -- interruptible point: a member of 'scope_threads' is therefore
            -- always a live thread, deregistered by 'finished' below.
            atomically $ modifyTVar' state \ st ->
              st { scope_threads = Set.insert me ( scope_threads st ) }
            ( ( unmask ( action r ) `catch` record_failure )
                `finally` ( uninterruptibleMask_ ( release r ) `catch` record_failure ) )
              `finally` finished me )
        -- If the fork itself fails (e.g. resource exhaustion), the thread that
        -- was supposed to run 'release' never existed, so unclaim and run it now.
        `onException` do
          unclaim
          uninterruptibleMask_ ( release r )
  where
    unclaim :: IO ()
    unclaim =
      atomically $ modifyTVar' state \ st ->
        st { scope_running = scope_running st - 1 }

    record_failure :: SomeException -> IO ()
    record_failure e =
      case fromException e of
        Just ScopeInterrupt -> pure ()
        _ -> do
          mb_owner <-
            atomically do
              st <- readTVar state
              case scope_failure st of
                Just {} -> pure Nothing  -- only the first failure is kept
                Nothing -> do
                  writeTVar state $
                    st { scope_failure = Just e }
                  -- See Note [Scope teardown].
                  pure $
                    if scope_starting st
                    then Just $ scope_owner st
                    else Nothing
          -- Deliver the failure to the scope's owner, tearing the scope down.
          for_ mb_owner \ owner ->
            -- Give up if interrupted: see Note [Scope teardown].
            void $ try @ScopeInterrupt ( throwTo owner e )

    finished :: ThreadId -> IO ()
    finished me =
      -- Runs masked: this ensures correct accounting of 'scope_running',
      -- which we rely on to avoid deadlock in 'awaitAll'.
      atomically $ modifyTVar' state \ st ->
        st { scope_running = scope_running st - 1
           , scope_threads = Set.delete me ( scope_threads st ) }

-- | Start a thread that holds no resource in the scope; see 'forkIn'.
fork :: Scope -> IO () -> IO ()
fork scope action =
  forkIn scope ( pure () ) ( \ () -> pure () ) ( \ () -> action )

-- | How many threads are active within the scope.
activeCount :: Scope -> STM Int
activeCount ( Scope state ) = scope_running <$> readTVar state

-- | Wait until every thread in the scope has finished, rethrowing the first
-- exception one of them failed with (if any).
awaitAll :: Scope -> IO ()
awaitAll ( Scope state ) = do
  failure <-
    atomically do
      st <- readTVar state
      check $ scope_running st == 0
      pure $ scope_failure st
  traverse_ throwIO failure

{- Note [Scope teardown]
~~~~~~~~~~~~~~~~~~~~~~~~
A scoped thread that fails with an exception notifies the scope's owner by
throwing the exception to it (see 'record_failure'). This blocks until the owner
receives the exception, at which point the owner then tears the scope down by
interrupting every thread and waiting for all of them to end (see 'killAll').

Teardown is uninterruptible: it must not end while a thread is still running.
In particular, any exceptions thrown to the owner in the meantime (such as a
user interrupt) must not cancel teardown.

A thread must not wait to notify the owner which is itself tearing the scope
down, as each would be waiting for the other. We avoid this deadlock as follows:

  - Teardown closes the scope (unsetting 'scope_starting') before it interrupts
    any thread.

  - A thread that fails once the scope is closed does not notify the owner.

  - A thread that is blocked on notifying the owner when teardown begins gets
    interrupted by it ('throwTo' is an interruptible operation), and gives up.

Nothing is lost when a thread does not notify the owner: the owner is already
ending the scope with an exception.
-}

-- | Tear down a scope: prevent further threads from being started, interrupt
-- every running thread, wait for all of them to end.
killAll :: Scope -> IO ()
killAll scope@( Scope state ) =
  -- See Note [Scope teardown].
  uninterruptibleMask_ do
    atomically $ modifyTVar' state \ st ->
      st { scope_starting = False }
    interruptAll scope

-- | Interrupt every thread in the scope, and wait for all of them to end (this
-- includes threads started in the meantime).
--
-- Does not tear down the scope: further threads can still be started.
interruptAll :: Scope -> IO ()
interruptAll ( Scope state ) = go Set.empty
  where
    go :: Set.Set ThreadId -- threads that have already been interrupted
       -> IO ()
    go interrupted = do
      todo <-
        atomically do
          st <- readTVar state
          let new = scope_threads st Set.\\ interrupted
          if
            | not $ Set.null new
            -> pure $ Just new
            | scope_running st == 0
            -> pure Nothing
            | otherwise
            -> retry
      for_ todo \ new -> do
        for_ new ( `throwTo` ScopeInterrupt )
        go $ Set.union interrupted new
