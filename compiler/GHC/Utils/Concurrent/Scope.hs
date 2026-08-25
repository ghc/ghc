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
  , forkIn
  )
  where

import GHC.Prelude

import Control.Concurrent
  ( ThreadId, forkIOWithUnmask, myThreadId, throwTo )
import Control.Concurrent.STM
  ( STM, TVar, atomically, check, modifyTVar', newTVarIO, readTVar, retry, writeTVar )
import Control.Exception
  ( Exception(..), SomeException
  , asyncExceptionFromException, asyncExceptionToException
  , catch, finally, mask, mask_, onException, throwIO, try, uninterruptibleMask_ )
import Control.Monad
  ( when )
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
      -- Used only to know which threads to interrupt when tearing down the
      -- scope, never directly awaited on.
    , scope_failure  :: !( Maybe SomeException )
      -- ^ The first exception that a thread in the scope failed with (if any).
    , scope_starting :: !Bool
      -- ^ Does the scope still allow starting new threads?
    }

-- | The exception delivered to the scope's threads on teardown.
--
-- Asynchronous, and separate from 'ThreadKilled' so that we can distinguish
-- the two.
data ScopeTeardown = ScopeTeardown
  deriving stock Show

-- | Asynchronous, so that worker thread code catching synchronous exceptions
-- does not swallow teardown.
instance Exception ScopeTeardown where
  toException   = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Run an action within a new scope, waiting for all threads in the scope
-- to complete before returning.
scoped :: ( Scope -> IO a ) -> IO a
scoped body = do
  owner <- myThreadId
  scope <-
    Scope <$> newTVarIO
      ScopeState { scope_owner    = owner
                 , scope_running  = 0
                 , scope_threads  = Set.empty
                 , scope_failure  = Nothing
                 , scope_starting = True }
  mask \ restore -> do
    result <- try @SomeException $ restore do
      result <- body scope
      awaitAll scope
      pure result
    case result of
      Right a -> pure a
      Left  e -> do
        killAll scope
        throwIO e

-- | Try to start a thread in the scope, returning whether it was started
-- (a scope refuses new threads once one has failed or teardown has begun).
-- A refused thread's @acquire@, @action@ and @release@ never run at all.
--
-- An exception escaping @action@ or @release@ (other than scope teardown)
-- becomes the scope's failure: it tears the scope down and is rethrown.
forkIn
  :: Scope
  -> IO r           -- ^ thread resource acquire (masked, on the caller)
  -> ( r -> IO () ) -- ^ thread resource release (uninterruptible, on the thread)
  -> ( r -> IO () ) -- ^ the thread's action
  -> IO Bool
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
    tid <-
      -- The child starts masked, so its handlers below are installed before
      -- its first interruptible point.
      forkIOWithUnmask
        ( \ unmask -> do
            me <- myThreadId
            ( ( unmask ( action r ) `catch` record_failure )
                `finally` ( uninterruptibleMask_ ( release r ) `catch` record_failure ) )
              `finally` finished me )
        -- If the fork itself fails (e.g. resource exhaustion), the thread that
        -- was supposed to run @release@ never existed, so run it now and unclaim.
        `onException` do
          uninterruptibleMask_ ( release r ) `catch` record_failure
          unclaim
    -- The thread may already have finished, in which case the following
    -- leaves a dead 'ThreadId' behind. That's harmless: all 'killAll' does
    -- with it is a no-op 'throwTo'.
    atomically $ modifyTVar' state \ st ->
      st { scope_threads = Set.insert tid ( scope_threads st ) }
  pure started
  where
    unclaim :: IO ()
    unclaim =
      atomically $ modifyTVar' state \ st ->
        st { scope_running = scope_running st - 1 }

    record_failure :: SomeException -> IO ()
    record_failure e =
      case fromException e of
        Just ScopeTeardown -> pure ()
        _ -> do
          mb_owner <- uninterruptibleMask_ $
            atomically do
              st <- readTVar state
              case scope_failure st of
                Just {} -> pure Nothing  -- only the first failure is kept
                Nothing -> do
                  writeTVar state st { scope_failure = Just e }
                  pure ( Just ( scope_owner st ) )
          -- Deliver the failure to the scope's owner, tearing the scope down.
          --
          -- Interruptible: 'throwTo' blocks until delivered, and the owner may
          -- at this very moment be in 'killAll'.
          for_ mb_owner ( `throwTo` e )

    finished :: ThreadId -> IO ()
    finished me =
      -- Uninterruptible: 'scope_running' accounting must remain correct to
      -- avoid deadlock in 'awaitAll'.
      uninterruptibleMask_ $ atomically $ modifyTVar' state \ st ->
        st { scope_running = scope_running st - 1
           , scope_threads = Set.delete me ( scope_threads st ) }

-- | Wait until every thread in the scope has finished, rethrowing the first
-- exception one of them failed with (if any).
awaitAll :: Scope -> IO ()
awaitAll ( Scope state ) = do
  failure <-
    atomically do
      st <- readTVar state
      check ( scope_running st == 0 )
      pure ( scope_failure st )
  traverse_ throwIO failure

-- | Tear down a scope: prevent further threads from being started, interrupt
-- every running thread, wait for all of them to end.
--
-- Never returns early: exceptions delivered during the wait are absorbed.
killAll :: Scope -> IO ()
killAll ( Scope state ) =
  -- Interruptible: a failing thread may be trying to tear the owner down
  -- (see 'record_failure' in 'forkIn'). Don't deadlock with that.
  mask_ $ go Set.empty
  where
    go :: Set.Set ThreadId -> IO ()
    go killed = do
      step <-
        -- Every interruptible point of the loop is inside this 'try', so a
        -- delivered exception cannot end the wait: it is absorbed and the loop
        -- restarts. This ensures we never finish while any threads are still running.
        try @SomeException do
          todo <-
            atomically do
              st <- readTVar state
              writeTVar state $ st { scope_starting = False }
              let new = scope_threads st Set.\\ killed
              if | not ( Set.null new )  -> pure ( Just new )
                 | scope_running st == 0 -> pure Nothing
                 | otherwise             -> retry
          for_ todo \ new -> for_ new ( `throwTo` ScopeTeardown )
          pure ( Set.union killed <$> todo )
      case step of
        Left _absorbed         -> go killed
        Right Nothing          -> pure ()
        Right ( Just killed' ) -> go killed'
