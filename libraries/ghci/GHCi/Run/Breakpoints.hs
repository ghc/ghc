{-# LANGUAGE GADTs, RecordWildCards, MagicHash, ScopedTypeVariables, CPP,
    UnboxedTuples, LambdaCase, UnliftedFFITypes, NamedFieldPuns, BangPatterns #-}
module GHCi.Run.Breakpoints
  (
  -- *  Global break action
    globalBreakAction

  -- * Per-thread breakpoints
  , readThreadEvalStatus, readAnyThreadEvalBreak
  , writeThreadEvalStatus
  , readCtxEvalStatus, writeCtxEvalStatus

  -- ** Isolating thread breakpoints
  , setIsolatedThread, unsetIsolatedThread
  , setIsolatedCtx, unsetIsolatedCtx

  -- * Per-thread resume-ing
  , getThreadResumeContext, resumeStoppedThread
  , ResumeContext(..)
  , ResumeThreadStatus(..), ResumedAlreadyRunningThreadException(..)

  )
  where

import Prelude -- See note [Why do we import Prelude here?]

import GHCi.Debugger
import GHCi.Message
import GHCi.RemoteTypes

import qualified Data.Map as Map
import Data.Word
import Control.Exception
import Control.Concurrent
import qualified Data.ByteString.Short.Internal as BS
import GHC.Exts
import GHC.Stack
import GHC.Conc.Sync
import GHC.IO hiding ( bracket, onException )
import Control.Monad

--------------------------------------------------------------------------------
-- * Global break action
--------------------------------------------------------------------------------

-- | The function which is called whenever a breakpoint is hit.
globalBreakAction :: BreakpointCallback
globalBreakAction info_mod# info_mod_uid# infox# is_exception apStack = do
  tid <- myThreadId
  ctx@ResumeContext{..} <- getThreadResumeContext tid
  resume_r <- mkRemoteRef tid
  apStack_r <- mkRemoteRef apStack
  ccs <- toRemotePtr <$> getCCSOf apStack
  mb_breakpoint <-
    if is_exception
    then pure Nothing
    else do
      info_mod <- BS.packCString (Ptr info_mod#)
      info_mod_uid <- BS.packCString (Ptr info_mod_uid#)
      pure (Just (EvalBreakpoint info_mod info_mod_uid (I# infox#)))
  writeCtxEvalStatus ctx $ EvalBreak apStack_r mb_breakpoint resume_r ccs

  {- Block until this thread is resumed

     This function must have been called from `rts/Interpreter.c` when
     interpreting a `BRK_FUN`. After taking from the MVar, the function returns
     to the continuation on the stack which is where the interpreter was
     stopped.

     If the thread is instead resumed by an exception (e.g. by :abandon), it is
     marked as running. We may be stopped in the context of updating a thunk,
     so we have to carefully use (`catchNoPropagate` + `throwTo =<< myThreadId`) to
     rethrow *asynchronously* to ensure the thunk is resumable from the
     breakpoint rather than being updated with the exception.
  -}
  takeMVar resumeBreakMVar `catchNoPropagate` \(ExceptionWithContext _ (e :: SomeException)) -> do
    atomically $ writeTVar resumeEvalStatus ThreadStatusRunning
    (`throwTo` e) =<< myThreadId

--------------------------------------------------------------------------------
-- * Per-thread breakpoints
--------------------------------------------------------------------------------

-- | To evaluate a statement in the interpreter, a thread is forked by
-- `sandboxIO`. The statement result will be written by that thread using
-- `writeThreadEvalStatus`.
--
-- We can await either for any breakpoint to be hit ('readAnyThreadEvalBreak')
-- /or/ for a thread specifically to write its result. To wait for that thread
-- specifically, use this function.
--
-- Succeeds when the given thread writes to the global thread status map an 'EvalStatus'.
readThreadEvalStatus :: ThreadId -> STM (EvalStatus [HValueRef])
readThreadEvalStatus tid = do
  ctxs <- readTVar threadContextsVar
  case Map.lookup (fromThreadId tid) ctxs of
    Just ctx -> readCtxEvalStatus ctx
    Nothing  -> retry -- to write a thread break or result, the
                      -- 'threadContextsVar' entry will be created

-- | Like 'readThreadEvalStatus', but when the 'ResumeContext' is already
-- available (to avoid looking it up unnecessarily).
readCtxEvalStatus :: ResumeContext -> STM (EvalStatus [HValueRef])
readCtxEvalStatus ResumeContext{resumeEvalStatus} =
  readTVar resumeEvalStatus >>= \case
    ThreadStatusStopped r seen
      | StatusNotYetRead <- seen -> do
        writeTVar resumeEvalStatus (ThreadStatusStopped r StatusAlreadyRead)
        pure r
      | StatusAlreadyRead <- seen ->
        pure r
    ThreadStatusRunning ->
        retry -- until someone writes a result for this thread

-- | When a thread evaluating bytecode with breakpoints hits an enabled
-- breakpoint, the 'globalBreakAction' is executed and, in turn, an 'EvalBreak'
-- is written to the global thread status map.
--
-- This function reads any 'EvalBreak' written to the global thread status map,
-- except threads which are "isolated" (see 'resumeIsolated').
--
-- Succeeds when any thread hits a breakpoint.
readAnyThreadEvalBreak :: STM EvalBreak
-- TODO: Instead of reading all Map.elems, the global break action could write
-- breakpoints hit into a global `TChan ThreadId` and we'd read off of this
-- channel here?
readAnyThreadEvalBreak = do
  ctxs <- readTVar threadContextsVar
  -- try reading any thread's 'EvalBreak' and retry if none have yielded one yet
  foldl' (\next ctx -> next `orElse` readBreak ctx) retry (Map.elems ctxs)
  where
    readBreak ResumeContext{resumeEvalStatus, resumeIsolated} = do
      isolated <- readTVar resumeIsolated
      if isolated
        then retry
        else readTVar resumeEvalStatus >>= \case
          ThreadStatusStopped s@(EvalPaused b) StatusNotYetRead -> do
            writeTVar resumeEvalStatus (ThreadStatusStopped s StatusAlreadyRead)
            pure b
          ThreadStatusStopped (EvalPaused _) StatusAlreadyRead -> do
            retry -- this break's already been read, so ignore it
          ThreadStatusStopped EvalComplete{} _ ->
            retry -- successes can be read with `readThreadEvalStatus`
          ThreadStatusRunning ->
            retry -- no status for this thread yet

writeThreadEvalStatus :: ThreadId -> EvalStatus [HValueRef] -> IO ()
writeThreadEvalStatus tid status = do
  -- get or create a 'ResumeContext' for the thread (e.g. we may write the
  -- EvalComplete without breakpoints occurring, so the map won't have an entry):
  ctx <- getThreadResumeContext tid
  writeCtxEvalStatus ctx status

-- | Like 'writeThreadEvalStatus', but for when the 'ResumeContext' is already
-- available
writeCtxEvalStatus :: ResumeContext -> EvalStatus [HValueRef] -> IO ()
writeCtxEvalStatus ResumeContext{resumeEvalStatus} status =
  atomically $ do
    readTVar resumeEvalStatus >>= \case
      ThreadStatusRunning
        -- We've hit a breakpoint or completion on a thread that had been resumed
        -> writeTVar resumeEvalStatus (ThreadStatusStopped status StatusNotYetRead)
      ThreadStatusStopped{}
        -> error "writeCtxEvalStatus: impossible -- overwriting stopped thread"


-- ** Isolating thread breakpoints ---------------------------------------------

-- | Make sure that anyone watching for threads hitting breakpoints with
-- 'readAnyThreadEvalBreak' ignore the given thread if it ever hits a breakpoint.
--
-- This forces this thread's result to be received solely through
-- 'readThreadEvalStatus' for that ThreadId, which is typically desired when an
-- expression to run should not be seen at all by the debugger (e.g. when
-- evaluating an expr to :force a variable)
setIsolatedThread :: ThreadId -> IO ()
setIsolatedThread = getThreadResumeContext >=> setIsolatedCtx

-- | Undoes 'setIsolatedThread'
unsetIsolatedThread :: ThreadId -> IO ()
unsetIsolatedThread = getThreadResumeContext >=> unsetIsolatedCtx

setIsolatedCtx, unsetIsolatedCtx :: ResumeContext -> IO ()
setIsolatedCtx   ctx = atomically $ writeTVar (resumeIsolated ctx) True
unsetIsolatedCtx ctx = atomically $ writeTVar (resumeIsolated ctx) False

--------------------------------------------------------------------------------
-- * Per-thread resume-ing
--------------------------------------------------------------------------------

-- | The function to get or create a thread's global 'ResumeContext' in the global map.
--
-- The entry is removed once the thread is collected using a finalizer.
getThreadResumeContext :: ThreadId -> IO ResumeContext
getThreadResumeContext tid = do
  let !key = fromThreadId tid
  ctxs0 <- readTVarIO threadContextsVar
  case Map.lookup key ctxs0 of
    Just ctx -> pure ctx -- common case: hence read without transaction
    Nothing  -> do
      new_ctx <- ResumeContext <$> newEmptyMVar
                               <*> newTVarIO ThreadStatusRunning
                               <*> newTVarIO False
      (ctx, inserted) <- atomically $ do -- write new thread context atomically
        ctxs1 <- readTVar threadContextsVar
        case Map.lookup key ctxs1 of
          Just ctx -> pure (ctx, False) -- already exists
          Nothing  -> do
            let !ctxs2 = Map.insert key new_ctx ctxs1
            writeTVar threadContextsVar ctxs2
            pure (new_ctx, True)
      when inserted $
        addThreadFinalizer tid $
          atomically $ readTVar threadContextsVar >>= \ctxs ->
            writeTVar threadContextsVar $! Map.delete key ctxs
      pure ctx

-- | Run the given action once the thread is collected.
addThreadFinalizer :: ThreadId -> IO () -> IO ()
addThreadFinalizer (ThreadId t#) (IO fin) = IO $ \s ->
  case mkWeak# t# () fin s of (# s1, _ #) -> (# s1, () #)

-- | Awaken a thread stopped on a breakpoint. Await the result with
-- `readThreadEvalStatus` or `readAnyThreadEvalBreak`.
--
-- Throws @ResumedAlreadyRunningThreadException@ if thread is already running.
resumeStoppedThread :: ResumeContext -> IO ()
resumeStoppedThread ResumeContext{..} = do
  stopped <- atomically $ do
    s <- readTVar resumeEvalStatus
    case s of
      ThreadStatusStopped EvalComplete{} _
        -- We tried to resume after eval has completed.
        --
        -- This may happen, for instance, if an exception kills a paused thread
        -- and writes @EvalComplete EvalException...@ before we do resume.
        --
        -- In that case, there's nothing to resume. The thread is done.
        -> pure False
      ThreadStatusStopped EvalBreak{} _
        -> writeTVar resumeEvalStatus ThreadStatusRunning >> pure True
      ThreadStatusRunning
        -> throwSTM ResumedAlreadyRunningThreadException
  when stopped $ do
    putMVar resumeBreakMVar () -- this awakens the stopped thread

--------------------------------------------------------------------------------
-- Global Thread Status Map
--------------------------------------------------------------------------------

-- | A global mapping from threads to their per-thread state 'ResumeContext'
-- (that e.g. contain the MVars on which they block when stopped).
--
-- Keyed by 'fromThreadId' rather than 'ThreadId' so as not to keep the threads
-- alive: entries are removed when their thread is collected (see
-- 'getThreadResumeContext').
threadContextsVar :: TVar (Map.Map Word64 ResumeContext)
threadContextsVar = unsafePerformIO $ newTVarIO Map.empty
{-# NOINLINE threadContextsVar #-}

-- | Global context for a thread, which is created when a thread hits a
-- breakpoint, i.e. when the 'globalBreakAction' is run, or in other calls to
-- 'getThreadResumeContext'
data ResumeContext = ResumeContext
  { resumeBreakMVar :: !(MVar ())
  -- ^ A thread that hits a breakpoint blocks reading its corresponding MVar
  -- (gotten from the 'threadContextsVar').
  -- The debugger can unblock that thread by signaling its MVar.
  , resumeEvalStatus :: !(TVar ResumeThreadStatus)
  -- ^ When a thread hits a breakpoint, the 'globalBreakAction' is run and
  -- writes an 'EvalBreak' in this @TVar@.
  --
  -- Additionally, the 'EvalComplete' result of a thread may be written here
  -- using `writeThreadEvalStatus tid (EvalComplete ...)` after the main
  -- expression finishes evaluating, see e.g. 'sandboxIO'.
  , resumeIsolated :: !(TVar Bool)
  -- ^ When set, this thread is meant to be ignored by 'readAnyThreadEvalBreak'.
  -- That is, if a thread with resumeIsolated=True hits a breakpoint, its
  -- 'EvalBreak' can only be read with @'readThreadEvalStatus' tid@ for this
  -- thread's @tid@ directly.
  --
  -- By default, the "main thread" of an evaluation is always isolated, whereas
  -- threads forked by the "main thread" aren't.
  }

data ResumeThreadStatus

  -- | The thread has hit a breakpoint or completed with a result.
  -- Concurrent reads of this value are fine, since a thread can only be
  -- resumed from a breakpoint once (see 'ThreadStatusRunning') anyway.
  --
  -- That is, many threads may 'readThreadEvalStatus' this status while the
  -- thread is stopped. As soon as 'ThreadStatusRunning', the reads will block
  -- waiting for another stopped result.
  = ThreadStatusStopped (EvalStatus [HValueRef]) StatusAlreadyRead

  -- \| This thread is paused on a breakpoint, but someone has already taken its
  -- 'EvalBreak' status.
  --
  -- When a @'ThreadStatusStopped' 'EvalBreak'{...}@ is read, it gets swapped
  -- by @'ThreadStatusPausedButTaken'@. The point is @'readAnyThreadEvalBreak'@
  -- should not return the same breakpoint twice, nor should it return a
  -- breakpoint already returned by @'readThreadEvalStatus'@.
  -- \| ThreadStatusPausedButTaken

  -- | When resuming a thread, the 'ResumeThreadStatus' must be set to
  -- 'ThreadStatusRunning' so that further attempts to resume from the same
  -- breakpoint fail. Even when resuming a thread by means of throwing it an
  -- exception.
  | ThreadStatusRunning

-- | We don't want to return any already read breakpoint from 'readAnyThreadEvalBreak'.
--
-- Once we read a status off of a stopped thread, we swap 'StatusNotYetRead'
-- with 'AlreadyRead' to make sure 'readAnyThreadEvalBreak' doesn't read it
-- again.
--
-- On breakpoint hit we write 'ThreadStatusStopped' with 'StatusNotYetRead'
--
-- We don't just swap out the 'ResumeThreadStatus' for 'Nothing' to avoid races
-- in which a thread could have taken that resume lock and be canceled before
-- having the chance to resume it.
data StatusAlreadyRead = StatusNotYetRead | StatusAlreadyRead

-- | Attempted to resume a thread that was already running!
data ResumedAlreadyRunningThreadException = ResumedAlreadyRunningThreadException deriving Show
instance Exception ResumedAlreadyRunningThreadException
