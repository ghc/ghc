{-# LANGUAGE GADTs, RecordWildCards, MagicHash, ScopedTypeVariables, CPP,
    UnboxedTuples, LambdaCase, UnliftedFFITypes, NamedFieldPuns, BangPatterns #-}
module GHCi.Run.Breakpoints
  (
  -- *  Global break action
    globalBreakStablePtr, globalBreakAction

  -- * Per-thread breakpoints
  , readThreadEvalStatus, readAnyThreadEvalBreak
  , writeThreadEvalStatus
  , readCtxEvalStatus, writeCtxEvalStatus

  -- ** Isolating thread breakpoints
  , withIsolatedThread
  , setIsolatedThread, unsetIsolatedThread
  , setIsolatedCtx, unsetIsolatedCtx

  -- ** Querying the global thread breakpoint map
  , listOtherPausedThreads

  -- * Per-thread resume-ing
  , getThreadResumeContext, ResumeContext(..)

  )
  where

import Prelude -- See note [Why do we import Prelude here?]

import GHCi.Debugger
import GHCi.Message
import GHCi.RemoteTypes

import qualified Data.Map as Map
import Control.Concurrent
import Control.Exception
import qualified Data.ByteString.Short.Internal as BS
import GHC.Exts
import GHC.Stack
import Foreign hiding (void)
import GHC.Conc.Sync
import GHC.IO hiding ( bracket )
import Control.Monad

--------------------------------------------------------------------------------
-- * Global break action
--------------------------------------------------------------------------------
-- Global Debugger Per-Thread Context
-- TODO: when to clean MVar? how to figure out when Threads are dead and we'll
-- never need their mvars again? perhaps a finalizer on the thread id?
--
-- TODO: Move to independent module, and don't expose the global variables at all. move also the global break action maybe.
-- Maybe there could even be something in that module for setting the global
-- break action on start. maybe just an init function
--
-- TODO: Note which explains when things are evacuated from the map, why
-- EvalSuccess's don't leak, what is the idea, why have MVar and TVar, why have
-- isolated threads. Must also explain whatever we come up with for Finalizers
-- to make sure threads evacuate themselves out of the map. Explain in the note
-- as well the status of GHCi multi-threaded, explaining it currently uses
-- single-threaded
--
-- TODO: Write a blog post about multi-threaded debugging (mby with the contents of the note..)

-- TODO: Maybe this could be the return value of doing initialization for the
-- debugger, which allocates this stable pointer and sets it in the global action.
--
-- And de-initialization deallocates this stable pointer and then some other things
globalBreakStablePtr :: StablePtr BreakpointCallback
globalBreakStablePtr = unsafePerformIO $ newStablePtr globalBreakAction

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

  -- Block until this thread is resumed (by the thread which took the
  -- `ResumeContext` from the `statusMVar`).
  --
  -- The `onBreak` function must have been called from `rts/Interpreter.c`
  -- when interpreting a `BRK_FUN`. After taking from the MVar, the function
  -- returns to the continuation on the stack which is where the interpreter
  -- was stopped.
  takeMVar resumeBreakMVar

--------------------------------------------------------------------------------
-- * Per-thread breakpoints
--------------------------------------------------------------------------------

-- | To evaluate a statement in the interpreter, a thread is forked by
-- `sandboxIO` to run the statement. To synchronize, the statement result will
-- be written by the thread to the global thread dbg status
-- (`writeThreadEvalStatus`) and `sandboxIO` will block waiting for either any
-- breakpoint to be hit ('readAnyThreadEvalBreak') /or/ for the thread
-- specifically to write its final result. To wait for that thread
-- specifically, use this function.
--
-- Succeeds when the given thread writes to the global debugger map an eval result.
readThreadEvalStatus :: ThreadId -> STM (EvalStatus [HValueRef])
readThreadEvalStatus tid = do
  ctxs <- readTVar threadContextsVar
  case Map.lookup tid ctxs of
    Just ctx -> readCtxEvalStatus ctx
    Nothing  -> retry -- to write a thread break or result, the
                      -- 'threadContextsVar' entry will be created

-- | Like 'readThreadEvalStatus', but when the 'ResumeContext' is already
-- available (to avoid looking it up unnecessarily).
readCtxEvalStatus :: ResumeContext -> STM (EvalStatus [HValueRef])
readCtxEvalStatus ResumeContext{resumeEvalStatus} =
  readTVar resumeEvalStatus >>= \case
    Just r  -> do
      writeTVar resumeEvalStatus Nothing -- clear it
      pure r
    Nothing -> retry -- until someone writes the result for this thread

-- | All threads running in the interpreter will be debugger-enabled, meaning
-- if they hit an enabled breakpoint, the 'globalBreakAction' will be
-- executed and write a 'EvalBreak' value to the global thread dbg status map.
-- The debugger will be waiting for threads to break using `readAnyThreadEvalBreak`.
--
-- Succeeds when any thread hits a breakpoint.
readAnyThreadEvalBreak :: STM EvalBreak
readAnyThreadEvalBreak = do
  ctxs <- readTVar threadContextsVar
  -- try reading any thread's 'EvalBreak' and retry if none have yielded one yet
  foldr (\ctx next -> readBreak ctx `orElse` next) retry (Map.elems ctxs)
  where
    readBreak ResumeContext{resumeEvalStatus, resumeIsolated} = do
      isolated <- readTVar resumeIsolated
      if isolated
        then retry -- don't look at isolated threads
        else readTVar resumeEvalStatus >>= \case
          Just (EvalPaused b) -> do
            writeTVar resumeEvalStatus Nothing
            pure b
          Just EvalComplete{} ->
            retry -- successes can be read with `readThreadEvalStatus`
          Nothing ->
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
      Just{}  -> error "writeCtxEvalStatus: should be impossible"
      Nothing -> writeTVar resumeEvalStatus (Just status)


-- ** Isolating thread breakpoints ---------------------------------------------

-- | Brackets 'setIsolatedThread'
withIsolatedThread :: ThreadId -> IO a -> IO a
withIsolatedThread tid = bracket_ (setIsolatedThread tid) (unsetIsolatedThread tid)

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

-- ** Querying the global thread breakpoint map --------------------------------

-- | List all other threads paused on a breakpoint.
--
-- We say "other" because this function is meant to be used in the scenario
-- where you are already paused at a breakpoint (which has been popped off of
-- the global thread mapping) and are asking about which other threads are
-- paused to list them.
listOtherPausedThreads :: IO [ThreadId]
listOtherPausedThreads = atomically $ do
  ctxs <- readTVar threadContextsVar
  paused <- mapM isBreak (Map.toList ctxs)
  pure [tid | (tid, True) <- paused]
  where
    isBreak (tid, ResumeContext{resumeEvalStatus}) = do
      status <- readTVar resumeEvalStatus
      pure (tid, case status of
                   Just EvalBreak{}    -> True
                   Just EvalComplete{} -> False
                   Nothing             -> False)

--------------------------------------------------------------------------------
-- * Per-thread resume-ing
--------------------------------------------------------------------------------

getThreadResumeContext :: ThreadId -> IO ResumeContext
getThreadResumeContext tid = do
  ctxs0 <- readTVarIO threadContextsVar
  case Map.lookup tid ctxs0 of
    Just ctx -> pure ctx -- common case: hence read without transaction
    Nothing  -> do
      new_ctx <- ResumeContext <$> newEmptyMVar
                               <*> newTVarIO Nothing
                               <*> newTVarIO False
      atomically $ do -- write new thread context atomically
        ctxs1 <- readTVar threadContextsVar
        case Map.lookup tid ctxs1 of
          Just ctx -> pure ctx -- already exists
          Nothing  -> do
            let !ctxs2 = Map.insert tid new_ctx ctxs1
            writeTVar threadContextsVar ctxs2
            pure new_ctx

-- | A global mapping from threads to their per-thread state 'ResumeContext'
-- (that e.g. contain the MVars on which they block when stopped)
threadContextsVar :: TVar (Map.Map ThreadId ResumeContext)
threadContextsVar = unsafePerformIO $ newTVarIO Map.empty
{-# NOINLINE threadContextsVar #-}

-- | Global context for a thread, which is created and inserted in the global
-- 'threadContextsVar' when a thread hits a breakpoint (and, thus, the
-- 'globalBreakAction') is run, or when 'withBreakAction' sets the thread
-- isolation (e.g. in 'sandboxIO'), or in other calls to
-- 'getThreadResumeContext', the function which gets or creates a thread
-- 'ResumeContext'.
--
-- TODO: Rename ResumeContext to something like ThreadContext or
-- ThreadDbgContext or (Thread)BreakContext, and fields to something like
-- dbgEvalStatus, threadIsolated, ...
data ResumeContext = ResumeContext
  { resumeBreakMVar :: !(MVar ())
  -- ^ A thread that hits a breakpoint blocks reading its corresponding MVar
  -- (gotten from the 'threadContextsVar').
  -- The debugger can unblock that thread by signaling its MVar.
  , resumeEvalStatus :: !(TVar (Maybe (EvalStatus [HValueRef])))
  -- ^ When a thread hits a breakpoint, the 'globalBreakAction' is run and
  -- writes an 'EvalBreak' in this @TVar@ (which was found in 'threadContextsVar').
  --
  -- Additionally, the 'EvalComplete' result of a thread may be written here
  -- using `writeThreadEvalStatus tid (EvalComplete ...)` after the main
  -- expression finishes evaluating, see e.g. 'sandboxIO'.
  --
  -- The variable should be emptied (i.e. set to 'Nothing') by whoever
  -- reads the status.
  , resumeIsolated :: !(TVar Bool)
  -- ^ When set, this thread is meant to be ignored by 'readAnyThreadEvalBreak'.
  -- That is, if a thread with resumeIsolated=True hits a breakpoint, its
  -- 'EvalBreak' can only be read with @'readThreadEvalStatus' tid@ for this
  -- thread's @tid@ directly.
  }
