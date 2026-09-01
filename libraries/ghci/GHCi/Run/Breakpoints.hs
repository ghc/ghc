{-# LANGUAGE GADTs, RecordWildCards, MagicHash, ScopedTypeVariables, CPP,
    UnboxedTuples, LambdaCase, UnliftedFFITypes, NamedFieldPuns, BangPatterns #-}
module GHCi.Run.Breakpoints
  (
  -- *  Global break action
    globalBreakStablePtr, globalBreakAction

  -- * Per-thread breakpoints
  , readThreadEvalStatus, readAnyThreadEvalBreak
  , writeThreadEvalStatus

  -- ** Isolating thread breakpoints
  , withIsolatedThread
  , setIsolatedThread, unsetIsolatedThread

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
import qualified Data.Set as Set
import Control.Concurrent
import Control.Exception
import qualified Data.ByteString.Short.Internal as BS
#if defined(PROFILING)
import GHC.Data.ShortByteString
#endif
import GHC.Exts
import GHC.Stack
import Foreign hiding (void)
import GHC.Conc.Sync
import GHC.IO hiding ( bracket )

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
  ResumeContext{..} <- getThreadResumeContext tid
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
  writeThreadEvalStatus tid $ EvalBreak apStack_r mb_breakpoint resume_r ccs

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
  GlobalDbgStatus{..} <- readTVar globalDbgStatusVar
  case Map.lookup tid threadEvalStatuses of
    Just r  -> do
      writeTVar globalDbgStatusVar
        GlobalDbgStatus
          { threadEvalStatuses = Map.delete tid threadEvalStatuses -- clear it
          , isolatedThreads
          }
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
  dbgs <- readTVar globalDbgStatusVar
  -- try reading any thread's 'EvalBreak' and retry if none have yielded one yet
  foldr (\x next -> readStatus dbgs x `orElse` next) retry (Map.toList (threadEvalStatuses dbgs))
  where
    readStatus GlobalDbgStatus{..} (tid, _) -- don't look at isolated threads
      | Set.member tid isolatedThreads
      = retry

    readStatus GlobalDbgStatus{..} (tid, status)
      = do
      case status of
        EvalComplete{} -> retry -- successes can be read with `readThreadEvalStatus`
        EvalPaused b   -> do
          writeTVar globalDbgStatusVar
            GlobalDbgStatus
              { threadEvalStatuses = Map.delete tid threadEvalStatuses
              , isolatedThreads
              }
          pure b

writeThreadEvalStatus :: ThreadId -> EvalStatus [HValueRef] -> IO ()
writeThreadEvalStatus tid status =
  atomically $ do
    GlobalDbgStatus{..} <- readTVar globalDbgStatusVar
    writeTVar globalDbgStatusVar $
      GlobalDbgStatus
        { threadEvalStatuses =
            Map.insertWith (\_ _ -> error "writeThreadEvalStatus: should be impossible")
                tid status threadEvalStatuses
        , isolatedThreads }


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
setIsolatedThread tid = atomically $ do
  GlobalDbgStatus{..} <- readTVar globalDbgStatusVar
  writeTVar globalDbgStatusVar
    GlobalDbgStatus
      { threadEvalStatuses
      , isolatedThreads = Set.insert tid isolatedThreads
      }

-- | Undoes 'setIsolatedThread'
unsetIsolatedThread :: ThreadId -> IO ()
unsetIsolatedThread tid = atomically $ do
  GlobalDbgStatus{..} <- readTVar globalDbgStatusVar
  writeTVar globalDbgStatusVar
    GlobalDbgStatus
      { threadEvalStatuses
      , isolatedThreads = Set.delete tid isolatedThreads
      }

-- ** Querying the global thread breakpoint map --------------------------------

-- | List all other threads paused on a breakpoint.
--
-- We say "other" because this function is meant to be used in the scenario
-- where you are already paused at a breakpoint (which has been popped off of
-- the global thread mapping) and are asking about which other threads are
-- paused to list them.
listOtherPausedThreads :: IO [ThreadId]
listOtherPausedThreads = do
  m <- atomically $ readTVar globalDbgStatusVar
  pure $ Map.keys $ Map.filter isBreak (threadEvalStatuses m)
  where
    isBreak EvalBreak{}     = True
    isBreak EvalComplete{} = False

-- ** Global thread exec status map --------------------------------------------

-- | When we launch a new thread using the debugger, we map ... TODO ... all
-- threads launched by that main stmt may further spawn threads, and any of
-- them might hit a breakpoint (break array toggled, or per-thread step in
-- might be used, or pause button, etc...).
--
-- When we launch that main thread we also write its result to this map. So
-- this map will only contain the results of threads we explicitly care to
-- observe the result of. Not all threads will write their results here,
-- certainly not ones spawned unknowningly by the main stmt.
-- ...
-- todo words
globalDbgStatusVar :: TVar GlobalDbgStatus
globalDbgStatusVar = unsafePerformIO $ newTVarIO $ GlobalDbgStatus Map.empty Set.empty
{-# NOINLINE globalDbgStatusVar #-}

data GlobalDbgStatus = GlobalDbgStatus
  { threadEvalStatuses :: !(Map.Map ThreadId (EvalStatus [HValueRef]))
  -- ^ Whenever a thread hits a breakpoint it inserts in this map its 'EvalBreak'.
  -- We can additionally observe a thread's result by calling
  -- `writeThreadEvalStatus tid (EvalComplete ...)` when the main expression
  -- finishes evaluating, see e.g. 'sandboxIO'.
  , isolatedThreads    :: !(Set.Set ThreadId)
  -- ^ Threads in this map are meant to be ignored by 'readAnyThreadEvalBreak'.
  -- That is, even if one of the threads in this set hits a breakpoint, we
  -- can only read its 'EvalBreak' with @'readThreadEvalStatus' tid@ directly.
  }

--------------------------------------------------------------------------------
-- * Per-thread resume-ing
--------------------------------------------------------------------------------

getThreadResumeContext :: ThreadId -> IO ResumeContext
getThreadResumeContext tid = modifyMVar globalBreakActionMap $ \gbm0 -> do
  case Map.lookup tid gbm0 of
    Nothing  -> do
      r <- ResumeContext <$> newEmptyMVar
      let !gbm1 = Map.insert tid r gbm0
      pure (gbm1, r)
    Just ctx -> do
      pure (gbm0, ctx)

-- | A global mapping from thread ids to the MVars on which they block when stopped.
globalBreakActionMap :: MVar (Map.Map ThreadId ResumeContext)
globalBreakActionMap = unsafePerformIO $ newMVar Map.empty
{-# NOINLINE globalBreakActionMap #-}

newtype ResumeContext = ResumeContext
  { resumeBreakMVar :: MVar ()
  -- ^ A thread that hits a breakpoint blocks reading its corresponding MVar
  -- (gotten from the 'globalBreakActionMap').
  -- The debugger can unblock that thread by signaling its MVar.
  }
