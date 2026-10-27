{-# LANGUAGE GADTs, RecordWildCards, MagicHash, ScopedTypeVariables, CPP,
    UnboxedTuples, LambdaCase, UnliftedFFITypes #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Execute GHCi messages.
--
-- For details on Remote GHCi, see Note [Remote GHCi] in
-- compiler/GHC/Runtime/Interpreter.hs.
--
module GHCi.Run
  ( run, redirectInterrupts
  ) where

import Prelude -- See note [Why do we import Prelude here?]

#if !defined(javascript_HOST_ARCH)
import GHCi.CreateBCO
import GHCi.InfoTable
#endif

import GHCi.Run.Breakpoints
import GHCi.Coverage
import qualified GHC.InfoProv as InfoProv
import GHCi.Debugger
import GHCi.FFI
import GHCi.Message
import GHCi.ObjLink
import GHCi.RemoteTypes
import GHCi.TH
import GHCi.BreakArray
import GHCi.StaticPtrTable

import Data.Function
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short.Internal as BS
import qualified Data.ByteString.Unsafe as B
#if defined(PROFILING)
import GHC.Data.ShortByteString
#endif
import GHC.Exts
import qualified GHC.Exts.Heap as Heap
import GHC.Stack
import Foreign hiding (void)
import Foreign.C
import GHC.Conc.Sync
import GHC.IO hiding ( bracket )
import System.Mem.Weak  ( deRefWeak )
import Unsafe.Coerce

-- -----------------------------------------------------------------------------
-- Implement messages

foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()
        -- Make it "safe", just in case

run :: Message a -> IO a
run m = case m of
#if defined(javascript_HOST_ARCH)
  LoadObj p                   -> withCString p loadJS
  InitLinker                  -> notSupportedJS m
  LoadDLLs {}                 -> notSupportedJS m
  LoadArchive {}              -> notSupportedJS m
  UnloadObj {}                -> notSupportedJS m
  AddLibrarySearchPath {}     -> notSupportedJS m
  RemoveLibrarySearchPath {}  -> notSupportedJS m
  MkConInfoTable {}           -> notSupportedJS m
  ResolveObjs                 -> notSupportedJS m
  FindSystemLibrary {}        -> notSupportedJS m
  CreateBCOs {}               -> notSupportedJS m
  LookupClosure str           -> lookupJSClosure str
#else
  InitLinker -> initObjLinker RetainCAFs
  LoadDLLs strs -> fmap (map toRemotePtr) <$> loadDLLs strs
  LoadArchive str -> loadArchive str
  LoadObj str -> loadObj str
  UnloadObj str -> unloadObj str
  AddLibrarySearchPath str -> toRemotePtr <$> addLibrarySearchPath str
  RemoveLibrarySearchPath ptr -> removeLibrarySearchPath (fromRemotePtr ptr)
  MkConInfoTable (ConInfoTable tc ptrs nptrs tag ptrtag desc) ->
    toRemotePtr <$> mkConInfoTable tc ptrs nptrs tag ptrtag desc
  ResolveObjs -> resolveObjs
  FindSystemLibrary str -> findSystemLibrary str
  CreateBCOs bcos -> createBCOs bcos
  LookupClosure str -> lookupClosure str
#endif
  RtsRevertCAFs -> rts_revertCAFs
  LookupSymbol str -> fmap toRemotePtr <$> lookupSymbol str
  LookupSymbolInDLL dll str ->
    fmap toRemotePtr <$> lookupSymbolInDLL (fromRemotePtr dll) str
  FreeHValueRefs rs -> mapM_ freeRemoteRef rs
  AddSptEntry fpr r -> localRef r >>= sptAddEntry fpr
  AddHpcModule modl ticks hash tickboxes -> hpcAddModule modl ticks hash tickboxes
  EvalStmt opts r -> evalStmt opts r
  ResumeStmt opts r -> resumeStmt opts r
  AbandonStmt r -> abandonStmt r
  EvalString r -> evalString r
  EvalStringToString r s -> evalStringToString r s
  EvalIO r -> evalIO r
  MkCostCentres mod ccs -> mkCostCentres mod ccs
  CostCentreStackInfo ptr -> ccsToStrings (fromRemotePtr ptr)
  NewBreakArray sz -> mkRemoteRef =<< newBreakArray sz
  SetupBreakpoint ref ix cnt -> do
    arr <- localRef ref;
    _ <- setupBreakpoint arr ix cnt
    return ()
  BreakpointStatus ref ix -> do
    arr <- localRef ref; r <- getBreak arr ix
    case r of
      Nothing -> return False
      Just w -> return (w == 0)
  GetBreakpointVar ref ix -> do
    aps <- localRef ref
    mapM mkRemoteRef =<< getIdValFromApStack aps ix
  MallocData bs -> mkString bs
  MallocStrings bss -> mapM mkString0 bss
  FreeRemoteResources rs -> mapM_ freeRes rs
  PrepFFI args res -> toRemotePtr <$> prepForeignCall args res
  FreeFFI p -> freeForeignCallInfo (fromRemotePtr p)
  StartTH -> startTH
  GetClosure ref -> do
    clos <- Heap.getClosureData =<< localRef ref
    mapM (\(Heap.Box x) -> mkRemoteRef (HValue x)) clos
  WhereFrom ref ->
    InfoProv.whereFrom =<< localRef ref
  Seq ref -> doSeq ref
  ResumeSeq ref -> resumeSeq ref

  Shutdown            -> unexpectedMessage m
  RunTH {}            -> unexpectedMessage m
  RunModFinalizers {} -> unexpectedMessage m
  CustomMessage {}    -> unexpectedMessage m
  where
    freeRes (RemoteHValueRef href) = freeRemoteRef href
    freeRes (RemoteRemotePtr rptr) = free $ fromRemotePtr rptr
    freeRes (RemoteFFI p) = freeForeignCallInfo (fromRemotePtr p)
    freeRes (RemoteLibrarySearchPath p) = void $ removeLibrarySearchPath $ fromRemotePtr p

unexpectedMessage :: Message a -> b
unexpectedMessage m = error ("GHCi.Run.Run: unexpected message: " ++ show m)

#if defined(javascript_HOST_ARCH)
foreign import javascript "((ptr,off) => globalThis.h$loadJS(h$decodeUtf8z(ptr,off)))" loadJS :: CString -> IO ()

foreign import javascript "((ptr,off) => globalThis.h$lookupClosure(h$decodeUtf8z(ptr,off)))" lookupJSClosure# :: CString -> State# RealWorld -> (# State# RealWorld, Int# #)

lookupJSClosure' :: BS.ShortByteString -> IO Int
lookupJSClosure' str = BS.useAsCString str $ \cstr -> IO (\s ->
  case lookupJSClosure# cstr s of
    (# s', r #) -> (# s', I# r #))

lookupJSClosure :: BS.ShortByteString -> IO (Maybe HValueRef)
lookupJSClosure str = lookupJSClosure' str >>= \case
  0 -> pure Nothing
  r -> pure (Just (RemoteRef (RemotePtr (fromIntegral r))))

notSupportedJS :: Message a -> b
notSupportedJS m = error ("Message not supported with the JavaScript interpreter: " ++ show m)
#endif

-- Malloc and copy the bytes.  We don't have any way to monitor the
-- lifetime of this memory, so it just leaks.
mkString :: ByteString -> IO (RemotePtr ())
mkString bs = B.unsafeUseAsCStringLen bs $ \(cstr,len) -> do
  ptr <- mallocBytes len
  copyBytes ptr cstr len
  return (castRemotePtr (toRemotePtr ptr))

mkString0 :: ByteString -> IO (RemotePtr ())
mkString0 bs = B.unsafeUseAsCStringLen bs $ \(cstr,len) -> do
  ptr <- mallocBytes (len+1)
  copyBytes ptr cstr len
  pokeElemOff (ptr :: Ptr CChar) len 0
  return (castRemotePtr (toRemotePtr ptr))

mkCostCentres :: RemotePtr () -> [(BS.ShortByteString, BS.ShortByteString)] -> IO [RemotePtr CostCentre]
#if defined(PROFILING)
mkCostCentres mod ccs = do
  let c_module = fromRemotePtr $ castRemotePtr mod
  mapM (mk_one c_module) ccs
 where
  mk_one c_module (decl_path,srcspan) = do
    c_name <- newCStringFromSBS decl_path
    c_srcspan <- newCStringFromSBS srcspan
    toRemotePtr <$> c_mkCostCentre c_name c_module c_srcspan

foreign import ccall unsafe "mkCostCentre"
  c_mkCostCentre :: Ptr CChar -> Ptr CChar -> Ptr CChar -> IO (Ptr CostCentre)
#else
mkCostCentres _ _ = return []
#endif

getIdValFromApStack :: HValue -> Int -> IO (Maybe HValue)
getIdValFromApStack apStack (I# stackDepth) = do
   case getApStackVal# apStack stackDepth of
        (# ok, result #) ->
            case ok of
              0# -> return Nothing -- AP_STACK not found
              _  -> return (Just (unsafeCoerce# result))

-- Exceptions can't be marshaled because they're dynamically typed, so
-- everything becomes a String.
tryEval :: IO a -> IO (EvalResult a)
tryEval io = do
  e <- try io
  case e of
    Left ex -> return (EvalException (toSerializableException ex))
    Right a -> return (EvalSuccess a)

--------------------------------------------------------------------------------

evalStmt :: EvalOpts -> EvalExpr HValueRef -> IO (EvalStatus [HValueRef])
evalStmt opts expr = do
  io <- mkIO expr
  sandboxIO opts $ do
    rs <- unsafeCoerce io :: IO [HValue]
    mapM mkRemoteRef rs
 where
  mkIO (EvalThis href) = localRef href
  mkIO (EvalApp l r) = do
    l' <- mkIO l
    r' <- mkIO r
    return ((unsafeCoerce l' :: HValue -> HValue) r')

evalIO :: HValueRef -> IO (EvalResult ())
evalIO r = do
  io <- localRef r
  tryEval (unsafeCoerce io :: IO ())

evalString :: HValueRef -> IO (EvalResult String)
evalString r = do
  io <- localRef r
  tryEval $ do
    r <- unsafeCoerce io :: IO String
    evaluate (force r)

evalStringToString :: HValueRef -> String -> IO (EvalResult String)
evalStringToString r str = do
  io <- localRef r
  tryEval $ do
    r <- (unsafeCoerce io :: String -> IO String) str
    evaluate (force r)


--------------------------------------------------------------------------------

-- | Process the Seq message to force a value.                       #2950
-- If during this processing a breakpoint is hit, return
-- an EvalBreak value in the EvalStatus to the UI process,
-- otherwise return an EvalComplete.
-- The UI process has more and therefore also can show more
-- information about the breakpoint than the current iserv
-- process.
doSeq :: RemoteRef a -> IO (EvalStatus [HValueRef])
doSeq ref = do
    sandboxIO evalOptsSeq $ do
      _ <- (void $ evaluate =<< localRef ref)
      return []

-- | Process a ResumeSeq message. Continue the :force processing     #2950
-- after a breakpoint.
resumeSeq :: RemoteRef ThreadId -> IO (EvalStatus [HValueRef])
resumeSeq = resumeStmt evalOptsSeq

evalOptsSeq :: EvalOpts
evalOptsSeq = EvalOpts
              { useSandboxThread = True
              , singleStep = False
              , stepOut    = False
              , breakOnException = False
              , breakOnError = False
              , readAllBreaks = False
              }

-- When running a computation, we redirect ^C exceptions to the running
-- thread.  ToDo: we might want a way to continue even if the target
-- thread doesn't die when it receives the exception... "this thread
-- is not responding".
--
-- Careful here: there may be ^C exceptions flying around, so we start the new
-- thread blocked (forkIO inherits mask from the parent, #1048), and unblock
-- only while we execute the user's code.  We can't afford to lose the final
-- 'writeThreadEvalStatus', otherwise deadlock ensues. (#1583, #1922, #1946)

sandboxIO :: EvalOpts -> IO [HValueRef] -> IO (EvalStatus [HValueRef])
sandboxIO opts io = do
  -- We are running in uninterruptibleMask
  withBreakAction $ do
    let runIt = measureAlloc $ tryEval $ rethrow opts $ clearCCS io
    if useSandboxThread opts
       then do

         tid <- forkIO $ do
           tid <- myThreadId
           setIsolatedThread tid  -- main thread is always isolated
           setBreakFlags opts tid -- set flags on the sandbox thread
           labelThread tid "GHCi sandbox"
           -- We only write the thread status map in two places:
           -- * When the "main" thread finishes (here)
           -- * When any thread hits a breakpoint (global break action)
           unsafeUnmask runIt >>= writeThreadEvalStatus tid

         atomically $ modifyTVar evaluationMainThreadGlobalVar (tid :)
         redirectInterrupts tid $ unsafeUnmask $
           atomically $ readEvalStatus opts tid
       else
          -- GLUT on OS X needs to run on the main thread. If you
          -- try to use it from another thread then you just get a
          -- white rectangle rendered. For this, or anything else
          -- with such restrictions, you can turn the GHCi sandbox off
          -- and things will be run in the main thread.
          --
          -- BUT, note that the debugging features (breakpoints,
          -- tracing, etc.) need the expression to be running in a
          -- separate thread, so debugging is only enabled when
          -- using the sandbox.
         runIt

-- We want to turn ^C into a break when -fbreak-on-exception is on,
-- but it's an async exception and we only break for sync exceptions.
-- Idea: if we catch and re-throw it, then the re-throw will trigger
-- a break.  Great - but we don't want to re-throw all exceptions, because
-- then we'll get a double break for ordinary sync exceptions (you'd have
-- to :continue twice, which looks strange).  So if the exception is
-- not "Interrupted", we unset the exception flag before throwing.
--
rethrow :: EvalOpts -> IO a -> IO a
rethrow EvalOpts{..} io =
  catchNoPropagate io $ \(ExceptionWithContext cx se) -> do
    -- If -fbreak-on-error, we break unconditionally,
    --  but with care of not breaking twice
    ThreadId mytid <- myThreadId
    if breakOnError && not breakOnException
       then rts_enableStopOnException mytid
       else case fromException se of
               -- If it is a "UserInterrupt" exception, we allow
               --  a possible break by way of -fbreak-on-exception
               Just UserInterrupt -> return ()
               -- In any other case, we don't want to break
               _ -> do
                  rts_disableStopOnException mytid
                  rts_disableStopOnExceptionAll
    rethrowIO (ExceptionWithContext cx se)

--
-- While we're waiting for the sandbox thread to return a result, if
-- the current thread receives an asynchronous exception we re-throw
-- it at the sandbox thread and continue to wait.
--
-- This is for two reasons:
--
--  * So that ^C interrupts runStmt (e.g. in GHCi), allowing the
--    computation to run its exception handlers before returning the
--    exception result to the caller of runStmt.
--
--  * clients of the GHC API can terminate a runStmt in progress
--    without knowing the ThreadId of the sandbox thread (#1381)
--
-- NB. use a weak pointer to the thread, so that the thread can still
-- be considered deadlocked by the RTS and sent a BlockedIndefinitely
-- exception.  A symptom of getting this wrong is that conc033(ghci)
-- will hang.
--
redirectInterrupts :: ThreadId -> IO a -> IO a
redirectInterrupts target wait = do
  wtid <- mkWeakThreadId target
  wait `catch` \e -> do
     m <- deRefWeak wtid
     case m of
       Nothing -> wait
       Just target -> do throwTo target (e :: SomeException); wait

measureAlloc :: IO (EvalResult a) -> IO (EvalStatus a)
measureAlloc io = do
  setAllocationCounter 0                                 -- #16012
  a <- io
  ctr <- getAllocationCounter
  let allocs = negate $ fromIntegral ctr
  return (EvalComplete allocs a)

-- This function sets up the interpreter for catching breakpoints, and
-- resets the breakpoint action when the computation has stopped running.
-- This is a not-very-good way to ensure that only the interactive
-- evaluation should generate breakpoints.
withBreakAction :: IO a -> IO a
withBreakAction act
 = bracket setBreakAction resetBreakAction (\_ -> act)
 where
   setBreakAction = do
     stablePtr <- newStablePtr globalBreakAction
     poke breakPointIOAction stablePtr
     return stablePtr

   resetBreakAction stablePtr = do
     poke breakPointIOAction noBreakStablePtr
     freeStablePtr stablePtr

-- | Set the single-step, step-out, and break-on-exception flags on the target
-- thread based on the given 'EvalOpts'.
--
-- Breaking on exceptions is not enabled by default, since it might be a
-- bit surprising. The exception flag is turned off as soon as it is hit.
--
-- See Note [Always setBreakFlags on running thread]
setBreakFlags :: EvalOpts -> ThreadId -> IO ()
setBreakFlags opts (ThreadId tid#) = do
  setFlag# breakOnException rts_enableStopOnException    rts_disableStopOnException
  setFlag# singleStep       rts_enableStopNextBreakpoint rts_disableStopNextBreakpoint
  setFlag# stepOut          rts_enableStopAfterReturn    rts_disableStopAfterReturn
  where
    setFlag# pred on off = if pred opts then on tid# else off tid#

{-
Note [Always setBreakFlags on running thread]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Whenever we run a expression in the interpreter or unblock a paused thread we
must make sure all breakpoint-flags for that thread are set according to the
given options. There are three scenarios in which we set something running:

- `evalStmt`, forks a thread to evaluate the given expression
- `resumeStmt`, unblocks a given thread to resume execution
- `abandonStmt`, kills a given thread to terminate execution (but the thread
- will resume to handle the exception)

In these three cases, if we musn't forget to set all breakpoint flags,
since otherwise we'd resume execution with whatever stale state we had of the
previous run command. A trivial example would be (tested by break037):

1. `evalStmt` with { singleStep = True, breakOnError = True } would e.g. stop on
  an exception and clear TSO_STOP_ON_EXCEPTION, but TSO_STOP_NEXT_BREAKPOINT
  would still be active.
2. `abandonStmt` if we didn't setBreakFlags again would kill the thread but
  hit the next breakpoint bc TSO_STOP_NEXT_BREAKPOINT would still be active.

Likewise for other interactions between eval/resume/abandon.
TL;DR: always `setBreakFlags` when running/resuming a thread
-}

resumeStmt :: EvalOpts -> RemoteRef ThreadId -> IO (EvalStatus [HValueRef])
resumeStmt opts rtid = do
  tid <- localRef rtid
  ctx <- getThreadResumeContext tid
  setBreakFlags opts tid
  main_tid:_ <- readTVarIO evaluationMainThreadGlobalVar
  withBreakAction $
    mask_ $ do
      resumeStoppedThread ctx -- this awakens the stopped thread...
      -- wait for the result of the main thread, not of the stopped one, but
      -- interrupt the thread which was resumed.
      redirectInterrupts tid $ unsafeUnmask $
        atomically $ readEvalStatus opts main_tid

-- | Wait for the given main thread of an evaluation to complete or break or,
-- if 'readAllBreaks', for any other thread to break.
readEvalStatus :: EvalOpts -> ThreadId -> STM (EvalStatus [HValueRef])
readEvalStatus opts main_tid = do
  r <- readThreadEvalStatus main_tid
    & if readAllBreaks opts
        then (`orElse` (EvalPaused <$> readAnyThreadEvalBreak))
        else id
  case r of
    EvalComplete{} -> popMainThread main_tid
    EvalPaused{}   -> pure ()
  pure r

-- | When abandoning a computation we have to
--      (a) kill the stopped thread with an async exception, so that the
--          computation itself is stopped, and
--      (b) on the stopped thread (in 'globalBreakAction') catch and re-throw
--         the kill exception *asynchronously* again. We could have been
--         evaluating a thunk when we stopped at the breakpoint, so the async
--         exception makes the thunk resumable from where we stopped instead of
--         updating it with an exception (tested in break010).
--      (c) wait for the main thread to terminate by waiting for its result.
--          This step is necessary to prevent race conditions with
--          -fbreak-on-exception (see #5975).
--  See test break010.
abandonStmt :: RemoteRef ThreadId -> IO ()
abandonStmt hvref = do
  main_tid:_ <- readTVarIO evaluationMainThreadGlobalVar
  tid <- localRef hvref
  setBreakFlags evalOptsAbandon tid
  killThread tid -- kill stopped thread, then wait for main thread
  atomically $
    readThreadEvalStatus main_tid >>= \case
      -- killThread may return before the thread is marked as running
      EvalPaused{}   -> retry
      EvalComplete{} -> popMainThread main_tid
  where
    -- Don't stop anywhere on abandon
    evalOptsAbandon = EvalOpts
      { useSandboxThread = True
      , singleStep = False
      , stepOut    = False
      , breakOnException = False
      , breakOnError = False
      , readAllBreaks = False
      }

noBreakStablePtr :: StablePtr BreakpointCallback
noBreakStablePtr = unsafePerformIO $ newStablePtr noBreakAction

noBreakAction :: BreakpointCallback
noBreakAction _ _ _ False _ = putStrLn "*** Ignoring breakpoint"
noBreakAction _ _ _ True  _ = return () -- exception: just continue

--------------------------------------------------------------------------------
-- To remove completely when sandboxIO/resume becomes asynchronous:
--------------------------------------------------------------------------------

-- | The main threads of the exprs being currently evaluated, the most recent
-- first. Evaluating an expr while stopped at a breakpoint pushes its thread on
-- top of the stopped one, until it completes.
--
-- FIXME: Remove this global variable. It is only needed while `resumeStmt` is
-- synchronous, since `resumeStmt` currently tries to return the Eval result of
-- the main thread (which is a bit non-sensical for a resume operation to do).
-- It is only needed to preserve the previous behavior, but as soon as
-- `evalStmt` returns the sandboxIO thread id we can wait on that explicitly
-- instead of resuming having anything to do with waiting for the main
-- evaluation result.
evaluationMainThreadGlobalVar :: TVar [ThreadId]
evaluationMainThreadGlobalVar = unsafePerformIO $ newTVarIO []
{-# NOINLINE evaluationMainThreadGlobalVar #-}

-- | FIXME: Kill this function, just like 'evaluationMainThreadGlobalVar' will be killed.
popMainThread :: ThreadId -> STM ()
popMainThread tid = modifyTVar evaluationMainThreadGlobalVar (filter (/= tid))

