{-# LANGUAGE GADTs, RecordWildCards, MagicHash, ScopedTypeVariables, CPP,
    UnboxedTuples, LambdaCase, UnliftedFFITypes #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}

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

import qualified Data.Map as Map
import Control.Concurrent
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

-- | Process the Seq message to force a value.                       #2950
-- If during this processing a breakpoint is hit, return
-- an EvalBreak value in the EvalStatus to the UI process,
-- otherwise return an EvalComplete.
-- The UI process has more and therefore also can show more
-- information about the breakpoint than the current iserv
-- process.
doSeq :: RemoteRef a -> IO (EvalStatus ())
doSeq ref = clearEvalStatus <$> do
    sandboxIO evalOptsSeq $ do
      _ <- (void $ evaluate =<< localRef ref)
      return []

-- | Process a ResumeSeq message. Continue the :force processing     #2950
-- after a breakpoint.
resumeSeq :: RemoteRef ThreadId -> IO (EvalStatus ())
resumeSeq hvref = clearEvalStatus <$> do
    resumeThreadId <- localRef hvref
    withBreakAction evalOptsSeq (Just resumeThreadId) $ do
      ResumeContext{..} <- getThreadResumeContext resumeThreadId
      mask_ $ do
        putMVar resumeBreakMVar () -- this awakens the stopped thread...
        redirectInterrupts resumeThreadId $
          readThreadEvalStatus resumeThreadId

evalOptsSeq :: EvalOpts
evalOptsSeq = EvalOpts
              { useSandboxThread = True
              , singleStep = False
              , stepOut    = False
              , breakOnException = False
              , breakOnError = False
              }

-- When running a computation, we redirect ^C exceptions to the running
-- thread.  ToDo: we might want a way to continue even if the target
-- thread doesn't die when it receives the exception... "this thread
-- is not responding".
--
-- Careful here: there may be ^C exceptions flying around, so we start the new
-- thread blocked (forkIO inherits mask from the parent, #1048), and unblock
-- only while we execute the user's code.  We can't afford to lose the final
-- putMVar, otherwise deadlock ensues. (#1583, #1922, #1946)

sandboxIO :: EvalOpts -> IO [HValueRef] -> IO (EvalStatus [HValueRef])
sandboxIO opts io = do
  -- We are running in uninterruptibleMask
  withBreakAction opts Nothing $ do
    let runIt = measureAlloc $ tryEval $ rethrow opts $ clearCCS io
    if useSandboxThread opts
       then do
         tid <- forkIO $ do
           tid <- myThreadId
           labelThread tid "GHCi sandbox"
           unsafeUnmask runIt >>= writeThreadEvalStatus tid

         redirectInterrupts tid $ unsafeUnmask $
           readThreadEvalStatus tid
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
    if breakOnError && not breakOnException
       then poke exceptionFlag 1
       else case fromException se of
               -- If it is a "UserInterrupt" exception, we allow
               --  a possible break by way of -fbreak-on-exception
               Just UserInterrupt -> return ()
               -- In any other case, we don't want to break
               _ -> poke exceptionFlag 0
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

-- Exceptions can't be marshaled because they're dynamically typed, so
-- everything becomes a String.
tryEval :: IO a -> IO (EvalResult a)
tryEval io = do
  e <- try io
  case e of
    Left ex -> return (EvalException (toSerializableException ex))
    Right a -> return (EvalSuccess a)

-- This function sets up the interpreter for catching breakpoints, and
-- resets everything when the computation has stopped running.  This
-- is a not-very-good way to ensure that only the interactive
-- evaluation should generate breakpoints.
withBreakAction :: EvalOpts
                -> Maybe ThreadId -- ^ If resuming, the current threadId
                -> IO a -> IO a
withBreakAction opts mtid act
 = bracket setBreakAction resetBreakAction (\_ -> act)
 where
   setBreakAction = do
     poke breakPointIOAction globalBreakStablePtr -- TODO: This is thread unsafe, as one thread might be accessing this global variable while this thread tries to overwrite it. We should rather do it when the interpreter process/internal is initialized somehow.
     when (breakOnException opts) $ poke exceptionFlag 1
     when (singleStep opts) $ do
      rts_enableStopNextBreakpointAll
      case mtid of
        Nothing -> pure ()
        Just (ThreadId tid) -> do
          rts_enableStopNextBreakpoint tid
     when (stepOut opts) $ do
      case mtid of
        Nothing -> pure ()
        Just (ThreadId tid) -> do
          rts_enableStopAfterReturn tid
     return ()
        -- Breaking on exceptions is not enabled by default, since it
        -- might be a bit surprising.  The exception flag is turned off
        -- as soon as it is hit, or in resetBreakAction below.

   resetBreakAction {-stablePtr-}() = do
     -- poke breakPointIOAction noBreakStablePtr -- Never unset it.
     -- TODO: What about evaluating a thunk with breakpoints? when did we get "ignoring breakpoint..." before?
     poke exceptionFlag 0
     case mtid of
      Just (ThreadId tid) -> rts_disableStopAfterReturn tid
      _                   -> pure ()
     case mtid of
       Nothing -> rts_disableStopNextBreakpointAll
       Just (ThreadId tid) -> do
         rts_disableStopNextBreakpoint tid
     -- freeStablePtr stablePtr

resumeStmt
  :: EvalOpts -> RemoteRef ThreadId
  -> IO (EvalStatus [HValueRef])
resumeStmt opts rtid = do
  resumeThreadId <- localRef rtid
  ResumeContext{..} <- getThreadResumeContext resumeThreadId
  withBreakAction opts (Just resumeThreadId) $
    mask_ $ do
      putMVar resumeBreakMVar () -- this awakens the stopped thread...
      redirectInterrupts resumeThreadId $
        readThreadEvalStatus resumeThreadId

-- when abandoning a computation we have to
--      (a) kill the thread with an async exception, so that the
--          computation itself is stopped, and
--      (b) fill in the MVar.  This step is necessary because any
--          thunks that were under evaluation will now be updated
--          with the partial computation, which still ends in takeMVar,
--          so any attempt to evaluate one of these thunks will block
--          unless we fill in the MVar.
--      (c) wait for the thread to terminate by taking its status MVar.  This
--          step is necessary to prevent race conditions with
--          -fbreak-on-exception (see #5975).
--  See test break010.
abandonStmt :: RemoteRef ThreadId -> IO ()
abandonStmt hvref = do
  tid <- localRef hvref
  ResumeContext{..} <- getThreadResumeContext tid
  killThread tid
  putMVar resumeBreakMVar ()
  _ <- readThreadEvalStatus tid
  return ()

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

clearEvalStatus :: EvalStatus a -> EvalStatus ()
clearEvalStatus = \case
  EvalComplete w (EvalException se) -> EvalComplete w (EvalException se)
  EvalComplete w (EvalSuccess _)    -> EvalComplete w (EvalSuccess ())
  EvalBreak ap mb rt rccs           -> EvalBreak ap mb rt rccs

--------------------------------------------------------------------------------
-- Global Debugger Per-Thread Context
-- TODO: when to clean MVar? how to figure out when Threads are dead and we'll never need their mvars again? perhaps a finalizer on the thread id?
-- TODO: Move to independent module, and don't expose the global variables at all. move also the global break action maybe.
-- Maybe there could even be something in that module for setting the global
-- break action on start. maybe just an init function

globalBreakStablePtr :: StablePtr BreakpointCallback
globalBreakStablePtr = unsafePerformIO $ newStablePtr globalBreakAction

globalBreakAction :: BreakpointCallback
globalBreakAction info_mod# info_mod_uid# infox# is_exception apStack = do
  tid <- myThreadId
  ResumeContext{..} <- getThreadResumeContext tid
  resume_r <- mkRemoteRef tid
  apStack_r <- mkRemoteRef apStack
  ccs <- toRemotePtr <$> getCCSOf apStack
  breakpoint <-
    if is_exception
    then pure Nothing
    else do
      info_mod <- BS.packCString (Ptr info_mod#)
      info_mod_uid <- BS.packCString (Ptr info_mod_uid#)
      pure (Just (EvalBreakpoint info_mod info_mod_uid (I# infox#)))
  writeThreadEvalStatus tid $ EvalBreak apStack_r breakpoint resume_r ccs

  -- Block until this thread is resumed (by the thread which took the
  -- `ResumeContext` from the `statusMVar`).
  --
  -- The `onBreak` function must have been called from `rts/Interpreter.c`
  -- when interpreting a `BRK_FUN`. After taking from the MVar, the function
  -- returns to the continuation on the stack which is where the interpreter
  -- was stopped.
  takeMVar resumeBreakMVar

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

-- brilliant STM
-- * only retries when the map is written again
-- * essentially blocks until the result for this thread is there
readThreadEvalStatus :: ThreadId -> IO (EvalStatus [HValueRef])
readThreadEvalStatus tid = do
  atomically $ do
    dbgmap <- readTVar globalDbgStatusVar
    case Map.lookup tid dbgmap of
      Just r  -> do
        writeTVar globalDbgStatusVar $!
          Map.delete tid dbgmap -- clear it
        pure r
      Nothing -> retry -- until someone writes the result for this thread

writeThreadEvalStatus :: ThreadId -> EvalStatus [HValueRef] -> IO ()
writeThreadEvalStatus tid new = do
  atomically $ do
    dbgmap <- readTVar globalDbgStatusVar
    writeTVar globalDbgStatusVar $!
      Map.insertWith (\_ _ -> error "writeThreadEvalStatus: should be impossible") tid new dbgmap

-- | A global synchronization variable which a thread writes an 'EvalBreak' to
-- when it hits a breakpoint. This variable is shared across all threads, and
-- the debugger can read the threads which hit breakpoints by reading from this
-- variable. It will also be written with 'EvalSuccess' when a `sandboxIO`
-- action finishes (TODO: wait what? that is a bit weird. so what if we run two
-- evalStmt? they will both race to write a result? but then how do we tell
-- which result we got from where? this won't do in general, but let's see if
-- it can solve some problems first)
--
-- IDEA: maybe `sandboxIO` and friends receive as an argument the var on which
-- to expect the result specifically? And distinguish that from the global break var?
-- Then we split the EvalSuccess and EvalBreak into two datatypes.
--
-- Instead, let's just assume we may wait for a single thread's result and this
-- kind of falls out nicely. See my impl. notes.
globalDbgStatusVar :: TVar (Map.Map ThreadId (EvalStatus [HValueRef]))
globalDbgStatusVar = unsafePerformIO $ newTVarIO Map.empty
{-# NOINLINE globalDbgStatusVar #-}
