{-# LANGUAGE GADTs, RecordWildCards, MagicHash, ScopedTypeVariables, CPP,
    UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Execute GHCi messages
--
module GHCi.Run
  ( run, redirectInterrupts
  , toSerializableException, fromSerializableException
  ) where

import GHCi.CreateBCO
import GHCi.InfoTable
import GHCi.FFI
import GHCi.Message
import GHCi.ObjLink
import GHCi.RemoteTypes
import GHCi.TH
import GHCi.BreakArray

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as B
import GHC.Exts
import GHC.Stack
import Foreign
import Foreign.C
import GHC.Conc.Sync
import GHC.IO hiding ( bracket )
import System.Exit
import System.Mem.Weak  ( deRefWeak )
import Unsafe.Coerce

-- -----------------------------------------------------------------------------
-- Implement messages

run :: Message a -> IO a
run m = case m of
  InitLinker -> initObjLinker
  LookupSymbol str -> fmap toRemotePtr <$> lookupSymbol str
  LookupClosure str -> lookupClosure str
  LoadDLL str -> loadDLL str
  LoadArchive str -> loadArchive str
  LoadObj str -> loadObj str
  UnloadObj str -> unloadObj str
  AddLibrarySearchPath str -> toRemotePtr <$> addLibrarySearchPath str
  RemoveLibrarySearchPath ptr -> removeLibrarySearchPath (fromRemotePtr ptr)
  ResolveObjs -> resolveObjs
  FindSystemLibrary str -> findSystemLibrary str
  CreateBCOs bcos -> createBCOs (concatMap (runGet get) bcos)
  FreeHValueRefs rs -> mapM_ freeRemoteRef rs
  EvalStmt opts r -> evalStmt opts r
  ResumeStmt opts r -> resumeStmt opts r
  AbandonStmt r -> abandonStmt r
  EvalString r -> evalString r
  EvalStringToString r s -> evalStringToString r s
  EvalIO r -> evalIO r
  MkCostCentres mod ccs -> mkCostCentres mod ccs
  CostCentreStackInfo ptr -> ccsToStrings (fromRemotePtr ptr)
  NewBreakArray sz -> mkRemoteRef =<< newBreakArray sz
  EnableBreakpoint ref ix b -> do
    arr <- localRef ref
    _ <- if b then setBreakOn arr ix else setBreakOff arr ix
    return ()
  BreakpointStatus ref ix -> do
    arr <- localRef ref; r <- getBreak arr ix
    case r of
      Nothing -> return False
      Just w -> return (w /= 0)
  GetBreakpointVar ref ix -> do
    aps <- localRef ref
    mapM mkRemoteRef =<< getIdValFromApStack aps ix
  MallocData bs -> mkString bs
  MallocStrings bss -> mapM mkString0 bss
  PrepFFI conv args res -> toRemotePtr <$> prepForeignCall conv args res
  FreeFFI p -> freeForeignCallInfo (fromRemotePtr p)
  MkConInfoTable ptrs nptrs tag desc ->
    toRemotePtr <$> mkConInfoTable ptrs nptrs tag desc
  StartTH -> startTH
  _other -> error "GHCi.Run.run"

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

-- When running a computation, we redirect ^C exceptions to the running
-- thread.  ToDo: we might want a way to continue even if the target
-- thread doesn't die when it receives the exception... "this thread
-- is not responding".
--
-- Careful here: there may be ^C exceptions flying around, so we start the new
-- thread blocked (forkIO inherits mask from the parent, #1048), and unblock
-- only while we execute the user's code.  We can't afford to lose the final
-- putMVar, otherwise deadlock ensues. (#1583, #1922, #1946)

sandboxIO :: EvalOpts -> IO a -> IO (EvalStatus a)
sandboxIO opts io = do
  -- We are running in uninterruptibleMask
  breakMVar <- newEmptyMVar
  statusMVar <- newEmptyMVar
  withBreakAction opts breakMVar statusMVar $ do
    let runIt = measureAlloc $ tryEval $ rethrow opts $ clearCCS io
    if useSandboxThread opts
       then do
         tid <- forkIO $ do unsafeUnmask runIt >>= putMVar statusMVar
                                -- empty: can't block
         redirectInterrupts tid $ unsafeUnmask $ takeMVar statusMVar
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
  catch io $ \se -> do
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
    throwIO se

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
  setAllocationCounter maxBound
  a <- io
  ctr <- getAllocationCounter
  let allocs = fromIntegral (maxBound::Int64) - fromIntegral ctr
  return (EvalComplete allocs a)

-- Exceptions can't be marshaled because they're dynamically typed, so
-- everything becomes a String.
tryEval :: IO a -> IO (EvalResult a)
tryEval io = do
  e <- try io
  case e of
    Left ex -> return (EvalException (toSerializableException ex))
    Right a -> return (EvalSuccess a)

toSerializableException :: SomeException -> SerializableException
toSerializableException ex
  | Just UserInterrupt <- fromException ex  = EUserInterrupt
  | Just (ec::ExitCode) <- fromException ex = (EExitCode ec)
  | otherwise = EOtherException (show (ex :: SomeException))

fromSerializableException :: SerializableException -> SomeException
fromSerializableException EUserInterrupt = toException UserInterrupt
fromSerializableException (EExitCode c) = toException c
fromSerializableException (EOtherException str) = toException (ErrorCall str)

-- This function sets up the interpreter for catching breakpoints, and
-- resets everything when the computation has stopped running.  This
-- is a not-very-good way to ensure that only the interactive
-- evaluation should generate breakpoints.
withBreakAction :: EvalOpts -> MVar () -> MVar (EvalStatus b) -> IO a -> IO a
withBreakAction opts breakMVar statusMVar act
 = bracket setBreakAction resetBreakAction (\_ -> act)
 where
   setBreakAction = do
     stablePtr <- newStablePtr onBreak
     poke breakPointIOAction stablePtr
     when (breakOnException opts) $ poke exceptionFlag 1
     when (singleStep opts) $ setStepFlag
     return stablePtr
        -- Breaking on exceptions is not enabled by default, since it
        -- might be a bit surprising.  The exception flag is turned off
        -- as soon as it is hit, or in resetBreakAction below.

   onBreak :: BreakpointCallback
   onBreak ix# uniq# is_exception apStack = do
     tid <- myThreadId
     let resume = ResumeContext
           { resumeBreakMVar = breakMVar
           , resumeStatusMVar = statusMVar
           , resumeThreadId = tid }
     resume_r <- mkRemoteRef resume
     apStack_r <- mkRemoteRef apStack
     ccs <- toRemotePtr <$> getCCSOf apStack
     putMVar statusMVar $ EvalBreak is_exception apStack_r (I# ix#) (I# uniq#) resume_r ccs
     takeMVar breakMVar

   resetBreakAction stablePtr = do
     poke breakPointIOAction noBreakStablePtr
     poke exceptionFlag 0
     resetStepFlag
     freeStablePtr stablePtr

resumeStmt
  :: EvalOpts -> RemoteRef (ResumeContext [HValueRef])
  -> IO (EvalStatus [HValueRef])
resumeStmt opts hvref = do
  ResumeContext{..} <- localRef hvref
  withBreakAction opts resumeBreakMVar resumeStatusMVar $
    mask_ $ do
      putMVar resumeBreakMVar () -- this awakens the stopped thread...
      redirectInterrupts resumeThreadId $ takeMVar resumeStatusMVar

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
abandonStmt :: RemoteRef (ResumeContext [HValueRef]) -> IO ()
abandonStmt hvref = do
  ResumeContext{..} <- localRef hvref
  killThread resumeThreadId
  putMVar resumeBreakMVar ()
  _ <- takeMVar resumeStatusMVar
  return ()

foreign import ccall "&rts_stop_next_breakpoint" stepFlag      :: Ptr CInt
foreign import ccall "&rts_stop_on_exception"    exceptionFlag :: Ptr CInt

setStepFlag :: IO ()
setStepFlag = poke stepFlag 1
resetStepFlag :: IO ()
resetStepFlag = poke stepFlag 0

type BreakpointCallback = Int# -> Int# -> Bool -> HValue -> IO ()

foreign import ccall "&rts_breakpoint_io_action"
   breakPointIOAction :: Ptr (StablePtr BreakpointCallback)

noBreakStablePtr :: StablePtr BreakpointCallback
noBreakStablePtr = unsafePerformIO $ newStablePtr noBreakAction

noBreakAction :: BreakpointCallback
noBreakAction _ _ False _ = putStrLn "*** Ignoring breakpoint"
noBreakAction _ _ True  _ = return () -- exception: just continue

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

mkCostCentres :: String -> [(String,String)] -> IO [RemotePtr CostCentre]
#if defined(PROFILING)
mkCostCentres mod ccs = do
  c_module <- newCString mod
  mapM (mk_one c_module) ccs
 where
  mk_one c_module (decl_path,srcspan) = do
    c_name <- newCString decl_path
    c_srcspan <- newCString srcspan
    toRemotePtr <$> c_mkCostCentre c_name c_module c_srcspan

foreign import ccall unsafe "mkCostCentre"
  c_mkCostCentre :: Ptr CChar -> Ptr CChar -> Ptr CChar -> IO (Ptr CostCentre)
#else
mkCostCentres _ _ = return []
#endif

getIdValFromApStack :: HValue -> Int -> IO (Maybe HValue)
getIdValFromApStack apStack (I# stackDepth) = do
   case getApStackVal# apStack (stackDepth +# 1#) of
                                -- The +1 is magic!  I don't know where it comes
                                -- from, but this makes things line up.  --SDM
        (# ok, result #) ->
            case ok of
              0# -> return Nothing -- AP_STACK not found
              _  -> return (Just (unsafeCoerce# result))
