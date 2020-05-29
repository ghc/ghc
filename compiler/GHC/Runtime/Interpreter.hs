{-# LANGUAGE RecordWildCards, ScopedTypeVariables, BangPatterns, CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Interacting with the iserv interpreter, whether it is running on an
-- external process or in the current process.
--
module GHC.Runtime.Interpreter
  ( -- * High-level interface to the interpreter
    evalStmt, EvalStatus_(..), EvalStatus, EvalResult(..), EvalExpr(..)
  , resumeStmt
  , abandonStmt
  , evalIO
  , evalString
  , evalStringToIOString
  , mallocData
  , createBCOs
  , addSptEntry
  , mkCostCentres
  , costCentreStackInfo
  , newBreakArray
  , enableBreakpoint
  , breakpointStatus
  , getBreakpointVar
  , getClosure
  , getModBreaks
  , seqHValue
  , interpreterDynamic
  , interpreterProfiled

  -- * The object-code linker
  , initObjLinker
  , lookupSymbol
  , lookupClosure
  , loadDLL
  , loadArchive
  , loadObj
  , unloadObj
  , addLibrarySearchPath
  , removeLibrarySearchPath
  , resolveObjs
  , findSystemLibrary

  -- * Lower-level API using messages
  , iservCmd, Message(..), withIServ, withIServ_
  , withInterp, hscInterp, stopInterp
  , iservCall, readIServ, writeIServ
  , purgeLookupSymbolCache
  , freeHValueRefs
  , mkFinalizedHValue
  , wormhole, wormholeRef
  , mkEvalOpts
  , fromEvalResult
  ) where

import GHC.Prelude

import GHC.Runtime.Interpreter.Types
import GHCi.Message
import GHCi.RemoteTypes
import GHCi.ResolvedBCO
import GHCi.BreakArray (BreakArray)
import GHC.Utils.Fingerprint
import GHC.Driver.Types
import GHC.Types.Unique.FM
import GHC.Utils.Panic
import GHC.Driver.Session
import GHC.Utils.Exception as Ex
import GHC.Types.Basic
import GHC.Data.FastString
import GHC.Utils.Misc
import GHC.Runtime.Eval.Types(BreakInfo(..))
import GHC.Utils.Outputable(brackets, ppr, showSDocUnqual)
import GHC.Types.SrcLoc
import GHC.Data.Maybe
import GHC.Unit.Module
import GHC.ByteCode.Types
import GHC.Types.Unique

#if defined(HAVE_INTERNAL_INTERPRETER)
import GHCi.Run
import GHC.Driver.Ways
#endif

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch as MC (mask, onException)
import Data.Binary
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Array ((!))
import Data.IORef
import Foreign hiding (void)
import GHC.Exts.Heap
import GHC.Stack.CCS (CostCentre,CostCentreStack)
import System.Exit
import GHC.IO.Handle.Types (Handle)
#if defined(mingw32_HOST_OS)
import Foreign.C
import GHC.IO.Handle.FD (fdToHandle)
#else
import System.Posix as Posix
#endif
import System.Directory
import System.Process
import GHC.Conc (getNumProcessors, pseq, par)

{- Note [Remote GHCi]

When the flag -fexternal-interpreter is given to GHC, interpreted code
is run in a separate process called iserv, and we communicate with the
external process over a pipe using Binary-encoded messages.

Motivation
~~~~~~~~~~

When the interpreted code is running in a separate process, it can
use a different "way", e.g. profiled or dynamic.  This means

- compiling Template Haskell code with -prof does not require
  building the code without -prof first

- when GHC itself is profiled, it can interpret unprofiled code,
  and the same applies to dynamic linking.

- An unprofiled GHCi can load and run profiled code, which means it
  can use the stack-trace functionality provided by profiling without
  taking the performance hit on the compiler that profiling would
  entail.

For other reasons see remote-GHCi on the wiki.

Implementation Overview
~~~~~~~~~~~~~~~~~~~~~~~

The main pieces are:

- libraries/ghci, containing:
  - types for talking about remote values (GHCi.RemoteTypes)
  - the message protocol (GHCi.Message),
  - implementation of the messages (GHCi.Run)
  - implementation of Template Haskell (GHCi.TH)
  - a few other things needed to run interpreted code

- top-level iserv directory, containing the codefor the external
  server.  This is a fairly simple wrapper, most of the functionality
  is provided by modules in libraries/ghci.

- This module which provides the interface to the server used
  by the rest of GHC.

GHC works with and without -fexternal-interpreter.  With the flag, all
interpreted code is run by the iserv binary.  Without the flag,
interpreted code is run in the same process as GHC.

Things that do not work with -fexternal-interpreter
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dynCompileExpr cannot work, because we have no way to run code of an
unknown type in the remote process.  This API fails with an error
message if it is used with -fexternal-interpreter.

Other Notes on Remote GHCi
~~~~~~~~~~~~~~~~~~~~~~~~~~
  * This wiki page has an implementation overview:
    https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/external-interpreter
  * Note [External GHCi pointers] in "GHC.Runtime.Interpreter"
  * Note [Remote Template Haskell] in libraries/ghci/GHCi/TH.hs
-}


-- | Run a command in the interpreter's context.  With
-- @-fexternal-interpreter@, the command is serialized and sent to an
-- external iserv process, and the response is deserialized (hence the
-- @Binary@ constraint).  With @-fno-external-interpreter@ we execute
-- the command directly here.
iservCmd :: Binary a => HscEnv -> Message a -> IO a
iservCmd hsc_env msg = withInterp hsc_env $ \case
#if defined(HAVE_INTERNAL_INTERPRETER)
  InternalInterp     -> run msg -- Just run it directly
#endif
  (ExternalInterp c i) -> withIServ_ c i $ \iserv ->
    uninterruptibleMask_ $ do -- Note [uninterruptibleMask_]
      iservCall iserv msg


-- | Execute an action with the interpreter
--
-- Fails if no target code interpreter is available
withInterp :: HscEnv -> (Interp -> IO a) -> IO a
withInterp hsc_env action = action (hscInterp hsc_env)

-- | Retreive the targe code interpreter
--
-- Fails if no target code interpreter is available
hscInterp :: HscEnv -> Interp
hscInterp hsc_env = case hsc_interp hsc_env of
   Nothing -> throw (InstallationError "Couldn't find a target code interpreter. Try with -fexternal-interpreter")
   Just i  -> i

-- Note [uninterruptibleMask_ and iservCmd]
--
-- If we receive an async exception, such as ^C, while communicating
-- with the iserv process then we will be out-of-sync and not be able
-- to recover.  Thus we use uninterruptibleMask_ during
-- communication.  A ^C will be delivered to the iserv process (because
-- signals get sent to the whole process group) which will interrupt
-- the running computation and return an EvalException result.

-- | Grab a lock on the 'IServ' and do something with it.
-- Overloaded because this is used from TcM as well as IO.
withIServ
  :: (ExceptionMonad m)
  => IServConfig -> IServ -> (IServInstance -> m (IServInstance, a)) -> m a
withIServ conf (IServ mIServState) action = do
  MC.mask $ \restore -> do
    state <- liftIO $ takeMVar mIServState

    iserv <- case state of
      -- start the external iserv process if we haven't done so yet
      IServPending ->
         liftIO (spawnIServ conf)
           `MC.onException` (liftIO $ putMVar mIServState state)

      IServRunning inst -> return inst


    let iserv'  = iserv{ iservPendingFrees = [] }

    (iserv'',a) <- (do
      -- free any ForeignHValues that have been garbage collected.
      liftIO $ when (not (null (iservPendingFrees iserv))) $
        iservCall iserv (FreeHValueRefs (iservPendingFrees iserv))
      -- run the inner action
      restore $ action iserv')
          `MC.onException` (liftIO $ putMVar mIServState (IServRunning iserv'))
    liftIO $ putMVar mIServState (IServRunning iserv'')
    return a

withIServ_
  :: (MonadIO m, ExceptionMonad m)
  => IServConfig -> IServ -> (IServInstance -> m a) -> m a
withIServ_ conf iserv action = withIServ conf iserv $ \inst ->
   (inst,) <$> action inst

-- -----------------------------------------------------------------------------
-- Wrappers around messages

-- | Execute an action of type @IO [a]@, returning 'ForeignHValue's for
-- each of the results.
evalStmt
  :: HscEnv -> Bool -> EvalExpr ForeignHValue
  -> IO (EvalStatus_ [ForeignHValue] [HValueRef])
evalStmt hsc_env step foreign_expr = do
  let dflags = hsc_dflags hsc_env
  status <- withExpr foreign_expr $ \expr ->
    iservCmd hsc_env (EvalStmt (mkEvalOpts dflags step) expr)
  handleEvalStatus hsc_env status
 where
  withExpr :: EvalExpr ForeignHValue -> (EvalExpr HValueRef -> IO a) -> IO a
  withExpr (EvalThis fhv) cont =
    withForeignRef fhv $ \hvref -> cont (EvalThis hvref)
  withExpr (EvalApp fl fr) cont =
    withExpr fl $ \fl' ->
    withExpr fr $ \fr' ->
    cont (EvalApp fl' fr')

resumeStmt
  :: HscEnv -> Bool -> ForeignRef (ResumeContext [HValueRef])
  -> IO (EvalStatus_ [ForeignHValue] [HValueRef])
resumeStmt hsc_env step resume_ctxt = do
  let dflags = hsc_dflags hsc_env
  status <- withForeignRef resume_ctxt $ \rhv ->
    iservCmd hsc_env (ResumeStmt (mkEvalOpts dflags step) rhv)
  handleEvalStatus hsc_env status

abandonStmt :: HscEnv -> ForeignRef (ResumeContext [HValueRef]) -> IO ()
abandonStmt hsc_env resume_ctxt = do
  withForeignRef resume_ctxt $ \rhv ->
    iservCmd hsc_env (AbandonStmt rhv)

handleEvalStatus
  :: HscEnv -> EvalStatus [HValueRef]
  -> IO (EvalStatus_ [ForeignHValue] [HValueRef])
handleEvalStatus hsc_env status =
  case status of
    EvalBreak a b c d e f -> return (EvalBreak a b c d e f)
    EvalComplete alloc res ->
      EvalComplete alloc <$> addFinalizer res
 where
  addFinalizer (EvalException e) = return (EvalException e)
  addFinalizer (EvalSuccess rs) = do
    EvalSuccess <$> mapM (mkFinalizedHValue hsc_env) rs

-- | Execute an action of type @IO ()@
evalIO :: HscEnv -> ForeignHValue -> IO ()
evalIO hsc_env fhv = do
  liftIO $ withForeignRef fhv $ \fhv ->
    iservCmd hsc_env (EvalIO fhv) >>= fromEvalResult

-- | Execute an action of type @IO String@
evalString :: HscEnv -> ForeignHValue -> IO String
evalString hsc_env fhv = do
  liftIO $ withForeignRef fhv $ \fhv ->
    iservCmd hsc_env (EvalString fhv) >>= fromEvalResult

-- | Execute an action of type @String -> IO String@
evalStringToIOString :: HscEnv -> ForeignHValue -> String -> IO String
evalStringToIOString hsc_env fhv str = do
  liftIO $ withForeignRef fhv $ \fhv ->
    iservCmd hsc_env (EvalStringToString fhv str) >>= fromEvalResult


-- | Allocate and store the given bytes in memory, returning a pointer
-- to the memory in the remote process.
mallocData :: HscEnv -> ByteString -> IO (RemotePtr ())
mallocData hsc_env bs = iservCmd hsc_env (MallocData bs)

mkCostCentres
  :: HscEnv -> String -> [(String,String)] -> IO [RemotePtr CostCentre]
mkCostCentres hsc_env mod ccs =
  iservCmd hsc_env (MkCostCentres mod ccs)

-- | Create a set of BCOs that may be mutually recursive.
createBCOs :: HscEnv -> [ResolvedBCO] -> IO [HValueRef]
createBCOs hsc_env rbcos = do
  n_jobs <- case parMakeCount (hsc_dflags hsc_env) of
              Nothing -> liftIO getNumProcessors
              Just n  -> return n
  -- Serializing ResolvedBCO is expensive, so if we're in parallel mode
  -- (-j<n>) parallelise the serialization.
  if (n_jobs == 1)
    then
      iservCmd hsc_env (CreateBCOs [runPut (put rbcos)])

    else do
      old_caps <- getNumCapabilities
      if old_caps == n_jobs
         then void $ evaluate puts
         else bracket_ (setNumCapabilities n_jobs)
                       (setNumCapabilities old_caps)
                       (void $ evaluate puts)
      iservCmd hsc_env (CreateBCOs puts)
 where
  puts = parMap doChunk (chunkList 100 rbcos)

  -- make sure we force the whole lazy ByteString
  doChunk c = pseq (LB.length bs) bs
    where bs = runPut (put c)

  -- We don't have the parallel package, so roll our own simple parMap
  parMap _ [] = []
  parMap f (x:xs) = fx `par` (fxs `pseq` (fx : fxs))
    where fx = f x; fxs = parMap f xs

addSptEntry :: HscEnv -> Fingerprint -> ForeignHValue -> IO ()
addSptEntry hsc_env fpr ref =
  withForeignRef ref $ \val ->
    iservCmd hsc_env (AddSptEntry fpr val)

costCentreStackInfo :: HscEnv -> RemotePtr CostCentreStack -> IO [String]
costCentreStackInfo hsc_env ccs =
  iservCmd hsc_env (CostCentreStackInfo ccs)

newBreakArray :: HscEnv -> Int -> IO (ForeignRef BreakArray)
newBreakArray hsc_env size = do
  breakArray <- iservCmd hsc_env (NewBreakArray size)
  mkFinalizedHValue hsc_env breakArray

enableBreakpoint :: HscEnv -> ForeignRef BreakArray -> Int -> Bool -> IO ()
enableBreakpoint hsc_env ref ix b = do
  withForeignRef ref $ \breakarray ->
    iservCmd hsc_env (EnableBreakpoint breakarray ix b)

breakpointStatus :: HscEnv -> ForeignRef BreakArray -> Int -> IO Bool
breakpointStatus hsc_env ref ix = do
  withForeignRef ref $ \breakarray ->
    iservCmd hsc_env (BreakpointStatus breakarray ix)

getBreakpointVar :: HscEnv -> ForeignHValue -> Int -> IO (Maybe ForeignHValue)
getBreakpointVar hsc_env ref ix =
  withForeignRef ref $ \apStack -> do
    mb <- iservCmd hsc_env (GetBreakpointVar apStack ix)
    mapM (mkFinalizedHValue hsc_env) mb

getClosure :: HscEnv -> ForeignHValue -> IO (GenClosure ForeignHValue)
getClosure hsc_env ref =
  withForeignRef ref $ \hval -> do
    mb <- iservCmd hsc_env (GetClosure hval)
    mapM (mkFinalizedHValue hsc_env) mb

-- | Send a Seq message to the iserv process to force a value      #2950
seqHValue :: HscEnv -> ForeignHValue -> IO (EvalResult ())
seqHValue hsc_env ref =
  withForeignRef ref $ \hval ->
    iservCmd hsc_env (Seq hval) >>= handleSeqHValueStatus hsc_env

-- | Process the result of a Seq or ResumeSeq message.             #2950
handleSeqHValueStatus :: HscEnv -> EvalStatus () -> IO (EvalResult ())
handleSeqHValueStatus hsc_env eval_status = do
  case eval_status of
    (EvalBreak is_exception _ ix mod_uniq resume_ctxt _) -> do
      -- A breakpoint was hit, inform the user and tell him
      -- which breakpoint was hit.
      resume_ctxt_fhv <- liftIO $ mkFinalizedHValue hsc_env resume_ctxt
      let hmi = expectJust "handleRunStatus" $
                  lookupHptDirectly (hsc_HPT hsc_env)
                    (mkUniqueGrimily mod_uniq)
          modl = mi_module (hm_iface hmi)
          bp | is_exception = Nothing
             | otherwise = Just (BreakInfo modl ix)
          sdocBpLoc = brackets . ppr . getSeqBpSpan
      putStrLn ("*** Ignoring breakpoint " ++
            (showSDocUnqual (hsc_dflags hsc_env) $ sdocBpLoc bp))
      -- resume the seq (:force) processing in the iserv process
      withForeignRef resume_ctxt_fhv $ \hval ->
        iservCmd hsc_env (ResumeSeq hval) >>= handleSeqHValueStatus hsc_env
    (EvalComplete _ r) -> return r
  where
    getSeqBpSpan :: Maybe BreakInfo -> SrcSpan
    -- Just case: Stopped at a breakpoint, extract SrcSpan information
    -- from the breakpoint.
    getSeqBpSpan (Just BreakInfo{..}) =
      (modBreaks_locs (breaks breakInfo_module)) ! breakInfo_number
    -- Nothing case - should not occur!
    -- Reason: Setting of flags in libraries/ghci/GHCi/Run.hs:evalOptsSeq
    getSeqBpSpan Nothing = mkGeneralSrcSpan (fsLit "<unknown>")
    breaks mod = getModBreaks $ expectJust "getSeqBpSpan" $
      lookupHpt (hsc_HPT hsc_env) (moduleName mod)


-- -----------------------------------------------------------------------------
-- Interface to the object-code linker

initObjLinker :: HscEnv -> IO ()
initObjLinker hsc_env = iservCmd hsc_env InitLinker

lookupSymbol :: HscEnv -> FastString -> IO (Maybe (Ptr ()))
lookupSymbol hsc_env str = withInterp hsc_env $ \case
#if defined(HAVE_INTERNAL_INTERPRETER)
  InternalInterp -> fmap fromRemotePtr <$> run (LookupSymbol (unpackFS str))
#endif

  ExternalInterp c i -> withIServ c i $ \iserv -> do
    -- Profiling of GHCi showed a lot of time and allocation spent
    -- making cross-process LookupSymbol calls, so I added a GHC-side
    -- cache which sped things up quite a lot.  We have to be careful
    -- to purge this cache when unloading code though.
    let cache = iservLookupSymbolCache iserv
    case lookupUFM cache str of
      Just p -> return (iserv, Just p)
      Nothing -> do
        m <- uninterruptibleMask_ $
                 iservCall iserv (LookupSymbol (unpackFS str))
        case m of
          Nothing -> return (iserv, Nothing)
          Just r -> do
            let p      = fromRemotePtr r
                cache' = addToUFM cache str p
                iserv' = iserv {iservLookupSymbolCache = cache'}
            return (iserv', Just p)

lookupClosure :: HscEnv -> String -> IO (Maybe HValueRef)
lookupClosure hsc_env str =
  iservCmd hsc_env (LookupClosure str)

purgeLookupSymbolCache :: HscEnv -> IO ()
purgeLookupSymbolCache hsc_env = case hsc_interp hsc_env of
  Nothing             -> pure ()
#if defined(HAVE_INTERNAL_INTERPRETER)
  Just InternalInterp -> pure ()
#endif
  Just (ExternalInterp _ (IServ mstate)) ->
    modifyMVar_ mstate $ \state -> pure $ case state of
      IServPending       -> state
      IServRunning iserv -> IServRunning
        (iserv { iservLookupSymbolCache = emptyUFM })


-- | loadDLL loads a dynamic library using the OS's native linker
-- (i.e. dlopen() on Unix, LoadLibrary() on Windows).  It takes either
-- an absolute pathname to the file, or a relative filename
-- (e.g. "libfoo.so" or "foo.dll").  In the latter case, loadDLL
-- searches the standard locations for the appropriate library.
--
-- Returns:
--
-- Nothing      => success
-- Just err_msg => failure
loadDLL :: HscEnv -> String -> IO (Maybe String)
loadDLL hsc_env str = iservCmd hsc_env (LoadDLL str)

loadArchive :: HscEnv -> String -> IO ()
loadArchive hsc_env path = do
  path' <- canonicalizePath path -- Note [loadObj and relative paths]
  iservCmd hsc_env (LoadArchive path')

loadObj :: HscEnv -> String -> IO ()
loadObj hsc_env path = do
  path' <- canonicalizePath path -- Note [loadObj and relative paths]
  iservCmd hsc_env (LoadObj path')

unloadObj :: HscEnv -> String -> IO ()
unloadObj hsc_env path = do
  path' <- canonicalizePath path -- Note [loadObj and relative paths]
  iservCmd hsc_env (UnloadObj path')

-- Note [loadObj and relative paths]
-- the iserv process might have a different current directory from the
-- GHC process, so we must make paths absolute before sending them
-- over.

addLibrarySearchPath :: HscEnv -> String -> IO (Ptr ())
addLibrarySearchPath hsc_env str =
  fromRemotePtr <$> iservCmd hsc_env (AddLibrarySearchPath str)

removeLibrarySearchPath :: HscEnv -> Ptr () -> IO Bool
removeLibrarySearchPath hsc_env p =
  iservCmd hsc_env (RemoveLibrarySearchPath (toRemotePtr p))

resolveObjs :: HscEnv -> IO SuccessFlag
resolveObjs hsc_env = successIf <$> iservCmd hsc_env ResolveObjs

findSystemLibrary :: HscEnv -> String -> IO (Maybe String)
findSystemLibrary hsc_env str = iservCmd hsc_env (FindSystemLibrary str)


-- -----------------------------------------------------------------------------
-- Raw calls and messages

-- | Send a 'Message' and receive the response from the iserv process
iservCall :: Binary a => IServInstance -> Message a -> IO a
iservCall iserv msg =
  remoteCall (iservPipe iserv) msg
    `catch` \(e :: SomeException) -> handleIServFailure iserv e

-- | Read a value from the iserv process
readIServ :: IServInstance -> Get a -> IO a
readIServ iserv get =
  readPipe (iservPipe iserv) get
    `catch` \(e :: SomeException) -> handleIServFailure iserv e

-- | Send a value to the iserv process
writeIServ :: IServInstance -> Put -> IO ()
writeIServ iserv put =
  writePipe (iservPipe iserv) put
    `catch` \(e :: SomeException) -> handleIServFailure iserv e

handleIServFailure :: IServInstance -> SomeException -> IO a
handleIServFailure iserv e = do
  let proc = iservProcess iserv
  ex <- getProcessExitCode proc
  case ex of
    Just (ExitFailure n) ->
      throwIO (InstallationError ("ghc-iserv terminated (" ++ show n ++ ")"))
    _ -> do
      terminateProcess proc
      _ <- waitForProcess proc
      throw e

-- | Spawn an external interpreter
spawnIServ :: IServConfig -> IO IServInstance
spawnIServ conf = do
  iservConfTrace conf
  let createProc = fromMaybe (\cp -> do { (_,_,_,ph) <- createProcess cp
                                        ; return ph })
                             (iservConfHook conf)
  (ph, rh, wh) <- runWithPipes createProc (iservConfProgram conf)
                                          (iservConfOpts    conf)
  lo_ref <- newIORef Nothing
  return $ IServInstance
    { iservPipe              = Pipe { pipeRead = rh, pipeWrite = wh, pipeLeftovers = lo_ref }
    , iservProcess           = ph
    , iservLookupSymbolCache = emptyUFM
    , iservPendingFrees      = []
    }

-- | Stop the interpreter
stopInterp :: HscEnv -> IO ()
stopInterp hsc_env = case hsc_interp hsc_env of
  Nothing             -> pure ()
#if defined(HAVE_INTERNAL_INTERPRETER)
  Just InternalInterp -> pure ()
#endif
  Just (ExternalInterp _ (IServ mstate)) ->
    MC.mask $ \_restore -> modifyMVar_ mstate $ \state -> do
      case state of
        IServPending    -> pure state -- already stopped
        IServRunning i  -> do
          ex <- getProcessExitCode (iservProcess i)
          if isJust ex
             then pure ()
             else iservCall i Shutdown
          pure IServPending

runWithPipes :: (CreateProcess -> IO ProcessHandle)
             -> FilePath -> [String] -> IO (ProcessHandle, Handle, Handle)
#if defined(mingw32_HOST_OS)
foreign import ccall "io.h _close"
   c__close :: CInt -> IO CInt

foreign import ccall unsafe "io.h _get_osfhandle"
   _get_osfhandle :: CInt -> IO CInt

runWithPipes createProc prog opts = do
    (rfd1, wfd1) <- createPipeFd -- we read on rfd1
    (rfd2, wfd2) <- createPipeFd -- we write on wfd2
    wh_client    <- _get_osfhandle wfd1
    rh_client    <- _get_osfhandle rfd2
    let args = show wh_client : show rh_client : opts
    ph <- createProc (proc prog args)
    rh <- mkHandle rfd1
    wh <- mkHandle wfd2
    return (ph, rh, wh)
      where mkHandle :: CInt -> IO Handle
            mkHandle fd = (fdToHandle fd) `Ex.onException` (c__close fd)

#else
runWithPipes createProc prog opts = do
    (rfd1, wfd1) <- Posix.createPipe -- we read on rfd1
    (rfd2, wfd2) <- Posix.createPipe -- we write on wfd2
    setFdOption rfd1 CloseOnExec True
    setFdOption wfd2 CloseOnExec True
    let args = show wfd1 : show rfd2 : opts
    ph <- createProc (proc prog args)
    closeFd wfd1
    closeFd rfd2
    rh <- fdToHandle rfd1
    wh <- fdToHandle wfd2
    return (ph, rh, wh)
#endif

-- -----------------------------------------------------------------------------
{- Note [External GHCi pointers]

We have the following ways to reference things in GHCi:

HValue
------

HValue is a direct reference to a value in the local heap.  Obviously
we cannot use this to refer to things in the external process.


RemoteRef
---------

RemoteRef is a StablePtr to a heap-resident value.  When
-fexternal-interpreter is used, this value resides in the external
process's heap.  RemoteRefs are mostly used to send pointers in
messages between GHC and iserv.

A RemoteRef must be explicitly freed when no longer required, using
freeHValueRefs, or by attaching a finalizer with mkForeignHValue.

To get from a RemoteRef to an HValue you can use 'wormholeRef', which
fails with an error message if -fexternal-interpreter is in use.

ForeignRef
----------

A ForeignRef is a RemoteRef with a finalizer that will free the
'RemoteRef' when it is garbage collected.  We mostly use ForeignHValue
on the GHC side.

The finalizer adds the RemoteRef to the iservPendingFrees list in the
IServ record.  The next call to iservCmd will free any RemoteRefs in
the list.  It was done this way rather than calling iservCmd directly,
because I didn't want to have arbitrary threads calling iservCmd.  In
principle it would probably be ok, but it seems less hairy this way.
-}

-- | Creates a 'ForeignRef' that will automatically release the
-- 'RemoteRef' when it is no longer referenced.
mkFinalizedHValue :: HscEnv -> RemoteRef a -> IO (ForeignRef a)
mkFinalizedHValue hsc_env rref = do
   let hvref = toHValueRef rref

   free <- case hsc_interp hsc_env of
     Nothing                           -> return (pure ())
#if defined(HAVE_INTERNAL_INTERPRETER)
     Just InternalInterp               -> return (freeRemoteRef hvref)
#endif
     Just (ExternalInterp _ (IServ i)) -> return $ modifyMVar_ i $ \state ->
       case state of
         IServPending {}   -> pure state -- already shut down
         IServRunning inst -> do
            let !inst' = inst {iservPendingFrees = hvref:iservPendingFrees inst}
            pure (IServRunning inst')

   mkForeignRef rref free


freeHValueRefs :: HscEnv -> [HValueRef] -> IO ()
freeHValueRefs _ [] = return ()
freeHValueRefs hsc_env refs = iservCmd hsc_env (FreeHValueRefs refs)

-- | Convert a 'ForeignRef' to the value it references directly.  This
-- only works when the interpreter is running in the same process as
-- the compiler, so it fails when @-fexternal-interpreter@ is on.
wormhole :: Interp -> ForeignRef a -> IO a
wormhole interp r = wormholeRef interp (unsafeForeignRefToRemoteRef r)

-- | Convert an 'RemoteRef' to the value it references directly.  This
-- only works when the interpreter is running in the same process as
-- the compiler, so it fails when @-fexternal-interpreter@ is on.
wormholeRef :: Interp -> RemoteRef a -> IO a
#if defined(HAVE_INTERNAL_INTERPRETER)
wormholeRef InternalInterp      _r = localRef _r
#endif
wormholeRef (ExternalInterp {}) _r
  = throwIO (InstallationError
      "this operation requires -fno-external-interpreter")

-- -----------------------------------------------------------------------------
-- Misc utils

mkEvalOpts :: DynFlags -> Bool -> EvalOpts
mkEvalOpts dflags step =
  EvalOpts
    { useSandboxThread = gopt Opt_GhciSandbox dflags
    , singleStep = step
    , breakOnException = gopt Opt_BreakOnException dflags
    , breakOnError = gopt Opt_BreakOnError dflags }

fromEvalResult :: EvalResult a -> IO a
fromEvalResult (EvalException e) = throwIO (fromSerializableException e)
fromEvalResult (EvalSuccess a) = return a

getModBreaks :: HomeModInfo -> ModBreaks
getModBreaks hmi
  | Just linkable <- hm_linkable hmi,
    [BCOs cbc _] <- linkableUnlinked linkable
  = fromMaybe emptyModBreaks (bc_breaks cbc)
  | otherwise
  = emptyModBreaks -- probably object code

-- | Interpreter uses Profiling way
interpreterProfiled :: Interp -> Bool
#if defined(HAVE_INTERNAL_INTERPRETER)
interpreterProfiled InternalInterp       = hostIsProfiled
#endif
interpreterProfiled (ExternalInterp c _) = iservConfProfiled c

-- | Interpreter uses Dynamic way
interpreterDynamic :: Interp -> Bool
#if defined(HAVE_INTERNAL_INTERPRETER)
interpreterDynamic InternalInterp       = hostIsDynamic
#endif
interpreterDynamic (ExternalInterp c _) = iservConfDynamic c
