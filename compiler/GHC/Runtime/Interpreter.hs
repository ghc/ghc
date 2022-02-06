{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Interacting with the iserv interpreter, whether it is running on an
-- external process or in the current process.
--
module GHC.Runtime.Interpreter
  ( module GHC.Runtime.Interpreter.Types

  -- * High-level interface to the interpreter
  , BCOOpts (..)
  , evalStmt, EvalStatus_(..), EvalStatus, EvalResult(..), EvalExpr(..)
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
  , storeBreakpoint
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
  , interpCmd, Message(..), withIServ, withIServ_
  , stopInterp
  , iservCall, readIServ, writeIServ
  , purgeLookupSymbolCache
  , freeHValueRefs
  , mkFinalizedHValue
  , wormhole, wormholeRef
  , fromEvalResult
  ) where

import GHC.Prelude

import GHC.IO (catchException)

import GHC.Runtime.Interpreter.Types
import GHCi.Message
import GHCi.RemoteTypes
import GHCi.ResolvedBCO
import GHCi.BreakArray (BreakArray)
import GHC.Types.BreakInfo (BreakInfo(..))
import GHC.ByteCode.Types

import GHC.Linker.Types

import GHC.Data.Maybe
import GHC.Data.FastString

import GHC.Types.Unique
import GHC.Types.SrcLoc
import GHC.Types.Unique.FM
import GHC.Types.Basic

import GHC.Utils.Panic
import GHC.Utils.Exception as Ex
import GHC.Utils.Outputable(brackets, ppr, showSDocUnsafe)
import GHC.Utils.Fingerprint
import GHC.Utils.Misc

import GHC.Unit.Module
import GHC.Unit.Module.ModIface
import GHC.Unit.Home.ModInfo
import GHC.Unit.Env

#if defined(HAVE_INTERNAL_INTERPRETER)
import GHCi.Run
import GHC.Platform.Ways
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
import qualified GHC.Exts.Heap as Heap
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
import GHC.Conc (pseq, par)

{- Note [Remote GHCi]
   ~~~~~~~~~~~~~~~~~~
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
interpCmd :: Binary a => Interp -> Message a -> IO a
interpCmd interp msg = case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
  InternalInterp     -> run msg -- Just run it directly
#endif
  ExternalInterp c i -> withIServ_ c i $ \iserv ->
    uninterruptibleMask_ $ -- Note [uninterruptibleMask_]
      iservCall iserv msg


-- Note [uninterruptibleMask_ and interpCmd]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
withIServ conf (IServ mIServState) action =
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
  :: Interp
  -> EvalOpts
  -> EvalExpr ForeignHValue
  -> IO (EvalStatus_ [ForeignHValue] [HValueRef])
evalStmt interp opts foreign_expr = do
  status <- withExpr foreign_expr $ \expr ->
    interpCmd interp (EvalStmt opts expr)
  handleEvalStatus interp status
 where
  withExpr :: EvalExpr ForeignHValue -> (EvalExpr HValueRef -> IO a) -> IO a
  withExpr (EvalThis fhv) cont =
    withForeignRef fhv $ \hvref -> cont (EvalThis hvref)
  withExpr (EvalApp fl fr) cont =
    withExpr fl $ \fl' ->
    withExpr fr $ \fr' ->
    cont (EvalApp fl' fr')

resumeStmt
  :: Interp
  -> EvalOpts
  -> ForeignRef (ResumeContext [HValueRef])
  -> IO (EvalStatus_ [ForeignHValue] [HValueRef])
resumeStmt interp opts resume_ctxt = do
  status <- withForeignRef resume_ctxt $ \rhv ->
    interpCmd interp (ResumeStmt opts rhv)
  handleEvalStatus interp status

abandonStmt :: Interp -> ForeignRef (ResumeContext [HValueRef]) -> IO ()
abandonStmt interp resume_ctxt =
  withForeignRef resume_ctxt $ \rhv ->
    interpCmd interp (AbandonStmt rhv)

handleEvalStatus
  :: Interp
  -> EvalStatus [HValueRef]
  -> IO (EvalStatus_ [ForeignHValue] [HValueRef])
handleEvalStatus interp status =
  case status of
    EvalBreak a b c d e f -> return (EvalBreak a b c d e f)
    EvalComplete alloc res ->
      EvalComplete alloc <$> addFinalizer res
 where
  addFinalizer (EvalException e) = return (EvalException e)
  addFinalizer (EvalSuccess rs)  =
    EvalSuccess <$> mapM (mkFinalizedHValue interp) rs

-- | Execute an action of type @IO ()@
evalIO :: Interp -> ForeignHValue -> IO ()
evalIO interp fhv =
  liftIO $ withForeignRef fhv $ \fhv ->
    interpCmd interp (EvalIO fhv) >>= fromEvalResult

-- | Execute an action of type @IO String@
evalString :: Interp -> ForeignHValue -> IO String
evalString interp fhv =
  liftIO $ withForeignRef fhv $ \fhv ->
    interpCmd interp (EvalString fhv) >>= fromEvalResult

-- | Execute an action of type @String -> IO String@
evalStringToIOString :: Interp -> ForeignHValue -> String -> IO String
evalStringToIOString interp fhv str =
  liftIO $ withForeignRef fhv $ \fhv ->
    interpCmd interp (EvalStringToString fhv str) >>= fromEvalResult


-- | Allocate and store the given bytes in memory, returning a pointer
-- to the memory in the remote process.
mallocData :: Interp -> ByteString -> IO (RemotePtr ())
mallocData interp bs = interpCmd interp (MallocData bs)

mkCostCentres :: Interp -> String -> [(String,String)] -> IO [RemotePtr CostCentre]
mkCostCentres interp mod ccs =
  interpCmd interp (MkCostCentres mod ccs)

newtype BCOOpts = BCOOpts
  { bco_n_jobs :: Int -- ^ Number of parallel jobs doing BCO serialization
  }

-- | Create a set of BCOs that may be mutually recursive.
createBCOs :: Interp -> BCOOpts -> [ResolvedBCO] -> IO [HValueRef]
createBCOs interp opts rbcos = do
  let n_jobs = bco_n_jobs opts
  -- Serializing ResolvedBCO is expensive, so if we support doing it in parallel
  if (n_jobs == 1)
    then
      interpCmd interp (CreateBCOs [runPut (put rbcos)])
    else do
      old_caps <- getNumCapabilities
      if old_caps == n_jobs
         then void $ evaluate puts
         else bracket_ (setNumCapabilities n_jobs)
                       (setNumCapabilities old_caps)
                       (void $ evaluate puts)
      interpCmd interp (CreateBCOs puts)
 where
  puts = parMap doChunk (chunkList 100 rbcos)

  -- make sure we force the whole lazy ByteString
  doChunk c = pseq (LB.length bs) bs
    where bs = runPut (put c)

  -- We don't have the parallel package, so roll our own simple parMap
  parMap _ [] = []
  parMap f (x:xs) = fx `par` (fxs `pseq` (fx : fxs))
    where fx = f x; fxs = parMap f xs

addSptEntry :: Interp -> Fingerprint -> ForeignHValue -> IO ()
addSptEntry interp fpr ref =
  withForeignRef ref $ \val ->
    interpCmd interp (AddSptEntry fpr val)

costCentreStackInfo :: Interp -> RemotePtr CostCentreStack -> IO [String]
costCentreStackInfo interp ccs =
  interpCmd interp (CostCentreStackInfo ccs)

newBreakArray :: Interp -> Int -> IO (ForeignRef BreakArray)
newBreakArray interp size = do
  breakArray <- interpCmd interp (NewBreakArray size)
  mkFinalizedHValue interp breakArray

storeBreakpoint :: Interp -> ForeignRef BreakArray -> Int -> Int -> IO ()
storeBreakpoint interp ref ix cnt = do                               -- #19157
  withForeignRef ref $ \breakarray ->
    interpCmd interp (SetupBreakpoint breakarray ix cnt)

breakpointStatus :: Interp -> ForeignRef BreakArray -> Int -> IO Bool
breakpointStatus interp ref ix =
  withForeignRef ref $ \breakarray ->
    interpCmd interp (BreakpointStatus breakarray ix)

getBreakpointVar :: Interp -> ForeignHValue -> Int -> IO (Maybe ForeignHValue)
getBreakpointVar interp ref ix =
  withForeignRef ref $ \apStack -> do
    mb <- interpCmd interp (GetBreakpointVar apStack ix)
    mapM (mkFinalizedHValue interp) mb

getClosure :: Interp -> ForeignHValue -> IO (Heap.GenClosure ForeignHValue)
getClosure interp ref =
  withForeignRef ref $ \hval -> do
    mb <- interpCmd interp (GetClosure hval)
    mapM (mkFinalizedHValue interp) mb

-- | Send a Seq message to the iserv process to force a value      #2950
seqHValue :: Interp -> UnitEnv -> ForeignHValue -> IO (EvalResult ())
seqHValue interp unit_env ref =
  withForeignRef ref $ \hval -> do
    status <- interpCmd interp (Seq hval)
    handleSeqHValueStatus interp unit_env status

-- | Process the result of a Seq or ResumeSeq message.             #2950
handleSeqHValueStatus :: Interp -> UnitEnv -> EvalStatus () -> IO (EvalResult ())
handleSeqHValueStatus interp unit_env eval_status =
  case eval_status of
    (EvalBreak is_exception _ ix mod_uniq resume_ctxt _) -> do
      -- A breakpoint was hit; inform the user and tell them
      -- which breakpoint was hit.
      resume_ctxt_fhv <- liftIO $ mkFinalizedHValue interp resume_ctxt
      let hmi = expectJust "handleRunStatus" $
                  lookupHptDirectly (ue_hpt unit_env)
                    (mkUniqueGrimily mod_uniq)
          modl = mi_module (hm_iface hmi)
          bp | is_exception = Nothing
             | otherwise = Just (BreakInfo modl ix)
          sdocBpLoc = brackets . ppr . getSeqBpSpan
      putStrLn ("*** Ignoring breakpoint " ++
            (showSDocUnsafe $ sdocBpLoc bp))
      -- resume the seq (:force) processing in the iserv process
      withForeignRef resume_ctxt_fhv $ \hval -> do
        status <- interpCmd interp (ResumeSeq hval)
        handleSeqHValueStatus interp unit_env status
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
      lookupHpt (ue_hpt unit_env) (moduleName mod)


-- -----------------------------------------------------------------------------
-- Interface to the object-code linker

initObjLinker :: Interp -> IO ()
initObjLinker interp = interpCmd interp InitLinker

lookupSymbol :: Interp -> FastString -> IO (Maybe (Ptr ()))
lookupSymbol interp str = case interpInstance interp of
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

lookupClosure :: Interp -> String -> IO (Maybe HValueRef)
lookupClosure interp str =
  interpCmd interp (LookupClosure str)

purgeLookupSymbolCache :: Interp -> IO ()
purgeLookupSymbolCache interp = case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
  InternalInterp -> pure ()
#endif
  ExternalInterp _ (IServ mstate) ->
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
loadDLL :: Interp -> String -> IO (Maybe String)
loadDLL interp str = interpCmd interp (LoadDLL str)

loadArchive :: Interp -> String -> IO ()
loadArchive interp path = do
  path' <- canonicalizePath path -- Note [loadObj and relative paths]
  interpCmd interp (LoadArchive path')

loadObj :: Interp -> String -> IO ()
loadObj interp path = do
  path' <- canonicalizePath path -- Note [loadObj and relative paths]
  interpCmd interp (LoadObj path')

unloadObj :: Interp -> String -> IO ()
unloadObj interp path = do
  path' <- canonicalizePath path -- Note [loadObj and relative paths]
  interpCmd interp (UnloadObj path')

-- Note [loadObj and relative paths]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- the iserv process might have a different current directory from the
-- GHC process, so we must make paths absolute before sending them
-- over.

addLibrarySearchPath :: Interp -> String -> IO (Ptr ())
addLibrarySearchPath interp str =
  fromRemotePtr <$> interpCmd interp (AddLibrarySearchPath str)

removeLibrarySearchPath :: Interp -> Ptr () -> IO Bool
removeLibrarySearchPath interp p =
  interpCmd interp (RemoveLibrarySearchPath (toRemotePtr p))

resolveObjs :: Interp -> IO SuccessFlag
resolveObjs interp = successIf <$> interpCmd interp ResolveObjs

findSystemLibrary :: Interp -> String -> IO (Maybe String)
findSystemLibrary interp str = interpCmd interp (FindSystemLibrary str)


-- -----------------------------------------------------------------------------
-- Raw calls and messages

-- | Send a 'Message' and receive the response from the iserv process
iservCall :: Binary a => IServInstance -> Message a -> IO a
iservCall iserv msg =
  remoteCall (iservPipe iserv) msg
    `catchException` \(e :: SomeException) -> handleIServFailure iserv e

-- | Read a value from the iserv process
readIServ :: IServInstance -> Get a -> IO a
readIServ iserv get =
  readPipe (iservPipe iserv) get
    `catchException` \(e :: SomeException) -> handleIServFailure iserv e

-- | Send a value to the iserv process
writeIServ :: IServInstance -> Put -> IO ()
writeIServ iserv put =
  writePipe (iservPipe iserv) put
    `catchException` \(e :: SomeException) -> handleIServFailure iserv e

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
stopInterp :: Interp -> IO ()
stopInterp interp = case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
    InternalInterp -> pure ()
#endif
    ExternalInterp _ (IServ mstate) ->
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
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
IServ record.  The next call to interpCmd will free any RemoteRefs in
the list.  It was done this way rather than calling interpCmd directly,
because I didn't want to have arbitrary threads calling interpCmd.  In
principle it would probably be ok, but it seems less hairy this way.
-}

-- | Creates a 'ForeignRef' that will automatically release the
-- 'RemoteRef' when it is no longer referenced.
mkFinalizedHValue :: Interp -> RemoteRef a -> IO (ForeignRef a)
mkFinalizedHValue interp rref = do
   let hvref = toHValueRef rref

   free <- case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
      InternalInterp             -> return (freeRemoteRef hvref)
#endif
      ExternalInterp _ (IServ i) -> return $ modifyMVar_ i $ \state ->
       case state of
         IServPending {}   -> pure state -- already shut down
         IServRunning inst -> do
            let !inst' = inst {iservPendingFrees = hvref:iservPendingFrees inst}
            pure (IServRunning inst')

   mkForeignRef rref free


freeHValueRefs :: Interp -> [HValueRef] -> IO ()
freeHValueRefs _ [] = return ()
freeHValueRefs interp refs = interpCmd interp (FreeHValueRefs refs)

-- | Convert a 'ForeignRef' to the value it references directly.  This
-- only works when the interpreter is running in the same process as
-- the compiler, so it fails when @-fexternal-interpreter@ is on.
wormhole :: Interp -> ForeignRef a -> IO a
wormhole interp r = wormholeRef interp (unsafeForeignRefToRemoteRef r)

-- | Convert an 'RemoteRef' to the value it references directly.  This
-- only works when the interpreter is running in the same process as
-- the compiler, so it fails when @-fexternal-interpreter@ is on.
wormholeRef :: Interp -> RemoteRef a -> IO a
wormholeRef interp _r = case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
  InternalInterp -> localRef _r
#endif
  ExternalInterp {}
    -> throwIO (InstallationError "this operation requires -fno-external-interpreter")

-- -----------------------------------------------------------------------------
-- Misc utils

fromEvalResult :: EvalResult a -> IO a
fromEvalResult (EvalException e) = throwIO (fromSerializableException e)
fromEvalResult (EvalSuccess a) = return a

getModBreaks :: HomeModInfo -> ModBreaks
getModBreaks hmi
  | Just linkable <- hm_linkable hmi,
    [cbc] <- mapMaybe onlyBCOs $ linkableUnlinked linkable
  = fromMaybe emptyModBreaks (bc_breaks cbc)
  | otherwise
  = emptyModBreaks -- probably object code
  where
    -- The linkable may have 'DotO's as well; only consider BCOs. See #20570.
    onlyBCOs :: Unlinked -> Maybe CompiledByteCode
    onlyBCOs (BCOs cbc _) = Just cbc
    onlyBCOs _            = Nothing

-- | Interpreter uses Profiling way
interpreterProfiled :: Interp -> Bool
interpreterProfiled interp = case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
  InternalInterp     -> hostIsProfiled
#endif
  ExternalInterp c _ -> iservConfProfiled c

-- | Interpreter uses Dynamic way
interpreterDynamic :: Interp -> Bool
interpreterDynamic interp = case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
  InternalInterp     -> hostIsDynamic
#endif
  ExternalInterp c _ -> iservConfDynamic c
