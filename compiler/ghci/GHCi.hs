{-# LANGUAGE RecordWildCards, ScopedTypeVariables, BangPatterns, CPP #-}

--
-- | Interacting with the interpreter, whether it is running on an
-- external process or in the current process.
--
module GHCi
  ( -- * High-level interface to the interpreter
    evalStmt, EvalStatus_(..), EvalStatus, EvalResult(..), EvalExpr(..)
  , resumeStmt
  , abandonStmt
  , evalIO
  , evalString
  , evalStringToIOString
  , mallocData
  , createBCOs
  , mkCostCentres
  , costCentreStackInfo
  , newBreakArray
  , enableBreakpoint
  , breakpointStatus
  , getBreakpointVar

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
  , iservCmd, Message(..), withIServ, stopIServ
  , iservCall, readIServ, writeIServ
  , purgeLookupSymbolCache
  , freeHValueRefs
  , mkFinalizedHValue
  , wormhole, wormholeRef
  , mkEvalOpts
  , fromEvalResult
  ) where

import GHCi.Message
import GHCi.Run
import GHCi.RemoteTypes
import GHCi.ResolvedBCO
import GHCi.BreakArray (BreakArray)
import HscTypes
import UniqFM
import Panic
import DynFlags
import ErrUtils
import Outputable
import Exception
import BasicTypes
import FastString
import Util
import Hooks

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.IORef
import Foreign hiding (void)
import GHC.Stack.CCS (CostCentre,CostCentreStack)
import System.Exit
import Data.Maybe
import GHC.IO.Handle.Types (Handle)
#ifdef mingw32_HOST_OS
import Foreign.C
import GHC.IO.Handle.FD (fdToHandle)
#else
import System.Posix as Posix
#endif
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

For other reasons see RemoteGHCi on the wiki.

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

- This module (GHCi) which provides the interface to the server used
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
    https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/ExternalInterpreter
  * Note [External GHCi pointers] in compiler/ghci/GHCi.hs
  * Note [Remote Template Haskell] in libraries/ghci/GHCi/TH.hs
-}

-- | Run a command in the interpreter's context.  With
-- @-fexternal-interpreter@, the command is serialized and sent to an
-- external iserv process, and the response is deserialized (hence the
-- @Binary@ constraint).  With @-fno-external-interpreter@ we execute
-- the command directly here.
iservCmd :: Binary a => HscEnv -> Message a -> IO a
iservCmd hsc_env@HscEnv{..} msg
 | gopt Opt_ExternalInterpreter hsc_dflags =
     withIServ hsc_env $ \iserv ->
       uninterruptibleMask_ $ do -- Note [uninterruptibleMask_]
         iservCall iserv msg
 | otherwise = -- Just run it directly
   run msg


-- Note [uninterruptibleMask_ and iservCmd]
--
-- If we receive an async exception, such as ^C, while communicating
-- with the iserv process then we will be out-of-sync and not be able
-- to recoever.  Thus we use uninterruptibleMask_ during
-- communication.  A ^C will be delivered to the iserv process (because
-- signals get sent to the whole process group) which will interrupt
-- the running computation and return an EvalException result.

-- | Grab a lock on the 'IServ' and do something with it.
-- Overloaded because this is used from TcM as well as IO.
withIServ
  :: (MonadIO m, ExceptionMonad m)
  => HscEnv -> (IServ -> m a) -> m a
withIServ HscEnv{..} action =
  gmask $ \restore -> do
    m <- liftIO $ takeMVar hsc_iserv
      -- start the iserv process if we haven't done so yet
    iserv <- maybe (liftIO $ startIServ hsc_dflags) return m
               `gonException` (liftIO $ putMVar hsc_iserv Nothing)
      -- free any ForeignHValues that have been garbage collected.
    let iserv' = iserv{ iservPendingFrees = [] }
    a <- (do
      liftIO $ when (not (null (iservPendingFrees iserv))) $
        iservCall iserv (FreeHValueRefs (iservPendingFrees iserv))
        -- run the inner action
      restore $ action iserv)
          `gonException` (liftIO $ putMVar hsc_iserv (Just iserv'))
    liftIO $ putMVar hsc_iserv (Just iserv')
    return a


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

-- -----------------------------------------------------------------------------
-- Interface to the object-code linker

initObjLinker :: HscEnv -> IO ()
initObjLinker hsc_env = iservCmd hsc_env InitLinker

lookupSymbol :: HscEnv -> FastString -> IO (Maybe (Ptr ()))
lookupSymbol hsc_env@HscEnv{..} str
 | gopt Opt_ExternalInterpreter hsc_dflags =
     -- Profiling of GHCi showed a lot of time and allocation spent
     -- making cross-process LookupSymbol calls, so I added a GHC-side
     -- cache which sped things up quite a lot.  We have to be careful
     -- to purge this cache when unloading code though.
     withIServ hsc_env $ \iserv@IServ{..} -> do
       cache <- readIORef iservLookupSymbolCache
       case lookupUFM cache str of
         Just p -> return (Just p)
         Nothing -> do
           m <- uninterruptibleMask_ $
                    iservCall iserv (LookupSymbol (unpackFS str))
           case m of
             Nothing -> return Nothing
             Just r -> do
               let p = fromRemotePtr r
               writeIORef iservLookupSymbolCache $! addToUFM cache str p
               return (Just p)
 | otherwise =
   fmap fromRemotePtr <$> run (LookupSymbol (unpackFS str))

lookupClosure :: HscEnv -> String -> IO (Maybe HValueRef)
lookupClosure hsc_env str =
  iservCmd hsc_env (LookupClosure str)

purgeLookupSymbolCache :: HscEnv -> IO ()
purgeLookupSymbolCache hsc_env@HscEnv{..} =
 when (gopt Opt_ExternalInterpreter hsc_dflags) $
   withIServ hsc_env $ \IServ{..} ->
     writeIORef iservLookupSymbolCache emptyUFM


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
loadArchive hsc_env str = iservCmd hsc_env (LoadArchive str)

loadObj :: HscEnv -> String -> IO ()
loadObj hsc_env str = iservCmd hsc_env (LoadObj str)

unloadObj :: HscEnv -> String -> IO ()
unloadObj hsc_env str = iservCmd hsc_env (UnloadObj str)

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
iservCall :: Binary a => IServ -> Message a -> IO a
iservCall iserv@IServ{..} msg =
  remoteCall iservPipe msg
    `catch` \(e :: SomeException) -> handleIServFailure iserv e

-- | Read a value from the iserv process
readIServ :: IServ -> Get a -> IO a
readIServ iserv@IServ{..} get =
  readPipe iservPipe get
    `catch` \(e :: SomeException) -> handleIServFailure iserv e

-- | Send a value to the iserv process
writeIServ :: IServ -> Put -> IO ()
writeIServ iserv@IServ{..} put =
  writePipe iservPipe put
    `catch` \(e :: SomeException) -> handleIServFailure iserv e

handleIServFailure :: IServ -> SomeException -> IO a
handleIServFailure IServ{..} e = do
  ex <- getProcessExitCode iservProcess
  case ex of
    Just (ExitFailure n) ->
      throw (InstallationError ("ghc-iserv terminated (" ++ show n ++ ")"))
    _ -> do
      terminateProcess iservProcess
      _ <- waitForProcess iservProcess
      throw e

-- -----------------------------------------------------------------------------
-- Starting and stopping the iserv process

startIServ :: DynFlags -> IO IServ
startIServ dflags = do
  let flavour
        | WayProf `elem` ways dflags = "-prof"
        | WayDyn `elem` ways dflags = "-dyn"
        | otherwise = ""
      prog = pgm_i dflags ++ flavour
      opts = getOpts dflags opt_i
  debugTraceMsg dflags 3 $ text "Starting " <> text prog
  let createProc = lookupHook createIservProcessHook
                              (\cp -> do { (_,_,_,ph) <- createProcess cp
                                         ; return ph })
                              dflags
  (ph, rh, wh) <- runWithPipes createProc prog opts
  lo_ref <- newIORef Nothing
  cache_ref <- newIORef emptyUFM
  return $ IServ
    { iservPipe = Pipe { pipeRead = rh
                       , pipeWrite = wh
                       , pipeLeftovers = lo_ref }
    , iservProcess = ph
    , iservLookupSymbolCache = cache_ref
    , iservPendingFrees = []
    }

stopIServ :: HscEnv -> IO ()
stopIServ HscEnv{..} =
  gmask $ \_restore -> do
    m <- takeMVar hsc_iserv
    maybe (return ()) stop m
    putMVar hsc_iserv Nothing
 where
  stop iserv = do
    ex <- getProcessExitCode (iservProcess iserv)
    if isJust ex
       then return ()
       else iservCall iserv Shutdown

runWithPipes :: (CreateProcess -> IO ProcessHandle)
             -> FilePath -> [String] -> IO (ProcessHandle, Handle, Handle)
#ifdef mingw32_HOST_OS
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
            mkHandle fd = (fdToHandle fd) `onException` (c__close fd)
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

HValue is a direct reference to an value in the local heap.  Obviously
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
'RemoteRef' when it is gargabe collected.  We mostly use ForeignHValue
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
mkFinalizedHValue HscEnv{..} rref = mkForeignRef rref free
 where
  !external = gopt Opt_ExternalInterpreter hsc_dflags
  hvref = toHValueRef rref

  free :: IO ()
  free
    | not external = freeRemoteRef hvref
    | otherwise =
      modifyMVar_ hsc_iserv $ \mb_iserv ->
        case mb_iserv of
          Nothing -> return Nothing -- already shut down
          Just iserv@IServ{..} ->
            return (Just iserv{iservPendingFrees = hvref : iservPendingFrees})

freeHValueRefs :: HscEnv -> [HValueRef] -> IO ()
freeHValueRefs _ [] = return ()
freeHValueRefs hsc_env refs = iservCmd hsc_env (FreeHValueRefs refs)

-- | Convert a 'ForeignRef' to the value it references directly.  This
-- only works when the interpreter is running in the same process as
-- the compiler, so it fails when @-fexternal-interpreter@ is on.
wormhole :: DynFlags -> ForeignRef a -> IO a
wormhole dflags r = wormholeRef dflags (unsafeForeignRefToRemoteRef r)

-- | Convert an 'RemoteRef' to the value it references directly.  This
-- only works when the interpreter is running in the same process as
-- the compiler, so it fails when @-fexternal-interpreter@ is on.
wormholeRef :: DynFlags -> RemoteRef a -> IO a
wormholeRef dflags r
  | gopt Opt_ExternalInterpreter dflags
  = throwIO (InstallationError
      "this operation requires -fno-external-interpreter")
  | otherwise
  = localRef r

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
