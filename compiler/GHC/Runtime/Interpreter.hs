{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Interacting with the iserv interpreter, whether it is running on an
-- external process or in the current process.
--
module GHC.Runtime.Interpreter
  ( module GHC.Runtime.Interpreter.Types

  -- * High-level interface to the interpreter
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
  , whereFrom
  , getModBreaks
  , readIModBreaks
  , readIModBreaksMaybe
  , readIModModBreaks
  , seqHValue
  , evalBreakpointToId

  -- * The object-code linker
  , initObjLinker
  , lookupSymbol
  , lookupSymbolInDLL
  , lookupClosure
  , loadDLL
  , loadArchive
  , loadObj
  , unloadObj
  , addLibrarySearchPath
  , removeLibrarySearchPath
  , resolveObjs
  , findSystemLibrary

  , interpCmd
  , withExtInterp
  , withExtInterpStatus
  , withIServ
  , withJSInterp
  , stopInterp
  , purgeLookupSymbolCache
  , freeReallyRemoteRef
  , freeHValueRefs
  , mkFinalizedHValue
  , wormhole, wormholeRef
  , fromEvalResult

  -- * Reexport for convenience
  , Message (..)
  , module GHC.Runtime.Interpreter.Process
  ) where

import GHC.Prelude

import GHC.Runtime.Interpreter.Types
import GHC.Runtime.Interpreter.JS
import GHC.Runtime.Interpreter.Wasm
import GHC.Runtime.Interpreter.Process
import GHC.Runtime.Utils
import GHCi.Message
import GHCi.RemoteTypes
import GHCi.ResolvedBCO
import GHCi.BreakArray (BreakArray)
import GHC.ByteCode.Breakpoints

import GHC.ByteCode.Types
import GHC.Linker.Types

import GHC.Data.Maybe
import GHC.Data.FastString

import GHC.Types.SrcLoc
import GHC.Types.Basic

import GHC.Utils.Panic
import GHC.Utils.Exception as Ex
import GHC.Utils.Outputable(brackets, ppr, showSDocUnsafe)
import GHC.Utils.Fingerprint

import GHC.Unit.Module
import GHC.Unit.Home.ModInfo
import GHC.Unit.Env

#if defined(HAVE_INTERNAL_INTERPRETER)
import GHCi.Run
#endif

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch as MC (mask)
import Data.Binary
import Data.ByteString (ByteString)
import Foreign hiding (void)
import qualified GHC.Exts.Heap as Heap
import GHC.Stack.CCS (CostCentre,CostCentreStack)
import System.Directory
import System.Process
import qualified GHC.InfoProv as InfoProv

import GHC.Builtin.Names
import GHC.Types.Name
import qualified GHC.Unit.Home.Graph as HUG

-- Standard libraries
import GHC.Exts

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

- top-level iserv directory, containing the code for the external
  server. This is a fairly simple wrapper, most of the functionality
  is provided by modules in libraries/ghci.

- This module which provides the interface to the server used
  by the rest of GHC.

GHC works with and without -fexternal-interpreter. With the flag, all
interpreted code is run by the iserv binary. Without the flag,
interpreted code is run in the same process as GHC.

Things that do not work with -fexternal-interpreter
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dynCompileExpr cannot work, because we have no way to run code of an
unknown type in the remote process. This API fails with an error
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
  ExternalInterp ext -> withExtInterp ext $ \inst ->
    uninterruptibleMask_ $ -- Note [uninterruptibleMask_ and interpCmd]
      sendMessage inst msg


withExtInterp :: ExceptionMonad m => ExtInterp -> (forall d. ExtInterpInstance d -> m a) -> m a
withExtInterp ext action = case ext of
  ExtJS    i -> withJSInterp i action
  ExtWasm  i -> withWasmInterp i action
  ExtIServ i -> withIServ    i action

withExtInterpStatus :: ExtInterp -> (forall d. ExtInterpStatusVar d -> m a) -> m a
withExtInterpStatus ext action = case ext of
  ExtJS    i -> action (interpStatus i)
  ExtWasm  i -> action $ interpStatus i
  ExtIServ i -> action (interpStatus i)

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
  => IServ -> (ExtInterpInstance () -> m a) -> m a
withIServ (ExtInterpState cfg mstate) action = do
  inst <- spawnInterpMaybe cfg spawnIServ mstate
  action inst

-- | Spawn JS interpreter if it isn't already running and execute the given action
--
-- Update the interpreter state.
withJSInterp :: ExceptionMonad m => JSInterp -> (ExtInterpInstance JSInterpExtra -> m a) -> m a
withJSInterp (ExtInterpState cfg mstate) action = do
  inst <- spawnInterpMaybe cfg spawnJSInterp mstate
  action inst

withWasmInterp :: ExceptionMonad m => WasmInterp -> (ExtInterpInstance () -> m a) -> m a
withWasmInterp (ExtInterpState cfg mstate) action = do
  inst <- spawnInterpMaybe cfg spawnWasmInterp mstate
  action inst

-- | Spawn an interpreter if not already running according to the status in the
-- MVar. Update the status, free pending heap references, and return the
-- interpreter instance.
--
-- This function is generic to support both the native external interpreter and
-- the JS one.
spawnInterpMaybe :: ExceptionMonad m => cfg -> (cfg -> IO (ExtInterpInstance d)) -> ExtInterpStatusVar d -> m (ExtInterpInstance d)
spawnInterpMaybe cfg spawn mstatus = do
  inst <- liftIO $ modifyMVarMasked mstatus $ \case
    -- start the external iserv process if we haven't done so yet
    InterpPending -> do
      inst <- spawn cfg
      pure (InterpRunning inst, inst)

    InterpRunning inst -> do
      pure (InterpRunning inst, inst)

  -- free any ForeignRef that have been garbage collected.
  pending_frees <- liftIO $ swapMVar (instPendingFrees inst) []
  liftIO $ when (not (null (pending_frees))) $
    sendMessage inst (FreeHValueRefs pending_frees)

  -- run the inner action
  pure inst

withExtInterpMaybe
  :: (ExceptionMonad m)
  => ExtInterp -> (forall d. Maybe (ExtInterpInstance d) -> m a) -> m a
withExtInterpMaybe ext action = withExtInterpStatus ext $ \mstate -> do
  liftIO (readMVar mstate) >>= \case
    InterpPending {}   -> action Nothing -- already shut down or never launched
    InterpRunning inst -> action (Just inst)

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
    EvalBreak a b c d -> return (EvalBreak a b c d)
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

-- | Create a set of BCOs that may be mutually recursive.
createBCOs :: Interp -> [ResolvedBCO] -> IO [HValueRef]
createBCOs interp rbcos = do
  interpCmd interp (CreateBCOs rbcos)

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

whereFrom :: Interp -> ForeignHValue -> IO (Maybe InfoProv.InfoProv)
whereFrom interp ref =
  withForeignRef ref $ \hval -> do
    interpCmd interp (WhereFrom hval)

-- | Send a Seq message to the iserv process to force a value      #2950
seqHValue :: Interp -> UnitEnv -> ForeignHValue -> IO (EvalResult ())
seqHValue interp unit_env ref =
  withForeignRef ref $ \hval -> do
    status <- interpCmd interp (Seq hval)
    handleSeqHValueStatus interp unit_env status

evalBreakpointToId :: EvalBreakpoint -> InternalBreakpointId
evalBreakpointToId eval_break =
  let
    mkUnitId u = fsToUnit $ mkFastStringShortByteString u
    toModule u n = mkModule (mkUnitId u) (mkModuleName n)
  in
    InternalBreakpointId
      { ibi_info_mod   = toModule (eb_info_mod_unit eval_break) (eb_info_mod eval_break)
      , ibi_info_index = eb_info_index eval_break
      }

-- | Process the result of a Seq or ResumeSeq message.             #2950
handleSeqHValueStatus :: Interp -> UnitEnv -> EvalStatus () -> IO (EvalResult ())
handleSeqHValueStatus interp unit_env eval_status =
  case eval_status of
    (EvalBreak _ maybe_break resume_ctxt _) -> do
      -- A breakpoint was hit; inform the user and tell them
      -- which breakpoint was hit.
      resume_ctxt_fhv <- liftIO $ mkFinalizedHValue interp resume_ctxt

      let put x = putStrLn ("*** Ignoring breakpoint " ++ (showSDocUnsafe x))
      let nothing_case = put $ brackets . ppr $ mkGeneralSrcSpan (fsLit "<unknown>")
      case maybe_break of
        Nothing -> nothing_case
          -- Nothing case - should not occur!
          -- Reason: Setting of flags in libraries/ghci/GHCi/Run.hs:evalOptsSeq

        Just break -> do
          let ibi = evalBreakpointToId break
              hug = ue_home_unit_graph unit_env

          -- Just case: Stopped at a breakpoint, extract SrcSpan information
          -- from the breakpoint.
          mb_modbreaks <- readIModBreaksMaybe hug (ibi_info_mod ibi)
          case mb_modbreaks of
            -- Nothing case - should not occur! We should have the appropriate
            -- breakpoint information
            Nothing -> nothing_case
            Just modbreaks -> put . brackets . ppr =<<
              getBreakLoc (readIModModBreaks hug) ibi modbreaks

      -- resume the seq (:force) processing in the iserv process
      withForeignRef resume_ctxt_fhv $ \hval -> do
        status <- interpCmd interp (ResumeSeq hval)
        handleSeqHValueStatus interp unit_env status
    (EvalComplete _ r) -> return r


-- -----------------------------------------------------------------------------
-- Interface to the object-code linker

initObjLinker :: Interp -> IO ()
initObjLinker interp = interpCmd interp InitLinker

lookupSymbol :: Interp -> InterpSymbol s -> IO (Maybe (Ptr ()))
lookupSymbol interp str = withSymbolCache interp str $
  case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
    InternalInterp -> fmap fromRemotePtr <$> run (LookupSymbol (unpackFS (interpSymbolToCLabel str)))
#endif
    ExternalInterp ext -> case ext of
      ExtIServ i -> withIServ i $ \inst -> fmap fromRemotePtr <$> do
        uninterruptibleMask_ $
          sendMessage inst (LookupSymbol (unpackFS (interpSymbolToCLabel str)))
      ExtJS {} -> pprPanic "lookupSymbol not supported by the JS interpreter" (ppr str)
      ExtWasm i -> withWasmInterp i $ \inst -> fmap fromRemotePtr <$> do
        uninterruptibleMask_ $
          sendMessage inst (LookupSymbol (unpackFS (interpSymbolToCLabel str)))

lookupSymbolInDLL :: Interp -> RemotePtr LoadedDLL -> InterpSymbol s -> IO (Maybe (Ptr ()))
lookupSymbolInDLL interp dll str = withSymbolCache interp str $
  case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
    InternalInterp -> fmap fromRemotePtr <$> run (LookupSymbolInDLL dll (unpackFS (interpSymbolToCLabel str)))
#endif
    ExternalInterp ext -> case ext of
      ExtIServ i -> withIServ i $ \inst -> fmap fromRemotePtr <$> do
        uninterruptibleMask_ $
          sendMessage inst (LookupSymbolInDLL dll (unpackFS (interpSymbolToCLabel str)))
      ExtJS {} -> pprPanic "lookupSymbol not supported by the JS interpreter" (ppr str)
      -- wasm dyld doesn't track which symbol comes from which .so
      ExtWasm {} -> lookupSymbol interp str

interpSymbolToCLabel :: forall s . InterpSymbol s -> FastString
interpSymbolToCLabel s = eliminateInterpSymbol s interpretedInterpSymbol $ \is ->
  let
    n = interpSymbolName is
    suffix = interpSymbolSuffix is

    encodeZ = fastZStringToByteString . zEncodeFS
    (Module pkgKey modName) = assert (isExternalName n) $ case nameModule n of
        -- Primops are exported from GHC.Prim, their HValues live in GHC.PrimopWrappers
        -- See Note [Primop wrappers] in GHC.Builtin.PrimOps.
        mod | mod == gHC_PRIM -> gHC_PRIMOPWRAPPERS
        mod -> mod
    packagePart = encodeZ (unitFS pkgKey)
    modulePart  = encodeZ (moduleNameFS modName)
    occPart     = encodeZ $ occNameMangledFS (nameOccName n)

    label = mconcat $
        [ packagePart `mappend` "_" | pkgKey /= mainUnit ]
        ++
        [modulePart
        , "_"
        , occPart
        , "_"
        , fromString suffix
        ]
  in mkFastStringByteString label

lookupClosure :: Interp -> InterpSymbol s -> IO (Maybe HValueRef)
lookupClosure interp str =
  interpCmd interp (LookupClosure (unpackFS (interpSymbolToCLabel str)))

-- | 'withSymbolCache' tries to find a symbol in the 'interpLookupSymbolCache'
-- which maps symbols to the address where they are loaded.
-- When there's a cache hit we simply return the cached address, when there is
-- a miss we run the action which determines the symbol's address and populate
-- the cache with the answer.
withSymbolCache :: Interp
                -> InterpSymbol s
                -- ^ The symbol we are looking up in the cache
                -> IO (Maybe (Ptr ()))
                -- ^ An action which determines the address of the symbol we
                -- are looking up in the cache, which is run if there is a
                -- cache miss. The result will be cached.
                -> IO (Maybe (Ptr ()))
withSymbolCache interp str determine_addr = do

  -- Profiling of GHCi showed a lot of time and allocation spent
  -- making cross-process LookupSymbol calls, so I added a GHC-side
  -- cache which sped things up quite a lot. We have to be careful
  -- to purge this cache when unloading code though.
  --
  -- The analysis in #23415 further showed this cache should also benefit the
  -- internal interpreter's loading times, and needn't be used by the external
  -- interpreter only.
  cached_val <- lookupInterpSymbolCache str (interpSymbolCache interp)
  case cached_val of
    Just {} -> return cached_val
    Nothing -> do
      maddr <- determine_addr
      case maddr of
        Nothing -> return Nothing
        Just p -> do
          updateInterpSymbolCache str (interpSymbolCache interp) p
          return maddr

purgeLookupSymbolCache :: Interp -> IO ()
purgeLookupSymbolCache interp = purgeInterpSymbolCache (interpSymbolCache interp)

-- | loadDLL loads a dynamic library using the OS's native linker
-- (i.e. dlopen() on Unix, LoadLibrary() on Windows).  It takes either
-- an absolute pathname to the file, or a relative filename
-- (e.g. "libfoo.so" or "foo.dll").  In the latter case, loadDLL
-- searches the standard locations for the appropriate library.
loadDLL :: Interp -> String -> IO (Either String (RemotePtr LoadedDLL))
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
-- IServ specific calls and messages

-- | Spawn an external interpreter
spawnIServ :: IServConfig -> IO (ExtInterpInstance ())
spawnIServ conf = do
  iservConfTrace conf
  let createProc = fromMaybe (\cp -> do { (_,_,_,ph) <- createProcess cp
                                        ; return ph })
                             (iservConfHook conf)
  (ph, rh, wh) <- runWithPipes createProc (iservConfProgram conf)
                                          []
                                          (iservConfOpts    conf)
  interpPipe <- mkPipeFromHandles rh wh
  lock <- newMVar ()
  let process = InterpProcess
                  { interpHandle = ph
                  , interpPipe
                  , interpLock   = lock
                  }

  pending_frees <- newMVar []
  let inst = ExtInterpInstance
        { instProcess           = process
        , instPendingFrees      = pending_frees
        , instExtra             = ()
        }
  pure inst

-- | Stop the interpreter
stopInterp :: Interp -> IO ()
stopInterp interp = case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
    InternalInterp -> pure ()
#endif
    ExternalInterp ext -> withExtInterpStatus ext $ \mstate -> do
      MC.mask $ \_restore -> modifyMVar_ mstate $ \state -> do
        case state of
          InterpPending    -> pure state -- already stopped
          InterpRunning i  -> do
            ex <- getProcessExitCode (interpHandle (instProcess i))
            if isJust ex
               then pure ()
               else sendMessage i Shutdown
            pure InterpPending

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

RemoteRef is a StablePtr to a heap-resident value.  When -fexternal-interpreter
or the JS interpreter is used, this value resides in the external process's
heap. RemoteRefs are mostly used to send pointers in messages between GHC and
iserv.

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
  case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
    InternalInterp     -> mkForeignRef rref (freeRemoteRef rref)
#endif
    ExternalInterp ext -> withExtInterpMaybe ext $ \case
      Nothing   -> mkForeignRef rref (pure ()) -- nothing to do, interpreter already stopped
      Just inst -> mkForeignRef rref (freeReallyRemoteRef inst rref)

freeReallyRemoteRef :: ExtInterpInstance d -> RemoteRef a -> IO ()
freeReallyRemoteRef inst rref =
  -- add to the list of HValues to free
  modifyMVar_ (instPendingFrees inst) (\xs -> pure (castRemoteRef rref : xs))


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

--------------------------------------------------------------------------------
-- * Finding breakpoint information
--------------------------------------------------------------------------------

-- | Get the breakpoint information from the ByteCode object associated to this
-- 'HomeModInfo'.
getModBreaks :: HomeModInfo -> Maybe InternalModBreaks
getModBreaks hmi
  | Just linkable <- homeModInfoByteCode hmi,
    -- The linkable may have 'DotO's as well; only consider BCOs. See #20570.
    [cbc] <- linkableBCOs linkable
  = bc_breaks cbc
  | otherwise
  = Nothing -- probably object code

-- | Read the 'InternalModBreaks' of the given home 'Module' (via
-- 'InternalBreakpointId') from the 'HomeUnitGraph'.
readIModBreaks :: HomeUnitGraph -> InternalBreakpointId -> IO InternalModBreaks
readIModBreaks hug ibi = expectJust <$> readIModBreaksMaybe hug (ibi_info_mod ibi)

-- | Read the 'InternalModBreaks' of the given home 'Module' from the 'HomeUnitGraph'.
readIModBreaksMaybe :: HomeUnitGraph -> Module -> IO (Maybe InternalModBreaks)
readIModBreaksMaybe hug mod = getModBreaks . expectJust <$> HUG.lookupHugByModule mod hug

-- | Read the 'ModBreaks' from the given module's 'InternalModBreaks'
readIModModBreaks :: HUG.HomeUnitGraph -> Module -> IO ModBreaks
readIModModBreaks hug mod = imodBreaks_modBreaks . expectJust <$> readIModBreaksMaybe hug mod

-- -----------------------------------------------------------------------------
-- Misc utils

fromEvalResult :: EvalResult a -> IO a
fromEvalResult (EvalException e) = throwIO (fromSerializableException e)
fromEvalResult (EvalSuccess a) = return a

