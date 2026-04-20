{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -fprof-auto-top #-}

-------------------------------------------------------------------------------
--
-- | Aspects of GHC.Driver.Main dealing with Hsc[Env] and misc low level helpers.
--
-------------------------------------------------------------------------------

module GHC.Driver.Main.Hsc
    (
    -- * Making an HscEnv
      newHscEnv
    , newHscEnvWithHUG
    , initHscEnv

    -- * Utilities to modify/use Hsc[Env]
    , getDiagnostics
    , clearDiagnostics
    , logDiagnostics

    , add_iface_to_hpt
    , initModDetails
    , iface_core_bindings

    , handleWarnings
    , logWarningsReportErrors
    , handleWarningsThrowErrors
    , ioMsgMaybe
    , ioMsgMaybe'
    , dumpIfaceStats

        -- * Low-level exports for hooks(?)
    , hscSimpleIface
    , hscSimpleIface'
    , writeInterfaceOnlyMode
    , genModDetails

    ) where

import GHC.Prelude

import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Env
import GHC.Driver.Env.KnotVars
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Config.Logger   (initLogFlags)
import GHC.Driver.LlvmConfigCache  (initLlvmConfigCache)
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Hooks

import GHC.Runtime.Context

import GHC.IfaceToCore  ( typecheckIface )
import GHC.Iface.Load   ( ifaceStats )
import GHC.Iface.Make
import GHC.Iface.Tidy

import GHC.Core

import GHC.Parser.Errors.Types

import GHC.Tc.Utils.Monad

import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.Finder
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModSummary
import GHC.Unit.Home.ModInfo
import GHC.Unit.Home.PackageTable
import qualified GHC.Unit.Home.Graph as HUG

import GHC.Types.SourceError
import GHC.Types.SafeHaskell
import GHC.Types.Error
import GHC.Types.Name.Cache ( newNameCache )

import GHC.Utils.Panic
import GHC.Utils.Logger
import GHC.Utils.TmpFs

import GHC.SysTools (initSysTools)
import GHC.SysTools.BaseDir (findTopDir)

import Data.Functor ((<&>))
import Control.Monad
import Data.IORef
import GHC.Unit.Module.WholeCoreBindings
import System.IO



{- **********************************************************************
%*                                                                      *
                Initialisation
%*                                                                      *
%********************************************************************* -}

newHscEnv :: FilePath -> DynFlags -> IO HscEnv
newHscEnv top_dir dflags = do
  hpt <- emptyHomePackageTable
  newHscEnvWithHUG top_dir dflags (homeUnitId_ dflags) (home_unit_graph hpt)
  where
    home_unit_graph hpt = HUG.unitEnv_singleton
                        (homeUnitId_ dflags)
                        (HUG.mkHomeUnitEnv emptyUnitState Nothing dflags hpt Nothing)

newHscEnvWithHUG :: FilePath -> DynFlags -> UnitId -> HomeUnitGraph -> IO HscEnv
newHscEnvWithHUG top_dir top_dynflags cur_unit home_unit_graph = do
    nc_var  <- newNameCache
    fc_var  <- initFinderCache
    logger  <- initLogger
    tmpfs   <- initTmpFs
    let dflags = homeUnitEnv_dflags $ HUG.unitEnv_lookup cur_unit home_unit_graph
    unit_env <- initUnitEnv cur_unit home_unit_graph (ghcNameVersion dflags) (targetPlatform dflags)
    llvm_config <- initLlvmConfigCache top_dir
    return HscEnv { hsc_dflags         = top_dynflags
                  , hsc_logger         = setLogFlags logger (initLogFlags top_dynflags)
                  , hsc_targets        = []
                  , hsc_IC             = emptyInteractiveContext dflags
                  , hsc_NC             = nc_var
                  , hsc_FC             = fc_var
                  , hsc_type_env_vars  = emptyKnotVars
                  , hsc_interp         = Nothing
                  , hsc_unit_env       = unit_env
                  , hsc_plugins        = emptyPlugins
                  , hsc_hooks          = emptyHooks
                  , hsc_tmpfs          = tmpfs
                  , hsc_llvm_config    = llvm_config
                  }

-- | Initialize HscEnv from an optional top_dir path
initHscEnv :: Maybe FilePath -> IO HscEnv
initHscEnv mb_top_dir = do
  top_dir <- findTopDir mb_top_dir
  mySettings <- initSysTools top_dir
  dflags <- initDynFlags (defaultDynFlags mySettings)
  hsc_env <- newHscEnv top_dir dflags
  setUnsafeGlobalDynFlags dflags
   -- c.f. DynFlags.parseDynamicFlagsFull, which
   -- creates DynFlags and sets the UnsafeGlobalDynFlags
  return hsc_env


-- -----------------------------------------------------------------------------

getDiagnostics :: Hsc (Messages GhcMessage)
getDiagnostics = Hsc $ \_ w -> return (w, w)

clearDiagnostics :: Hsc ()
clearDiagnostics = Hsc $ \_ _ -> return ((), emptyMessages)

logDiagnostics :: Messages GhcMessage -> Hsc ()
logDiagnostics w = Hsc $ \_ w0 -> return ((), w0 `unionMessages` w)

handleWarnings :: Hsc ()
handleWarnings = do
    diag_opts <- initDiagOpts <$> getDynFlags
    print_config <- initPrintConfig <$> getDynFlags
    logger <- getLogger
    w <- getDiagnostics
    liftIO $ printOrThrowDiagnostics logger print_config diag_opts w
    clearDiagnostics

-- -----------------------------------------------------------------------------

add_iface_to_hpt :: ModIface -> ModDetails -> HscEnv -> IO ()
add_iface_to_hpt iface details =
  hscInsertHPT (HomeModInfo iface details emptyHomeModInfoLinkable)

-- Knot tying!  See Note [Knot-tying typecheckIface]
-- See Note [ModDetails and --make mode]
initModDetails :: HscEnv -> ModIface -> IO ModDetails
initModDetails hsc_env iface =
  fixIO $ \details' -> do
    add_iface_to_hpt iface details' hsc_env
    -- NB: This result is actually not that useful
    -- in one-shot mode, since we're not going to do
    -- any further typechecking.  It's much more useful
    -- in make mode, since this HMI will go into the HPT.
    genModDetails hsc_env iface


-- | Assemble 'WholeCoreBindings' if the interface contains Core bindings.
iface_core_bindings :: ModIface -> ModLocation -> Maybe WholeCoreBindings
iface_core_bindings iface wcb_mod_location =
  mi_simplified_core <&> \(IfaceSimplifiedCore bindings foreign') ->
    WholeCoreBindings {
      wcb_bindings = bindings,
      wcb_module = mi_module,
      wcb_mod_location,
      wcb_foreign = foreign'
    }
  where
    ModIface {mi_module, mi_simplified_core} = iface

-- | log warning in the monad, and if there are errors then
-- throw a SourceError exception.
logWarningsReportErrors :: (Messages PsWarning, Messages PsError) -> Hsc ()
logWarningsReportErrors (warnings,errors) = do
    sec <- getSourceErrorContext
    logDiagnostics (GhcPsMessage <$> warnings)
    when (not $ isEmptyMessages errors) $ throwErrors sec (GhcPsMessage <$> errors)

-- | Log warnings and throw errors, assuming the messages
-- contain at least one error (e.g. coming from PFailed)
handleWarningsThrowErrors :: (Messages PsWarning, Messages PsError) -> Hsc a
handleWarningsThrowErrors (warnings, errors) = do
    diag_opts <- initDiagOpts <$> getDynFlags
    logDiagnostics (GhcPsMessage <$> warnings)
    logger <- getLogger
    let (wWarns, wErrs) = partitionMessages warnings
    sec <- getSourceErrorContext
    liftIO $ printMessages logger NoDiagnosticOpts diag_opts wWarns
    throwErrors sec $ fmap GhcPsMessage $ errors `unionMessages` wErrs

-- | Deal with errors and warnings returned by a compilation step
--
-- In order to reduce dependencies to other parts of the compiler, functions
-- outside the "main" parts of GHC return warnings and errors as a parameter
-- and signal success via by wrapping the result in a 'Maybe' type. This
-- function logs the returned warnings and propagates errors as exceptions
-- (of type 'SourceError').
--
-- This function assumes the following invariants:
--
--  1. If the second result indicates success (is of the form 'Just x'),
--     there must be no error messages in the first result.
--
--  2. If there are no error messages, but the second result indicates failure
--     there should be warnings in the first result. That is, if the action
--     failed, it must have been due to the warnings (i.e., @-Werror@).
ioMsgMaybe :: IO (Messages GhcMessage, Maybe a) -> Hsc a
ioMsgMaybe ioA = do
    (msgs, mb_r) <- liftIO ioA
    let (warns, errs) = partitionMessages msgs
    sec <- getSourceErrorContext
    logDiagnostics warns
    case mb_r of
        Nothing -> throwErrors sec errs
        Just r  -> assert (isEmptyMessages errs ) return r

-- | like ioMsgMaybe, except that we ignore error messages and return
-- 'Nothing' instead.
ioMsgMaybe' :: IO (Messages GhcMessage, Maybe a) -> Hsc (Maybe a)
ioMsgMaybe' ioA = do
    (msgs, mb_r) <- liftIO $ ioA
    logDiagnostics (mkMessages $ getWarningMessages msgs)
    return mb_r

{- **********************************************************************
%*                                                                      *
        Statistics on reading interfaces
%*                                                                      *
%********************************************************************* -}

dumpIfaceStats :: HscEnv -> IO ()
dumpIfaceStats hsc_env = do
  eps <- hscEPS hsc_env
  let
    logger = hsc_logger hsc_env
    dump_rn_stats = logHasDumpFlag logger Opt_D_dump_rn_stats
    dump_if_trace = logHasDumpFlag logger Opt_D_dump_if_trace
  when (dump_if_trace || dump_rn_stats) $
    logDumpMsg logger "Interface statistics" (ifaceStats eps)



--------------------------------------------------------------
-- NoRecomp handlers
--------------------------------------------------------------


-- | genModDetails is used to initialise 'ModDetails' at the end of compilation.
-- This has two main effects:
-- 1. Increases memory usage by unloading a lot of the TypeEnv
-- 2. Globalising certain parts (DFunIds) in the TypeEnv (which used to be achieved using UpdateIdInfos)
-- For the second part to work, it's critical that we use 'initIfaceLoadModule' here rather than
-- 'initIfaceCheck' as 'initIfaceLoadModule' removes the module from the KnotVars, otherwise name lookups
-- succeed by hitting the old TypeEnv, which missing out the critical globalisation step for DFuns.

-- After the DFunIds are globalised, it's critical to overwrite the old TypeEnv with the new
-- more compact and more correct version. This reduces memory usage whilst compiling the rest of
-- the module loop.
genModDetails :: HscEnv -> ModIface -> IO ModDetails
genModDetails hsc_env old_iface
  = do
    -- CRITICAL: To use initIfaceLoadModule as that removes the current module from the KnotVars and
    -- hence properly globalises DFunIds.
    new_details <- {-# SCC "tcRnIface" #-}
                  initIfaceLoadModule hsc_env (mi_module old_iface) (typecheckIface old_iface)
    case lookupKnotVars (hsc_type_env_vars hsc_env) (mi_module old_iface) of
      Nothing -> return ()
      Just te_var -> writeIORef te_var (md_types new_details)
    dumpIfaceStats hsc_env
    return new_details


--------------------------------------------------------------
-- Interface generators
--------------------------------------------------------------

-- | Generate a stripped down interface file, e.g. for boot files or when ghci
-- generates interface files. See Note [hscSimpleIface - mkBootModDetailsTc]
hscSimpleIface :: HscEnv
               -> Maybe CoreProgram
               -> TcGblEnv
               -> ModSummary
               -> IO (ModIface, ModDetails)
hscSimpleIface hsc_env mb_core_program tc_result summary
    = runHsc hsc_env $ hscSimpleIface' mb_core_program tc_result summary

hscSimpleIface' :: Maybe CoreProgram
                -> TcGblEnv
                -> ModSummary
                -> Hsc (ModIface, ModDetails)
hscSimpleIface' mb_core_program tc_result summary = do
    hsc_env   <- getHscEnv
    logger    <- getLogger
    details   <- liftIO $ mkBootModDetailsTc logger tc_result
    safe_mode <- hscGetSafeMode tc_result
    new_iface
        <- {-# SCC "MkFinalIface" #-}
           liftIO $
               mkIfaceTc hsc_env safe_mode details summary mb_core_program tc_result
    -- And the answer is ...
    liftIO $ dumpIfaceStats hsc_env
    return (new_iface, details)

-- | Figure out the final correct safe haskell mode
hscGetSafeMode :: TcGblEnv -> Hsc SafeHaskellMode
hscGetSafeMode tcg_env = do
    dflags  <- getDynFlags
    liftIO $ finalSafeMode dflags tcg_env



{- **********************************************************************
%*                                                                      *
        Statistics on reading interfaces
%*                                                                      *
%********************************************************************* -}

writeInterfaceOnlyMode :: DynFlags -> Bool
writeInterfaceOnlyMode dflags =
 gopt Opt_WriteInterface dflags &&
 not (backendGeneratesCode (backend dflags))
