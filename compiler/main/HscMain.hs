{-# LANGUAGE BangPatterns, CPP, MagicHash, NondecreasingIndentation #-}

-------------------------------------------------------------------------------
--
-- | Main API for compiling plain Haskell source code.
--
-- This module implements compilation of a Haskell source. It is
-- /not/ concerned with preprocessing of source files; this is handled
-- in "DriverPipeline".
--
-- There are various entry points depending on what mode we're in:
-- "batch" mode (@--make@), "one-shot" mode (@-c@, @-S@ etc.), and
-- "interactive" mode (GHCi). There are also entry points for
-- individual passes: parsing, typechecking/renaming, desugaring, and
-- simplification.
--
-- All the functions here take an 'HscEnv' as a parameter, but none of
-- them return a new one: 'HscEnv' is treated as an immutable value
-- from here on in (although it has mutable components, for the
-- caches).
--
-- Warning messages are dealt with consistently throughout this API:
-- during compilation warnings are collected, and before any function
-- in @HscMain@ returns, the warnings are either printed, or turned
-- into a real compialtion error if the @-Werror@ flag is enabled.
--
-- (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
--
-------------------------------------------------------------------------------

module HscMain
    (
    -- * Making an HscEnv
      newHscEnv

    -- * Compiling complete source files
    , Messager, batchMsg
    , HscStatus (..)
    , hscCompileOneShot
    , hscCompileCmmFile
    , hscCompileCore

    , genericHscCompileGetFrontendResult

    , genModDetails
    , hscSimpleIface
    , hscWriteIface
    , hscNormalIface
    , hscGenHardCode
    , hscInteractive

    -- * Running passes separately
    , hscParse
    , hscTypecheckRename
    , hscDesugar
    , makeSimpleIface
    , makeSimpleDetails
    , hscSimplify -- ToDo, shouldn't really export this

    -- * Support for interactive evaluation
    , hscParseIdentifier
    , hscTcRcLookupName
    , hscTcRnGetInfo
    , hscCheckSafe
    , hscGetSafe
#ifdef GHCI
    , hscIsGHCiMonad
    , hscGetModuleInterface
    , hscRnImportDecls
    , hscTcRnLookupRdrName
    , hscStmt, hscStmtWithLocation
    , hscDecls, hscDeclsWithLocation
    , hscTcExpr, hscImport, hscKcType
    , hscCompileCoreExpr
    -- * Low-level exports for hooks
    , hscCompileCoreExpr'
#endif
      -- We want to make sure that we export enough to be able to redefine
      -- hscFileFrontEnd in client code
    , hscParse', hscSimplify', hscDesugar', tcRnModule'
    , getHscEnv
    , hscSimpleIface', hscNormalIface'
    , oneShotMsg
    , hscFileFrontEnd, genericHscFrontend, dumpIfaceStats
    ) where

#ifdef GHCI
import Id
import BasicTypes       ( HValue )
import ByteCodeGen      ( byteCodeGen, coreExprToBCOs )
import Linker
import CoreTidy         ( tidyExpr )
import Type             ( Type )
import PrelNames
import {- Kind parts of -} Type         ( Kind )
import CoreMonad        ( lintInteractiveExpr )
import DsMeta           ( templateHaskellNames )
import VarEnv           ( emptyTidyEnv )
import Panic

import GHC.Exts
#endif

import Module
import Packages
import RdrName
import HsSyn
import CoreSyn
import StringBuffer
import Parser
import Lexer
import SrcLoc
import TcRnDriver
import TcIface          ( typecheckIface )
import TcRnMonad
import IfaceEnv         ( initNameCache )
import LoadIface        ( ifaceStats, initExternalPackageState )
import PrelInfo
import MkIface
import Desugar
import SimplCore
import TidyPgm
import CorePrep
import CoreToStg        ( coreToStg )
import qualified StgCmm ( codeGen )
import StgSyn
import CostCentre
import ProfInit
import TyCon
import Name
import SimplStg         ( stg2stg )
import Cmm
import CmmParse         ( parseCmmFile )
import CmmBuildInfoTables
import CmmPipeline
import CmmInfo
import CodeOutput
import NameEnv          ( emptyNameEnv )
import NameSet          ( emptyNameSet )
import InstEnv
import FamInstEnv
import Fingerprint      ( Fingerprint )
import Hooks

import DynFlags
import ErrUtils

import Outputable
import HscStats         ( ppSourceStats )
import HscTypes
import FastString
import UniqFM           ( emptyUFM )
import UniqSupply
import Bag
import Exception
import qualified Stream
import Stream (Stream)

import Util

import Data.List
import Control.Monad
import Data.Maybe
import Data.IORef
import System.FilePath as FilePath
import System.Directory

#include "HsVersions.h"


{- **********************************************************************
%*                                                                      *
                Initialisation
%*                                                                      *
%********************************************************************* -}

newHscEnv :: DynFlags -> IO HscEnv
newHscEnv dflags = do
    eps_var <- newIORef initExternalPackageState
    us      <- mkSplitUniqSupply 'r'
    nc_var  <- newIORef (initNameCache us knownKeyNames)
    fc_var  <- newIORef emptyUFM
    mlc_var <- newIORef emptyModuleEnv
    return HscEnv {  hsc_dflags       = dflags,
                     hsc_targets      = [],
                     hsc_mod_graph    = [],
                     hsc_IC           = emptyInteractiveContext dflags,
                     hsc_HPT          = emptyHomePackageTable,
                     hsc_EPS          = eps_var,
                     hsc_NC           = nc_var,
                     hsc_FC           = fc_var,
                     hsc_MLC          = mlc_var,
                     hsc_type_env_var = Nothing }


knownKeyNames :: [Name]      -- Put here to avoid loops involving DsMeta,
knownKeyNames =              -- where templateHaskellNames are defined
    map getName wiredInThings
        ++ basicKnownKeyNames
#ifdef GHCI
        ++ templateHaskellNames
#endif

-- -----------------------------------------------------------------------------

getWarnings :: Hsc WarningMessages
getWarnings = Hsc $ \_ w -> return (w, w)

clearWarnings :: Hsc ()
clearWarnings = Hsc $ \_ _ -> return ((), emptyBag)

logWarnings :: WarningMessages -> Hsc ()
logWarnings w = Hsc $ \_ w0 -> return ((), w0 `unionBags` w)

getHscEnv :: Hsc HscEnv
getHscEnv = Hsc $ \e w -> return (e, w)

handleWarnings :: Hsc ()
handleWarnings = do
    dflags <- getDynFlags
    w <- getWarnings
    liftIO $ printOrThrowWarnings dflags w
    clearWarnings

-- | log warning in the monad, and if there are errors then
-- throw a SourceError exception.
logWarningsReportErrors :: Messages -> Hsc ()
logWarningsReportErrors (warns,errs) = do
    logWarnings warns
    when (not $ isEmptyBag errs) $ throwErrors errs

-- | Throw some errors.
throwErrors :: ErrorMessages -> Hsc a
throwErrors = liftIO . throwIO . mkSrcErr

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
ioMsgMaybe :: IO (Messages, Maybe a) -> Hsc a
ioMsgMaybe ioA = do
    ((warns,errs), mb_r) <- liftIO ioA
    logWarnings warns
    case mb_r of
        Nothing -> throwErrors errs
        Just r  -> ASSERT( isEmptyBag errs ) return r

-- | like ioMsgMaybe, except that we ignore error messages and return
-- 'Nothing' instead.
ioMsgMaybe' :: IO (Messages, Maybe a) -> Hsc (Maybe a)
ioMsgMaybe' ioA = do
    ((warns,_errs), mb_r) <- liftIO $ ioA
    logWarnings warns
    return mb_r

-- -----------------------------------------------------------------------------
-- | Lookup things in the compiler's environment

#ifdef GHCI
hscTcRnLookupRdrName :: HscEnv -> RdrName -> IO [Name]
hscTcRnLookupRdrName hsc_env0 rdr_name = runInteractiveHsc hsc_env0 $ do
   hsc_env <- getHscEnv
   ioMsgMaybe $ tcRnLookupRdrName hsc_env rdr_name
#endif

hscTcRcLookupName :: HscEnv -> Name -> IO (Maybe TyThing)
hscTcRcLookupName hsc_env0 name = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe' $ tcRnLookupName hsc_env name
      -- ignore errors: the only error we're likely to get is
      -- "name not found", and the Maybe in the return type
      -- is used to indicate that.

hscTcRnGetInfo :: HscEnv -> Name -> IO (Maybe (TyThing, Fixity, [ClsInst], [FamInst]))
hscTcRnGetInfo hsc_env0 name
  = runInteractiveHsc hsc_env0 $
    do { hsc_env <- getHscEnv
       ; ioMsgMaybe' $ tcRnGetInfo hsc_env name }

#ifdef GHCI
hscIsGHCiMonad :: HscEnv -> String -> IO Name
hscIsGHCiMonad hsc_env name
  = runHsc hsc_env $ ioMsgMaybe $ isGHCiMonad hsc_env name

hscGetModuleInterface :: HscEnv -> Module -> IO ModIface
hscGetModuleInterface hsc_env0 mod = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe $ getModuleInterface hsc_env mod

-- -----------------------------------------------------------------------------
-- | Rename some import declarations
hscRnImportDecls :: HscEnv -> [LImportDecl RdrName] -> IO GlobalRdrEnv
hscRnImportDecls hsc_env0 import_decls = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe $ tcRnImportDecls hsc_env import_decls
#endif

-- -----------------------------------------------------------------------------
-- | parse a file, returning the abstract syntax

hscParse :: HscEnv -> ModSummary -> IO HsParsedModule
hscParse hsc_env mod_summary = runHsc hsc_env $ hscParse' mod_summary

-- internal version, that doesn't fail due to -Werror
hscParse' :: ModSummary -> Hsc HsParsedModule
hscParse' mod_summary = do
    dflags <- getDynFlags
    let src_filename  = ms_hspp_file mod_summary
        maybe_src_buf = ms_hspp_buf  mod_summary

    --------------------------  Parser  ----------------
    liftIO $ showPass dflags "Parser"
    {-# SCC "Parser" #-} do

    -- sometimes we already have the buffer in memory, perhaps
    -- because we needed to parse the imports out of it, or get the
    -- module name.
    buf <- case maybe_src_buf of
               Just b  -> return b
               Nothing -> liftIO $ hGetStringBuffer src_filename

    let loc = mkRealSrcLoc (mkFastString src_filename) 1 1

    case unP parseModule (mkPState dflags buf loc) of
        PFailed span err ->
            liftIO $ throwOneError (mkPlainErrMsg dflags span err)

        POk pst rdr_module -> do
            logWarningsReportErrors (getMessages pst)
            liftIO $ dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" $
                                   ppr rdr_module
            liftIO $ dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics" $
                                   ppSourceStats False rdr_module

            -- To get the list of extra source files, we take the list
            -- that the parser gave us,
            --   - eliminate files beginning with '<'.  gcc likes to use
            --     pseudo-filenames like "<built-in>" and "<command-line>"
            --   - normalise them (elimiante differences between ./f and f)
            --   - filter out the preprocessed source file
            --   - filter out anything beginning with tmpdir
            --   - remove duplicates
            --   - filter out the .hs/.lhs source filename if we have one
            --
            let n_hspp  = FilePath.normalise src_filename
                srcs0 = nub $ filter (not . (tmpDir dflags `isPrefixOf`))
                            $ filter (not . (== n_hspp))
                            $ map FilePath.normalise
                            $ filter (not . (isPrefixOf "<"))
                            $ map unpackFS
                            $ srcfiles pst
                srcs1 = case ml_hs_file (ms_location mod_summary) of
                          Just f  -> filter (/= FilePath.normalise f) srcs0
                          Nothing -> srcs0

            -- sometimes we see source files from earlier
            -- preprocessing stages that cannot be found, so just
            -- filter them out:
            srcs2 <- liftIO $ filterM doesFileExist srcs1

            return HsParsedModule {
                      hpm_module    = rdr_module,
                      hpm_src_files = srcs2
                   }

-- XXX: should this really be a Maybe X?  Check under which circumstances this
-- can become a Nothing and decide whether this should instead throw an
-- exception/signal an error.
type RenamedStuff =
        (Maybe (HsGroup Name, [LImportDecl Name], Maybe [LIE Name],
                Maybe LHsDocString))

-- | Rename and typecheck a module, additionally returning the renamed syntax
hscTypecheckRename :: HscEnv -> ModSummary -> HsParsedModule
                   -> IO (TcGblEnv, RenamedStuff)
hscTypecheckRename hsc_env mod_summary rdr_module = runHsc hsc_env $ do
    tc_result <- tcRnModule' hsc_env mod_summary True rdr_module

        -- This 'do' is in the Maybe monad!
    let rn_info = do decl <- tcg_rn_decls tc_result
                     let imports = tcg_rn_imports tc_result
                         exports = tcg_rn_exports tc_result
                         doc_hdr = tcg_doc_hdr tc_result
                     return (decl,imports,exports,doc_hdr)

    return (tc_result, rn_info)

-- wrapper around tcRnModule to handle safe haskell extras
tcRnModule' :: HscEnv -> ModSummary -> Bool -> HsParsedModule
            -> Hsc TcGblEnv
tcRnModule' hsc_env sum save_rn_syntax mod = do
    tcg_res <- {-# SCC "Typecheck-Rename" #-}
               ioMsgMaybe $
                   tcRnModule hsc_env (ms_hsc_src sum) save_rn_syntax mod

    tcSafeOK <- liftIO $ readIORef (tcg_safeInfer tcg_res)
    dflags   <- getDynFlags

    -- end of the Safe Haskell line, how to respond to user?
    if not (safeHaskellOn dflags) || (safeInferOn dflags && not tcSafeOK)
        -- if safe haskell off or safe infer failed, wipe trust
        then wipeTrust tcg_res emptyBag

        -- module safe, throw warning if needed
        else do
            tcg_res' <- hscCheckSafeImports tcg_res
            safe <- liftIO $ readIORef (tcg_safeInfer tcg_res')
            when (safe && wopt Opt_WarnSafe dflags)
                 (logWarnings $ unitBag $
                     mkPlainWarnMsg dflags (warnSafeOnLoc dflags) $ errSafe tcg_res')
            return tcg_res'
  where
    pprMod t  = ppr $ moduleName $ tcg_mod t
    errSafe t = quotes (pprMod t) <+> text "has been inferred as safe!"

-- | Convert a typechecked module to Core
hscDesugar :: HscEnv -> ModSummary -> TcGblEnv -> IO ModGuts
hscDesugar hsc_env mod_summary tc_result =
    runHsc hsc_env $ hscDesugar' (ms_location mod_summary) tc_result

hscDesugar' :: ModLocation -> TcGblEnv -> Hsc ModGuts
hscDesugar' mod_location tc_result = do
    hsc_env <- getHscEnv
    r <- ioMsgMaybe $
      {-# SCC "deSugar" #-}
      deSugar hsc_env mod_location tc_result

    -- always check -Werror after desugaring, this is the last opportunity for
    -- warnings to arise before the backend.
    handleWarnings
    return r

-- | Make a 'ModIface' from the results of typechecking. Used when
-- not optimising, and the interface doesn't need to contain any
-- unfoldings or other cross-module optimisation info.
-- ToDo: the old interface is only needed to get the version numbers,
-- we should use fingerprint versions instead.
makeSimpleIface :: HscEnv -> Maybe ModIface -> TcGblEnv -> ModDetails
                -> IO (ModIface,Bool)
makeSimpleIface hsc_env maybe_old_iface tc_result details = runHsc hsc_env $ do
    safe_mode <- hscGetSafeMode tc_result
    ioMsgMaybe $ do
        mkIfaceTc hsc_env (fmap mi_iface_hash maybe_old_iface) safe_mode
                  details tc_result

-- | Make a 'ModDetails' from the results of typechecking. Used when
-- typechecking only, as opposed to full compilation.
makeSimpleDetails :: HscEnv -> TcGblEnv -> IO ModDetails
makeSimpleDetails hsc_env tc_result = mkBootModDetailsTc hsc_env tc_result


{- **********************************************************************
%*                                                                      *
                The main compiler pipeline
%*                                                                      *
%********************************************************************* -}

{-
                   --------------------------------
                        The compilation proper
                   --------------------------------

It's the task of the compilation proper to compile Haskell, hs-boot and core
files to either byte-code, hard-code (C, asm, LLVM, ect) or to nothing at all
(the module is still parsed and type-checked. This feature is mostly used by
IDE's and the likes). Compilation can happen in either 'one-shot', 'batch',
'nothing', or 'interactive' mode. 'One-shot' mode targets hard-code, 'batch'
mode targets hard-code, 'nothing' mode targets nothing and 'interactive' mode
targets byte-code.

The modes are kept separate because of their different types and meanings:

 * In 'one-shot' mode, we're only compiling a single file and can therefore
 discard the new ModIface and ModDetails. This is also the reason it only
 targets hard-code; compiling to byte-code or nothing doesn't make sense when
 we discard the result.

 * 'Batch' mode is like 'one-shot' except that we keep the resulting ModIface
 and ModDetails. 'Batch' mode doesn't target byte-code since that require us to
 return the newly compiled byte-code.

 * 'Nothing' mode has exactly the same type as 'batch' mode but they're still
 kept separate. This is because compiling to nothing is fairly special: We
 don't output any interface files, we don't run the simplifier and we don't
 generate any code.

 * 'Interactive' mode is similar to 'batch' mode except that we return the
 compiled byte-code together with the ModIface and ModDetails.

Trying to compile a hs-boot file to byte-code will result in a run-time error.
This is the only thing that isn't caught by the type-system.
-}


type Messager = HscEnv -> (Int,Int) -> RecompileRequired -> ModSummary -> IO ()

genericHscCompileGetFrontendResult ::
                     Bool -- always do basic recompilation check?
                  -> Maybe TcGblEnv
                  -> Maybe Messager
                  -> HscEnv
                  -> ModSummary
                  -> SourceModified
                  -> Maybe ModIface  -- Old interface, if available
                  -> (Int,Int)       -- (i,n) = module i of n (for msgs)
                  -> IO (Either ModIface (TcGblEnv, Maybe Fingerprint))

genericHscCompileGetFrontendResult
  always_do_basic_recompilation_check m_tc_result
  mHscMessage hsc_env mod_summary source_modified mb_old_iface mod_index
    = do

    let msg what = case mHscMessage of
                   Just hscMessage -> hscMessage hsc_env mod_index what mod_summary
                   Nothing -> return ()

        skip iface = do
            msg UpToDate
            return $ Left iface

        compile mb_old_hash reason = do
            msg reason
            tc_result <- runHsc hsc_env $ genericHscFrontend mod_summary
            return $ Right (tc_result, mb_old_hash)

        stable = case source_modified of
                     SourceUnmodifiedAndStable -> True
                     _                         -> False

    case m_tc_result of
         Just tc_result
          | not always_do_basic_recompilation_check ->
             return $ Right (tc_result, Nothing)
         _ -> do
            (recomp_reqd, mb_checked_iface)
                <- {-# SCC "checkOldIface" #-}
                   checkOldIface hsc_env mod_summary
                                source_modified mb_old_iface
            -- save the interface that comes back from checkOldIface.
            -- In one-shot mode we don't have the old iface until this
            -- point, when checkOldIface reads it from the disk.
            let mb_old_hash = fmap mi_iface_hash mb_checked_iface

            case mb_checked_iface of
                Just iface | not (recompileRequired recomp_reqd) ->
                    -- If the module used TH splices when it was last
                    -- compiled, then the recompilation check is not
                    -- accurate enough (#481) and we must ignore
                    -- it.  However, if the module is stable (none of
                    -- the modules it depends on, directly or
                    -- indirectly, changed), then we *can* skip
                    -- recompilation. This is why the SourceModified
                    -- type contains SourceUnmodifiedAndStable, and
                    -- it's pretty important: otherwise ghc --make
                    -- would always recompile TH modules, even if
                    -- nothing at all has changed. Stability is just
                    -- the same check that make is doing for us in
                    -- one-shot mode.
                    case m_tc_result of
                    Nothing
                     | mi_used_th iface && not stable ->
                        compile mb_old_hash (RecompBecause "TH")
                    _ ->
                        skip iface
                _ ->
                    case m_tc_result of
                    Nothing -> compile mb_old_hash recomp_reqd
                    Just tc_result ->
                        return $ Right (tc_result, mb_old_hash)

genericHscFrontend :: ModSummary -> Hsc TcGblEnv
genericHscFrontend mod_summary =
  getHooked hscFrontendHook genericHscFrontend' >>= ($ mod_summary)

genericHscFrontend' :: ModSummary -> Hsc TcGblEnv
genericHscFrontend' mod_summary = hscFileFrontEnd mod_summary

--------------------------------------------------------------
-- Compilers
--------------------------------------------------------------

hscCompileOneShot :: HscEnv
                  -> ModSummary
                  -> SourceModified
                  -> IO HscStatus
hscCompileOneShot env =
  lookupHook hscCompileOneShotHook hscCompileOneShot' (hsc_dflags env) env

-- Compile Haskell/boot in OneShot mode.
hscCompileOneShot' :: HscEnv
                   -> ModSummary
                   -> SourceModified
                   -> IO HscStatus
hscCompileOneShot' hsc_env mod_summary src_changed
  = do
    -- One-shot mode needs a knot-tying mutable variable for interface
    -- files. See TcRnTypes.TcGblEnv.tcg_type_env_var.
    type_env_var <- newIORef emptyNameEnv
    let mod = ms_mod mod_summary
        hsc_env' = hsc_env{ hsc_type_env_var = Just (mod, type_env_var) }

        msg what = oneShotMsg hsc_env' what

        skip = do msg UpToDate
                  dumpIfaceStats hsc_env'
                  return HscUpToDate

        compile mb_old_hash reason = runHsc hsc_env' $ do
            liftIO $ msg reason
            tc_result <- genericHscFrontend mod_summary
            guts0 <- hscDesugar' (ms_location mod_summary) tc_result
            dflags <- getDynFlags
            case hscTarget dflags of
                HscNothing -> do
                    (iface, changed, _) <- hscSimpleIface' tc_result mb_old_hash
                    liftIO $ hscWriteIface dflags iface changed mod_summary
                    return HscNotGeneratingCode
                _ ->
                    case ms_hsc_src mod_summary of
                    HsBootFile ->
                        do (iface, changed, _) <- hscSimpleIface' tc_result mb_old_hash
                           liftIO $ hscWriteIface dflags iface changed mod_summary
                           return HscUpdateBoot
                    _ ->
                        do guts <- hscSimplify' guts0
                           (iface, changed, _details, cgguts) <- hscNormalIface' guts mb_old_hash
                           liftIO $ hscWriteIface dflags iface changed mod_summary
                           return $ HscRecomp cgguts mod_summary

        -- XXX This is always False, because in one-shot mode the
        -- concept of stability does not exist.  The driver never
        -- passes SourceUnmodifiedAndStable in here.
        stable = case src_changed of
                     SourceUnmodifiedAndStable -> True
                     _                         -> False

    (recomp_reqd, mb_checked_iface)
        <- {-# SCC "checkOldIface" #-}
           checkOldIface hsc_env' mod_summary src_changed Nothing
    -- save the interface that comes back from checkOldIface.
    -- In one-shot mode we don't have the old iface until this
    -- point, when checkOldIface reads it from the disk.
    let mb_old_hash = fmap mi_iface_hash mb_checked_iface

    case mb_checked_iface of
        Just iface | not (recompileRequired recomp_reqd) ->
            -- If the module used TH splices when it was last compiled,
            -- then the recompilation check is not accurate enough (#481)
            -- and we must ignore it. However, if the module is stable
            -- (none of the modules it depends on, directly or indirectly,
            -- changed), then we *can* skip recompilation. This is why
            -- the SourceModified type contains SourceUnmodifiedAndStable,
            -- and it's pretty important: otherwise ghc --make would
            -- always recompile TH modules, even if nothing at all has
            -- changed. Stability is just the same check that make is
            -- doing for us in one-shot mode.
            if mi_used_th iface && not stable
            then compile mb_old_hash (RecompBecause "TH")
            else skip
        _ ->
            compile mb_old_hash recomp_reqd

--------------------------------------------------------------
-- NoRecomp handlers
--------------------------------------------------------------

genModDetails :: HscEnv -> ModIface -> IO ModDetails
genModDetails hsc_env old_iface
  = do
    new_details <- {-# SCC "tcRnIface" #-}
                   initIfaceCheck hsc_env (typecheckIface old_iface)
    dumpIfaceStats hsc_env
    return new_details

--------------------------------------------------------------
-- Progress displayers.
--------------------------------------------------------------

oneShotMsg :: HscEnv -> RecompileRequired -> IO ()
oneShotMsg hsc_env recomp =
    case recomp of
        UpToDate ->
            compilationProgressMsg (hsc_dflags hsc_env) $
                   "compilation IS NOT required"
        _ ->
            return ()

batchMsg :: Messager
batchMsg hsc_env mod_index recomp mod_summary =
    case recomp of
        MustCompile -> showMsg "Compiling " ""
        UpToDate
            | verbosity (hsc_dflags hsc_env) >= 2 -> showMsg "Skipping  " ""
            | otherwise -> return ()
        RecompBecause reason -> showMsg "Compiling " (" [" ++ reason ++ "]")
    where
        dflags = hsc_dflags hsc_env
        showMsg msg reason =
            compilationProgressMsg dflags $
            (showModuleIndex mod_index ++
            msg ++ showModMsg dflags (hscTarget dflags)
                              (recompileRequired recomp) mod_summary)
                ++ reason

--------------------------------------------------------------
-- FrontEnds
--------------------------------------------------------------

hscFileFrontEnd :: ModSummary -> Hsc TcGblEnv
hscFileFrontEnd mod_summary = do
    hpm <- hscParse' mod_summary
    hsc_env <- getHscEnv
    tcg_env <- tcRnModule' hsc_env mod_summary False hpm
    return tcg_env

--------------------------------------------------------------
-- Safe Haskell
--------------------------------------------------------------

-- Note [Safe Haskell Trust Check]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Safe Haskell checks that an import is trusted according to the following
-- rules for an import of module M that resides in Package P:
--
--   * If M is recorded as Safe and all its trust dependencies are OK
--     then M is considered safe.
--   * If M is recorded as Trustworthy and P is considered trusted and
--     all M's trust dependencies are OK then M is considered safe.
--
-- By trust dependencies we mean that the check is transitive. So if
-- a module M that is Safe relies on a module N that is trustworthy,
-- importing module M will first check (according to the second case)
-- that N is trusted before checking M is trusted.
--
-- This is a minimal description, so please refer to the user guide
-- for more details. The user guide is also considered the authoritative
-- source in this matter, not the comments or code.


-- Note [Safe Haskell Inference]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Safe Haskell does Safe inference on modules that don't have any specific
-- safe haskell mode flag. The basic aproach to this is:
--   * When deciding if we need to do a Safe language check, treat
--     an unmarked module as having -XSafe mode specified.
--   * For checks, don't throw errors but return them to the caller.
--   * Caller checks if there are errors:
--     * For modules explicitly marked -XSafe, we throw the errors.
--     * For unmarked modules (inference mode), we drop the errors
--       and mark the module as being Unsafe.


-- | Check that the safe imports of the module being compiled are valid.
-- If not we either issue a compilation error if the module is explicitly
-- using Safe Haskell, or mark the module as unsafe if we're in safe
-- inference mode.
hscCheckSafeImports :: TcGblEnv -> Hsc TcGblEnv
hscCheckSafeImports tcg_env = do
    dflags   <- getDynFlags
    tcg_env' <- checkSafeImports dflags tcg_env
    case safeLanguageOn dflags of
        True -> do
            -- we nuke user written RULES in -XSafe
            logWarnings $ warns dflags (tcg_rules tcg_env')
            return tcg_env' { tcg_rules = [] }
        False
              -- user defined RULES, so not safe or already unsafe
            | safeInferOn dflags && not (null $ tcg_rules tcg_env') ||
              safeHaskell dflags == Sf_None
            -> wipeTrust tcg_env' $ warns dflags (tcg_rules tcg_env')

              -- trustworthy OR safe inferred with no RULES
            | otherwise
            -> return tcg_env'

  where
    warns dflags rules = listToBag $ map (warnRules dflags) rules
    warnRules dflags (L loc (HsRule n _ _ _ _ _ _)) =
        mkPlainWarnMsg dflags loc $
            text "Rule \"" <> ftext n <> text "\" ignored" $+$
            text "User defined rules are disabled under Safe Haskell"

-- | Validate that safe imported modules are actually safe.  For modules in the
-- HomePackage (the package the module we are compiling in resides) this just
-- involves checking its trust type is 'Safe' or 'Trustworthy'. For modules
-- that reside in another package we also must check that the external pacakge
-- is trusted. See the Note [Safe Haskell Trust Check] above for more
-- information.
--
-- The code for this is quite tricky as the whole algorithm is done in a few
-- distinct phases in different parts of the code base. See
-- RnNames.rnImportDecl for where package trust dependencies for a module are
-- collected and unioned.  Specifically see the Note [RnNames . Tracking Trust
-- Transitively] and the Note [RnNames . Trust Own Package].
checkSafeImports :: DynFlags -> TcGblEnv -> Hsc TcGblEnv
checkSafeImports dflags tcg_env
    = do
        -- We want to use the warning state specifically for detecting if safe
        -- inference has failed, so store and clear any existing warnings.
        oldErrs <- getWarnings
        clearWarnings

        imps <- mapM condense imports'
        pkgs <- mapM checkSafe imps

        -- grab any safe haskell specific errors and restore old warnings
        errs <- getWarnings
        clearWarnings
        logWarnings oldErrs

        -- See the Note [Safe Haskell Inference]
        case (not $ isEmptyBag errs) of

            -- We have errors!
            True ->
                -- did we fail safe inference or fail -XSafe?
                case safeInferOn dflags of
                    True  -> wipeTrust tcg_env errs
                    False -> liftIO . throwIO . mkSrcErr $ errs

            -- All good matey!
            False -> do
                when (packageTrustOn dflags) $ checkPkgTrust dflags pkg_reqs
                -- add in trusted package requirements for this module
                let new_trust = emptyImportAvails { imp_trust_pkgs = catMaybes pkgs }
                return tcg_env { tcg_imports = imp_info `plusImportAvails` new_trust }

  where
    imp_info = tcg_imports tcg_env     -- ImportAvails
    imports  = imp_mods imp_info       -- ImportedMods
    imports' = moduleEnvToList imports -- (Module, [ImportedModsVal])
    pkg_reqs = imp_trust_pkgs imp_info -- [PackageId]

    condense :: (Module, [ImportedModsVal]) -> Hsc (Module, SrcSpan, IsSafeImport)
    condense (_, [])   = panic "HscMain.condense: Pattern match failure!"
    condense (m, x:xs) = do (_,_,l,s) <- foldlM cond' x xs
                            -- we turn all imports into safe ones when
                            -- inference mode is on.
                            let s' = if safeInferOn dflags then True else s
                            return (m, l, s')

    -- ImportedModsVal = (ModuleName, Bool, SrcSpan, IsSafeImport)
    cond' :: ImportedModsVal -> ImportedModsVal -> Hsc ImportedModsVal
    cond' v1@(m1,_,l1,s1) (_,_,_,s2)
        | s1 /= s2
        = throwErrors $ unitBag $ mkPlainErrMsg dflags l1
              (text "Module" <+> ppr m1 <+>
              (text $ "is imported both as a safe and unsafe import!"))
        | otherwise
        = return v1

    -- easier interface to work with
    checkSafe (_, _, False) = return Nothing
    checkSafe (m, l, True ) = fst `fmap` hscCheckSafe' dflags m l

-- | Check that a module is safe to import.
--
-- We return True to indicate the import is safe and False otherwise
-- although in the False case an exception may be thrown first.
hscCheckSafe :: HscEnv -> Module -> SrcSpan -> IO Bool
hscCheckSafe hsc_env m l = runHsc hsc_env $ do
    dflags <- getDynFlags
    pkgs <- snd `fmap` hscCheckSafe' dflags m l
    when (packageTrustOn dflags) $ checkPkgTrust dflags pkgs
    errs <- getWarnings
    return $ isEmptyBag errs

-- | Return if a module is trusted and the pkgs it depends on to be trusted.
hscGetSafe :: HscEnv -> Module -> SrcSpan -> IO (Bool, [PackageId])
hscGetSafe hsc_env m l = runHsc hsc_env $ do
    dflags       <- getDynFlags
    (self, pkgs) <- hscCheckSafe' dflags m l
    good         <- isEmptyBag `fmap` getWarnings
    clearWarnings -- don't want them printed...
    let pkgs' | Just p <- self = p:pkgs
              | otherwise      = pkgs
    return (good, pkgs')

-- | Is a module trusted? If not, throw or log errors depending on the type.
-- Return (regardless of trusted or not) if the trust type requires the modules
-- own package be trusted and a list of other packages required to be trusted
-- (these later ones haven't been checked) but the own package trust has been.
hscCheckSafe' :: DynFlags -> Module -> SrcSpan -> Hsc (Maybe PackageId, [PackageId])
hscCheckSafe' dflags m l = do
    (tw, pkgs) <- isModSafe m l
    case tw of
        False              -> return (Nothing, pkgs)
        True | isHomePkg m -> return (Nothing, pkgs)
             | otherwise   -> return (Just $ modulePackageId m, pkgs)
  where
    isModSafe :: Module -> SrcSpan -> Hsc (Bool, [PackageId])
    isModSafe m l = do
        iface <- lookup' m
        case iface of
            -- can't load iface to check trust!
            Nothing -> throwErrors $ unitBag $ mkPlainErrMsg dflags l
                         $ text "Can't load the interface file for" <+> ppr m
                           <> text ", to check that it can be safely imported"

            -- got iface, check trust
            Just iface' ->
                let trust = getSafeMode $ mi_trust iface'
                    trust_own_pkg = mi_trust_pkg iface'
                    -- check module is trusted
                    safeM = trust `elem` [Sf_SafeInferred, Sf_Safe, Sf_Trustworthy]
                    -- check package is trusted
                    safeP = packageTrusted trust trust_own_pkg m
                    -- pkg trust reqs
                    pkgRs = map fst $ filter snd $ dep_pkgs $ mi_deps iface'
                    -- General errors we throw but Safe errors we log
                    errs = case (safeM, safeP) of
                        (True, True ) -> emptyBag
                        (True, False) -> pkgTrustErr
                        (False, _   ) -> modTrustErr
                in do
                    logWarnings errs
                    return (trust == Sf_Trustworthy, pkgRs)

                where
                    pkgTrustErr = unitBag $ mkPlainErrMsg dflags l $
                        sep [ ppr (moduleName m)
                                <> text ": Can't be safely imported!"
                            , text "The package (" <> ppr (modulePackageId m)
                                <> text ") the module resides in isn't trusted."
                            ]
                    modTrustErr = unitBag $ mkPlainErrMsg dflags l $
                        sep [ ppr (moduleName m)
                                <> text ": Can't be safely imported!"
                            , text "The module itself isn't safe." ]

    -- | Check the package a module resides in is trusted. Safe compiled
    -- modules are trusted without requiring that their package is trusted. For
    -- trustworthy modules, modules in the home package are trusted but
    -- otherwise we check the package trust flag.
    packageTrusted :: SafeHaskellMode -> Bool -> Module -> Bool
    packageTrusted Sf_None             _ _ = False -- shouldn't hit these cases
    packageTrusted Sf_Unsafe           _ _ = False -- prefer for completeness.
    packageTrusted _ _ _
        | not (packageTrustOn dflags)      = True
    packageTrusted Sf_Safe         False _ = True
    packageTrusted Sf_SafeInferred False _ = True
    packageTrusted _ _ m
        | isHomePkg m = True
        | otherwise   = trusted $ getPackageDetails (pkgState dflags)
                                                    (modulePackageId m)

    lookup' :: Module -> Hsc (Maybe ModIface)
    lookup' m = do
        hsc_env <- getHscEnv
        hsc_eps <- liftIO $ hscEPS hsc_env
        let pkgIfaceT = eps_PIT hsc_eps
            homePkgT  = hsc_HPT hsc_env
            iface     = lookupIfaceByModule dflags homePkgT pkgIfaceT m
#ifdef GHCI
        -- the 'lookupIfaceByModule' method will always fail when calling from GHCi
        -- as the compiler hasn't filled in the various module tables
        -- so we need to call 'getModuleInterface' to load from disk
        iface' <- case iface of
            Just _  -> return iface
            Nothing -> snd `fmap` (liftIO $ getModuleInterface hsc_env m)
        return iface'
#else
        return iface
#endif


    isHomePkg :: Module -> Bool
    isHomePkg m
        | thisPackage dflags == modulePackageId m = True
        | otherwise                               = False

-- | Check the list of packages are trusted.
checkPkgTrust :: DynFlags -> [PackageId] -> Hsc ()
checkPkgTrust dflags pkgs =
    case errors of
        [] -> return ()
        _  -> (liftIO . throwIO . mkSrcErr . listToBag) errors
    where
        errors = catMaybes $ map go pkgs
        go pkg
            | trusted $ getPackageDetails (pkgState dflags) pkg
            = Nothing
            | otherwise
            = Just $ mkPlainErrMsg dflags noSrcSpan
                   $ text "The package (" <> ppr pkg <> text ") is required" <>
                     text " to be trusted but it isn't!"

-- | Set module to unsafe and wipe trust information.
--
-- Make sure to call this method to set a module to inferred unsafe,
-- it should be a central and single failure method.
wipeTrust :: TcGblEnv -> WarningMessages -> Hsc TcGblEnv
wipeTrust tcg_env whyUnsafe = do
    dflags <- getDynFlags

    when (wopt Opt_WarnUnsafe dflags)
         (logWarnings $ unitBag $
             mkPlainWarnMsg dflags (warnUnsafeOnLoc dflags) (whyUnsafe' dflags))

    liftIO $ writeIORef (tcg_safeInfer tcg_env) False
    return $ tcg_env { tcg_imports = wiped_trust }

  where
    wiped_trust   = (tcg_imports tcg_env) { imp_trust_pkgs = [] }
    pprMod        = ppr $ moduleName $ tcg_mod tcg_env
    whyUnsafe' df = vcat [ quotes pprMod <+> text "has been inferred as unsafe!"
                         , text "Reason:"
                         , nest 4 $ (vcat $ badFlags df) $+$
                                    (vcat $ pprErrMsgBagWithLoc whyUnsafe)
                         ]
    badFlags df   = concat $ map (badFlag df) unsafeFlags
    badFlag df (str,loc,on,_)
        | on df     = [mkLocMessage SevOutput (loc df) $
                            text str <+> text "is not allowed in Safe Haskell"]
        | otherwise = []

-- | Figure out the final correct safe haskell mode
hscGetSafeMode :: TcGblEnv -> Hsc SafeHaskellMode
hscGetSafeMode tcg_env = do
    dflags  <- getDynFlags
    liftIO $ finalSafeMode dflags tcg_env

--------------------------------------------------------------
-- Simplifiers
--------------------------------------------------------------

hscSimplify :: HscEnv -> ModGuts -> IO ModGuts
hscSimplify hsc_env modguts = runHsc hsc_env $ hscSimplify' modguts

hscSimplify' :: ModGuts -> Hsc ModGuts
hscSimplify' ds_result = do
    hsc_env <- getHscEnv
    {-# SCC "Core2Core" #-}
      liftIO $ core2core hsc_env ds_result

--------------------------------------------------------------
-- Interface generators
--------------------------------------------------------------

hscSimpleIface :: HscEnv
               -> TcGblEnv
               -> Maybe Fingerprint
               -> IO (ModIface, Bool, ModDetails)
hscSimpleIface hsc_env tc_result mb_old_iface
    = runHsc hsc_env $ hscSimpleIface' tc_result mb_old_iface

hscSimpleIface' :: TcGblEnv
                -> Maybe Fingerprint
                -> Hsc (ModIface, Bool, ModDetails)
hscSimpleIface' tc_result mb_old_iface = do
    hsc_env   <- getHscEnv
    details   <- liftIO $ mkBootModDetailsTc hsc_env tc_result
    safe_mode <- hscGetSafeMode tc_result
    (new_iface, no_change)
        <- {-# SCC "MkFinalIface" #-}
           ioMsgMaybe $
               mkIfaceTc hsc_env mb_old_iface safe_mode details tc_result
    -- And the answer is ...
    liftIO $ dumpIfaceStats hsc_env
    return (new_iface, no_change, details)

hscNormalIface :: HscEnv
               -> ModGuts
               -> Maybe Fingerprint
               -> IO (ModIface, Bool, ModDetails, CgGuts)
hscNormalIface hsc_env simpl_result mb_old_iface =
    runHsc hsc_env $ hscNormalIface' simpl_result mb_old_iface

hscNormalIface' :: ModGuts
                -> Maybe Fingerprint
                -> Hsc (ModIface, Bool, ModDetails, CgGuts)
hscNormalIface' simpl_result mb_old_iface = do
    hsc_env <- getHscEnv
    (cg_guts, details) <- {-# SCC "CoreTidy" #-}
                          liftIO $ tidyProgram hsc_env simpl_result

    -- BUILD THE NEW ModIface and ModDetails
    --  and emit external core if necessary
    -- This has to happen *after* code gen so that the back-end
    -- info has been set. Not yet clear if it matters waiting
    -- until after code output
    (new_iface, no_change)
        <- {-# SCC "MkFinalIface" #-}
           ioMsgMaybe $
               mkIface hsc_env mb_old_iface details simpl_result

    liftIO $ dumpIfaceStats hsc_env

    -- Return the prepared code.
    return (new_iface, no_change, details, cg_guts)

--------------------------------------------------------------
-- BackEnd combinators
--------------------------------------------------------------

hscWriteIface :: DynFlags -> ModIface -> Bool -> ModSummary -> IO ()
hscWriteIface dflags iface no_change mod_summary
  | not (gopt Opt_WriteInterface dflags) = return ()
  | otherwise = do
    let ifaceFile = ml_hi_file (ms_location mod_summary)
    unless no_change $
        {-# SCC "writeIface" #-}
        writeIfaceFile dflags ifaceFile iface
    whenGeneratingDynamicToo dflags $ do
        -- TODO: We should do a no_change check for the dynamic
        --       interface file too
        -- TODO: Should handle the dynamic hi filename properly
        let dynIfaceFile = replaceExtension ifaceFile (dynHiSuf dflags)
            dynIfaceFile' = addBootSuffix_maybe (mi_boot iface) dynIfaceFile
            dynDflags = dynamicTooMkDynamicDynFlags dflags
        writeIfaceFile dynDflags dynIfaceFile' iface

-- | Compile to hard-code.
hscGenHardCode :: HscEnv -> CgGuts -> ModSummary -> FilePath
               -> IO (FilePath, Maybe FilePath) -- ^ @Just f@ <=> _stub.c is f
hscGenHardCode hsc_env cgguts mod_summary output_filename = do
        let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                    -- From now on, we just use the bits we need.
                    cg_module   = this_mod,
                    cg_binds    = core_binds,
                    cg_tycons   = tycons,
                    cg_foreign  = foreign_stubs0,
                    cg_dep_pkgs = dependencies,
                    cg_hpc_info = hpc_info } = cgguts
            dflags = hsc_dflags hsc_env
            location = ms_location mod_summary
            data_tycons = filter isDataTyCon tycons
            -- cg_tycons includes newtypes, for the benefit of External Core,
            -- but we don't generate any code for newtypes

        -------------------
        -- PREPARE FOR CODE GENERATION
        -- Do saturation and convert to A-normal form
        prepd_binds <- {-# SCC "CorePrep" #-}
                       corePrepPgm dflags hsc_env core_binds data_tycons ;
        -----------------  Convert to STG ------------------
        (stg_binds, cost_centre_info)
            <- {-# SCC "CoreToStg" #-}
               myCoreToStg dflags this_mod prepd_binds

        let prof_init = profilingInitCode this_mod cost_centre_info
            foreign_stubs = foreign_stubs0 `appendStubC` prof_init

        ------------------  Code generation ------------------

        -- The back-end is streamed: each top-level function goes
        -- from Stg all the way to asm before dealing with the next
        -- top-level function, so showPass isn't very useful here.
        -- Hence we have one showPass for the whole backend, the
        -- next showPass after this will be "Assembler".
        showPass dflags "CodeGen"

        cmms <- {-# SCC "StgCmm" #-}
                         doCodeGen hsc_env this_mod data_tycons
                             cost_centre_info
                             stg_binds hpc_info

        ------------------  Code output -----------------------
        rawcmms0 <- {-# SCC "cmmToRawCmm" #-}
                   cmmToRawCmm dflags cmms

        let dump a = do dumpIfSet_dyn dflags Opt_D_dump_cmm_raw "Raw Cmm"
                           (ppr a)
                        return a
            rawcmms1 = Stream.mapM dump rawcmms0

        (output_filename, (_stub_h_exists, stub_c_exists))
            <- {-# SCC "codeOutput" #-}
               codeOutput dflags this_mod output_filename location
               foreign_stubs dependencies rawcmms1
        return (output_filename, stub_c_exists)


hscInteractive :: HscEnv
               -> CgGuts
               -> ModSummary
               -> IO (Maybe FilePath, CompiledByteCode, ModBreaks)
#ifdef GHCI
hscInteractive hsc_env cgguts mod_summary = do
    let dflags = hsc_dflags hsc_env
    let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                -- From now on, we just use the bits we need.
               cg_module   = this_mod,
               cg_binds    = core_binds,
               cg_tycons   = tycons,
               cg_foreign  = foreign_stubs,
               cg_modBreaks = mod_breaks } = cgguts

        location = ms_location mod_summary
        data_tycons = filter isDataTyCon tycons
        -- cg_tycons includes newtypes, for the benefit of External Core,
        -- but we don't generate any code for newtypes

    -------------------
    -- PREPARE FOR CODE GENERATION
    -- Do saturation and convert to A-normal form
    prepd_binds <- {-# SCC "CorePrep" #-}
                   corePrepPgm dflags hsc_env core_binds data_tycons
    -----------------  Generate byte code ------------------
    comp_bc <- byteCodeGen dflags this_mod prepd_binds data_tycons mod_breaks
    ------------------ Create f-x-dynamic C-side stuff ---
    (_istub_h_exists, istub_c_exists)
        <- outputForeignStubs dflags this_mod location foreign_stubs
    return (istub_c_exists, comp_bc, mod_breaks)
#else
hscInteractive _ _ = panic "GHC not compiled with interpreter"
#endif

------------------------------

hscCompileCmmFile :: HscEnv -> FilePath -> FilePath -> IO ()
hscCompileCmmFile hsc_env filename output_filename = runHsc hsc_env $ do
    let dflags = hsc_dflags hsc_env
    cmm <- ioMsgMaybe $ parseCmmFile dflags filename
    liftIO $ do
        us <- mkSplitUniqSupply 'S'
        let initTopSRT = initUs_ us emptySRT
        dumpIfSet_dyn dflags Opt_D_dump_cmm "Parsed Cmm" (ppr cmm)
        (_, cmmgroup) <- cmmPipeline hsc_env initTopSRT cmm
        rawCmms <- cmmToRawCmm dflags (Stream.yield cmmgroup)
        _ <- codeOutput dflags no_mod output_filename no_loc NoStubs [] rawCmms
        return ()
  where
    no_mod = panic "hscCmmFile: no_mod"
    no_loc = ModLocation{ ml_hs_file  = Just filename,
                          ml_hi_file  = panic "hscCmmFile: no hi file",
                          ml_obj_file = panic "hscCmmFile: no obj file" }

-------------------- Stuff for new code gen ---------------------

doCodeGen   :: HscEnv -> Module -> [TyCon]
            -> CollectedCCs
            -> [StgBinding]
            -> HpcInfo
            -> IO (Stream IO CmmGroup ())
         -- Note we produce a 'Stream' of CmmGroups, so that the
         -- backend can be run incrementally.  Otherwise it generates all
         -- the C-- up front, which has a significant space cost.
doCodeGen hsc_env this_mod data_tycons
              cost_centre_info stg_binds hpc_info = do
    let dflags = hsc_dflags hsc_env

    let cmm_stream :: Stream IO CmmGroup ()
        cmm_stream = {-# SCC "StgCmm" #-}
            StgCmm.codeGen dflags this_mod data_tycons
                           cost_centre_info stg_binds hpc_info

        -- codegen consumes a stream of CmmGroup, and produces a new
        -- stream of CmmGroup (not necessarily synchronised: one
        -- CmmGroup on input may produce many CmmGroups on output due
        -- to proc-point splitting).

    let dump1 a = do dumpIfSet_dyn dflags Opt_D_dump_cmm
                       "Cmm produced by new codegen" (ppr a)
                     return a

        ppr_stream1 = Stream.mapM dump1 cmm_stream

    -- We are building a single SRT for the entire module, so
    -- we must thread it through all the procedures as we cps-convert them.
    us <- mkSplitUniqSupply 'S'

    -- When splitting, we generate one SRT per split chunk, otherwise
    -- we generate one SRT for the whole module.
    let
     pipeline_stream
      | gopt Opt_SplitObjs dflags
        = {-# SCC "cmmPipeline" #-}
          let run_pipeline us cmmgroup = do
                let (topSRT', us') = initUs us emptySRT
                (topSRT, cmmgroup) <- cmmPipeline hsc_env topSRT' cmmgroup
                let srt | isEmptySRT topSRT = []
                        | otherwise         = srtToData topSRT
                return (us', srt ++ cmmgroup)

          in do _ <- Stream.mapAccumL run_pipeline us ppr_stream1
                return ()

      | otherwise
        = {-# SCC "cmmPipeline" #-}
          let initTopSRT = initUs_ us emptySRT
              run_pipeline = cmmPipeline hsc_env
          in do topSRT <- Stream.mapAccumL run_pipeline initTopSRT ppr_stream1
                Stream.yield (srtToData topSRT)

    let
        dump2 a = do dumpIfSet_dyn dflags Opt_D_dump_cmm "Output Cmm" $ ppr a
                     return a

        ppr_stream2 = Stream.mapM dump2 pipeline_stream

    return ppr_stream2



myCoreToStg :: DynFlags -> Module -> CoreProgram
            -> IO ( [StgBinding] -- output program
                  , CollectedCCs) -- cost centre info (declared and used)
myCoreToStg dflags this_mod prepd_binds = do
    stg_binds
        <- {-# SCC "Core2Stg" #-}
           coreToStg dflags this_mod prepd_binds

    (stg_binds2, cost_centre_info)
        <- {-# SCC "Stg2Stg" #-}
           stg2stg dflags this_mod stg_binds

    return (stg_binds2, cost_centre_info)


{- **********************************************************************
%*                                                                      *
\subsection{Compiling a do-statement}
%*                                                                      *
%********************************************************************* -}

{-
When the UnlinkedBCOExpr is linked you get an HValue of type *IO [HValue]* When
you run it you get a list of HValues that should be the same length as the list
of names; add them to the ClosureEnv.

A naked expression returns a singleton Name [it]. The stmt is lifted into the
IO monad as explained in Note [Interactively-bound Ids in GHCi] in HscTypes
-}

#ifdef GHCI
-- | Compile a stmt all the way to an HValue, but don't run it
--
-- We return Nothing to indicate an empty statement (or comment only), not a
-- parse error.
hscStmt :: HscEnv -> String -> IO (Maybe ([Id], IO [HValue], FixityEnv))
hscStmt hsc_env stmt = hscStmtWithLocation hsc_env stmt "<interactive>" 1

-- | Compile a stmt all the way to an HValue, but don't run it
--
-- We return Nothing to indicate an empty statement (or comment only), not a
-- parse error.
hscStmtWithLocation :: HscEnv
                    -> String -- ^ The statement
                    -> String -- ^ The source
                    -> Int    -- ^ Starting line
                    -> IO (Maybe ([Id], IO [HValue], FixityEnv))
hscStmtWithLocation hsc_env0 stmt source linenumber =
 runInteractiveHsc hsc_env0 $ do
    maybe_stmt <- hscParseStmtWithLocation source linenumber stmt
    case maybe_stmt of
        Nothing -> return Nothing

        Just parsed_stmt -> do
            -- Rename and typecheck it
            hsc_env <- getHscEnv
            (ids, tc_expr, fix_env) <- ioMsgMaybe $ tcRnStmt hsc_env parsed_stmt

            -- Desugar it
            ds_expr <- ioMsgMaybe $ deSugarExpr hsc_env tc_expr
            liftIO (lintInteractiveExpr "desugar expression" hsc_env ds_expr)
            handleWarnings

            -- Then code-gen, and link it
            -- It's important NOT to have package 'interactive' as thisPackageId
            -- for linking, else we try to link 'main' and can't find it.
            -- Whereas the linker already knows to ignore 'interactive'
            let  src_span     = srcLocSpan interactiveSrcLoc
            hval <- liftIO $ hscCompileCoreExpr hsc_env src_span ds_expr
            let hval_io = unsafeCoerce# hval :: IO [HValue]

            return $ Just (ids, hval_io, fix_env)

-- | Compile a decls
hscDecls :: HscEnv
         -> String -- ^ The statement
         -> IO ([TyThing], InteractiveContext)
hscDecls hsc_env str = hscDeclsWithLocation hsc_env str "<interactive>" 1

-- | Compile a decls
hscDeclsWithLocation :: HscEnv
                     -> String -- ^ The statement
                     -> String -- ^ The source
                     -> Int    -- ^ Starting line
                     -> IO ([TyThing], InteractiveContext)
hscDeclsWithLocation hsc_env0 str source linenumber =
 runInteractiveHsc hsc_env0 $ do
    L _ (HsModule{ hsmodDecls = decls }) <-
        hscParseThingWithLocation source linenumber parseModule str

    {- Rename and typecheck it -}
    hsc_env <- getHscEnv
    tc_gblenv <- ioMsgMaybe $ tcRnDeclsi hsc_env decls

    {- Grab the new instances -}
    -- We grab the whole environment because of the overlapping that may have
    -- been done. See the notes at the definition of InteractiveContext
    -- (ic_instances) for more details.
    let finsts = tcg_fam_insts tc_gblenv
        insts  = tcg_insts     tc_gblenv

    let defaults = tcg_default tc_gblenv

    {- Desugar it -}
    -- We use a basically null location for iNTERACTIVE
    let iNTERACTIVELoc = ModLocation{ ml_hs_file   = Nothing,
                                      ml_hi_file   = panic "hsDeclsWithLocation:ml_hi_file",
                                      ml_obj_file  = panic "hsDeclsWithLocation:ml_hi_file"}
    ds_result <- hscDesugar' iNTERACTIVELoc tc_gblenv

    {- Simplify -}
    simpl_mg <- liftIO $ hscSimplify hsc_env ds_result

    {- Tidy -}
    (tidy_cg, _mod_details) <- liftIO $ tidyProgram hsc_env simpl_mg

    let dflags = hsc_dflags hsc_env
        !CgGuts{ cg_module    = this_mod,
                 cg_binds     = core_binds,
                 cg_tycons    = tycons,
                 cg_modBreaks = mod_breaks } = tidy_cg
        data_tycons = filter isDataTyCon tycons

    {- Prepare For Code Generation -}
    -- Do saturation and convert to A-normal form
    prepd_binds <- {-# SCC "CorePrep" #-}
                    liftIO $ corePrepPgm dflags hsc_env core_binds data_tycons

    {- Generate byte code -}
    cbc <- liftIO $ byteCodeGen dflags this_mod
                                prepd_binds data_tycons mod_breaks

    let src_span = srcLocSpan interactiveSrcLoc
    liftIO $ linkDecls hsc_env src_span cbc

    let tcs = filterOut isImplicitTyCon (mg_tcs simpl_mg)

        ext_ids = [ id | id <- bindersOfBinds core_binds
                       , isExternalName (idName id)
                       , not (isDFunId id || isImplicitId id) ]
            -- We only need to keep around the external bindings
            -- (as decided by TidyPgm), since those are the only ones
            -- that might be referenced elsewhere.
            -- The DFunIds are in 'insts' (see Note [ic_tythings] in HscTypes
            -- Implicit Ids are implicit in tcs

        tythings =  map AnId ext_ids ++ map ATyCon tcs

    let icontext = hsc_IC hsc_env
        ictxt1   = extendInteractiveContext icontext tythings
        ictxt    = ictxt1 { ic_instances = (insts, finsts)
                          , ic_default   = defaults }

    return (tythings, ictxt)

hscImport :: HscEnv -> String -> IO (ImportDecl RdrName)
hscImport hsc_env str = runInteractiveHsc hsc_env $ do
    (L _ (HsModule{hsmodImports=is})) <-
       hscParseThing parseModule str
    case is of
        [i] -> return (unLoc i)
        _ -> liftIO $ throwOneError $
                 mkPlainErrMsg (hsc_dflags hsc_env) noSrcSpan $
                     ptext (sLit "parse error in import declaration")

-- | Typecheck an expression (but don't run it)
-- Returns its most general type
hscTcExpr :: HscEnv
          -> String -- ^ The expression
          -> IO Type
hscTcExpr hsc_env0 expr = runInteractiveHsc hsc_env0 $ do
    hsc_env <- getHscEnv
    maybe_stmt <- hscParseStmt expr
    case maybe_stmt of
        Just (L _ (BodyStmt expr _ _ _)) ->
            ioMsgMaybe $ tcRnExpr hsc_env expr
        _ ->
            throwErrors $ unitBag $ mkPlainErrMsg (hsc_dflags hsc_env) noSrcSpan
                (text "not an expression:" <+> quotes (text expr))

-- | Find the kind of a type
-- Currently this does *not* generalise the kinds of the type
hscKcType
  :: HscEnv
  -> Bool            -- ^ Normalise the type
  -> String          -- ^ The type as a string
  -> IO (Type, Kind) -- ^ Resulting type (possibly normalised) and kind
hscKcType hsc_env0 normalise str = runInteractiveHsc hsc_env0 $ do
    hsc_env <- getHscEnv
    ty <- hscParseType str
    ioMsgMaybe $ tcRnType hsc_env normalise ty

hscParseStmt :: String -> Hsc (Maybe (GhciLStmt RdrName))
hscParseStmt = hscParseThing parseStmt

hscParseStmtWithLocation :: String -> Int -> String
                         -> Hsc (Maybe (GhciLStmt RdrName))
hscParseStmtWithLocation source linenumber stmt =
    hscParseThingWithLocation source linenumber parseStmt stmt

hscParseType :: String -> Hsc (LHsType RdrName)
hscParseType = hscParseThing parseType
#endif

hscParseIdentifier :: HscEnv -> String -> IO (Located RdrName)
hscParseIdentifier hsc_env str =
    runInteractiveHsc hsc_env $ hscParseThing parseIdentifier str

hscParseThing :: (Outputable thing) => Lexer.P thing -> String -> Hsc thing
hscParseThing = hscParseThingWithLocation "<interactive>" 1

hscParseThingWithLocation :: (Outputable thing) => String -> Int
                          -> Lexer.P thing -> String -> Hsc thing
hscParseThingWithLocation source linenumber parser str
  = {-# SCC "Parser" #-} do
    dflags <- getDynFlags
    liftIO $ showPass dflags "Parser"

    let buf = stringToStringBuffer str
        loc = mkRealSrcLoc (fsLit source) linenumber 1

    case unP parser (mkPState dflags buf loc) of
        PFailed span err -> do
            let msg = mkPlainErrMsg dflags span err
            throwErrors $ unitBag msg

        POk pst thing -> do
            logWarningsReportErrors (getMessages pst)
            liftIO $ dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr thing)
            return thing

hscCompileCore :: HscEnv -> Bool -> SafeHaskellMode -> ModSummary
               -> CoreProgram -> FilePath -> IO ()
hscCompileCore hsc_env simplify safe_mode mod_summary binds output_filename
  = runHsc hsc_env $ do
        guts <- maybe_simplify (mkModGuts (ms_mod mod_summary) safe_mode binds)
        (iface, changed, _details, cgguts) <- hscNormalIface' guts Nothing
        liftIO $ hscWriteIface (hsc_dflags hsc_env) iface changed mod_summary
        _ <- liftIO $ hscGenHardCode hsc_env cgguts mod_summary output_filename
        return ()

  where
    maybe_simplify mod_guts | simplify = hscSimplify' mod_guts
                            | otherwise = return mod_guts

-- Makes a "vanilla" ModGuts.
mkModGuts :: Module -> SafeHaskellMode -> CoreProgram -> ModGuts
mkModGuts mod safe binds =
    ModGuts {
        mg_module       = mod,
        mg_boot         = False,
        mg_exports      = [],
        mg_deps         = noDependencies,
        mg_dir_imps     = emptyModuleEnv,
        mg_used_names   = emptyNameSet,
        mg_used_th      = False,
        mg_rdr_env      = emptyGlobalRdrEnv,
        mg_fix_env      = emptyFixityEnv,
        mg_tcs          = [],
        mg_insts        = [],
        mg_fam_insts    = [],
        mg_patsyns      = [],
        mg_rules        = [],
        mg_vect_decls   = [],
        mg_binds        = binds,
        mg_foreign      = NoStubs,
        mg_warns        = NoWarnings,
        mg_anns         = [],
        mg_hpc_info     = emptyHpcInfo False,
        mg_modBreaks    = emptyModBreaks,
        mg_vect_info    = noVectInfo,
        mg_inst_env     = emptyInstEnv,
        mg_fam_inst_env = emptyFamInstEnv,
        mg_safe_haskell = safe,
        mg_trust_pkg    = False,
        mg_dependent_files = []
    }


{- **********************************************************************
%*                                                                      *
        Desugar, simplify, convert to bytecode, and link an expression
%*                                                                      *
%********************************************************************* -}

#ifdef GHCI
hscCompileCoreExpr :: HscEnv -> SrcSpan -> CoreExpr -> IO HValue
hscCompileCoreExpr hsc_env =
  lookupHook hscCompileCoreExprHook hscCompileCoreExpr' (hsc_dflags hsc_env) hsc_env

hscCompileCoreExpr' :: HscEnv -> SrcSpan -> CoreExpr -> IO HValue
hscCompileCoreExpr' hsc_env srcspan ds_expr
    | rtsIsProfiled
    = throwIO (InstallationError "You can't call hscCompileCoreExpr in a profiled compiler")
            -- Otherwise you get a seg-fault when you run it

    | otherwise
    = do { let dflags = hsc_dflags hsc_env

           {- Simplify it -}
         ; simpl_expr <- simplifyExpr dflags ds_expr

           {- Tidy it (temporary, until coreSat does cloning) -}
         ; let tidy_expr = tidyExpr emptyTidyEnv simpl_expr

           {- Prepare for codegen -}
         ; prepd_expr <- corePrepExpr dflags hsc_env tidy_expr

           {- Lint if necessary -}
         ; lintInteractiveExpr "hscCompileExpr" hsc_env prepd_expr

           {- Convert to BCOs -}
         ; bcos <- coreExprToBCOs dflags (icInteractiveModule (hsc_IC hsc_env)) prepd_expr

           {- link it -}
         ; hval <- linkExpr hsc_env srcspan bcos

         ; return hval }
#endif


{- **********************************************************************
%*                                                                      *
        Statistics on reading interfaces
%*                                                                      *
%********************************************************************* -}

dumpIfaceStats :: HscEnv -> IO ()
dumpIfaceStats hsc_env = do
    eps <- readIORef (hsc_EPS hsc_env)
    dumpIfSet dflags (dump_if_trace || dump_rn_stats)
              "Interface statistics"
              (ifaceStats eps)
  where
    dflags = hsc_dflags hsc_env
    dump_rn_stats = dopt Opt_D_dump_rn_stats dflags
    dump_if_trace = dopt Opt_D_dump_if_trace dflags


{- **********************************************************************
%*                                                                      *
        Progress Messages: Module i of n
%*                                                                      *
%********************************************************************* -}

showModuleIndex :: (Int, Int) -> String
showModuleIndex (i,n) = "[" ++ padded ++ " of " ++ n_str ++ "] "
  where
    n_str = show n
    i_str = show i
    padded = replicate (length n_str - length i_str) ' ' ++ i_str
