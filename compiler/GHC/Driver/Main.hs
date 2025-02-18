{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -fprof-auto-top #-}

-------------------------------------------------------------------------------
--
-- | Main API for compiling plain Haskell source code.
--
-- This module implements compilation of a Haskell source. It is
-- /not/ concerned with preprocessing of source files; this is handled
-- in "GHC.Driver.Pipeline"
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
-- We use the Hsc monad to deal with warning messages consistently:
-- specifically, while executing within an Hsc monad, warnings are
-- collected. When a Hsc monad returns to an IO monad, the
-- warnings are printed, or compilation aborts if the @-Werror@
-- flag is enabled.
--
-- (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
--
-------------------------------------------------------------------------------

module GHC.Driver.Main
    (
    -- * Making an HscEnv
      newHscEnv
    , newHscEnvWithHUG
    , initHscEnv

    -- * Compiling complete source files
    , Messager, batchMsg, batchMultiMsg
    , HscBackendAction (..), HscRecompStatus (..)
    , initModDetails
    , initWholeCoreBindings
    , loadIfaceByteCode
    , hscMaybeWriteIface
    , hscCompileCmmFile

    , hscGenHardCode
    , hscInteractive
    , mkCgInteractiveGuts
    , CgInteractiveGuts
    , generateByteCode
    , generateFreshByteCode

    -- * Running passes separately
    , hscRecompStatus
    , hscParse
    , hscTypecheckRename
    , hscTypecheckRenameWithDiagnostics
    , hscTypecheckAndGetWarnings
    , hscDesugar
    , makeSimpleDetails
    , hscSimplify -- ToDo, shouldn't really export this
    , hscDesugarAndSimplify

    -- * Safe Haskell
    , hscCheckSafe
    , hscGetSafe

    -- * Support for interactive evaluation
    , hscParseIdentifier
    , hscTcRcLookupName
    , hscTcRnGetInfo
    , hscIsGHCiMonad
    , hscGetModuleInterface
    , hscRnImportDecls
    , hscTcRnLookupRdrName
    , hscStmt, hscParseStmtWithLocation, hscStmtWithLocation, hscParsedStmt
    , hscParseDeclsWithLocation, hscParsedDecls
    , hscParseModuleWithLocation
    , hscTcExpr, TcRnExprMode(..), hscImport, hscKcType
    , hscParseExpr
    , hscParseType
    , hscCompileCoreExpr
    , hscTidy


    -- * Low-level exports for hooks
    , hscCompileCoreExpr'
      -- We want to make sure that we export enough to be able to redefine
      -- hsc_typecheck in client code
    , hscParse', hscSimplify', hscDesugar', tcRnModule', doCodeGen
    , getHscEnv
    , hscSimpleIface'
    , oneShotMsg
    , dumpIfaceStats
    , ioMsgMaybe
    , showModuleIndex
    , hscAddSptEntries
    , writeInterfaceOnlyMode
    , loadByteCode
    ) where

import GHC.Prelude

import GHC.Platform

import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Env
import GHC.Driver.Env.KnotVars
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.CodeOutput
import GHC.Driver.Config.Cmm.Parser (initCmmParserConfig)
import GHC.Driver.Config.Core.Opt.Simplify ( initSimplifyExprOpts )
import GHC.Driver.Config.Core.Lint ( endPassHscEnvIO )
import GHC.Driver.Config.Core.Lint.Interactive ( lintInteractiveExpr )
import GHC.Driver.Config.CoreToStg
import GHC.Driver.Config.CoreToStg.Prep
import GHC.Driver.Config.Logger   (initLogFlags)
import GHC.Driver.Config.Parser   (initParserOpts)
import GHC.Driver.Config.Stg.Ppr  (initStgPprOpts)
import GHC.Driver.Config.Stg.Pipeline (initStgPipelineOpts)
import GHC.Driver.Config.StgToCmm  (initStgToCmmConfig)
import GHC.Driver.Config.Cmm       (initCmmConfig)
import GHC.Driver.LlvmConfigCache  (initLlvmConfigCache)
import GHC.Driver.Config.StgToJS  (initStgToJSConfig)
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Config.Tidy
import GHC.Driver.Hooks
import GHC.Driver.GenerateCgIPEStub (generateCgIPEStub, lookupEstimatedTicks)

import GHC.Runtime.Context
import GHC.Runtime.Interpreter
import GHC.Runtime.Interpreter.JS
import GHC.Runtime.Loader      ( initializePlugins )
import GHCi.RemoteTypes
import GHC.ByteCode.Types

import GHC.Linker.Loader
import GHC.Linker.Types
import GHC.Linker.Deps

import GHC.Hs
import GHC.Hs.Dump
import GHC.Hs.Stats         ( ppSourceStats )

import GHC.HsToCore

import GHC.StgToByteCode    ( byteCodeGen )
import GHC.StgToJS          ( stgToJS )
import GHC.StgToJS.Ids
import GHC.StgToJS.Types
import GHC.JS.Syntax

import GHC.IfaceToCore  ( typecheckIface, typecheckWholeCoreBindings )

import GHC.Iface.Load   ( ifaceStats, writeIface, flagsToIfCompression, getGhcPrimIface )
import GHC.Iface.Make
import GHC.Iface.Recomp
import GHC.Iface.Tidy
import GHC.Iface.Ext.Ast    ( mkHieFile )
import GHC.Iface.Ext.Types  ( getAsts, hie_asts, hie_module )
import GHC.Iface.Ext.Binary ( readHieFile, writeHieFile , hie_file_result)
import GHC.Iface.Ext.Debug  ( diffFile, validateScopes )

import GHC.Core
import GHC.Core.Lint.Interactive ( interactiveInScope )
import GHC.Core.Tidy           ( tidyExpr )
import GHC.Core.Utils          ( exprType )
import GHC.Core.ConLike
import GHC.Core.Opt.Pipeline
import GHC.Core.Opt.Pipeline.Types      ( CoreToDo (..))
import GHC.Core.TyCon
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv
import GHC.Core.Rules
import GHC.Core.Stats
import GHC.Core.LateCC
import GHC.Core.LateCC.Types


import GHC.CoreToStg.Prep
import GHC.CoreToStg    ( coreToStg )

import GHC.Parser.Errors.Types
import GHC.Parser
import GHC.Parser.Lexer as Lexer

import GHC.Tc.Module
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Tc.Zonk.Env ( ZonkFlexi (DefaultFlexi) )

import GHC.Stg.Syntax
import GHC.Stg.Pipeline ( stg2stg, StgCgInfos )

import GHC.Builtin.Names

import qualified GHC.StgToCmm as StgToCmm ( codeGen )
import GHC.StgToCmm.Types (CmmCgInfos (..), ModuleLFInfos, LambdaFormInfo(..))
import GHC.StgToCmm.CgUtils (CgStream)

import GHC.Cmm
import GHC.Cmm.Info.Build
import GHC.Cmm.Pipeline
import GHC.Cmm.Info
import GHC.Cmm.Parser
import GHC.Cmm.UniqueRenamer

import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.Finder
import GHC.Unit.External
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Graph
import GHC.Unit.Module.Imported
import GHC.Unit.Module.Deps
import GHC.Unit.Module.Status
import GHC.Unit.Home.ModInfo

import GHC.Types.Id
import GHC.Types.SourceError
import GHC.Types.SafeHaskell
import GHC.Types.ForeignStubs
import GHC.Types.Name.Env      ( mkNameEnv )
import GHC.Types.Var.Env       ( mkEmptyTidyEnv )
import GHC.Types.Var.Set
import GHC.Types.Error
import GHC.Types.Fixity.Env
import GHC.Types.CostCentre
import GHC.Types.IPE
import GHC.Types.SourceFile
import GHC.Types.SrcLoc
import GHC.Types.Name
import GHC.Types.Name.Cache ( newNameCache, knownKeysOrigNameCache )
import GHC.Types.Name.Reader
import GHC.Types.Name.Ppr
import GHC.Types.TyThing
import GHC.Types.Unique.Supply (uniqFromTag)
import GHC.Types.Unique.Set

import GHC.Utils.Fingerprint ( Fingerprint )
import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Logger
import GHC.Utils.TmpFs
import GHC.Utils.Touch

import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.FastString
import GHC.Data.Bag
import GHC.Data.OsPath (unsafeEncodeUtf)
import GHC.Data.StringBuffer
import qualified GHC.Data.Stream as Stream
import GHC.Data.Maybe

import GHC.SysTools (initSysTools)
import GHC.SysTools.BaseDir (findTopDir)

import Data.Data hiding (Fixity, TyCon)
import Data.Functor ((<&>))
import Data.List ( nub, isPrefixOf, partition )
import qualified Data.List.NonEmpty as NE
import Control.Monad
import Data.IORef
import System.FilePath as FilePath
import System.Directory
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Control.DeepSeq (force)
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Unit.Module.WholeCoreBindings
import GHC.Types.TypeEnv
import System.IO
import {-# SOURCE #-} GHC.Driver.Pipeline
import Data.Time

import System.IO.Unsafe ( unsafeInterleaveIO )
import GHC.Iface.Env ( trace_if )
import GHC.Platform.Ways
import GHC.Stg.EnforceEpt.TagSig (seqTagSig)
import GHC.StgToCmm.Utils (IPEStats)
import GHC.Types.Unique.FM
import GHC.Types.Unique.DFM
import GHC.Cmm.Config (CmmConfig)
import Data.Bifunctor
import qualified GHC.Unit.Home.Graph as HUG
import GHC.Unit.Home.PackageTable

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
    nc_var  <- newNameCache 'r' knownKeysOrigNameCache
    fc_var  <- initFinderCache
    logger  <- initLogger
    tmpfs   <- initTmpFs
    let dflags = homeUnitEnv_dflags $ HUG.unitEnv_lookup cur_unit home_unit_graph
    unit_env <- initUnitEnv cur_unit home_unit_graph (ghcNameVersion dflags) (targetPlatform dflags)
    llvm_config <- initLlvmConfigCache top_dir
    return HscEnv { hsc_dflags         = top_dynflags
                  , hsc_logger         = setLogFlags logger (initLogFlags top_dynflags)
                  , hsc_targets        = []
                  , hsc_mod_graph      = emptyMG
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

getHscEnv :: Hsc HscEnv
getHscEnv = Hsc $ \e w -> return (e, w)

handleWarnings :: Hsc ()
handleWarnings = do
    diag_opts <- initDiagOpts <$> getDynFlags
    print_config <- initPrintConfig <$> getDynFlags
    logger <- getLogger
    w <- getDiagnostics
    liftIO $ printOrThrowDiagnostics logger print_config diag_opts w
    clearDiagnostics

-- | log warning in the monad, and if there are errors then
-- throw a SourceError exception.
logWarningsReportErrors :: (Messages PsWarning, Messages PsError) -> Hsc ()
logWarningsReportErrors (warnings,errors) = do
    logDiagnostics (GhcPsMessage <$> warnings)
    when (not $ isEmptyMessages errors) $ throwErrors (GhcPsMessage <$> errors)

-- | Log warnings and throw errors, assuming the messages
-- contain at least one error (e.g. coming from PFailed)
handleWarningsThrowErrors :: (Messages PsWarning, Messages PsError) -> Hsc a
handleWarningsThrowErrors (warnings, errors) = do
    diag_opts <- initDiagOpts <$> getDynFlags
    logDiagnostics (GhcPsMessage <$> warnings)
    logger <- getLogger
    let (wWarns, wErrs) = partitionMessages warnings
    liftIO $ printMessages logger NoDiagnosticOpts diag_opts wWarns
    throwErrors $ fmap GhcPsMessage $ errors `unionMessages` wErrs

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
    logDiagnostics warns
    case mb_r of
        Nothing -> throwErrors errs
        Just r  -> assert (isEmptyMessages errs ) return r

-- | like ioMsgMaybe, except that we ignore error messages and return
-- 'Nothing' instead.
ioMsgMaybe' :: IO (Messages GhcMessage, Maybe a) -> Hsc (Maybe a)
ioMsgMaybe' ioA = do
    (msgs, mb_r) <- liftIO $ ioA
    logDiagnostics (mkMessages $ getWarningMessages msgs)
    return mb_r

-- -----------------------------------------------------------------------------
-- | Lookup things in the compiler's environment

hscTcRnLookupRdrName :: HscEnv -> LocatedN RdrName -> IO (NonEmpty Name)
hscTcRnLookupRdrName hsc_env0 rdr_name
  = runInteractiveHsc hsc_env0 $
    do { hsc_env <- getHscEnv
       -- tcRnLookupRdrName can return empty list only together with TcRnUnknownMessage.
       -- Once errors has been dealt with in hoistTcRnMessage, we can enforce
       -- this invariant in types by converting to NonEmpty.
       ; ioMsgMaybe $ fmap (fmap (>>= NE.nonEmpty)) $ hoistTcRnMessage $
          tcRnLookupRdrName hsc_env rdr_name }

hscTcRcLookupName :: HscEnv -> Name -> IO (Maybe TyThing)
hscTcRcLookupName hsc_env0 name = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe' $ hoistTcRnMessage $ tcRnLookupName hsc_env name
      -- ignore errors: the only error we're likely to get is
      -- "name not found", and the Maybe in the return type
      -- is used to indicate that.

hscTcRnGetInfo :: HscEnv -> Name
               -> IO (Maybe (TyThing, Fixity, [ClsInst], [FamInst], SDoc))
hscTcRnGetInfo hsc_env0 name
  = runInteractiveHsc hsc_env0 $
    do { hsc_env <- getHscEnv
       ; ioMsgMaybe' $ hoistTcRnMessage $ tcRnGetInfo hsc_env name }

hscIsGHCiMonad :: HscEnv -> String -> IO Name
hscIsGHCiMonad hsc_env name
  = runHsc hsc_env $ ioMsgMaybe $ hoistTcRnMessage $ isGHCiMonad hsc_env name

hscGetModuleInterface :: HscEnv -> Module -> IO ModIface
hscGetModuleInterface hsc_env0 mod = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe $ hoistTcRnMessage $ getModuleInterface hsc_env mod

-- -----------------------------------------------------------------------------
-- | Rename some import declarations
hscRnImportDecls :: HscEnv -> [LImportDecl GhcPs] -> IO GlobalRdrEnv
hscRnImportDecls hsc_env0 import_decls = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe $ hoistTcRnMessage $ tcRnImportDecls hsc_env import_decls

-- -----------------------------------------------------------------------------
-- | parse a file, returning the abstract syntax

hscParse :: HscEnv -> ModSummary -> IO HsParsedModule
hscParse hsc_env mod_summary = runHsc hsc_env $ hscParse' mod_summary

-- internal version, that doesn't fail due to -Werror
hscParse' :: ModSummary -> Hsc HsParsedModule
hscParse' mod_summary
 | Just r <- ms_parsed_mod mod_summary = return r
 | otherwise = do
    dflags <- getDynFlags
    logger <- getLogger
    {-# SCC "Parser" #-} withTiming logger
                (text "Parser"<+>brackets (ppr $ ms_mod mod_summary))
                (const ()) $ do
    let src_filename  = ms_hspp_file mod_summary
        maybe_src_buf = ms_hspp_buf  mod_summary

    --------------------------  Parser  ----------------
    -- sometimes we already have the buffer in memory, perhaps
    -- because we needed to parse the imports out of it, or get the
    -- module name.
    buf <- case maybe_src_buf of
               Just b  -> return b
               Nothing -> liftIO $ hGetStringBuffer src_filename

    let loc = mkRealSrcLoc (mkFastString src_filename) 1 1

    let diag_opts = initDiagOpts dflags
    when (wopt Opt_WarnUnicodeBidirectionalFormatCharacters dflags) $ do
      case checkBidirectionFormatChars (PsLoc loc (BufPos 0)) buf of
        Nothing -> pure ()
        Just chars@((eloc,chr,_) :| _) ->
          let span = mkSrcSpanPs $ mkPsSpan eloc (advancePsLoc eloc chr)
          in logDiagnostics $ singleMessage $
               mkPlainMsgEnvelope diag_opts span $
               GhcPsMessage $ PsWarnBidirectionalFormatChars chars

    let parseMod | HsigFile == ms_hsc_src mod_summary
                 = parseSignature
                 | otherwise = parseModule

    case unP parseMod (initParserState (initParserOpts dflags) buf loc) of
        PFailed pst ->
            handleWarningsThrowErrors (getPsMessages pst)
        POk pst rdr_module -> do
            liftIO $ putDumpFileMaybe logger Opt_D_dump_parsed "Parser"
                        FormatHaskell (ppr rdr_module)
            liftIO $ putDumpFileMaybe logger Opt_D_dump_parsed_ast "Parser AST"
                        FormatHaskell (showAstData NoBlankSrcSpan
                                                   NoBlankEpAnnotations
                                                   rdr_module)
            liftIO $ putDumpFileMaybe logger Opt_D_source_stats "Source Statistics"
                        FormatText (ppSourceStats False rdr_module)

            -- To get the list of extra source files, we take the list
            -- that the parser gave us,
            --   - eliminate files beginning with '<'.  gcc likes to use
            --     pseudo-filenames like "<built-in>" and "<command-line>"
            --   - normalise them (eliminate differences between ./f and f)
            --   - filter out the preprocessed source file
            --   - filter out anything beginning with tmpdir
            --   - remove duplicates
            --   - filter out the .hs/.lhs source filename if we have one
            --
            let n_hspp  = FilePath.normalise src_filename
                TempDir tmp_dir = tmpDir dflags
                srcs0 = nub $ filter (not . (tmp_dir `isPrefixOf`))
                            $ filter (not . (== n_hspp))
                            $ map FilePath.normalise
                            $ filter (not . isPrefixOf "<")
                            $ map unpackFS
                            $ srcfiles pst
                srcs1 = case ml_hs_file (ms_location mod_summary) of
                          Just f  -> filter (/= FilePath.normalise f) srcs0
                          Nothing -> srcs0

            -- sometimes we see source files from earlier
            -- preprocessing stages that cannot be found, so just
            -- filter them out:
            srcs2 <- liftIO $ filterM doesFileExist srcs1

            let res = HsParsedModule {
                      hpm_module    = rdr_module,
                      hpm_src_files = srcs2
                   }

            -- apply parse transformation of plugins
            let applyPluginAction p opts
                  = parsedResultAction p opts mod_summary
            hsc_env <- getHscEnv
            (ParsedResult transformed (PsMessages warns errs)) <-
              withPlugins (hsc_plugins hsc_env) applyPluginAction
                (ParsedResult res (uncurry PsMessages $ getPsMessages pst))

            logDiagnostics (GhcPsMessage <$> warns)
            unless (isEmptyMessages errs) $ throwErrors (GhcPsMessage <$> errs)

            return transformed

checkBidirectionFormatChars :: PsLoc -> StringBuffer -> Maybe (NonEmpty (PsLoc, Char, String))
checkBidirectionFormatChars start_loc sb
  | containsBidirectionalFormatChar sb = Just $ go start_loc sb
  | otherwise = Nothing
  where
    go :: PsLoc -> StringBuffer -> NonEmpty (PsLoc, Char, String)
    go loc sb
      | atEnd sb = panic "checkBidirectionFormatChars: no char found"
      | otherwise = case nextChar sb of
          (chr, sb)
            | Just desc <- lookup chr bidirectionalFormatChars ->
                (loc, chr, desc) :| go1 (advancePsLoc loc chr) sb
            | otherwise -> go (advancePsLoc loc chr) sb

    go1 :: PsLoc -> StringBuffer -> [(PsLoc, Char, String)]
    go1 loc sb
      | atEnd sb = []
      | otherwise = case nextChar sb of
          (chr, sb)
            | Just desc <- lookup chr bidirectionalFormatChars ->
                (loc, chr, desc) : go1 (advancePsLoc loc chr) sb
            | otherwise -> go1 (advancePsLoc loc chr) sb


-- -----------------------------------------------------------------------------
-- | If the renamed source has been kept, extract it. Dump it if requested.


extract_renamed_stuff :: ModSummary -> TcGblEnv -> Hsc RenamedStuff
extract_renamed_stuff mod_summary tc_result = do
    let rn_info = getRenamedStuff tc_result

    dflags <- getDynFlags
    logger <- getLogger
    liftIO $ putDumpFileMaybe logger Opt_D_dump_rn_ast "Renamer"
                FormatHaskell (showAstData NoBlankSrcSpan NoBlankEpAnnotations rn_info)

    -- Create HIE files
    when (gopt Opt_WriteHie dflags) $ do
        -- I assume this fromJust is safe because `-fwrite-hie-file`
        -- enables the option which keeps the renamed source.
        hieFile <- mkHieFile mod_summary tc_result (fromJust rn_info)
        let out_file = ml_hie_file $ ms_location mod_summary
        liftIO $ writeHieFile out_file hieFile
        liftIO $ putDumpFileMaybe logger Opt_D_dump_hie "HIE AST" FormatHaskell (ppr $ hie_asts hieFile)

        -- Validate HIE files
        when (gopt Opt_ValidateHie dflags) $ do
            hs_env <- getHscEnv
            liftIO $ do
              -- Validate Scopes
              case validateScopes (hie_module hieFile) $ getAsts $ hie_asts hieFile of
                  [] -> putMsg logger $ text "Got valid scopes"
                  xs -> do
                    putMsg logger $ text "Got invalid scopes"
                    mapM_ (putMsg logger) xs
              -- Roundtrip testing
              file' <- readHieFile (hsc_NC hs_env) out_file
              case diffFile hieFile (hie_file_result file') of
                [] ->
                  putMsg logger $ text "Got no roundtrip errors"
                xs -> do
                  putMsg logger $ text "Got roundtrip errors"
                  let logger' = updateLogFlags logger (log_set_dopt Opt_D_ppr_debug)
                  mapM_ (putMsg logger') xs
    return rn_info


-- -----------------------------------------------------------------------------
-- | Rename and typecheck a module, additionally returning the renamed syntax
hscTypecheckRename :: HscEnv -> ModSummary -> HsParsedModule
                   -> IO (TcGblEnv, RenamedStuff)
hscTypecheckRename hsc_env mod_summary rdr_module =
    fst <$> hscTypecheckRenameWithDiagnostics hsc_env mod_summary rdr_module

-- | Rename and typecheck a module, additionally returning the renamed syntax
-- and the diagnostics produced.
hscTypecheckRenameWithDiagnostics :: HscEnv -> ModSummary -> HsParsedModule
                                  -> IO ((TcGblEnv, RenamedStuff), Messages GhcMessage)
hscTypecheckRenameWithDiagnostics hsc_env mod_summary rdr_module = runHsc' hsc_env $
    hsc_typecheck True mod_summary (Just rdr_module)

-- | Do Typechecking without throwing SourceError exception with -Werror
hscTypecheckAndGetWarnings :: HscEnv ->  ModSummary -> IO (FrontendResult, WarningMessages)
hscTypecheckAndGetWarnings hsc_env summary = runHsc' hsc_env $ do
  case hscFrontendHook (hsc_hooks hsc_env) of
    Nothing -> FrontendTypecheck . fst <$> hsc_typecheck False summary Nothing
    Just h  -> h summary

-- | A bunch of logic piled around @tcRnModule'@, concerning a) backpack
-- b) concerning dumping rename info and hie files. It would be nice to further
-- separate this stuff out, probably in conjunction better separating renaming
-- and type checking (#17781).
hsc_typecheck :: Bool -- ^ Keep renamed source?
              -> ModSummary -> Maybe HsParsedModule
              -> Hsc (TcGblEnv, RenamedStuff)
hsc_typecheck keep_rn mod_summary mb_rdr_module = do
    hsc_env <- getHscEnv
    let hsc_src = ms_hsc_src mod_summary
        dflags = hsc_dflags hsc_env
        home_unit = hsc_home_unit hsc_env
        outer_mod = ms_mod mod_summary
        mod_name = moduleName outer_mod
        outer_mod' = mkHomeModule home_unit mod_name
        inner_mod = homeModuleNameInstantiation home_unit mod_name
        src_filename  = ms_hspp_file mod_summary
        real_loc = realSrcLocSpan $ mkRealSrcLoc (mkFastString src_filename) 1 1
        keep_rn' = gopt Opt_WriteHie dflags || keep_rn
    massert (isHomeModule home_unit outer_mod)
    tc_result <- if hsc_src == HsigFile && not (isHoleModule inner_mod)
        then ioMsgMaybe $ hoistTcRnMessage $ tcRnInstantiateSignature hsc_env outer_mod' real_loc
        else
         do hpm <- case mb_rdr_module of
                    Just hpm -> return hpm
                    Nothing -> hscParse' mod_summary
            tc_result0 <- tcRnModule' mod_summary keep_rn' hpm
            if hsc_src == HsigFile
                then do (iface, _) <- liftIO $ hscSimpleIface hsc_env Nothing tc_result0 mod_summary
                        ioMsgMaybe $ hoistTcRnMessage $
                            tcRnMergeSignatures hsc_env hpm tc_result0 iface
                else return tc_result0
    -- TODO are we extracting anything when we merely instantiate a signature?
    -- If not, try to move this into the "else" case above.
    rn_info <- extract_renamed_stuff mod_summary tc_result
    return (tc_result, rn_info)

-- wrapper around tcRnModule to handle safe haskell extras
tcRnModule' :: ModSummary -> Bool -> HsParsedModule
            -> Hsc TcGblEnv
tcRnModule' sum save_rn_syntax mod = do
    hsc_env <- getHscEnv
    dflags  <- getDynFlags

    let diag_opts = initDiagOpts dflags
    -- -Wmissing-safe-haskell-mode
    when (not (safeHaskellModeEnabled dflags)
          && wopt Opt_WarnMissingSafeHaskellMode dflags) $
        logDiagnostics $ singleMessage $
        mkPlainMsgEnvelope diag_opts (getLoc (hpm_module mod)) $
        GhcDriverMessage $ DriverMissingSafeHaskellMode (ms_mod sum)

    tcg_res <- {-# SCC "Typecheck-Rename" #-}
               ioMsgMaybe $ hoistTcRnMessage $
                   tcRnModule hsc_env sum
                     save_rn_syntax mod

    -- See Note [Safe Haskell Overlapping Instances Implementation]
    -- although this is used for more than just that failure case.
    tcSafeOK <- liftIO $ readIORef (tcg_safe_infer tcg_res)
    whyUnsafe <- liftIO $ readIORef (tcg_safe_infer_reasons tcg_res)
    let allSafeOK = safeInferred dflags && tcSafeOK

    -- end of the safe haskell line, how to respond to user?
    if not (safeHaskellOn dflags)
         || (safeInferOn dflags && not allSafeOK)
      -- if safe Haskell off or safe infer failed, mark unsafe
      then markUnsafeInfer tcg_res whyUnsafe

      -- module (could be) safe, throw warning if needed
      else do
          tcg_res' <- hscCheckSafeImports tcg_res
          safe <- liftIO $ readIORef (tcg_safe_infer tcg_res')
          when safe $
            case wopt Opt_WarnSafe dflags of
              True
                | safeHaskell dflags == Sf_Safe -> return ()
                | otherwise -> (logDiagnostics $ singleMessage $
                       mkPlainMsgEnvelope diag_opts (warnSafeOnLoc dflags) $
                       GhcDriverMessage $ DriverInferredSafeModule (tcg_mod tcg_res'))
              False | safeHaskell dflags == Sf_Trustworthy &&
                      wopt Opt_WarnTrustworthySafe dflags ->
                      (logDiagnostics $ singleMessage $
                       mkPlainMsgEnvelope diag_opts (trustworthyOnLoc dflags) $
                       GhcDriverMessage $ DriverMarkedTrustworthyButInferredSafe (tcg_mod tcg_res'))
              False -> return ()
          return tcg_res'

-- | Convert a typechecked module to Core
hscDesugar :: HscEnv -> ModSummary -> TcGblEnv -> IO ModGuts
hscDesugar hsc_env mod_summary tc_result =
    runHsc hsc_env $ hscDesugar' (ms_location mod_summary) tc_result

hscDesugar' :: ModLocation -> TcGblEnv -> Hsc ModGuts
hscDesugar' mod_location tc_result = do
    hsc_env <- getHscEnv
    ioMsgMaybe $ hoistDsMessage $
      {-# SCC "deSugar" #-}
      deSugar hsc_env mod_location tc_result

-- | Make a 'ModDetails' from the results of typechecking. Used when
-- typechecking only, as opposed to full compilation.
makeSimpleDetails :: Logger -> TcGblEnv -> IO ModDetails
makeSimpleDetails logger tc_result = mkBootModDetailsTc logger tc_result


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
files to either byte-code, hard-code (C, asm, LLVM, etc.) or to nothing at all
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


type Messager = HscEnv -> (Int,Int) -> RecompileRequired -> ModuleGraphNode -> IO ()

-- | Do the recompilation avoidance checks for both one-shot and --make modes
-- This function is the *only* place in the compiler where we decide whether to
-- recompile a module or not!
hscRecompStatus :: Maybe Messager
                -> HscEnv
                -> ModSummary
                -> Maybe ModIface
                -> HomeModLinkable
                -> (Int,Int)
                -> IO HscRecompStatus
hscRecompStatus
    mHscMessage hsc_env mod_summary mb_old_iface old_linkable mod_index
  = do
    let
        msg what = case mHscMessage of
          Just hscMessage -> hscMessage hsc_env mod_index what (ModuleNode [] mod_summary)
          Nothing -> return ()

    -- First check to see if the interface file agrees with the
    -- source file.
    --
    -- Save the interface that comes back from checkOldIface.
    -- In one-shot mode we don't have the old iface until this
    -- point, when checkOldIface reads it from the disk.
    recomp_if_result
          <- {-# SCC "checkOldIface" #-}
             liftIO $ checkOldIface hsc_env mod_summary mb_old_iface
    case recomp_if_result of
      OutOfDateItem reason mb_checked_iface -> do
        msg $ NeedsRecompile reason
        return $ HscRecompNeeded $ fmap (mi_iface_hash . mi_final_exts) mb_checked_iface
      UpToDateItem checked_iface -> do
        let lcl_dflags = ms_hspp_opts mod_summary
        if | not (backendGeneratesCode (backend lcl_dflags)) -> do
               -- No need for a linkable, we're good to go
               msg UpToDate
               return $ HscUpToDate checked_iface emptyHomeModInfoLinkable
           | not (backendGeneratesCodeForHsBoot (backend lcl_dflags))
           , IsBoot <- isBootSummary mod_summary -> do
               msg UpToDate
               return $ HscUpToDate checked_iface emptyHomeModInfoLinkable

           -- Always recompile with the JS backend when TH is enabled until
           -- #23013 is fixed.
           | ArchJavaScript <- platformArch (targetPlatform lcl_dflags)
           , xopt LangExt.TemplateHaskell lcl_dflags
           -> do
              msg $ needsRecompileBecause THWithJS
              return $ HscRecompNeeded $ Just $ mi_iface_hash $ mi_final_exts $ checked_iface

           | otherwise -> do
               -- Do need linkable
               -- 1. Just check whether we have bytecode/object linkables and then
               -- we will decide if we need them or not.
               bc_linkable <- checkByteCode checked_iface mod_summary (homeMod_bytecode old_linkable)
               obj_linkable <- liftIO $ checkObjects lcl_dflags (homeMod_object old_linkable) mod_summary
               trace_if (hsc_logger hsc_env) (vcat [text "BCO linkable", nest 2 (ppr bc_linkable), text "Object Linkable", ppr obj_linkable])

               let just_bc = justBytecode <$> bc_linkable
                   just_o  = justObjects  <$> obj_linkable
                   _maybe_both_os = case (bc_linkable, obj_linkable) of
                               (UpToDateItem bc, UpToDateItem o) -> UpToDateItem (bytecodeAndObjects bc o)
                               -- If missing object code, just say we need to recompile because of object code.
                               (_, OutOfDateItem reason _) -> OutOfDateItem reason Nothing
                               -- If just missing byte code, just use the object code
                               -- so you should use -fprefer-byte-code with -fwrite-if-simplified-core or you'll
                               -- end up using bytecode on recompilation
                               (_, UpToDateItem {} ) -> just_o

                   definitely_both_os = case (bc_linkable, obj_linkable) of
                               (UpToDateItem bc, UpToDateItem o) -> UpToDateItem (bytecodeAndObjects bc o)
                               -- If missing object code, just say we need to recompile because of object code.
                               (_, OutOfDateItem reason _) -> OutOfDateItem reason Nothing
                               -- If just missing byte code, just use the object code
                               -- so you should use -fprefer-byte-code with -fwrite-if-simplified-core or you'll
                               -- end up using bytecode on recompilation
                               (OutOfDateItem reason _,  _ ) -> OutOfDateItem reason Nothing

--               pprTraceM "recomp" (ppr just_bc <+> ppr just_o)
               -- 2. Decide which of the products we will need
               let recomp_linkable_result = case () of
                     _ | backendCanReuseLoadedCode (backend lcl_dflags) ->
                           case bc_linkable of
                             -- If bytecode is available for Interactive then don't load object code
                             UpToDateItem _ -> just_bc
                             _ -> case obj_linkable of
                                     -- If o is availabe, then just use that
                                     UpToDateItem _ -> just_o
                                     _ -> outOfDateItemBecause MissingBytecode Nothing
                        -- Need object files for making object files
                        | backendWritesFiles (backend lcl_dflags) ->
                           if gopt Opt_ByteCodeAndObjectCode lcl_dflags
                             -- We say we are going to write both, so recompile unless we have both
                             then definitely_both_os
                             -- Only load the object file unless we are saying we need to produce both.
                             -- Unless we do this then you can end up using byte-code for a module you specify -fobject-code for.
                             else just_o
                        | otherwise -> pprPanic "hscRecompStatus" (text $ show $ backend lcl_dflags)
               case recomp_linkable_result of
                 UpToDateItem linkable -> do
                   msg $ UpToDate
                   return $ HscUpToDate checked_iface $ linkable
                 OutOfDateItem reason _ -> do
                   msg $ NeedsRecompile reason
                   return $ HscRecompNeeded $ Just $ mi_iface_hash $ mi_final_exts $ checked_iface

-- | Check that the .o files produced by compilation are already up-to-date
-- or not.
checkObjects :: DynFlags -> Maybe Linkable -> ModSummary -> IO (MaybeValidated Linkable)
checkObjects dflags mb_old_linkable summary = do
  let
    dt_enabled  = gopt Opt_BuildDynamicToo dflags
    this_mod    = ms_mod summary
    mb_obj_date = ms_obj_date summary
    mb_dyn_obj_date = ms_dyn_obj_date summary
    mb_if_date  = ms_iface_date summary
    obj_fn      = ml_obj_file (ms_location summary)
    -- dynamic-too *also* produces the dyn_o_file, so have to check
    -- that's there, and if it's not, regenerate both .o and
    -- .dyn_o
    checkDynamicObj k = if dt_enabled
      then case (>=) <$> mb_dyn_obj_date <*> mb_if_date of
        Just True -> k
        _ -> return $ outOfDateItemBecause MissingDynObjectFile Nothing
      -- Not in dynamic-too mode
      else k

  checkDynamicObj $
    case (,) <$> mb_obj_date <*> mb_if_date of
      Just (obj_date, if_date)
        | obj_date >= if_date ->
            case mb_old_linkable of
              Just old_linkable
                | linkableIsNativeCodeOnly old_linkable, linkableTime old_linkable == obj_date
                -> return $ UpToDateItem old_linkable
              _ -> UpToDateItem <$> findObjectLinkable this_mod obj_fn obj_date
      _ -> return $ outOfDateItemBecause MissingObjectFile Nothing

-- | Check to see if we can reuse the old linkable, by this point we will
-- have just checked that the old interface matches up with the source hash, so
-- no need to check that again here
checkByteCode :: ModIface -> ModSummary -> Maybe Linkable -> IO (MaybeValidated Linkable)
checkByteCode iface mod_sum mb_old_linkable =
  case mb_old_linkable of
    Just old_linkable
      | not (linkableIsNativeCodeOnly old_linkable)
      -> return $ (UpToDateItem old_linkable)
    _ -> loadByteCode iface mod_sum

loadByteCode :: ModIface -> ModSummary -> IO (MaybeValidated Linkable)
loadByteCode iface mod_sum = do
    let
      this_mod   = ms_mod mod_sum
      if_date    = fromJust $ ms_iface_date mod_sum
    case mi_extra_decls iface of
      Just extra_decls -> do
          let fi = WholeCoreBindings extra_decls this_mod (ms_location mod_sum)
                   (mi_foreign iface)
          return (UpToDateItem (Linkable if_date this_mod (NE.singleton (CoreBindings fi))))
      _ -> return $ outOfDateItemBecause MissingBytecode Nothing

--------------------------------------------------------------
-- Compilers
--------------------------------------------------------------

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

-- | Modify flags such that objects are compiled for the interpreter's way.
-- This is necessary when building foreign objects for Template Haskell, since
-- those are object code built outside of the pipeline, which means they aren't
-- subject to the mechanism in 'enableCodeGenWhen' that requests dynamic build
-- outputs for dependencies when the interpreter used for TH is dynamic but the
-- main outputs aren't.
-- Furthermore, the HPT only stores one set of objects with different names for
-- bytecode linking in 'HomeModLinkable', so the usual hack for switching
-- between ways in 'get_link_deps' doesn't work.
compile_for_interpreter :: HscEnv -> (HscEnv -> IO a) -> IO a
compile_for_interpreter hsc_env use =
  use (hscUpdateFlags update hsc_env)
  where
    update dflags = dflags {
      targetWays_ = adapt_way interpreterDynamic WayDyn $
                    adapt_way interpreterProfiled WayProf $
                    targetWays_ dflags
      }

    adapt_way want = if want (hscInterp hsc_env) then addWay else removeWay

-- | Assemble 'WholeCoreBindings' if the interface contains Core bindings.
iface_core_bindings :: ModIface -> ModLocation -> Maybe WholeCoreBindings
iface_core_bindings iface wcb_mod_location =
  mi_extra_decls <&> \ wcb_bindings ->
    WholeCoreBindings {
      wcb_bindings,
      wcb_module = mi_module,
      wcb_mod_location,
      wcb_foreign = mi_foreign
    }
  where
    ModIface {mi_module, mi_extra_decls, mi_foreign} = iface

-- | Return an 'IO' that hydrates Core bindings and compiles them to bytecode if
-- the interface contains any, using the supplied type env for typechecking.
--
-- Unlike 'initWholeCoreBindings', this does not use lazy IO.
-- Instead, the 'IO' is only evaluated (in @get_link_deps@) when it is clear
-- that it will be used immediately (because we're linking TH with
-- @-fprefer-byte-code@ in oneshot mode), and the result is cached in
-- 'LoaderState'.
--
-- 'initWholeCoreBindings' needs the laziness because it is used to populate
-- 'HomeModInfo', which is done preemptively, in anticipation of downstream
-- modules using the bytecode for TH in make mode, which might never happen.
loadIfaceByteCode ::
  HscEnv ->
  ModIface ->
  ModLocation ->
  TypeEnv ->
  Maybe (IO Linkable)
loadIfaceByteCode hsc_env iface location type_env =
  compile <$> iface_core_bindings iface location
  where
    compile decls = do
      (bcos, fos) <- compileWholeCoreBindings hsc_env type_env decls
      linkable $ BCOs bcos :| [DotO fo ForeignObject | fo <- fos]

    linkable parts = do
      if_time <- modificationTimeIfExists (ml_hi_file location)
      time <- maybe getCurrentTime pure if_time
      return $! Linkable time (mi_module iface) parts

-- | If the 'Linkable' contains Core bindings loaded from an interface, replace
-- them with a lazy IO thunk that compiles them to bytecode and foreign objects,
-- using the supplied environment for type checking.
--
-- The laziness is necessary because this value is stored purely in a
-- 'HomeModLinkable' in the home package table, rather than some dedicated
-- mutable state that would generate bytecode on demand, so we have to call this
-- function even when we don't know that we'll need the bytecode.
--
-- In addition, the laziness has to be hidden inside 'LazyBCOs' because
-- 'Linkable' is used too generally, so that looking at the constructor to
-- decide whether to discard it when linking native code would force the thunk
-- otherwise, incurring a significant performance penalty.
--
-- This is sound because generateByteCode just depends on things already loaded
-- in the interface file.
initWholeCoreBindings ::
  HscEnv ->
  ModIface ->
  ModDetails ->
  Linkable ->
  IO Linkable
initWholeCoreBindings hsc_env iface details (Linkable utc_time this_mod uls) = do
  Linkable utc_time this_mod <$> mapM (go hsc_env) uls
  where
    go hsc_env' = \case
      CoreBindings wcb -> do
        add_iface_to_hpt iface details hsc_env
        ~(bco, fos) <- unsafeInterleaveIO $
                       compileWholeCoreBindings hsc_env' type_env wcb
        pure (LazyBCOs bco fos)
      l -> pure l

    type_env = md_types details

-- | Hydrate interface Core bindings and compile them to bytecode.
--
-- This consists of:
--
-- 1. Running a typechecking step to insert the global names that were removed
--    when the interface was written or were unavailable due to boot import
--    cycles, converting the bindings to 'CoreBind'.
--
-- 2. Restoring the foreign build inputs from their serialized format, resulting
--    in a set of foreign import stubs and source files added via
--    'qAddForeignFilePath'.
--
-- 3. Generating bytecode and foreign objects from the results of the previous
--    steps using the usual pipeline actions.
compileWholeCoreBindings ::
  HscEnv ->
  TypeEnv ->
  WholeCoreBindings ->
  IO (CompiledByteCode, [FilePath])
compileWholeCoreBindings hsc_env type_env wcb = do
  core_binds <- typecheck
  (stubs, foreign_files) <- decode_foreign
  gen_bytecode core_binds stubs foreign_files
  where
    typecheck = do
      types_var <- newIORef type_env
      let
        tc_env = hsc_env {
          hsc_type_env_vars =
            knotVarsFromModuleEnv (mkModuleEnv [(wcb_module, types_var)])
        }
      initIfaceCheck (text "l") tc_env $
        typecheckWholeCoreBindings types_var wcb

    decode_foreign =
      decodeIfaceForeign logger (hsc_tmpfs hsc_env)
      (tmpDir (hsc_dflags hsc_env)) wcb_foreign

    gen_bytecode core_binds stubs foreign_files = do
      let cgi_guts = CgInteractiveGuts wcb_module core_binds
                      (typeEnvTyCons type_env) stubs foreign_files
                      Nothing []
      trace_if logger (text "Generating ByteCode for" <+> ppr wcb_module)
      generateByteCode hsc_env cgi_guts wcb_mod_location

    WholeCoreBindings {wcb_module, wcb_mod_location, wcb_foreign} = wcb

    logger = hsc_logger hsc_env

{-
Note [ModDetails and --make mode]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An interface file consists of two parts

* The `ModIface` which ends up getting written to disk.
  The `ModIface` is a completely acyclic tree, which can be serialised
  and de-serialised completely straightforwardly.  The `ModIface` is
  also the structure that is finger-printed for recompilation control.

* The `ModDetails` which provides a more structured view that is suitable
  for usage during compilation.  The `ModDetails` is heavily cyclic:
  An `Id` contains a `Type`, which mentions a `TyCon` that contains kind
  that mentions other `TyCons`; the `Id` also includes an unfolding that
  in turn mentions more `Id`s;  And so on.

The `ModIface` can be created from the `ModDetails` and the `ModDetails` from
a `ModIface`.

During tidying, just before interfaces are written to disk,
the ModDetails is calculated and then converted into a ModIface (see GHC.Iface.Make.mkIface_).
Then when GHC needs to restart typechecking from a certain point it can read the
interface file, and regenerate the ModDetails from the ModIface (see GHC.IfaceToCore.typecheckIface).
The key part about the loading is that the ModDetails is regenerated lazily
from the ModIface, so that there's only a detailed in-memory representation
for declarations which are actually used from the interface. This mode is
also used when reading interface files from external packages.

In the old --make mode implementation, the interface was written after compiling a module
but the in-memory ModDetails which was used to compute the ModIface was retained.
The result was that --make mode used much more memory than `-c` mode, because a large amount of
information about a module would be kept in the ModDetails but never used.

The new idea is that even in `--make` mode, when there is an in-memory `ModDetails`
at hand, we re-create the `ModDetails` from the `ModIface`. Doing this means that
we only have to keep the `ModIface` decls in memory and then lazily load
detailed representations if needed. It turns out this makes a really big difference
to memory usage, halving maximum memory used in some cases.

See !5492 and #13586
-}

-- Runs the post-typechecking frontend (desugar and simplify). We want to
-- generate most of the interface as late as possible. This gets us up-to-date
-- and good unfoldings and other info in the interface file.
--
-- We might create a interface right away, in which case we also return the
-- updated HomeModInfo. But we might also need to run the backend first. In the
-- later case Status will be HscRecomp and we return a function from ModIface ->
-- HomeModInfo.
--
-- HscRecomp in turn will carry the information required to compute a interface
-- when passed the result of the code generator. So all this can and is done at
-- the call site of the backend code gen if it is run.
hscDesugarAndSimplify :: ModSummary
       -> FrontendResult
       -> Messages GhcMessage
       -> Maybe Fingerprint
       -> Hsc HscBackendAction
hscDesugarAndSimplify summary (FrontendTypecheck tc_result) tc_warnings mb_old_hash = do
  hsc_env <- getHscEnv
  dflags <- getDynFlags
  logger <- getLogger
  let bcknd  = backend dflags
      hsc_src = ms_hsc_src summary
      diag_opts = initDiagOpts dflags
      print_config = initPrintConfig dflags

  -- Desugar, if appropriate
  --
  -- We usually desugar even when we are not generating code, otherwise we
  -- would miss errors thrown by the desugaring (see #10600). The only
  -- exception is when it is not a HsSrcFile module.
  mb_desugar <- if
    | hsc_src /= HsSrcFile       -> pure Nothing
    -- Desugar an empty ghc-prim:GHC.Prim module by filtering out all its
    -- bindings: the reason is that some of them are invalid (such as top-level
    -- unlifted ones like void# or proxy#) and cause HsToCore failures.
    --
    -- We still need to desugar *something* because the driver and the linkers
    -- expect a valid object file (.o) to be generated for this module.
    | ms_mod summary == gHC_PRIM -> Just <$> hscDesugar' (ms_location summary) (tc_result { tcg_binds = [] })
    | otherwise                  -> Just <$> hscDesugar' (ms_location summary) tc_result

  -- Report the warnings from both typechecking and desugar together
  w <- getDiagnostics
  liftIO $ printOrThrowDiagnostics logger print_config diag_opts (unionMessages tc_warnings w)
  clearDiagnostics

  -- Simplify, if appropriate, and (whether we simplified or not) generate an
  -- interface file.
  case mb_desugar of
      -- Just cause we desugared doesn't mean we are generating code, see above.
      Just desugared_guts | backendGeneratesCode bcknd -> do
          plugins <- liftIO $ readIORef (tcg_th_coreplugins tc_result)
          simplified_guts <- hscSimplify' plugins desugared_guts

          (cg_guts, details) <-
              liftIO $ hscTidy hsc_env simplified_guts

          let !partial_iface =
                {-# SCC "GHC.Driver.Main.mkPartialIface" #-}
                -- This `force` saves 2M residency in test T10370
                -- See Note [Avoiding space leaks in toIface*] for details.
                force (mkPartialIface hsc_env (cg_binds cg_guts) details summary (tcg_import_decls tc_result) simplified_guts)

          return HscRecomp { hscs_guts = cg_guts,
                             hscs_mod_location = ms_location summary,
                             hscs_partial_iface = partial_iface,
                             hscs_old_iface_hash = mb_old_hash
                           }

      Just desugared_guts | gopt Opt_WriteIfSimplifiedCore dflags -> do
          -- If -fno-code is enabled (hence we fall through to this case)
          -- Running the simplifier once is necessary before doing byte code generation
          -- in order to inline data con wrappers but we honour whatever level of simplificication the
          -- user requested. See #22008 for some discussion.
          plugins <- liftIO $ readIORef (tcg_th_coreplugins tc_result)
          simplified_guts <- hscSimplify' plugins desugared_guts
          (cg_guts, _) <-
              liftIO $ hscTidy hsc_env simplified_guts

          (iface, _details) <- liftIO $
            hscSimpleIface hsc_env (Just $ cg_binds cg_guts) tc_result summary

          liftIO $ hscMaybeWriteIface logger dflags True iface mb_old_hash (ms_location summary)

          -- when compiling gHC_PRIM without generating code (e.g. with
          -- Haddock), we still want the virtual interface in the cache
          if ms_mod summary == gHC_PRIM
            then return $ HscUpdate (getGhcPrimIface hsc_env)
            else return $ HscUpdate iface


      -- We are not generating code or writing an interface with simplified core so we can skip simplification
      -- and generate a simple interface.
      _ -> do
        (iface, _details) <- liftIO $
          hscSimpleIface hsc_env Nothing tc_result summary

        liftIO $ hscMaybeWriteIface logger dflags True iface mb_old_hash (ms_location summary)

        -- when compiling gHC_PRIM without generating code (e.g. with
        -- Haddock), we still want the virtual interface in the cache
        if ms_mod summary == gHC_PRIM
          then return $ HscUpdate (getGhcPrimIface hsc_env)
          else return $ HscUpdate iface

{-
Note [Writing interface files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We write one interface file per module and per compilation, except with
-dynamic-too where we write two interface files (non-dynamic and dynamic).

We can write two kinds of interfaces (see Note [Interface file stages] in
"GHC.Driver.Types"):

   * simple interface: interface generated after the core pipeline

   * full interface: simple interface completed with information from the
     backend

Depending on the situation, we write one or the other (using
`hscMaybeWriteIface`). We must be careful with `-dynamic-too` because only the
backend is run twice, so if we write a simple interface we need to write both
the non-dynamic and the dynamic interfaces at the same time (with the same
contents).

Cases for which we generate simple interfaces:

   * GHC.Driver.Main.hscDesugarAndSimplify: when a compilation does NOT require (re)compilation
   of the hard code

   * GHC.Driver.Pipeline.compileOne': when we run in One Shot mode and target
   bytecode (if interface writing is forced).

   * GHC.Driver.Backpack uses simple interfaces for indefinite units
   (units with module holes). It writes them indirectly by forcing the
   -fwrite-interface flag while setting backend to NoBackend.

Cases for which we generate full interfaces:

   * GHC.Driver.Pipeline.runPhase: when we must be compiling to regular hard
   code and/or require recompilation.

By default interface file names are derived from module file names by adding
suffixes. The interface file name can be overloaded with "-ohi", except when
`-dynamic-too` is used.

-}

-- | Write interface files
hscMaybeWriteIface
  :: Logger
  -> DynFlags
  -> Bool
  -- ^ Is this a simple interface generated after the core pipeline, or one
  -- with information from the backend? See: Note [Writing interface files]
  -> ModIface
  -> Maybe Fingerprint
  -- ^ The old interface hash, used to decide if we need to actually write the
  -- new interface.
  -> ModLocation
  -> IO ()
hscMaybeWriteIface logger dflags is_simple iface old_iface mod_location = do
    let force_write_interface = gopt Opt_WriteInterface dflags
        write_interface = backendWritesFiles (backend dflags)

        write_iface dflags' iface =
          let !iface_name = if dynamicNow dflags' then ml_dyn_hi_file mod_location else ml_hi_file mod_location
              profile     = targetProfile dflags'
          in
          {-# SCC "writeIface" #-}
          withTiming logger
              (text "WriteIface"<+>brackets (text iface_name))
              (const ())
              (writeIface logger profile (flagsToIfCompression dflags) iface_name iface)

    if (write_interface || force_write_interface) then do

      -- FIXME: with -dynamic-too, "change" is only meaningful for the
      -- non-dynamic interface, not for the dynamic one. We should have another
      -- flag for the dynamic interface. In the meantime:
      --
      --    * when we write a single full interface, we check if we are
      --    currently writing the dynamic interface due to -dynamic-too, in
      --    which case we ignore "change".
      --
      --    * when we write two simple interfaces at once because of
      --    dynamic-too, we use "change" both for the non-dynamic and the
      --    dynamic interfaces. Hopefully both the dynamic and the non-dynamic
      --    interfaces stay in sync...
      --
      let change = old_iface /= Just (mi_iface_hash (mi_final_exts iface))

      let dt = dynamicTooState dflags

      when (logHasDumpFlag logger Opt_D_dump_if_trace) $ putMsg logger $
        hang (text "Writing interface(s):") 2 $ vcat
         [ text "Kind:" <+> if is_simple then text "simple" else text "full"
         , text "Hash change:" <+> ppr change
         , text "DynamicToo state:" <+> text (show dt)
         ]

      if is_simple
         then when change $ do -- FIXME: see 'change' comment above
            write_iface dflags iface
            case dt of
               DT_Dont   -> return ()
               DT_Dyn    -> panic "Unexpected DT_Dyn state when writing simple interface"
               DT_OK     -> write_iface (setDynamicNow dflags) iface
         else case dt of
               DT_Dont | change                    -> write_iface dflags iface
               DT_OK   | change                    -> write_iface dflags iface
               -- FIXME: see change' comment above
               DT_Dyn                              -> write_iface dflags iface
               _                                   -> return ()

      when (gopt Opt_WriteHie dflags) $ do
          -- This is slightly hacky. A hie file is considered to be up to date
          -- if its modification time on disk is greater than or equal to that
          -- of the .hi file (since we should always write a .hi file if we are
          -- writing a .hie file). However, with the way this code is
          -- structured at the moment, the .hie file is often written before
          -- the .hi file; by touching the file here, we ensure that it is
          -- correctly considered up-to-date.
          --
          -- The file should exist by the time we get here, but we check for
          -- existence just in case, so that we don't accidentally create empty
          -- .hie files.
          let hie_file = ml_hie_file mod_location
          whenM (doesFileExist hie_file) $
            GHC.Utils.Touch.touch hie_file
    else
        -- See Note [Strictness in ModIface]
        forceModIface iface

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
-- Progress displayers.
--------------------------------------------------------------

oneShotMsg :: Logger -> RecompileRequired -> IO ()
oneShotMsg logger recomp =
    case recomp of
        UpToDate -> compilationProgressMsg logger $ text "compilation IS NOT required"
        NeedsRecompile _ -> return ()

batchMsg :: Messager
batchMsg = batchMsgWith (\_ _ _ _ -> empty)
batchMultiMsg :: Messager
batchMultiMsg = batchMsgWith (\_ _ _ node -> brackets (ppr (mgNodeUnitId node)))

batchMsgWith :: (HscEnv -> (Int, Int) -> RecompileRequired -> ModuleGraphNode -> SDoc) -> Messager
batchMsgWith extra hsc_env_start mod_index recomp node =
      case recomp of
        UpToDate
          | logVerbAtLeast logger 2 -> showMsg (text "Skipping") empty
          | otherwise -> return ()
        NeedsRecompile reason0 -> showMsg (text herald) $ case reason0 of
          MustCompile            -> empty
          (RecompBecause reason) -> text " [" <> pprWithUnitState state (ppr reason) <> text "]"
    where
        herald = case node of
                    LinkNode {} -> "Linking"
                    InstantiationNode {} -> "Instantiating"
                    ModuleNode {} -> "Compiling"
                    UnitNode {} -> "Loading"
        hsc_env = hscSetActiveUnitId (mgNodeUnitId node) hsc_env_start
        dflags = hsc_dflags hsc_env
        logger = hsc_logger hsc_env
        state  = hsc_units hsc_env
        showMsg msg reason =
            compilationProgressMsg logger $
            (showModuleIndex mod_index <>
            msg <+> showModMsg dflags (recompileRequired recomp) node)
                <> extra hsc_env mod_index recomp node
                <> reason

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
-- safe haskell mode flag. The basic approach to this is:
--   * When deciding if we need to do a Safe language check, treat
--     an unmarked module as having -XSafe mode specified.
--   * For checks, don't throw errors but return them to the caller.
--   * Caller checks if there are errors:
--     * For modules explicitly marked -XSafe, we throw the errors.
--     * For unmarked modules (inference mode), we drop the errors
--       and mark the module as being Unsafe.
--
-- It used to be that we only did safe inference on modules that had no Safe
-- Haskell flags, but now we perform safe inference on all modules as we want
-- to allow users to set the `-Wsafe`, `-Wunsafe` and
-- `-Wtrustworthy-safe` flags on Trustworthy and Unsafe modules so that a
-- user can ensure their assumptions are correct and see reasons for why a
-- module is safe or unsafe.
--
-- This is tricky as we must be careful when we should throw an error compared
-- to just warnings. For checking safe imports we manage it as two steps. First
-- we check any imports that are required to be safe, then we check all other
-- imports to see if we can infer them to be safe.


-- | Check that the safe imports of the module being compiled are valid.
-- If not we either issue a compilation error if the module is explicitly
-- using Safe Haskell, or mark the module as unsafe if we're in safe
-- inference mode.
hscCheckSafeImports :: TcGblEnv -> Hsc TcGblEnv
hscCheckSafeImports tcg_env = do
    dflags   <- getDynFlags
    tcg_env' <- checkSafeImports tcg_env
    checkRULES dflags tcg_env'

  where
    checkRULES dflags tcg_env' =
      let diag_opts = initDiagOpts dflags
      in case safeLanguageOn dflags of
          True -> do
              -- XSafe: we nuke user written RULES
              logDiagnostics $ fmap GhcDriverMessage $ warns diag_opts (tcg_rules tcg_env')
              return tcg_env' { tcg_rules = [] }
          False
                -- SafeInferred: user defined RULES, so not safe
              | safeInferOn dflags && not (null $ tcg_rules tcg_env')
              -> markUnsafeInfer tcg_env' $ warns diag_opts (tcg_rules tcg_env')

                -- Trustworthy OR SafeInferred: with no RULES
              | otherwise
              -> return tcg_env'

    warns diag_opts rules = mkMessages $ listToBag $ map (warnRules diag_opts) rules

    warnRules :: DiagOpts -> LRuleDecl GhcTc -> MsgEnvelope DriverMessage
    warnRules diag_opts (L loc rule) =
        mkPlainMsgEnvelope diag_opts (locA loc) $ DriverUserDefinedRuleIgnored rule

-- | Validate that safe imported modules are actually safe.  For modules in the
-- HomePackage (the package the module we are compiling in resides) this just
-- involves checking its trust type is 'Safe' or 'Trustworthy'. For modules
-- that reside in another package we also must check that the external package
-- is trusted. See the Note [Safe Haskell Trust Check] above for more
-- information.
--
-- The code for this is quite tricky as the whole algorithm is done in a few
-- distinct phases in different parts of the code base. See
-- 'GHC.Rename.Names.rnImportDecl' for where package trust dependencies for a
-- module are collected and unioned.  Specifically see the Note [Tracking Trust
-- Transitively] in "GHC.Rename.Names" and the Note [Trust Own Package] in
-- "GHC.Rename.Names".
checkSafeImports :: TcGblEnv -> Hsc TcGblEnv
checkSafeImports tcg_env
    = do
        dflags <- getDynFlags
        imps <- mapM condense imports'
        let (safeImps, regImps) = partition (\(_,_,s) -> s) imps

        -- We want to use the warning state specifically for detecting if safe
        -- inference has failed, so store and clear any existing warnings.
        oldErrs <- getDiagnostics
        clearDiagnostics

        -- Check safe imports are correct
        safePkgs <- S.fromList <$> mapMaybeM checkSafe safeImps
        safeErrs <- getDiagnostics
        clearDiagnostics

        -- Check non-safe imports are correct if inferring safety
        -- See the Note [Safe Haskell Inference]
        (infErrs, infPkgs) <- case (safeInferOn dflags) of
          False -> return (emptyMessages, S.empty)
          True -> do infPkgs <- S.fromList <$> mapMaybeM checkSafe regImps
                     infErrs <- getDiagnostics
                     clearDiagnostics
                     return (infErrs, infPkgs)

        -- restore old errors
        logDiagnostics oldErrs

        diag_opts <- initDiagOpts <$> getDynFlags
        print_config <- initPrintConfig <$> getDynFlags
        logger <- getLogger

        -- Will throw if failed safe check
        liftIO $ printOrThrowDiagnostics logger print_config diag_opts safeErrs

        -- No fatal warnings or errors: passed safe check
        let infPassed = isEmptyMessages infErrs
        tcg_env' <- case (not infPassed) of
          True  -> markUnsafeInfer tcg_env infErrs
          False -> return tcg_env
        when (packageTrustOn dflags) $ checkPkgTrust pkgReqs
        let newTrust = pkgTrustReqs dflags safePkgs infPkgs infPassed
        return tcg_env' { tcg_imports = impInfo `plusImportAvails` newTrust }

  where
    impInfo  = tcg_imports tcg_env     -- ImportAvails
    imports  = imp_mods impInfo        -- ImportedMods
    imports1 = M.toList imports -- (Module, [ImportedBy])
    imports' = map (fmap importedByUser) imports1 -- (Module, [ImportedModsVal])
    pkgReqs  = imp_trust_pkgs impInfo  -- [Unit]

    condense :: (Module, [ImportedModsVal]) -> Hsc (Module, SrcSpan, IsSafeImport)
    condense (_, [])   = panic "GHC.Driver.Main.condense: Pattern match failure!"
    condense (m, x:xs) = do imv <- foldlM cond' x xs
                            return (m, imv_span imv, imv_is_safe imv)

    -- ImportedModsVal = (ModuleName, Bool, SrcSpan, IsSafeImport)
    cond' :: ImportedModsVal -> ImportedModsVal -> Hsc ImportedModsVal
    cond' v1 v2
        | imv_is_safe v1 /= imv_is_safe v2
        = throwOneError $
            mkPlainErrorMsgEnvelope (imv_span v1) $
            GhcDriverMessage $ DriverMixedSafetyImport (imv_name v1)
        | otherwise
        = return v1

    -- easier interface to work with
    checkSafe :: (Module, SrcSpan, a) -> Hsc (Maybe UnitId)
    checkSafe (m, l, _) = fst `fmap` hscCheckSafe' m l

    -- what pkg's to add to our trust requirements
    pkgTrustReqs :: DynFlags -> Set UnitId -> Set UnitId ->
          Bool -> ImportAvails
    pkgTrustReqs dflags req inf infPassed | safeInferOn dflags
                                  && not (safeHaskellModeEnabled dflags) && infPassed
                                   = emptyImportAvails {
                                       imp_trust_pkgs = req `S.union` inf
                                   }
    pkgTrustReqs dflags _   _ _ | safeHaskell dflags == Sf_Unsafe
                         = emptyImportAvails
    pkgTrustReqs _ req _ _ = emptyImportAvails { imp_trust_pkgs = req }

-- | Check that a module is safe to import.
--
-- We return True to indicate the import is safe and False otherwise
-- although in the False case an exception may be thrown first.
hscCheckSafe :: HscEnv -> Module -> SrcSpan -> IO Bool
hscCheckSafe hsc_env m l = runHsc hsc_env $ do
    dflags <- getDynFlags
    pkgs <- snd `fmap` hscCheckSafe' m l
    when (packageTrustOn dflags) $ checkPkgTrust pkgs
    errs <- getDiagnostics
    return $ isEmptyMessages errs

-- | Return if a module is trusted and the pkgs it depends on to be trusted.
hscGetSafe :: HscEnv -> Module -> SrcSpan -> IO (Bool, Set UnitId)
hscGetSafe hsc_env m l = runHsc hsc_env $ do
    (self, pkgs) <- hscCheckSafe' m l
    good         <- isEmptyMessages `fmap` getDiagnostics
    clearDiagnostics -- don't want them printed...
    let pkgs' | Just p <- self = S.insert p pkgs
              | otherwise      = pkgs
    return (good, pkgs')

-- | Is a module trusted? If not, throw or log errors depending on the type.
-- Return (regardless of trusted or not) if the trust type requires the modules
-- own package be trusted and a list of other packages required to be trusted
-- (these later ones haven't been checked) but the own package trust has been.
hscCheckSafe' :: Module -> SrcSpan
  -> Hsc (Maybe UnitId, Set UnitId)
hscCheckSafe' m l = do
    hsc_env <- getHscEnv
    let home_unit = hsc_home_unit hsc_env
    (tw, pkgs) <- isModSafe home_unit m l
    case tw of
        False                           -> return (Nothing, pkgs)
        True | isHomeModule home_unit m -> return (Nothing, pkgs)
             -- TODO: do we also have to check the trust of the instantiation?
             -- Not necessary if that is reflected in dependencies
             | otherwise   -> return (Just $ toUnitId (moduleUnit m), pkgs)
  where
    isModSafe :: HomeUnit -> Module -> SrcSpan -> Hsc (Bool, Set UnitId)
    isModSafe home_unit m l = do
        hsc_env <- getHscEnv
        dflags <- getDynFlags
        iface <- lookup' m
        let diag_opts = initDiagOpts dflags
        case iface of
            -- can't load iface to check trust!
            Nothing -> throwOneError $
                         mkPlainErrorMsgEnvelope l $
                         GhcDriverMessage $ DriverCannotLoadInterfaceFile m

            -- got iface, check trust
            Just iface' ->
                let trust = getSafeMode $ mi_trust iface'
                    trust_own_pkg = mi_trust_pkg iface'
                    -- check module is trusted
                    safeM = trust `elem` [Sf_Safe, Sf_SafeInferred, Sf_Trustworthy]
                    -- check package is trusted
                    safeP = packageTrusted dflags (hsc_units hsc_env) home_unit trust trust_own_pkg m
                    -- pkg trust reqs
                    pkgRs = dep_trusted_pkgs $ mi_deps iface'
                    -- warn if Safe module imports Safe-Inferred module.
                    warns = if wopt Opt_WarnInferredSafeImports dflags
                                && safeLanguageOn dflags
                                && trust == Sf_SafeInferred
                                then inferredImportWarn diag_opts
                                else emptyMessages
                    -- General errors we throw but Safe errors we log
                    errs = case (safeM, safeP) of
                        (True, True ) -> emptyMessages
                        (True, False) -> pkgTrustErr
                        (False, _   ) -> modTrustErr
                in do
                    logDiagnostics warns
                    logDiagnostics errs
                    return (trust == Sf_Trustworthy, pkgRs)

                where
                    state = hsc_units hsc_env
                    inferredImportWarn diag_opts = singleMessage
                        $ mkMsgEnvelope diag_opts l (pkgQual state)
                        $ GhcDriverMessage $ DriverInferredSafeImport m
                    pkgTrustErr = singleMessage
                      $ mkErrorMsgEnvelope l (pkgQual state)
                      $ GhcDriverMessage $ DriverCannotImportFromUntrustedPackage state m
                    modTrustErr = singleMessage
                      $ mkErrorMsgEnvelope l (pkgQual state)
                      $ GhcDriverMessage $ DriverCannotImportUnsafeModule m

    -- Check the package a module resides in is trusted. Safe compiled
    -- modules are trusted without requiring that their package is trusted. For
    -- trustworthy modules, modules in the home package are trusted but
    -- otherwise we check the package trust flag.
    packageTrusted :: DynFlags -> UnitState -> HomeUnit -> SafeHaskellMode -> Bool -> Module -> Bool
    packageTrusted dflags unit_state home_unit safe_mode trust_own_pkg mod =
        case safe_mode of
            Sf_None      -> False -- shouldn't hit these cases
            Sf_Ignore    -> False -- shouldn't hit these cases
            Sf_Unsafe    -> False -- prefer for completeness.
            _ | not (packageTrustOn dflags)     -> True
            Sf_Safe | not trust_own_pkg         -> True
            Sf_SafeInferred | not trust_own_pkg -> True
            _ | isHomeModule home_unit mod      -> True
            _ -> unitIsTrusted $ unsafeLookupUnit unit_state (moduleUnit m)

    lookup' :: Module -> Hsc (Maybe ModIface)
    lookup' m = do
        hsc_env <- getHscEnv
        hsc_eps <- liftIO $ hscEPS hsc_env
        let pkgIfaceT = eps_PIT hsc_eps
            hug       = hsc_HUG hsc_env
        iface <- liftIO $ lookupIfaceByModule hug pkgIfaceT m
        -- the 'lookupIfaceByModule' method will always fail when calling from GHCi
        -- as the compiler hasn't filled in the various module tables
        -- so we need to call 'getModuleInterface' to load from disk
        case iface of
            Just _  -> return iface
            Nothing -> snd `fmap` (liftIO $ getModuleInterface hsc_env m)


-- | Check the list of packages are trusted.
checkPkgTrust :: Set UnitId -> Hsc ()
checkPkgTrust pkgs = do
    hsc_env <- getHscEnv
    let errors = S.foldr go emptyBag pkgs
        state  = hsc_units hsc_env
        go pkg acc
            | unitIsTrusted $ unsafeLookupUnitId state pkg
            = acc
            | otherwise
            = (`consBag` acc)
                     $ mkErrorMsgEnvelope noSrcSpan (pkgQual state)
                     $ GhcDriverMessage
                     $ DriverPackageNotTrusted state pkg
    if isEmptyBag errors
      then return ()
      else liftIO $ throwErrors $ mkMessages errors

-- | Set module to unsafe and (potentially) wipe trust information.
--
-- Make sure to call this method to set a module to inferred unsafe, it should
-- be a central and single failure method. We only wipe the trust information
-- when we aren't in a specific Safe Haskell mode.
--
-- While we only use this for recording that a module was inferred unsafe, we
-- may call it on modules using Trustworthy or Unsafe flags so as to allow
-- warning flags for safety to function correctly. See Note [Safe Haskell
-- Inference].
markUnsafeInfer :: forall e . Diagnostic e => TcGblEnv -> Messages e -> Hsc TcGblEnv
markUnsafeInfer tcg_env whyUnsafe = do
    dflags <- getDynFlags

    let reason = WarningWithFlag Opt_WarnUnsafe
    let diag_opts = initDiagOpts dflags
    when (diag_wopt Opt_WarnUnsafe diag_opts)
         (logDiagnostics $ singleMessage $
             mkPlainMsgEnvelope diag_opts (warnUnsafeOnLoc dflags) $
             GhcDriverMessage $ DriverUnknownMessage $
             mkSimpleUnknownDiagnostic $
             mkPlainDiagnostic reason noHints $
             whyUnsafe' dflags)

    liftIO $ writeIORef (tcg_safe_infer tcg_env) False
    liftIO $ writeIORef (tcg_safe_infer_reasons tcg_env) emptyMessages
    -- NOTE: Only wipe trust when not in an explicitly safe haskell mode. Other
    -- times inference may be on but we are in Trustworthy mode -- so we want
    -- to record safe-inference failed but not wipe the trust dependencies.
    case not (safeHaskellModeEnabled dflags) of
      True  -> return $ tcg_env { tcg_imports = wiped_trust }
      False -> return tcg_env

  where
    wiped_trust   = (tcg_imports tcg_env) { imp_trust_pkgs = S.empty }
    pprMod        = ppr $ moduleName $ tcg_mod tcg_env
    whyUnsafe' df = vcat [ quotes pprMod <+> text "has been inferred as unsafe!"
                         , text "Reason:"
                         , nest 4 $ (vcat $ badFlags df) $+$
                                    -- MP: Using defaultDiagnosticOpts here is not right but it's also not right to handle these
                                    -- unsafety error messages in an unstructured manner.
                                    (vcat $ pprMsgEnvelopeBagWithLoc (defaultDiagnosticOpts @e) (getMessages whyUnsafe)) $+$
                                    (vcat $ badInsts $ tcg_insts tcg_env)
                         ]
    badFlags df   = concatMap (badFlag df) unsafeFlagsForInfer
    badFlag df (ext,loc,on,_)
        | on df     = [mkLocMessage MCOutput (loc df) $
                            text "-X" <> ppr ext <+> text "is not allowed in Safe Haskell"]
        | otherwise = []
    badInsts insts = concatMap badInst insts

    checkOverlap (NoOverlap _) = False
    checkOverlap _             = True

    badInst ins | checkOverlap (overlapMode (is_flag ins))
                = [mkLocMessage MCOutput (nameSrcSpan $ getName $ is_dfun ins) $
                      ppr (overlapMode $ is_flag ins) <+>
                      text "overlap mode isn't allowed in Safe Haskell"]
                | otherwise = []

-- | Figure out the final correct safe haskell mode
hscGetSafeMode :: TcGblEnv -> Hsc SafeHaskellMode
hscGetSafeMode tcg_env = do
    dflags  <- getDynFlags
    liftIO $ finalSafeMode dflags tcg_env

--------------------------------------------------------------
-- Simplifiers
--------------------------------------------------------------

-- | Run Core2Core simplifier. The list of String is a list of (Core) plugin
-- module names added via TH (cf 'addCorePlugin').
hscSimplify :: HscEnv -> [String] -> ModGuts -> IO ModGuts
hscSimplify hsc_env plugins modguts =
    runHsc hsc_env $ hscSimplify' plugins modguts

-- | Run Core2Core simplifier. The list of String is a list of (Core) plugin
-- module names added via TH (cf 'addCorePlugin').
hscSimplify' :: [String] -> ModGuts -> Hsc ModGuts
hscSimplify' plugins ds_result = do
    hsc_env <- getHscEnv
    hsc_env_with_plugins <- if null plugins -- fast path
        then return hsc_env
        else liftIO $ initializePlugins
                    $ hscUpdateFlags (\dflags -> foldr addPluginModuleName dflags plugins)
                      hsc_env
    {-# SCC "Core2Core" #-}
      liftIO $ core2core hsc_env_with_plugins ds_result

--------------------------------------------------------------
-- Interface generators
--------------------------------------------------------------

-- | Generate a stripped down interface file, e.g. for boot files or when ghci
-- generates interface files. See Note [simpleTidyPgm - mkBootModDetailsTc]
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

--------------------------------------------------------------
-- BackEnd combinators
--------------------------------------------------------------

-- | Compile to hard-code.
hscGenHardCode :: HscEnv -> CgGuts -> ModLocation -> FilePath
               -> IO (FilePath, Maybe FilePath, [(ForeignSrcLang, FilePath)], Maybe StgCgInfos, Maybe CmmCgInfos )
                -- ^ @Just f@ <=> _stub.c is f
hscGenHardCode hsc_env cgguts location output_filename = do
        let CgGuts{ cg_module   = this_mod,
                    cg_binds    = core_binds,
                    cg_ccs      = local_ccs
                    } = cgguts
            dflags = hsc_dflags hsc_env
            logger = hsc_logger hsc_env


        -------------------
        -- Insert late cost centres based on the provided flags.
        --
        -- If -fprof-late-inline is enabled, we will skip adding CCs on any
        -- top-level bindings here (via shortcut in `addLateCostCenters`),
        -- since it will have already added a superset of the CCs we would add
        -- here.
        let
          late_cc_config :: LateCCConfig
          late_cc_config =
            LateCCConfig
              { lateCCConfig_whichBinds =
                  if gopt Opt_ProfLateInlineCcs dflags then
                    LateCCNone
                  else if gopt Opt_ProfLateCcs dflags then
                    LateCCBinds
                  else if gopt Opt_ProfLateOverloadedCcs dflags then
                    LateCCOverloadedBinds
                  else
                    LateCCNone
              , lateCCConfig_overloadedCalls =
                  gopt Opt_ProfLateoverloadedCallsCCs dflags
              , lateCCConfig_env =
                  LateCCEnv
                    { lateCCEnv_module = this_mod
                    , lateCCEnv_file = fsLit <$> ml_hs_file location
                    , lateCCEnv_countEntries= gopt Opt_ProfCountEntries dflags
                    , lateCCEnv_collectCCs = True
                    }
              }

        (late_cc_binds, late_cc_state) <-
          addLateCostCenters logger late_cc_config core_binds

        when (dopt Opt_D_dump_late_cc dflags || dopt Opt_D_verbose_core2core dflags) $
          putDumpFileMaybe logger Opt_D_dump_late_cc "LateCC" FormatCore (vcat (map ppr late_cc_binds))

        -------------------
        -- Run late plugins
        -- This is the last use of the ModGuts in a compilation.
        -- From now on, we just use the bits we need.
        ( CgGuts
            { cg_tycons        = tycons,
              cg_foreign       = foreign_stubs0,
              cg_foreign_files = foreign_files,
              cg_dep_pkgs      = dependencies,
              cg_spt_entries   = spt_entries,
              cg_binds         = late_binds,
              cg_ccs           = late_local_ccs
            }
          , _
          ) <-
          {-# SCC latePlugins #-}
          withTiming
            logger
            (text "LatePlugins"<+>brackets (ppr this_mod))
            (const ()) $
            withPlugins (hsc_plugins hsc_env)
              (($ hsc_env) . latePlugin)
                ( cgguts
                    { cg_binds = late_cc_binds
                    , cg_ccs = S.toList (lateCCState_ccs late_cc_state) ++ local_ccs
                    }
                , lateCCState_ccState late_cc_state
                )

        let
          hooks  = hsc_hooks hsc_env
          tmpfs  = hsc_tmpfs hsc_env
          llvm_config = hsc_llvm_config hsc_env
          profile = targetProfile dflags
          data_tycons = filter isDataTyCon tycons
          -- cg_tycons includes newtypes, for the benefit of External Core,
          -- but we don't generate any code for newtypes



        -------------------
        -- PREPARE FOR CODE GENERATION
        -- Do saturation and convert to A-normal form
        (prepd_binds) <- {-# SCC "CorePrep" #-} do
          cp_cfg <- initCorePrepConfig hsc_env
          corePrepPgm
            (hsc_logger hsc_env)
            cp_cfg
            (initCorePrepPgmConfig (hsc_dflags hsc_env) (interactiveInScope $ hsc_IC hsc_env))
            this_mod location late_binds data_tycons

        -----------------  Convert to STG ------------------
        (stg_binds_with_deps, denv, (caf_ccs, caf_cc_stacks), stg_cg_infos)
            <- {-# SCC "CoreToStg" #-}
               withTiming logger
                   (text "CoreToStg"<+>brackets (ppr this_mod))
                   (\(a, b, (c,d), tag_env) ->
                        a `seqList`
                        b `seq`
                        c `seqList`
                        d `seqList`
                        (seqEltsUFM (seqTagSig) tag_env))
                   (myCoreToStg logger dflags (interactiveInScope (hsc_IC hsc_env)) False this_mod location prepd_binds)

        let (stg_binds,_stg_deps) = unzip stg_binds_with_deps

        let cost_centre_info =
              (late_local_ccs ++ caf_ccs, caf_cc_stacks)
            platform = targetPlatform dflags
            prof_init
              | sccProfilingEnabled dflags = profilingInitCode platform this_mod cost_centre_info
              | otherwise = mempty

        ------------------  Code generation ------------------
        -- The back-end is streamed: each top-level function goes
        -- from Stg all the way to asm before dealing with the next
        -- top-level function, so withTiming isn't very useful here.
        -- Hence we have one withTiming for the whole backend, the
        -- next withTiming after this will be "Assembler" (hard code only).
        withTiming logger (text "CodeGen"<+>brackets (ppr this_mod)) (const ())
         $ case backendCodeOutput (backend dflags) of
            JSCodeOutput ->
              do
              let js_config = initStgToJSConfig dflags

                  -- The JavaScript backend does not create CmmCgInfos like the Cmm backend,
                  -- but it is needed for writing the interface file. Here we compute a very
                  -- conservative but correct value.
                  lf_infos (StgTopLifted (StgNonRec b _)) = [(idName b, LFUnknown True)]
                  lf_infos (StgTopLifted (StgRec bs))     = map (\(b,_) -> (idName b, LFUnknown True)) bs
                  lf_infos (StgTopStringLit b _)          = [(idName b, LFUnlifted)]

                  cmm_cg_infos  = CmmCgInfos
                    { cgNonCafs = mempty
                    , cgLFInfos = mkNameEnv (concatMap lf_infos stg_binds)
                    , cgIPEStub = mempty
                    }
                  stub_c_exists = Nothing
                  foreign_fps   = []

              putDumpFileMaybe logger Opt_D_dump_stg_final "Final STG:" FormatSTG
                  (pprGenStgTopBindings (initStgPprOpts dflags) stg_binds)

              -- do the unfortunately effectual business
              stgToJS logger js_config stg_binds this_mod spt_entries foreign_stubs0 cost_centre_info output_filename
              return (output_filename, stub_c_exists, foreign_fps, Just stg_cg_infos, Just cmm_cg_infos)

            _          ->
              do
              cmms <- {-# SCC "StgToCmm" #-}
                doCodeGen hsc_env this_mod denv data_tycons
                cost_centre_info
                stg_binds

              ------------------  Code output -----------------------
              rawcmms0 <- {-# SCC "cmmToRawCmm" #-}
                case cmmToRawCmmHook hooks of
                  Nothing -> cmmToRawCmm logger profile cmms
                  Just h  -> h dflags (Just this_mod) cmms

              let dump a = do
                    unless (null a) $ putDumpFileMaybe logger Opt_D_dump_cmm_raw "Raw Cmm" FormatCMM (pdoc platform a)
                    return a
                  rawcmms1 = Stream.mapM (liftIO . dump) rawcmms0

              let foreign_stubs st = foreign_stubs0
                                     `appendStubC` prof_init
                                     `appendStubC` cgIPEStub st

              (output_filename, (_stub_h_exists, stub_c_exists), foreign_fps, cmm_cg_infos)
                  <- {-# SCC "codeOutput" #-}
                    codeOutput logger tmpfs llvm_config dflags (hsc_units hsc_env) this_mod output_filename location
                    foreign_stubs foreign_files dependencies (initDUniqSupply 'n' 0) rawcmms1
              return  ( output_filename, stub_c_exists, foreign_fps
                      , Just stg_cg_infos, Just cmm_cg_infos)


-- The part of CgGuts that we need for HscInteractive
data CgInteractiveGuts = CgInteractiveGuts { cgi_module :: Module
                                           , cgi_binds  :: CoreProgram
                                           , cgi_tycons :: [TyCon]
                                           , cgi_foreign :: ForeignStubs
                                           , cgi_foreign_files :: [(ForeignSrcLang, FilePath)]
                                           , cgi_modBreaks ::  Maybe ModBreaks
                                           , cgi_spt_entries :: [SptEntry]
                                           }

mkCgInteractiveGuts :: CgGuts -> CgInteractiveGuts
mkCgInteractiveGuts CgGuts{cg_module, cg_binds, cg_tycons, cg_foreign, cg_foreign_files, cg_modBreaks, cg_spt_entries}
  = CgInteractiveGuts cg_module cg_binds cg_tycons cg_foreign cg_foreign_files cg_modBreaks cg_spt_entries

hscInteractive :: HscEnv
               -> CgInteractiveGuts
               -> ModLocation
               -> IO (Maybe FilePath, CompiledByteCode) -- ^ .c stub path (if any) and ByteCode
hscInteractive hsc_env cgguts location = do
    let dflags = hsc_dflags hsc_env
    let logger = hsc_logger hsc_env
    let tmpfs  = hsc_tmpfs hsc_env
    let CgInteractiveGuts{ -- This is the last use of the ModGuts in a compilation.
                -- From now on, we just use the bits we need.
               cgi_module   = this_mod,
               cgi_binds    = core_binds,
               cgi_tycons   = tycons,
               cgi_foreign  = foreign_stubs,
               cgi_modBreaks = mod_breaks,
               cgi_spt_entries = spt_entries } = cgguts

        data_tycons = filter isDataTyCon tycons
        -- cg_tycons includes newtypes, for the benefit of External Core,
        -- but we don't generate any code for newtypes

    -------------------
    -- PREPARE FOR CODE GENERATION
    -- Do saturation and convert to A-normal form
    prepd_binds <- {-# SCC "CorePrep" #-} do
      cp_cfg <- initCorePrepConfig hsc_env
      corePrepPgm
        (hsc_logger hsc_env)
        cp_cfg
        (initCorePrepPgmConfig (hsc_dflags hsc_env) (interactiveInScope $ hsc_IC hsc_env))
        this_mod location core_binds data_tycons

    -- The stg cg info only provides a runtime benfit, but is not requires so we just
    -- omit it here
    (stg_binds_with_deps, _infotable_prov, _caf_ccs__caf_cc_stacks, _ignore_stg_cg_infos)
      <- {-# SCC "CoreToStg" #-}
          myCoreToStg logger dflags (interactiveInScope (hsc_IC hsc_env)) True this_mod location prepd_binds

    let (stg_binds,_stg_deps) = unzip stg_binds_with_deps

    -----------------  Generate byte code ------------------
    comp_bc <- byteCodeGen hsc_env this_mod stg_binds data_tycons mod_breaks spt_entries

    ------------------ Create f-x-dynamic C-side stuff -----
    (_istub_h_exists, istub_c_exists)
        <- outputForeignStubs logger tmpfs dflags (hsc_units hsc_env) this_mod location foreign_stubs
    return (istub_c_exists, comp_bc)

-- | Compile Core bindings and foreign inputs that were loaded from an
-- interface, to produce bytecode and potential foreign objects for the purpose
-- of linking splices.
generateByteCode :: HscEnv
  -> CgInteractiveGuts
  -> ModLocation
  -> IO (CompiledByteCode, [FilePath])
generateByteCode hsc_env cgguts mod_location = do
  (hasStub, comp_bc) <- hscInteractive hsc_env cgguts mod_location
  compile_for_interpreter hsc_env $ \ i_env -> do
    stub_o <- traverse (compileForeign i_env LangC) hasStub
    foreign_files_o <- traverse (uncurry (compileForeign i_env)) (cgi_foreign_files cgguts)
    pure (comp_bc, maybeToList stub_o ++ foreign_files_o)

generateFreshByteCode :: HscEnv
  -> ModuleName
  -> CgInteractiveGuts
  -> ModLocation
  -> IO Linkable
generateFreshByteCode hsc_env mod_name cgguts mod_location = do
  bco_time <- getCurrentTime
  (bcos, fos) <- generateByteCode hsc_env cgguts mod_location
  return $!
    Linkable bco_time
    (mkHomeModule (hsc_home_unit hsc_env) mod_name)
    (BCOs bcos :| [DotO fo ForeignObject | fo <- fos])
------------------------------

hscCompileCmmFile :: HscEnv -> FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)
hscCompileCmmFile hsc_env original_filename filename output_filename = runHsc hsc_env $ do
    let dflags   = hsc_dflags hsc_env
        logger   = hsc_logger hsc_env
        hooks    = hsc_hooks hsc_env
        tmpfs    = hsc_tmpfs hsc_env
        profile  = targetProfile dflags
        home_unit = hsc_home_unit hsc_env
        platform  = targetPlatform dflags
        llvm_config = hsc_llvm_config hsc_env
        cmm_config = initCmmConfig dflags
        do_info_table = gopt Opt_InfoTableMap dflags
        -- Make up a module name to give the NCG. We can't pass bottom here
        -- lest we reproduce #11784.
        mod_name = mkModuleName $ "Cmm$" ++ original_filename
        cmm_mod = mkHomeModule home_unit mod_name
        cmmpConfig = initCmmParserConfig dflags
    (dcmm, ipe_ents) <- ioMsgMaybe
               $ do
                  (warns,errs,cmm) <- withTiming logger (text "ParseCmm"<+>brackets (text filename)) (\_ -> ())
                                       $ parseCmmFile cmmpConfig cmm_mod home_unit filename
                  let msgs = warns `unionMessages` errs
                  return (GhcPsMessage <$> msgs, cmm)
    -- Probably need to rename cmm here
    let cmm = removeDeterm dcmm
    liftIO $ do
        putDumpFileMaybe logger Opt_D_dump_cmm_verbose_by_proc "Parsed Cmm" FormatCMM (pdoc platform cmm)

        -- Compile decls in Cmm files one decl at a time, to avoid re-ordering
        -- them in SRT analysis.
        --
        -- Re-ordering here causes breakage when booting with C backend because
        -- in C we must declare before use, but SRT algorithm is free to
        -- re-order [A, B] (B refers to A) when A is not CAFFY and return [B, A]
        ((_,dus1), cmmgroup) <- second concat <$>
          mapAccumLM (\(msrt0, dus0) cmm -> do
            ((msrt1, cmm'), dus1) <- cmmPipeline logger cmm_config msrt0 [cmm] dus0
            return ((msrt1, dus1), cmm')) (emptySRT cmm_mod, initDUniqSupply 'u' 0) cmm

        unless (null cmmgroup) $
          putDumpFileMaybe logger Opt_D_dump_cmm "Output Cmm"
            FormatCMM (pdoc platform cmmgroup)

        rawCmms0 <- case cmmToRawCmmHook hooks of
          Nothing -> cmmToRawCmm logger profile (Stream.yield cmmgroup)
          Just h  -> h           dflags Nothing (Stream.yield cmmgroup)

        let dump a = do
              unless (null a) $ putDumpFileMaybe logger Opt_D_dump_cmm_raw "Raw Cmm" FormatCMM (pdoc platform a)
              return a
            rawCmms = Stream.mapM (liftIO . dump) rawCmms0

        let foreign_stubs _
              | not $ null ipe_ents =
                  let ip_init = ipInitCode do_info_table platform cmm_mod
                  in NoStubs `appendStubC` ip_init
              | otherwise     = NoStubs
        (_output_filename, (_stub_h_exists, stub_c_exists), _foreign_fps, _caf_infos)
          <- codeOutput logger tmpfs llvm_config dflags (hsc_units hsc_env) cmm_mod output_filename no_loc foreign_stubs [] S.empty
             dus1 rawCmms
        return stub_c_exists
  where
    no_loc = OsPathModLocation
        { ml_hs_file_ospath  = Just $ unsafeEncodeUtf original_filename,
          ml_hi_file_ospath  = panic "hscCompileCmmFile: no hi file",
          ml_obj_file_ospath = panic "hscCompileCmmFile: no obj file",
          ml_dyn_obj_file_ospath = panic "hscCompileCmmFile: no dyn obj file",
          ml_dyn_hi_file_ospath  = panic "hscCompileCmmFile: no dyn obj file",
          ml_hie_file_ospath = panic "hscCompileCmmFile: no hie file"}

-------------------- Stuff for new code gen ---------------------

{-
Note [Forcing of stg_binds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The two last steps in the STG pipeline are:

* Sorting the bindings in dependency order.
* Annotating them with free variables.

We want to make sure we do not keep references to unannotated STG bindings
alive, nor references to bindings which have already been compiled to Cmm.

We explicitly force the bindings to avoid this.

This reduces residency towards the end of the CodeGen phase significantly
(5-10%).
-}

doCodeGen :: HscEnv -> Module -> InfoTableProvMap -> [TyCon]
          -> CollectedCCs
          -> [CgStgTopBinding] -- ^ Bindings come already annotated with fvs
          -> IO (CgStream CmmGroupSRTs CmmCgInfos)
         -- Note we produce a 'Stream' of CmmGroups, so that the
         -- backend can be run incrementally.  Otherwise it generates all
         -- the C-- up front, which has a significant space cost.
doCodeGen hsc_env this_mod denv data_tycons
              cost_centre_info stg_binds_w_fvs = do
    let dflags     = hsc_dflags hsc_env
        logger     = hsc_logger hsc_env
        hooks      = hsc_hooks  hsc_env
        tmpfs      = hsc_tmpfs  hsc_env
        platform   = targetPlatform dflags
        stg_ppr_opts = (initStgPprOpts dflags)

    putDumpFileMaybe logger Opt_D_dump_stg_final "Final STG:" FormatSTG
        (pprGenStgTopBindings stg_ppr_opts stg_binds_w_fvs)

    let stg_to_cmm dflags mod a b c d = case stgToCmmHook hooks of
          Nothing -> StgToCmm.codeGen logger tmpfs (initStgToCmmConfig dflags mod) a b c d
          Just h  -> (,emptyDetUFM) <$> h          (initStgToCmmConfig dflags mod) a b c d

    let cmm_stream :: CgStream CmmGroup (ModuleLFInfos, DetUniqFM)
        -- See Note [Forcing of stg_binds]
        cmm_stream = stg_binds_w_fvs `seqList` {-# SCC "StgToCmm" #-}
            stg_to_cmm dflags this_mod denv data_tycons cost_centre_info stg_binds_w_fvs

        -- codegen consumes a stream of CmmGroup, and produces a new
        -- stream of CmmGroup (not necessarily synchronised: one
        -- CmmGroup on input may produce many CmmGroups on output due
        -- to proc-point splitting).

    let dump1 a = do
          unless (null a) $
            putDumpFileMaybe logger Opt_D_dump_cmm_from_stg
              "Cmm produced by codegen" FormatCMM (pdoc platform a)
          return a

        ppr_stream1 = Stream.mapM (liftIO . dump1) cmm_stream

        cmm_config = initCmmConfig dflags

        pipeline_stream :: CgStream CmmGroupSRTs CmmCgInfos
        pipeline_stream = do
          ((mod_srt_info, ipes, ipe_stats), (lf_infos, detRnEnv)) <-
            {-# SCC "cmmPipeline" #-}
            Stream.mapAccumL_ (pipeline_action logger cmm_config) (emptySRT this_mod, M.empty, mempty) ppr_stream1

          let nonCaffySet = srtMapNonCAFs (moduleSRTMap mod_srt_info)

              -- denv::InfoTableProvMap refers to symbols that no longer exist
              -- if -fobject-determinism is on, since it was created before the
              -- Cmm was renamed. Update all the symbols by renaming them with
              -- the renaming map in that case.
              (_drn, rn_denv)
                | gopt Opt_ObjectDeterminism dflags = detRenameIPEMap detRnEnv denv
                | otherwise = (detRnEnv, denv)

          cmmCgInfos <- generateCgIPEStub hsc_env this_mod rn_denv (nonCaffySet, lf_infos, ipes, ipe_stats)
          return cmmCgInfos

        pipeline_action
          :: Logger
          -> CmmConfig
          -> (ModuleSRTInfo, Map CmmInfoTable (Maybe IpeSourceLocation), IPEStats)
          -> CmmGroup
          -> UniqDSMT IO ((ModuleSRTInfo, Map CmmInfoTable (Maybe IpeSourceLocation), IPEStats), CmmGroupSRTs)
        pipeline_action logger cmm_config (mod_srt_info, ipes, stats) cmm_group = do
          (mod_srt_info', cmm_srts) <- withDUS $ cmmPipeline logger cmm_config mod_srt_info cmm_group

          -- If -finfo-table-map is enabled, we precompute a map from info
          -- tables to source locations. See Note [Mapping Info Tables to Source
          -- Positions] in GHC.Stg.Debug.
          (ipes', stats') <-
            if (gopt Opt_InfoTableMap dflags) then
              liftIO $ lookupEstimatedTicks hsc_env ipes stats cmm_srts
            else
              return (ipes, stats)

          return ((mod_srt_info', ipes', stats'), cmm_srts)

        dump2 a = do
          unless (null a) $
            putDumpFileMaybe logger Opt_D_dump_cmm "Output Cmm" FormatCMM (pdoc platform a)
          return a

    return $ Stream.mapM (liftIO . dump2) pipeline_stream

myCoreToStg :: Logger -> DynFlags -> [Var]
            -> Bool
            -> Module -> ModLocation -> CoreProgram
            -> IO ( [(CgStgTopBinding,IdSet)] -- output program and its dependencies
                  , InfoTableProvMap
                  , CollectedCCs -- CAF cost centre info (declared and used)
                  , StgCgInfos )
myCoreToStg logger dflags ic_inscope for_bytecode this_mod ml prepd_binds = do
    let (stg_binds, denv, cost_centre_info)
         = {-# SCC "Core2Stg" #-}
           coreToStg (initCoreToStgOpts dflags) this_mod ml prepd_binds

    (stg_binds_with_fvs,stg_cg_info)
        <- {-# SCC "Stg2Stg" #-}
           stg2stg logger ic_inscope (initStgPipelineOpts dflags for_bytecode)
                   this_mod stg_binds

    putDumpFileMaybe logger Opt_D_dump_stg_cg "CodeGenInput STG:" FormatSTG
        (pprGenStgTopBindings (initStgPprOpts dflags) (fmap fst stg_binds_with_fvs))

    return (stg_binds_with_fvs, denv, cost_centre_info, stg_cg_info)

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
IO monad as explained in Note [Interactively-bound Ids in GHCi] in GHC.Runtime.Context
-}

-- | Compile a stmt all the way to an HValue, but don't run it
--
-- We return Nothing to indicate an empty statement (or comment only), not a
-- parse error.
hscStmt :: HscEnv -> String -> IO (Maybe ([Id], ForeignHValue, FixityEnv))
hscStmt hsc_env stmt = hscStmtWithLocation hsc_env stmt "<interactive>" 1

-- | Compile a stmt all the way to an HValue, but don't run it
--
-- We return Nothing to indicate an empty statement (or comment only), not a
-- parse error.
hscStmtWithLocation :: HscEnv
                    -> String -- ^ The statement
                    -> String -- ^ The source
                    -> Int    -- ^ Starting line
                    -> IO ( Maybe ([Id]
                          , ForeignHValue {- IO [HValue] -}
                          , FixityEnv))
hscStmtWithLocation hsc_env0 stmt source linenumber =
  runInteractiveHsc hsc_env0 $ do
    maybe_stmt <- hscParseStmtWithLocation source linenumber stmt
    case maybe_stmt of
      Nothing -> return Nothing

      Just parsed_stmt -> do
        hsc_env <- getHscEnv
        liftIO $ hscParsedStmt hsc_env parsed_stmt

hscParsedStmt :: HscEnv
              -> GhciLStmt GhcPs  -- ^ The parsed statement
              -> IO ( Maybe ([Id]
                    , ForeignHValue {- IO [HValue] -}
                    , FixityEnv))
hscParsedStmt hsc_env stmt = runInteractiveHsc hsc_env $ do
  -- Rename and typecheck it
  (ids, tc_expr, fix_env) <- ioMsgMaybe $ hoistTcRnMessage $ tcRnStmt hsc_env stmt

  -- Desugar it
  ds_expr <- ioMsgMaybe $ hoistDsMessage $ deSugarExpr hsc_env tc_expr
  liftIO (lintInteractiveExpr (text "desugar expression") hsc_env ds_expr)
  handleWarnings

  -- Then code-gen, and link it
  -- It's important NOT to have package 'interactive' as thisUnitId
  -- for linking, else we try to link 'main' and can't find it.
  -- Whereas the linker already knows to ignore 'interactive'
  let src_span = srcLocSpan interactiveSrcLoc
  (hval,_,_) <- liftIO $ hscCompileCoreExpr hsc_env src_span ds_expr

  return $ Just (ids, hval, fix_env)

hscParseModuleWithLocation :: HscEnv -> String -> Int -> String -> IO (HsModule GhcPs)
hscParseModuleWithLocation hsc_env source line_num str = do
    L _ mod <-
      runInteractiveHsc hsc_env $
        hscParseThingWithLocation source line_num parseModule str
    return mod

hscParseDeclsWithLocation :: HscEnv -> String -> Int -> String -> IO [LHsDecl GhcPs]
hscParseDeclsWithLocation hsc_env source line_num str = do
  HsModule { hsmodDecls = decls } <- hscParseModuleWithLocation hsc_env source line_num str
  return decls

hscParsedDecls :: HscEnv -> [LHsDecl GhcPs] -> IO ([TyThing], InteractiveContext)
hscParsedDecls hsc_env decls = runInteractiveHsc hsc_env $ do
    hsc_env <- getHscEnv
    let interp = hscInterp hsc_env

    {- Rename and typecheck it -}
    tc_gblenv <- ioMsgMaybe $ hoistTcRnMessage $ tcRnDeclsi hsc_env decls

    {- Grab the new instances -}
    -- We grab the whole environment because of the overlapping that may have
    -- been done. See the notes at the definition of InteractiveContext
    -- (ic_instances) for more details.
    let defaults = tcg_default tc_gblenv

    {- Desugar it -}
    -- We use a basically null location for iNTERACTIVE
    let iNTERACTIVELoc = OsPathModLocation
            { ml_hs_file_ospath   = Nothing,
              ml_hi_file_ospath   = panic "hsDeclsWithLocation:ml_hi_file_ospath",
              ml_obj_file_ospath  = panic "hsDeclsWithLocation:ml_obj_file_ospath",
              ml_dyn_obj_file_ospath = panic "hsDeclsWithLocation:ml_dyn_obj_file_ospath",
              ml_dyn_hi_file_ospath = panic "hsDeclsWithLocation:ml_dyn_hi_file_ospath",
              ml_hie_file_ospath  = panic "hsDeclsWithLocation:ml_hie_file_ospath" }
    ds_result <- hscDesugar' iNTERACTIVELoc tc_gblenv

    {- Simplify -}
    simpl_mg <- liftIO $ do
      plugins <- readIORef (tcg_th_coreplugins tc_gblenv)
      hscSimplify hsc_env plugins ds_result

    {- Tidy -}
    (tidy_cg, mod_details) <- liftIO $ hscTidy hsc_env simpl_mg

    let !CgGuts{ cg_module    = this_mod,
                 cg_binds     = core_binds
                 } = tidy_cg

        !ModDetails { md_insts     = cls_insts
                    , md_fam_insts = fam_insts } = mod_details
            -- Get the *tidied* cls_insts and fam_insts

    {- Generate byte code & foreign stubs -}
    linkable <- liftIO $ generateFreshByteCode hsc_env
      (moduleName this_mod)
      (mkCgInteractiveGuts tidy_cg)
      iNTERACTIVELoc

    let src_span = srcLocSpan interactiveSrcLoc
    _ <- liftIO $ loadDecls interp hsc_env src_span linkable

    {- Load static pointer table entries -}
    liftIO $ hscAddSptEntries hsc_env (cg_spt_entries tidy_cg)

    let tcs = filterOut isImplicitTyCon (mg_tcs simpl_mg)
        patsyns = mg_patsyns simpl_mg

        ext_ids = [ id | id <- bindersOfBinds core_binds
                       , isExternalName (idName id)
                       , not (isDFunId id || isImplicitId id) ]
            -- We only need to keep around the external bindings
            -- (as decided by GHC.Iface.Tidy), since those are the only ones
            -- that might later be looked up by name.  But we can exclude
            --    - DFunIds, which are in 'cls_insts' (see Note [ic_tythings] in GHC.Runtime.Context
            --    - Implicit Ids, which are implicit in tcs
            -- c.f. GHC.Tc.Module.runTcInteractive, which reconstructs the TypeEnv

        new_tythings = map AnId ext_ids ++ map ATyCon tcs ++ map (AConLike . PatSynCon) patsyns
        ictxt        = hsc_IC hsc_env
        -- See Note [Fixity declarations in GHCi]
        fix_env      = tcg_fix_env tc_gblenv
        new_ictxt    = extendInteractiveContext ictxt new_tythings cls_insts
                                                fam_insts defaults fix_env
    return (new_tythings, new_ictxt)

-- | Load the given static-pointer table entries into the interpreter.
-- See Note [Grand plan for static forms] in "GHC.Iface.Tidy.StaticPtrTable".
hscAddSptEntries :: HscEnv -> [SptEntry] -> IO ()
hscAddSptEntries hsc_env entries = do
    let interp = hscInterp hsc_env
    let add_spt_entry :: SptEntry -> IO ()
        add_spt_entry (SptEntry i fpr) = do
            -- These are only names from the current module
            (val, _, _) <- loadName interp hsc_env (idName i)
            addSptEntry interp fpr val
    mapM_ add_spt_entry entries

{-
  Note [Fixity declarations in GHCi]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  To support fixity declarations on types defined within GHCi (as requested
  in #10018) we record the fixity environment in InteractiveContext.
  When we want to evaluate something GHC.Tc.Module.runTcInteractive pulls out this
  fixity environment and uses it to initialize the global typechecker environment.
  After the typechecker has finished its business, an updated fixity environment
  (reflecting whatever fixity declarations were present in the statements we
  passed it) will be returned from hscParsedStmt. This is passed to
  updateFixityEnv, which will stuff it back into InteractiveContext, to be
  used in evaluating the next statement.

-}

hscImport :: HscEnv -> String -> IO (ImportDecl GhcPs)
hscImport hsc_env str = runInteractiveHsc hsc_env $ do
    -- Use >>= \case instead of MonadFail desugaring to take into
    -- consideration `instance XXModule p = DataConCantHappen`.
    -- Tracked in #15681
    hscParseThing parseModule str >>= \case
      (L _ (HsModule{hsmodImports=is})) ->
        case is of
            [L _ i] -> return i
            _ -> liftIO $ throwOneError $
                     mkPlainErrorMsgEnvelope noSrcSpan $
                     GhcPsMessage $ PsUnknownMessage $
                     mkSimpleUnknownDiagnostic $
                      mkPlainError noHints $
                         text "parse error in import declaration"

-- | Typecheck an expression (but don't run it)
hscTcExpr :: HscEnv
          -> TcRnExprMode
          -> String -- ^ The expression
          -> IO Type
hscTcExpr hsc_env0 mode expr = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  parsed_expr <- hscParseExpr expr
  ioMsgMaybe $ hoistTcRnMessage $ tcRnExpr hsc_env mode parsed_expr

-- | Find the kind of a type, after generalisation
hscKcType
  :: HscEnv
  -> Bool            -- ^ Normalise the type
  -> String          -- ^ The type as a string
  -> IO (Type, Kind) -- ^ Resulting type (possibly normalised) and kind
hscKcType hsc_env0 normalise str = runInteractiveHsc hsc_env0 $ do
    hsc_env <- getHscEnv
    ty <- hscParseType str
    ioMsgMaybe $ hoistTcRnMessage $ tcRnType hsc_env DefaultFlexi normalise ty

hscParseExpr :: String -> Hsc (LHsExpr GhcPs)
hscParseExpr expr = do
  maybe_stmt <- hscParseStmt expr
  case maybe_stmt of
    Just (L _ (BodyStmt _ expr _ _)) -> return expr
    _ -> throwOneError $
           mkPlainErrorMsgEnvelope noSrcSpan $
           GhcPsMessage $ PsUnknownMessage
             $ mkSimpleUnknownDiagnostic
             $ mkPlainError noHints $
             text "not an expression:" <+> quotes (text expr)

hscParseStmt :: String -> Hsc (Maybe (GhciLStmt GhcPs))
hscParseStmt = hscParseThing parseStmt

hscParseStmtWithLocation :: String -> Int -> String
                         -> Hsc (Maybe (GhciLStmt GhcPs))
hscParseStmtWithLocation source linenumber stmt =
    hscParseThingWithLocation source linenumber parseStmt stmt

hscParseType :: String -> Hsc (LHsType GhcPs)
hscParseType = hscParseThing parseType

hscParseIdentifier :: HscEnv -> String -> IO (LocatedN RdrName)
hscParseIdentifier hsc_env str =
    runInteractiveHsc hsc_env $ hscParseThing parseIdentifier str

hscParseThing :: (Outputable thing, Data thing)
              => Lexer.P thing -> String -> Hsc thing
hscParseThing = hscParseThingWithLocation "<interactive>" 1

hscParseThingWithLocation :: (Outputable thing, Data thing) => String -> Int
                          -> Lexer.P thing -> String -> Hsc thing
hscParseThingWithLocation source linenumber parser str = do
    dflags <- getDynFlags
    logger <- getLogger
    withTiming logger
               (text "Parser [source]")
               (const ()) $ {-# SCC "Parser" #-} do

        let buf = stringToStringBuffer str
            loc = mkRealSrcLoc (fsLit source) linenumber 1

        case unP parser (initParserState (initParserOpts dflags) buf loc) of
            PFailed pst ->
                handleWarningsThrowErrors (getPsMessages pst)
            POk pst thing -> do
                logWarningsReportErrors (getPsMessages pst)
                liftIO $ putDumpFileMaybe logger Opt_D_dump_parsed "Parser"
                            FormatHaskell (ppr thing)
                liftIO $ putDumpFileMaybe logger Opt_D_dump_parsed_ast "Parser AST"
                            FormatHaskell (showAstData NoBlankSrcSpan NoBlankEpAnnotations thing)
                return thing

hscTidy :: HscEnv -> ModGuts -> IO (CgGuts, ModDetails)
hscTidy hsc_env guts = do
  let logger   = hsc_logger hsc_env
  let this_mod = mg_module guts

  opts <- initTidyOpts hsc_env
  (cgguts, details) <- withTiming logger
    (text "CoreTidy"<+>brackets (ppr this_mod))
    (const ())
    $! {-# SCC "CoreTidy" #-} tidyProgram opts guts

  -- post tidy pretty-printing and linting...
  let tidy_rules     = md_rules details
  let all_tidy_binds = cg_binds cgguts
  let name_ppr_ctx   = mkNamePprCtx ptc (hsc_unit_env hsc_env) (mg_rdr_env guts)
      ptc            = initPromotionTickContext (hsc_dflags hsc_env)

  endPassHscEnvIO hsc_env name_ppr_ctx CoreTidy all_tidy_binds tidy_rules

  -- If the endPass didn't print the rules, but ddump-rules is
  -- on, print now
  unless (logHasDumpFlag logger Opt_D_dump_simpl) $
    putDumpFileMaybe logger Opt_D_dump_rules
      "Tidy Core rules"
      FormatText
      (pprRulesForUser tidy_rules)

  -- Print one-line size info
  let cs = coreBindsStats all_tidy_binds
  putDumpFileMaybe logger Opt_D_dump_core_stats "Core Stats"
    FormatText
    (text "Tidy size (terms,types,coercions)"
     <+> ppr (moduleName this_mod) <> colon
     <+> int (cs_tm cs)
     <+> int (cs_ty cs)
     <+> int (cs_co cs))

  pure (cgguts, details)


{- **********************************************************************
%*                                                                      *
        Desugar, simplify, convert to bytecode, and link an expression
%*                                                                      *
%********************************************************************* -}

hscCompileCoreExpr :: HscEnv -> SrcSpan -> CoreExpr -> IO (ForeignHValue, [Linkable], PkgsLoaded)
hscCompileCoreExpr hsc_env loc expr =
  case hscCompileCoreExprHook (hsc_hooks hsc_env) of
      Nothing -> hscCompileCoreExpr' hsc_env loc expr
      Just h  -> h                   hsc_env loc expr

hscCompileCoreExpr' :: HscEnv -> SrcSpan -> CoreExpr -> IO (ForeignHValue, [Linkable], PkgsLoaded)
hscCompileCoreExpr' hsc_env srcspan ds_expr = do
  {- Simplify it -}
  -- Question: should we call SimpleOpt.simpleOptExpr here instead?
  -- It is, well, simpler, and does less inlining etc.
  let dflags = hsc_dflags hsc_env
  let logger = hsc_logger hsc_env
  let ic = hsc_IC hsc_env
  let unit_env = hsc_unit_env hsc_env
  let simplify_expr_opts = initSimplifyExprOpts dflags ic

  simpl_expr <- simplifyExpr logger (ue_eps unit_env) simplify_expr_opts ds_expr

  -- Create a unique temporary binding
  --
  -- The id has to be exported for the JS backend. This isn't required for the
  -- byte-code interpreter but it does no harm to always do it.
  u <- uniqFromTag 'I'
  let binding_name = mkSystemVarName u (fsLit ("BCO_toplevel"))
  let binding_id   = mkExportedVanillaId binding_name (exprType simpl_expr)

  {- Tidy it (temporary, until coreSat does cloning) -}
  let tidy_occ_env = initTidyOccEnv [occName binding_id]
  let tidy_env     = mkEmptyTidyEnv tidy_occ_env
  let tidy_expr    = tidyExpr tidy_env simpl_expr

  {- Prepare for codegen -}
  cp_cfg <- initCorePrepConfig hsc_env
  prepd_expr <- corePrepExpr
   logger cp_cfg
   tidy_expr

  {- Lint if necessary -}
  lintInteractiveExpr (text "hscCompileCoreExpr") hsc_env prepd_expr
  let this_loc = OsPathModLocation
          { ml_hs_file_ospath   = Nothing,
            ml_hi_file_ospath   = panic "hscCompileCoreExpr':ml_hi_file_ospath",
            ml_obj_file_ospath  = panic "hscCompileCoreExpr':ml_obj_file_ospath",
            ml_dyn_obj_file_ospath = panic "hscCompileCoreExpr': ml_obj_file_ospath",
            ml_dyn_hi_file_ospath  = panic "hscCompileCoreExpr': ml_dyn_hi_file_ospath",
            ml_hie_file_ospath  = panic "hscCompileCoreExpr':ml_hie_file_ospath" }

  -- Ensure module uniqueness by giving it a name like "GhciNNNN".
  -- This uniqueness is needed by the JS linker. Without it we break the 1-1
  -- relationship between modules and object files, i.e. we get different object
  -- files for the same module and the JS linker doesn't support this.
  --
  -- Note that we can't use icInteractiveModule because the ic_mod_index value
  -- isn't bumped between invocations of hscCompileCoreExpr, so uniqueness isn't
  -- guaranteed.
  --
  -- We reuse the unique we obtained for the binding, but any unique would do.
  let this_mod = mkInteractiveModule (show u)
  let for_bytecode = True

  (stg_binds_with_deps, _prov_map, _collected_ccs, _stg_cg_infos) <-
       myCoreToStg logger
                   dflags
                   (interactiveInScope (hsc_IC hsc_env))
                   for_bytecode
                   this_mod
                   this_loc
                   [NonRec binding_id prepd_expr]

  let (stg_binds, _stg_deps) = unzip stg_binds_with_deps

  let interp = hscInterp hsc_env

  case interp of
    -- always generate JS code for the JS interpreter (no bytecode!)
    Interp (ExternalInterp (ExtJS i)) _ _ ->
      jsCodeGen hsc_env srcspan i this_mod stg_binds_with_deps binding_id

    _ -> do
      {- Convert to BCOs -}
      bcos <- byteCodeGen hsc_env
                this_mod
                stg_binds
                []
                Nothing -- modbreaks
                [] -- spt entries

      {- load it -}
      bco_time <- getCurrentTime
      (fv_hvs, mods_needed, units_needed) <- loadDecls interp hsc_env srcspan $
        Linkable bco_time this_mod $ NE.singleton $ BCOs bcos
      {- Get the HValue for the root -}
      return (expectJust $ lookup (idName binding_id) fv_hvs, mods_needed, units_needed)



-- | Generate JS code for the given bindings and return the HValue for the given id
jsCodeGen
  :: HscEnv
  -> SrcSpan
  -> JSInterp
  -> Module
  -> [(CgStgTopBinding,IdSet)]
  -> Id
  -> IO (ForeignHValue, [Linkable], PkgsLoaded)
jsCodeGen hsc_env srcspan i this_mod stg_binds_with_deps binding_id = do
  let logger           = hsc_logger hsc_env
      tmpfs            = hsc_tmpfs hsc_env
      dflags           = hsc_dflags hsc_env
      interp           = hscInterp hsc_env
      tmp_dir          = tmpDir dflags
      unit_env         = hsc_unit_env hsc_env
      js_config        = initStgToJSConfig dflags

  -- We need to load all the dependencies first.
  --
  -- We get all the imported names from the Stg bindings and load their modules.
  --
  -- (logic adapted from GHC.Linker.Loader.loadDecls for the JS linker)
  let
    (stg_binds, stg_deps) = unzip stg_binds_with_deps
    imported_ids   = nonDetEltsUniqSet (unionVarSets stg_deps)
    imported_names = map idName imported_ids

    needed_mods :: [Module]
    needed_mods = [ nameModule n | n <- imported_names,
                    isExternalName n,       -- Names from other modules
                    not (isWiredInName n)   -- Exclude wired-in names
                  ]                         -- (see note below)
    -- Exclude wired-in names because we may not have read
    -- their interface files, so getLinkDeps will fail
    -- All wired-in names are in the base package, which we link
    -- by default, so we can safely ignore them here.

  -- Initialise the linker (if it's not been done already)
  initLoaderState interp hsc_env

  -- Take lock for the actual work.
  (dep_linkables, dep_units) <- modifyLoaderState interp $ \pls -> do
    let link_opts = initLinkDepsOpts hsc_env

    -- Find what packages and linkables are required
    deps <- getLinkDeps link_opts interp pls srcspan needed_mods
    -- We update the LinkerState even if the JS interpreter maintains its linker
    -- state independently to load new objects here.

    let objs = mapMaybe linkableFilterNative (ldNeededLinkables deps)
        (objs_loaded', _new_objs) = rmDupLinkables (objs_loaded pls) objs

    -- FIXME: we should make the JS linker load new_objs here, instead of
    -- on-demand.

    -- FIXME: we don't report needed units because we would have to find a way
    -- to build a meaningful LoadedPkgInfo (see the mess in
    -- GHC.Linker.Loader.{loadPackage,loadPackages'}). Detecting what to load
    -- and actually loading (using the native interpreter) are intermingled, so
    -- we can't directly reuse this code.
    let pls' = pls { objs_loaded = objs_loaded' }
    pure (pls', (ldAllLinkables deps, emptyUDFM {- ldNeededUnits deps -}) )


  let foreign_stubs    = NoStubs
      spt_entries      = mempty
      cost_centre_info = mempty

  -- codegen into object file whose path is in out_obj
  out_obj <- newTempName logger tmpfs tmp_dir TFL_CurrentModule "o"
  stgToJS logger js_config stg_binds this_mod spt_entries foreign_stubs cost_centre_info out_obj

  let TxtI id_sym = makeIdentForId binding_id Nothing IdPlain this_mod
  -- link code containing binding "id_sym = expr", using id_sym as root
  withJSInterp i $ \inst -> do
    let roots = mkExportedModFuns this_mod [id_sym]
    jsLinkObject logger tmpfs tmp_dir js_config unit_env inst out_obj roots

  -- look up "id_sym" closure and create a StablePtr (HValue) from it
  href <- lookupClosure interp (IFaststringSymbol id_sym) >>= \case
    Nothing -> pprPanic "Couldn't find just linked TH closure" (ppr id_sym)
    Just r  -> pure r

  binding_fref <- withJSInterp i $ \inst ->
                    mkForeignRef href (freeReallyRemoteRef inst href)

  return (castForeignRef binding_fref, dep_linkables, dep_units)


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


{- **********************************************************************
%*                                                                      *
        Progress Messages: Module i of n
%*                                                                      *
%********************************************************************* -}

showModuleIndex :: (Int, Int) -> SDoc
showModuleIndex (i,n) = text "[" <> pad <> int i <> text " of " <> int n <> text "] "
  where
    -- compute the length of x > 0 in base 10
    len x = ceiling (logBase 10 (fromIntegral x+1) :: Float)
    pad = text (replicate (len n - len i) ' ') -- TODO: use GHC.Utils.Ppr.RStr

writeInterfaceOnlyMode :: DynFlags -> Bool
writeInterfaceOnlyMode dflags =
 gopt Opt_WriteInterface dflags &&
 not (backendGeneratesCode (backend dflags))
