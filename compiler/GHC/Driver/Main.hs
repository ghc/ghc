{-# LANGUAGE BangPatterns, CPP, MagicHash, NondecreasingIndentation #-}
{-# OPTIONS_GHC -fprof-auto-top #-}

-------------------------------------------------------------------------------
--
-- | Main API for compiling plain Haskell source code.
--
-- This module implements compilation of a Haskell source. It is
-- /not/ concerned with preprocessing of source files; this is handled
-- in GHC.Driver.Pipeline
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

    -- * Compiling complete source files
    , Messager, batchMsg
    , HscStatus (..)
    , hscIncrementalCompile
    , hscMaybeWriteIface
    , hscCompileCmmFile

    , hscGenHardCode
    , hscInteractive

    -- * Running passes separately
    , hscParse
    , hscTypecheckRename
    , hscDesugar
    , makeSimpleDetails
    , hscSimplify -- ToDo, shouldn't really export this

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
    , hscDecls, hscParseDeclsWithLocation, hscDeclsWithLocation, hscParsedDecls
    , hscTcExpr, TcRnExprMode(..), hscImport, hscKcType
    , hscParseExpr
    , hscParseType
    , hscCompileCoreExpr
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
    ) where

import GhcPrelude

import Data.Data hiding (Fixity, TyCon)
import Data.Maybe       ( fromJust )
import GHC.Types.Id
import GHC.Runtime.Interpreter ( addSptEntry )
import GHCi.RemoteTypes        ( ForeignHValue )
import GHC.CoreToByteCode      ( byteCodeGen, coreExprToBCOs )
import GHC.Runtime.Linker
import GHC.Core.Op.Tidy        ( tidyExpr )
import GHC.Core.Type           ( Type, Kind )
import GHC.Core.Lint           ( lintInteractiveExpr )
import GHC.Types.Var.Env       ( emptyTidyEnv )
import Panic
import GHC.Core.ConLike

import ApiAnnotation
import GHC.Types.Module
import GHC.Driver.Packages
import GHC.Types.Name.Reader
import GHC.Hs
import GHC.Hs.Dump
import GHC.Core
import StringBuffer
import Parser
import Lexer
import GHC.Types.SrcLoc
import TcRnDriver
import GHC.IfaceToCore  ( typecheckIface )
import TcRnMonad
import TcHsSyn          ( ZonkFlexi (DefaultFlexi) )
import GHC.Types.Name.Cache ( initNameCache )
import PrelInfo
import GHC.Core.Op.Simplify.Driver
import GHC.HsToCore
import GHC.Iface.Load   ( ifaceStats, initExternalPackageState, writeIface )
import GHC.Iface.Make
import GHC.Iface.Recomp
import GHC.Iface.Tidy
import GHC.CoreToStg.Prep
import GHC.CoreToStg    ( coreToStg )
import GHC.Stg.Syntax
import GHC.Stg.FVs      ( annTopBindingsFreeVars )
import GHC.Stg.Pipeline ( stg2stg )
import qualified GHC.StgToCmm as StgToCmm ( codeGen )
import GHC.Types.CostCentre
import GHC.Core.TyCon
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Cmm
import GHC.Cmm.Parser         ( parseCmmFile )
import GHC.Cmm.Info.Build
import GHC.Cmm.Pipeline
import GHC.Cmm.Info
import GHC.Driver.CodeOutput
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv
import Fingerprint      ( Fingerprint )
import GHC.Driver.Hooks
import TcEnv
import PrelNames
import GHC.Driver.Plugins
import GHC.Runtime.Loader   ( initializePlugins )

import GHC.Driver.Session
import ErrUtils

import Outputable
import GHC.Types.Name.Env
import HscStats         ( ppSourceStats )
import GHC.Driver.Types
import FastString
import GHC.Types.Unique.Supply
import Bag
import Exception
import qualified Stream
import Stream (Stream)

import Util

import Data.List        ( nub, isPrefixOf, partition )
import Control.Monad
import Data.IORef
import System.FilePath as FilePath
import System.Directory
import System.IO (fixIO)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Functor
import Control.DeepSeq (force)

import GHC.Iface.Ext.Ast    ( mkHieFile )
import GHC.Iface.Ext.Types  ( getAsts, hie_asts, hie_module )
import GHC.Iface.Ext.Binary ( readHieFile, writeHieFile , hie_file_result)
import GHC.Iface.Ext.Debug  ( diffFile, validateScopes )

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
    fc_var  <- newIORef emptyInstalledModuleEnv
    emptyDynLinker <- uninitializedLinker
    return HscEnv {  hsc_dflags       = dflags
                  ,  hsc_targets      = []
                  ,  hsc_mod_graph    = emptyMG
                  ,  hsc_IC           = emptyInteractiveContext dflags
                  ,  hsc_HPT          = emptyHomePackageTable
                  ,  hsc_EPS          = eps_var
                  ,  hsc_NC           = nc_var
                  ,  hsc_FC           = fc_var
                  ,  hsc_type_env_var = Nothing
                  ,  hsc_interp       = Nothing
                  ,  hsc_dynLinker    = emptyDynLinker
                  }

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

-- | Log warnings and throw errors, assuming the messages
-- contain at least one error (e.g. coming from PFailed)
handleWarningsThrowErrors :: Messages -> Hsc a
handleWarningsThrowErrors (warns, errs) = do
    logWarnings warns
    dflags <- getDynFlags
    (wWarns, wErrs) <- warningsToMessages dflags <$> getWarnings
    liftIO $ printBagOfErrors dflags wWarns
    throwErrors (unionBags errs wErrs)

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

hscTcRnLookupRdrName :: HscEnv -> Located RdrName -> IO [Name]
hscTcRnLookupRdrName hsc_env0 rdr_name
  = runInteractiveHsc hsc_env0 $
    do { hsc_env <- getHscEnv
       ; ioMsgMaybe $ tcRnLookupRdrName hsc_env rdr_name }

hscTcRcLookupName :: HscEnv -> Name -> IO (Maybe TyThing)
hscTcRcLookupName hsc_env0 name = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe' $ tcRnLookupName hsc_env name
      -- ignore errors: the only error we're likely to get is
      -- "name not found", and the Maybe in the return type
      -- is used to indicate that.

hscTcRnGetInfo :: HscEnv -> Name
               -> IO (Maybe (TyThing, Fixity, [ClsInst], [FamInst], SDoc))
hscTcRnGetInfo hsc_env0 name
  = runInteractiveHsc hsc_env0 $
    do { hsc_env <- getHscEnv
       ; ioMsgMaybe' $ tcRnGetInfo hsc_env name }

hscIsGHCiMonad :: HscEnv -> String -> IO Name
hscIsGHCiMonad hsc_env name
  = runHsc hsc_env $ ioMsgMaybe $ isGHCiMonad hsc_env name

hscGetModuleInterface :: HscEnv -> Module -> IO ModIface
hscGetModuleInterface hsc_env0 mod = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe $ getModuleInterface hsc_env mod

-- -----------------------------------------------------------------------------
-- | Rename some import declarations
hscRnImportDecls :: HscEnv -> [LImportDecl GhcPs] -> IO GlobalRdrEnv
hscRnImportDecls hsc_env0 import_decls = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe $ tcRnImportDecls hsc_env import_decls

-- -----------------------------------------------------------------------------
-- | parse a file, returning the abstract syntax

hscParse :: HscEnv -> ModSummary -> IO HsParsedModule
hscParse hsc_env mod_summary = runHsc hsc_env $ hscParse' mod_summary

-- internal version, that doesn't fail due to -Werror
hscParse' :: ModSummary -> Hsc HsParsedModule
hscParse' mod_summary
 | Just r <- ms_parsed_mod mod_summary = return r
 | otherwise = {-# SCC "Parser" #-}
    withTimingD (text "Parser"<+>brackets (ppr $ ms_mod mod_summary))
                (const ()) $ do
    dflags <- getDynFlags
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
    let parseMod | HsigFile == ms_hsc_src mod_summary
                 = parseSignature
                 | otherwise = parseModule

    case unP parseMod (mkPState dflags buf loc) of
        PFailed pst ->
            handleWarningsThrowErrors (getMessages pst dflags)
        POk pst rdr_module -> do
            let (warns, errs) = getMessages pst dflags
            logWarnings warns
            liftIO $ dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser"
                        FormatHaskell (ppr rdr_module)
            liftIO $ dumpIfSet_dyn dflags Opt_D_dump_parsed_ast "Parser AST"
                        FormatHaskell (showAstData NoBlankSrcSpan rdr_module)
            liftIO $ dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
                        FormatText (ppSourceStats False rdr_module)
            when (not $ isEmptyBag errs) $ throwErrors errs

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
                srcs0 = nub $ filter (not . (tmpDir dflags `isPrefixOf`))
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

            let api_anns = ApiAnns {
                      apiAnnItems = M.fromListWith (++) $ annotations pst,
                      apiAnnEofPos = eof_pos pst,
                      apiAnnComments = M.fromList (annotations_comments pst),
                      apiAnnRogueComments = comment_q pst
                   }
                res = HsParsedModule {
                      hpm_module    = rdr_module,
                      hpm_src_files = srcs2,
                      hpm_annotations = api_anns
                   }

            -- apply parse transformation of plugins
            let applyPluginAction p opts
                  = parsedResultAction p opts mod_summary
            withPlugins dflags applyPluginAction res


-- -----------------------------------------------------------------------------
-- | If the renamed source has been kept, extract it. Dump it if requested.
extract_renamed_stuff :: ModSummary -> TcGblEnv -> Hsc RenamedStuff
extract_renamed_stuff mod_summary tc_result = do
    let rn_info = getRenamedStuff tc_result

    dflags <- getDynFlags
    liftIO $ dumpIfSet_dyn dflags Opt_D_dump_rn_ast "Renamer"
                FormatHaskell (showAstData NoBlankSrcSpan rn_info)

    -- Create HIE files
    when (gopt Opt_WriteHie dflags) $ do
        -- I assume this fromJust is safe because `-fwrite-hie-file`
        -- enables the option which keeps the renamed source.
        hieFile <- mkHieFile mod_summary tc_result (fromJust rn_info)
        let out_file = ml_hie_file $ ms_location mod_summary
        liftIO $ writeHieFile out_file hieFile
        liftIO $ dumpIfSet_dyn dflags Opt_D_dump_hie "HIE AST" FormatHaskell (ppr $ hie_asts hieFile)

        -- Validate HIE files
        when (gopt Opt_ValidateHie dflags) $ do
            hs_env <- Hsc $ \e w -> return (e, w)
            liftIO $ do
              -- Validate Scopes
              case validateScopes (hie_module hieFile) $ getAsts $ hie_asts hieFile of
                  [] -> putMsg dflags $ text "Got valid scopes"
                  xs -> do
                    putMsg dflags $ text "Got invalid scopes"
                    mapM_ (putMsg dflags) xs
              -- Roundtrip testing
              nc <- readIORef $ hsc_NC hs_env
              (file', _) <- readHieFile nc out_file
              case diffFile hieFile (hie_file_result file') of
                [] ->
                  putMsg dflags $ text "Got no roundtrip errors"
                xs -> do
                  putMsg dflags $ text "Got roundtrip errors"
                  mapM_ (putMsg (dopt_set dflags Opt_D_ppr_debug)) xs
    return rn_info


-- -----------------------------------------------------------------------------
-- | Rename and typecheck a module, additionally returning the renamed syntax
hscTypecheckRename :: HscEnv -> ModSummary -> HsParsedModule
                   -> IO (TcGblEnv, RenamedStuff)
hscTypecheckRename hsc_env mod_summary rdr_module = runHsc hsc_env $
    hsc_typecheck True mod_summary (Just rdr_module)


-- | A bunch of logic piled around around @tcRnModule'@, concerning a) backpack
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
        outer_mod = ms_mod mod_summary
        mod_name = moduleName outer_mod
        outer_mod' = mkModule (thisPackage dflags) mod_name
        inner_mod = canonicalizeHomeModule dflags mod_name
        src_filename  = ms_hspp_file mod_summary
        real_loc = realSrcLocSpan $ mkRealSrcLoc (mkFastString src_filename) 1 1
        keep_rn' = gopt Opt_WriteHie dflags || keep_rn
    MASSERT( moduleUnitId outer_mod == thisPackage dflags )
    tc_result <- if hsc_src == HsigFile && not (isHoleModule inner_mod)
        then ioMsgMaybe $ tcRnInstantiateSignature hsc_env outer_mod' real_loc
        else
         do hpm <- case mb_rdr_module of
                    Just hpm -> return hpm
                    Nothing -> hscParse' mod_summary
            tc_result0 <- tcRnModule' mod_summary keep_rn' hpm
            if hsc_src == HsigFile
                then do (iface, _, _) <- liftIO $ hscSimpleIface hsc_env tc_result0 Nothing
                        ioMsgMaybe $
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
    dflags   <- getDynFlags

    -- -Wmissing-safe-haskell-mode
    when (not (safeHaskellModeEnabled dflags)
          && wopt Opt_WarnMissingSafeHaskellMode dflags) $
        logWarnings $ unitBag $
        makeIntoWarning (Reason Opt_WarnMissingSafeHaskellMode) $
        mkPlainWarnMsg dflags (getLoc (hpm_module mod)) $
        warnMissingSafeHaskellMode

    tcg_res <- {-# SCC "Typecheck-Rename" #-}
               ioMsgMaybe $
                   tcRnModule hsc_env sum
                     save_rn_syntax mod

    -- See Note [Safe Haskell Overlapping Instances Implementation]
    -- although this is used for more than just that failure case.
    (tcSafeOK, whyUnsafe) <- liftIO $ readIORef (tcg_safeInfer tcg_res)
    let allSafeOK = safeInferred dflags && tcSafeOK

    -- end of the safe haskell line, how to respond to user?
    res <- if not (safeHaskellOn dflags)
                || (safeInferOn dflags && not allSafeOK)
             -- if safe Haskell off or safe infer failed, mark unsafe
             then markUnsafeInfer tcg_res whyUnsafe

             -- module (could be) safe, throw warning if needed
             else do
                 tcg_res' <- hscCheckSafeImports tcg_res
                 safe <- liftIO $ fst <$> readIORef (tcg_safeInfer tcg_res')
                 when safe $ do
                   case wopt Opt_WarnSafe dflags of
                     True
                       | safeHaskell dflags == Sf_Safe -> return ()
                       | otherwise -> (logWarnings $ unitBag $
                              makeIntoWarning (Reason Opt_WarnSafe) $
                              mkPlainWarnMsg dflags (warnSafeOnLoc dflags) $
                              errSafe tcg_res')
                     False | safeHaskell dflags == Sf_Trustworthy &&
                             wopt Opt_WarnTrustworthySafe dflags ->
                             (logWarnings $ unitBag $
                              makeIntoWarning (Reason Opt_WarnTrustworthySafe) $
                              mkPlainWarnMsg dflags (trustworthyOnLoc dflags) $
                              errTwthySafe tcg_res')
                     False -> return ()
                 return tcg_res'

    -- apply plugins to the type checking result


    return res
  where
    pprMod t  = ppr $ moduleName $ tcg_mod t
    errSafe t = quotes (pprMod t) <+> text "has been inferred as safe!"
    errTwthySafe t = quotes (pprMod t)
      <+> text "is marked as Trustworthy but has been inferred as safe!"
    warnMissingSafeHaskellMode = ppr (moduleName (ms_mod sum))
      <+> text "is missing Safe Haskell mode"

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


type Messager = HscEnv -> (Int,Int) -> RecompileRequired -> ModSummary -> IO ()

-- | This function runs GHC's frontend with recompilation
-- avoidance. Specifically, it checks if recompilation is needed,
-- and if it is, it parses and typechecks the input module.
-- It does not write out the results of typechecking (See
-- compileOne and hscIncrementalCompile).
hscIncrementalFrontend :: Bool -- always do basic recompilation check?
                       -> Maybe TcGblEnv
                       -> Maybe Messager
                       -> ModSummary
                       -> SourceModified
                       -> Maybe ModIface  -- Old interface, if available
                       -> (Int,Int)       -- (i,n) = module i of n (for msgs)
                       -> Hsc (Either ModIface (FrontendResult, Maybe Fingerprint))

hscIncrementalFrontend
  always_do_basic_recompilation_check m_tc_result
  mHscMessage mod_summary source_modified mb_old_iface mod_index
    = do
    hsc_env <- getHscEnv

    let msg what = case mHscMessage of
                   Just hscMessage -> hscMessage hsc_env mod_index what mod_summary
                   Nothing -> return ()

        skip iface = do
            liftIO $ msg UpToDate
            return $ Left iface

        compile mb_old_hash reason = do
            liftIO $ msg reason
            (tc_result, _) <- hsc_typecheck False mod_summary Nothing
            return $ Right (FrontendTypecheck tc_result, mb_old_hash)

        stable = case source_modified of
                     SourceUnmodifiedAndStable -> True
                     _                         -> False

    case m_tc_result of
         Just tc_result
          | not always_do_basic_recompilation_check ->
             return $ Right (FrontendTypecheck tc_result, Nothing)
         _ -> do
            (recomp_reqd, mb_checked_iface)
                <- {-# SCC "checkOldIface" #-}
                   liftIO $ checkOldIface hsc_env mod_summary
                                source_modified mb_old_iface
            -- save the interface that comes back from checkOldIface.
            -- In one-shot mode we don't have the old iface until this
            -- point, when checkOldIface reads it from the disk.
            let mb_old_hash = fmap (mi_iface_hash . mi_final_exts) mb_checked_iface

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
                        return $ Right (FrontendTypecheck tc_result, mb_old_hash)

--------------------------------------------------------------
-- Compilers
--------------------------------------------------------------

-- | Used by both OneShot and batch mode. Runs the pipeline HsSyn and Core parts
-- of the pipeline.
-- We return a interface if we already had an old one around and recompilation
-- was not needed. Otherwise it will be created during later passes when we
-- run the compilation pipeline.
hscIncrementalCompile :: Bool
                      -> Maybe TcGblEnv
                      -> Maybe Messager
                      -> HscEnv
                      -> ModSummary
                      -> SourceModified
                      -> Maybe ModIface
                      -> (Int,Int)
                      -> IO (HscStatus, DynFlags)
hscIncrementalCompile always_do_basic_recompilation_check m_tc_result
    mHscMessage hsc_env' mod_summary source_modified mb_old_iface mod_index
  = do
    dflags <- initializePlugins hsc_env' (hsc_dflags hsc_env')
    let hsc_env'' = hsc_env' { hsc_dflags = dflags }

    -- One-shot mode needs a knot-tying mutable variable for interface
    -- files. See TcRnTypes.TcGblEnv.tcg_type_env_var.
    -- See also Note [hsc_type_env_var hack]
    type_env_var <- newIORef emptyNameEnv
    let mod = ms_mod mod_summary
        hsc_env | isOneShot (ghcMode (hsc_dflags hsc_env''))
                = hsc_env'' { hsc_type_env_var = Just (mod, type_env_var) }
                | otherwise
                = hsc_env''

    -- NB: enter Hsc monad here so that we don't bail out early with
    -- -Werror on typechecker warnings; we also want to run the desugarer
    -- to get those warnings too. (But we'll always exit at that point
    -- because the desugarer runs ioMsgMaybe.)
    runHsc hsc_env $ do
    e <- hscIncrementalFrontend always_do_basic_recompilation_check m_tc_result mHscMessage
            mod_summary source_modified mb_old_iface mod_index
    case e of
        -- We didn't need to do any typechecking; the old interface
        -- file on disk was good enough.
        Left iface -> do
            -- Knot tying!  See Note [Knot-tying typecheckIface]
            details <- liftIO . fixIO $ \details' -> do
                let hsc_env' =
                        hsc_env {
                            hsc_HPT = addToHpt (hsc_HPT hsc_env)
                                        (ms_mod_name mod_summary) (HomeModInfo iface details' Nothing)
                        }
                -- NB: This result is actually not that useful
                -- in one-shot mode, since we're not going to do
                -- any further typechecking.  It's much more useful
                -- in make mode, since this HMI will go into the HPT.
                details <- genModDetails hsc_env' iface
                return details
            return (HscUpToDate iface details, dflags)
        -- We finished type checking.  (mb_old_hash is the hash of
        -- the interface that existed on disk; it's possible we had
        -- to retypecheck but the resulting interface is exactly
        -- the same.)
        Right (FrontendTypecheck tc_result, mb_old_hash) -> do
            status <- finish mod_summary tc_result mb_old_hash
            return (status, dflags)

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
finish :: ModSummary
       -> TcGblEnv
       -> Maybe Fingerprint
       -> Hsc HscStatus
finish summary tc_result mb_old_hash = do
  hsc_env <- getHscEnv
  let dflags = hsc_dflags hsc_env
      target = hscTarget dflags
      hsc_src = ms_hsc_src summary

  -- Desugar, if appropriate
  --
  -- We usually desugar even when we are not generating code, otherwise we
  -- would miss errors thrown by the desugaring (see #10600). The only
  -- exceptions are when the Module is Ghc.Prim or when it is not a
  -- HsSrcFile Module.
  mb_desugar <-
      if ms_mod summary /= gHC_PRIM && hsc_src == HsSrcFile
      then Just <$> hscDesugar' (ms_location summary) tc_result
      else pure Nothing

  -- Simplify, if appropriate, and (whether we simplified or not) generate an
  -- interface file.
  case mb_desugar of
      -- Just cause we desugared doesn't mean we are generating code, see above.
      Just desugared_guts | target /= HscNothing -> do
          plugins <- liftIO $ readIORef (tcg_th_coreplugins tc_result)
          simplified_guts <- hscSimplify' plugins desugared_guts

          (cg_guts, details) <- {-# SCC "CoreTidy" #-}
              liftIO $ tidyProgram hsc_env simplified_guts

          let !partial_iface =
                {-# SCC "GHC.Driver.Main.mkPartialIface" #-}
                -- This `force` saves 2M residency in test T10370
                -- See Note [Avoiding space leaks in toIface*] for details.
                force (mkPartialIface hsc_env details simplified_guts)

          return HscRecomp { hscs_guts = cg_guts,
                             hscs_mod_location = ms_location summary,
                             hscs_mod_details = details,
                             hscs_partial_iface = partial_iface,
                             hscs_old_iface_hash = mb_old_hash,
                             hscs_iface_dflags = dflags }

      -- We are not generating code, so we can skip simplification
      -- and generate a simple interface.
      _ -> do
        (iface, mb_old_iface_hash, details) <- liftIO $
          hscSimpleIface hsc_env tc_result mb_old_hash

        liftIO $ hscMaybeWriteIface dflags iface mb_old_iface_hash (ms_location summary)

        return $ case (target, hsc_src) of
          (HscNothing, _) -> HscNotGeneratingCode iface details
          (_, HsBootFile) -> HscUpdateBoot iface details
          (_, HsigFile) -> HscUpdateSig iface details
          _ -> panic "finish"

{-
Note [Writing interface files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We write interface files in GHC.Driver.Main and GHC.Driver.Pipeline using
hscMaybeWriteIface, but only once per compilation (twice with dynamic-too).

* If a compilation does NOT require (re)compilation of the hard code we call
  hscMaybeWriteIface inside GHC.Driver.Main:finish.
* If we run in One Shot mode and target bytecode we write it in compileOne'
* Otherwise we must be compiling to regular hard code and require recompilation.
  In this case we create the interface file inside RunPhase using the interface
  generator contained inside the HscRecomp status.
-}
hscMaybeWriteIface :: DynFlags -> ModIface -> Maybe Fingerprint -> ModLocation -> IO ()
hscMaybeWriteIface dflags iface old_iface location = do
    let force_write_interface = gopt Opt_WriteInterface dflags
        write_interface = case hscTarget dflags of
                            HscNothing      -> False
                            HscInterpreted  -> False
                            _               -> True
        no_change = old_iface == Just (mi_iface_hash (mi_final_exts iface))

    when (write_interface || force_write_interface) $
          hscWriteIface dflags iface no_change location

--------------------------------------------------------------
-- NoRecomp handlers
--------------------------------------------------------------

-- NB: this must be knot-tied appropriately, see hscIncrementalCompile
genModDetails :: HscEnv -> ModIface -> IO ModDetails
genModDetails hsc_env old_iface
  = do
    new_details <- {-# SCC "tcRnIface" #-}
                   initIfaceLoad hsc_env (typecheckIface old_iface)
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
    checkRULES dflags tcg_env' = do
      case safeLanguageOn dflags of
          True -> do
              -- XSafe: we nuke user written RULES
              logWarnings $ warns dflags (tcg_rules tcg_env')
              return tcg_env' { tcg_rules = [] }
          False
                -- SafeInferred: user defined RULES, so not safe
              | safeInferOn dflags && not (null $ tcg_rules tcg_env')
              -> markUnsafeInfer tcg_env' $ warns dflags (tcg_rules tcg_env')

                -- Trustworthy OR SafeInferred: with no RULES
              | otherwise
              -> return tcg_env'

    warns dflags rules = listToBag $ map (warnRules dflags) rules

    warnRules :: DynFlags -> GenLocated SrcSpan (RuleDecl GhcTc) -> ErrMsg
    warnRules dflags (L loc (HsRule { rd_name = n })) =
        mkPlainWarnMsg dflags loc $
            text "Rule \"" <> ftext (snd $ unLoc n) <> text "\" ignored" $+$
            text "User defined rules are disabled under Safe Haskell"
    warnRules _ (L _ (XRuleDecl nec)) = noExtCon nec

-- | Validate that safe imported modules are actually safe.  For modules in the
-- HomePackage (the package the module we are compiling in resides) this just
-- involves checking its trust type is 'Safe' or 'Trustworthy'. For modules
-- that reside in another package we also must check that the external package
-- is trusted. See the Note [Safe Haskell Trust Check] above for more
-- information.
--
-- The code for this is quite tricky as the whole algorithm is done in a few
-- distinct phases in different parts of the code base. See
-- GHC.Rename.Names.rnImportDecl for where package trust dependencies for a
-- module are collected and unioned.  Specifically see the Note [Tracking Trust
-- Transitively] in GHC.Rename.Names and the Note [Trust Own Package] in
-- GHC.Rename.Names.
checkSafeImports :: TcGblEnv -> Hsc TcGblEnv
checkSafeImports tcg_env
    = do
        dflags <- getDynFlags
        imps <- mapM condense imports'
        let (safeImps, regImps) = partition (\(_,_,s) -> s) imps

        -- We want to use the warning state specifically for detecting if safe
        -- inference has failed, so store and clear any existing warnings.
        oldErrs <- getWarnings
        clearWarnings

        -- Check safe imports are correct
        safePkgs <- S.fromList <$> mapMaybeM checkSafe safeImps
        safeErrs <- getWarnings
        clearWarnings

        -- Check non-safe imports are correct if inferring safety
        -- See the Note [Safe Haskell Inference]
        (infErrs, infPkgs) <- case (safeInferOn dflags) of
          False -> return (emptyBag, S.empty)
          True -> do infPkgs <- S.fromList <$> mapMaybeM checkSafe regImps
                     infErrs <- getWarnings
                     clearWarnings
                     return (infErrs, infPkgs)

        -- restore old errors
        logWarnings oldErrs

        case (isEmptyBag safeErrs) of
          -- Failed safe check
          False -> liftIO . throwIO . mkSrcErr $ safeErrs

          -- Passed safe check
          True -> do
            let infPassed = isEmptyBag infErrs
            tcg_env' <- case (not infPassed) of
              True  -> markUnsafeInfer tcg_env infErrs
              False -> return tcg_env
            when (packageTrustOn dflags) $ checkPkgTrust pkgReqs
            let newTrust = pkgTrustReqs dflags safePkgs infPkgs infPassed
            return tcg_env' { tcg_imports = impInfo `plusImportAvails` newTrust }

  where
    impInfo  = tcg_imports tcg_env     -- ImportAvails
    imports  = imp_mods impInfo        -- ImportedMods
    imports1 = moduleEnvToList imports -- (Module, [ImportedBy])
    imports' = map (fmap importedByUser) imports1 -- (Module, [ImportedModsVal])
    pkgReqs  = imp_trust_pkgs impInfo  -- [UnitId]

    condense :: (Module, [ImportedModsVal]) -> Hsc (Module, SrcSpan, IsSafeImport)
    condense (_, [])   = panic "GHC.Driver.Main.condense: Pattern match failure!"
    condense (m, x:xs) = do imv <- foldlM cond' x xs
                            return (m, imv_span imv, imv_is_safe imv)

    -- ImportedModsVal = (ModuleName, Bool, SrcSpan, IsSafeImport)
    cond' :: ImportedModsVal -> ImportedModsVal -> Hsc ImportedModsVal
    cond' v1 v2
        | imv_is_safe v1 /= imv_is_safe v2
        = do
            dflags <- getDynFlags
            throwOneError $ mkPlainErrMsg dflags (imv_span v1)
              (text "Module" <+> ppr (imv_name v1) <+>
              (text $ "is imported both as a safe and unsafe import!"))
        | otherwise
        = return v1

    -- easier interface to work with
    checkSafe :: (Module, SrcSpan, a) -> Hsc (Maybe InstalledUnitId)
    checkSafe (m, l, _) = fst `fmap` hscCheckSafe' m l

    -- what pkg's to add to our trust requirements
    pkgTrustReqs :: DynFlags -> Set InstalledUnitId -> Set InstalledUnitId ->
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
    errs <- getWarnings
    return $ isEmptyBag errs

-- | Return if a module is trusted and the pkgs it depends on to be trusted.
hscGetSafe :: HscEnv -> Module -> SrcSpan -> IO (Bool, Set InstalledUnitId)
hscGetSafe hsc_env m l = runHsc hsc_env $ do
    (self, pkgs) <- hscCheckSafe' m l
    good         <- isEmptyBag `fmap` getWarnings
    clearWarnings -- don't want them printed...
    let pkgs' | Just p <- self = S.insert p pkgs
              | otherwise      = pkgs
    return (good, pkgs')

-- | Is a module trusted? If not, throw or log errors depending on the type.
-- Return (regardless of trusted or not) if the trust type requires the modules
-- own package be trusted and a list of other packages required to be trusted
-- (these later ones haven't been checked) but the own package trust has been.
hscCheckSafe' :: Module -> SrcSpan
  -> Hsc (Maybe InstalledUnitId, Set InstalledUnitId)
hscCheckSafe' m l = do
    dflags <- getDynFlags
    (tw, pkgs) <- isModSafe m l
    case tw of
        False                     -> return (Nothing, pkgs)
        True | isHomePkg dflags m -> return (Nothing, pkgs)
             -- TODO: do we also have to check the trust of the instantiation?
             -- Not necessary if that is reflected in dependencies
             | otherwise   -> return (Just $ toInstalledUnitId (moduleUnitId m), pkgs)
  where
    isModSafe :: Module -> SrcSpan -> Hsc (Bool, Set InstalledUnitId)
    isModSafe m l = do
        dflags <- getDynFlags
        iface <- lookup' m
        case iface of
            -- can't load iface to check trust!
            Nothing -> throwOneError $ mkPlainErrMsg dflags l
                         $ text "Can't load the interface file for" <+> ppr m
                           <> text ", to check that it can be safely imported"

            -- got iface, check trust
            Just iface' ->
                let trust = getSafeMode $ mi_trust iface'
                    trust_own_pkg = mi_trust_pkg iface'
                    -- check module is trusted
                    safeM = trust `elem` [Sf_Safe, Sf_SafeInferred, Sf_Trustworthy]
                    -- check package is trusted
                    safeP = packageTrusted dflags trust trust_own_pkg m
                    -- pkg trust reqs
                    pkgRs = S.fromList . map fst $ filter snd $ dep_pkgs $ mi_deps iface'
                    -- warn if Safe module imports Safe-Inferred module.
                    warns = if wopt Opt_WarnInferredSafeImports dflags
                                && safeLanguageOn dflags
                                && trust == Sf_SafeInferred
                                then inferredImportWarn
                                else emptyBag
                    -- General errors we throw but Safe errors we log
                    errs = case (safeM, safeP) of
                        (True, True ) -> emptyBag
                        (True, False) -> pkgTrustErr
                        (False, _   ) -> modTrustErr
                in do
                    logWarnings warns
                    logWarnings errs
                    return (trust == Sf_Trustworthy, pkgRs)

                where
                    inferredImportWarn = unitBag
                        $ makeIntoWarning (Reason Opt_WarnInferredSafeImports)
                        $ mkErrMsg dflags l (pkgQual dflags)
                        $ sep
                            [ text "Importing Safe-Inferred module "
                                <> ppr (moduleName m)
                                <> text " from explicitly Safe module"
                            ]
                    pkgTrustErr = unitBag $ mkErrMsg dflags l (pkgQual dflags) $
                        sep [ ppr (moduleName m)
                                <> text ": Can't be safely imported!"
                            , text "The package (" <> ppr (moduleUnitId m)
                                <> text ") the module resides in isn't trusted."
                            ]
                    modTrustErr = unitBag $ mkErrMsg dflags l (pkgQual dflags) $
                        sep [ ppr (moduleName m)
                                <> text ": Can't be safely imported!"
                            , text "The module itself isn't safe." ]

    -- | Check the package a module resides in is trusted. Safe compiled
    -- modules are trusted without requiring that their package is trusted. For
    -- trustworthy modules, modules in the home package are trusted but
    -- otherwise we check the package trust flag.
    packageTrusted :: DynFlags -> SafeHaskellMode -> Bool -> Module -> Bool
    packageTrusted _ Sf_None      _ _ = False -- shouldn't hit these cases
    packageTrusted _ Sf_Ignore    _ _ = False -- shouldn't hit these cases
    packageTrusted _ Sf_Unsafe    _ _ = False -- prefer for completeness.
    packageTrusted dflags _ _ _
        | not (packageTrustOn dflags) = True
    packageTrusted _ Sf_Safe  False _ = True
    packageTrusted _ Sf_SafeInferred False _ = True
    packageTrusted dflags _ _ m
        | isHomePkg dflags m = True
        | otherwise = trusted $ getPackageDetails dflags (moduleUnitId m)

    lookup' :: Module -> Hsc (Maybe ModIface)
    lookup' m = do
        hsc_env <- getHscEnv
        hsc_eps <- liftIO $ hscEPS hsc_env
        let pkgIfaceT = eps_PIT hsc_eps
            homePkgT  = hsc_HPT hsc_env
            iface     = lookupIfaceByModule homePkgT pkgIfaceT m
        -- the 'lookupIfaceByModule' method will always fail when calling from GHCi
        -- as the compiler hasn't filled in the various module tables
        -- so we need to call 'getModuleInterface' to load from disk
        iface' <- case iface of
            Just _  -> return iface
            Nothing -> snd `fmap` (liftIO $ getModuleInterface hsc_env m)
        return iface'


    isHomePkg :: DynFlags -> Module -> Bool
    isHomePkg dflags m
        | thisPackage dflags == moduleUnitId m = True
        | otherwise                               = False

-- | Check the list of packages are trusted.
checkPkgTrust :: Set InstalledUnitId -> Hsc ()
checkPkgTrust pkgs = do
    dflags <- getDynFlags
    let errors = S.foldr go [] pkgs
        go pkg acc
            | trusted $ getInstalledPackageDetails (pkgState dflags) pkg
            = acc
            | otherwise
            = (:acc) $ mkErrMsg dflags noSrcSpan (pkgQual dflags)
                     $ text "The package (" <> ppr pkg <> text ") is required" <>
                       text " to be trusted but it isn't!"
    case errors of
        [] -> return ()
        _  -> (liftIO . throwIO . mkSrcErr . listToBag) errors

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
markUnsafeInfer :: TcGblEnv -> WarningMessages -> Hsc TcGblEnv
markUnsafeInfer tcg_env whyUnsafe = do
    dflags <- getDynFlags

    when (wopt Opt_WarnUnsafe dflags)
         (logWarnings $ unitBag $ makeIntoWarning (Reason Opt_WarnUnsafe) $
             mkPlainWarnMsg dflags (warnUnsafeOnLoc dflags) (whyUnsafe' dflags))

    liftIO $ writeIORef (tcg_safeInfer tcg_env) (False, whyUnsafe)
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
                                    (vcat $ pprErrMsgBagWithLoc whyUnsafe) $+$
                                    (vcat $ badInsts $ tcg_insts tcg_env)
                         ]
    badFlags df   = concatMap (badFlag df) unsafeFlagsForInfer
    badFlag df (str,loc,on,_)
        | on df     = [mkLocMessage SevOutput (loc df) $
                            text str <+> text "is not allowed in Safe Haskell"]
        | otherwise = []
    badInsts insts = concatMap badInst insts

    checkOverlap (NoOverlap _) = False
    checkOverlap _             = True

    badInst ins | checkOverlap (overlapMode (is_flag ins))
                = [mkLocMessage SevOutput (nameSrcSpan $ getName $ is_dfun ins) $
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

hscSimplify :: HscEnv -> [String] -> ModGuts -> IO ModGuts
hscSimplify hsc_env plugins modguts =
    runHsc hsc_env $ hscSimplify' plugins modguts

hscSimplify' :: [String] -> ModGuts -> Hsc ModGuts
hscSimplify' plugins ds_result = do
    hsc_env <- getHscEnv
    let hsc_env_with_plugins = hsc_env
          { hsc_dflags = foldr addPluginModuleName (hsc_dflags hsc_env) plugins
          }
    {-# SCC "Core2Core" #-}
      liftIO $ core2core hsc_env_with_plugins ds_result

--------------------------------------------------------------
-- Interface generators
--------------------------------------------------------------

-- | Generate a striped down interface file, e.g. for boot files or when ghci
-- generates interface files. See Note [simpleTidyPgm - mkBootModDetailsTc]
hscSimpleIface :: HscEnv
               -> TcGblEnv
               -> Maybe Fingerprint
               -> IO (ModIface, Maybe Fingerprint, ModDetails)
hscSimpleIface hsc_env tc_result mb_old_iface
    = runHsc hsc_env $ hscSimpleIface' tc_result mb_old_iface

hscSimpleIface' :: TcGblEnv
                -> Maybe Fingerprint
                -> Hsc (ModIface, Maybe Fingerprint, ModDetails)
hscSimpleIface' tc_result mb_old_iface = do
    hsc_env   <- getHscEnv
    details   <- liftIO $ mkBootModDetailsTc hsc_env tc_result
    safe_mode <- hscGetSafeMode tc_result
    new_iface
        <- {-# SCC "MkFinalIface" #-}
           liftIO $
               mkIfaceTc hsc_env safe_mode details tc_result
    -- And the answer is ...
    liftIO $ dumpIfaceStats hsc_env
    return (new_iface, mb_old_iface, details)

--------------------------------------------------------------
-- BackEnd combinators
--------------------------------------------------------------
{-
Note [Interface filename extensions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ModLocation only contains the base names, however when generating dynamic files
the actual extension might differ from the default.

So we only load the base name from ModLocation and replace the actual extension
according to the information in DynFlags.

If we generate a interface file right after running the core pipeline we will
have set -dynamic-too and potentially generate both interface files at the same
time.

If we generate a interface file after running the backend then dynamic-too won't
be set, however then the extension will be contained in the dynflags instead so
things still work out fine.
-}

hscWriteIface :: DynFlags -> ModIface -> Bool -> ModLocation -> IO ()
hscWriteIface dflags iface no_change mod_location = do
    -- mod_location only contains the base name, so we rebuild the
    -- correct file extension from the dynflags.
    let ifaceBaseFile = ml_hi_file mod_location
    unless no_change $
        let ifaceFile = buildIfName ifaceBaseFile (hiSuf dflags)
        in  {-# SCC "writeIface" #-}
            writeIface dflags ifaceFile iface
    whenGeneratingDynamicToo dflags $ do
        -- TODO: We should do a no_change check for the dynamic
        --       interface file too
        -- When we generate iface files after core
        let dynDflags = dynamicTooMkDynamicDynFlags dflags
            -- dynDflags will have set hiSuf correctly.
            dynIfaceFile = buildIfName ifaceBaseFile (hiSuf dynDflags)

        writeIface dynDflags dynIfaceFile iface
  where
    buildIfName :: String -> String -> String
    buildIfName baseName suffix
      | Just name <- outputHi dflags
      = name
      | otherwise
      = let with_hi = replaceExtension baseName suffix
        in  addBootSuffix_maybe (mi_boot iface) with_hi

-- | Compile to hard-code.
hscGenHardCode :: HscEnv -> CgGuts -> ModLocation -> FilePath
               -> IO (FilePath, Maybe FilePath, [(ForeignSrcLang, FilePath)], NameSet)
               -- ^ @Just f@ <=> _stub.c is f
hscGenHardCode hsc_env cgguts location output_filename = do
        let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                    -- From now on, we just use the bits we need.
                    cg_module   = this_mod,
                    cg_binds    = core_binds,
                    cg_tycons   = tycons,
                    cg_foreign  = foreign_stubs0,
                    cg_foreign_files = foreign_files,
                    cg_dep_pkgs = dependencies,
                    cg_hpc_info = hpc_info } = cgguts
            dflags = hsc_dflags hsc_env
            data_tycons = filter isDataTyCon tycons
            -- cg_tycons includes newtypes, for the benefit of External Core,
            -- but we don't generate any code for newtypes

        -------------------
        -- PREPARE FOR CODE GENERATION
        -- Do saturation and convert to A-normal form
        (prepd_binds, local_ccs) <- {-# SCC "CorePrep" #-}
                       corePrepPgm hsc_env this_mod location
                                   core_binds data_tycons
        -----------------  Convert to STG ------------------
        (stg_binds, (caf_ccs, caf_cc_stacks))
            <- {-# SCC "CoreToStg" #-}
               myCoreToStg dflags this_mod prepd_binds

        let cost_centre_info =
              (S.toList local_ccs ++ caf_ccs, caf_cc_stacks)
            prof_init = profilingInitCode this_mod cost_centre_info
            foreign_stubs = foreign_stubs0 `appendStubC` prof_init

        ------------------  Code generation ------------------

        -- The back-end is streamed: each top-level function goes
        -- from Stg all the way to asm before dealing with the next
        -- top-level function, so showPass isn't very useful here.
        -- Hence we have one showPass for the whole backend, the
        -- next showPass after this will be "Assembler".
        withTiming dflags
                   (text "CodeGen"<+>brackets (ppr this_mod))
                   (const ()) $ do
            cmms <- {-# SCC "StgToCmm" #-}
                            doCodeGen hsc_env this_mod data_tycons
                                cost_centre_info
                                stg_binds hpc_info

            ------------------  Code output -----------------------
            rawcmms0 <- {-# SCC "cmmToRawCmm" #-}
                      lookupHook cmmToRawCmmHook
                        (\dflg _ -> cmmToRawCmm dflg) dflags dflags (Just this_mod) cmms

            let dump a = do
                  unless (null a) $
                    dumpIfSet_dyn dflags Opt_D_dump_cmm_raw "Raw Cmm" FormatCMM (ppr a)
                  return a
                rawcmms1 = Stream.mapM dump rawcmms0

            (output_filename, (_stub_h_exists, stub_c_exists), foreign_fps, caf_infos)
                <- {-# SCC "codeOutput" #-}
                  codeOutput dflags this_mod output_filename location
                  foreign_stubs foreign_files dependencies rawcmms1
            return (output_filename, stub_c_exists, foreign_fps, caf_infos)


hscInteractive :: HscEnv
               -> CgGuts
               -> ModLocation
               -> IO (Maybe FilePath, CompiledByteCode, [SptEntry])
hscInteractive hsc_env cgguts location = do
    let dflags = hsc_dflags hsc_env
    let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                -- From now on, we just use the bits we need.
               cg_module   = this_mod,
               cg_binds    = core_binds,
               cg_tycons   = tycons,
               cg_foreign  = foreign_stubs,
               cg_modBreaks = mod_breaks,
               cg_spt_entries = spt_entries } = cgguts

        data_tycons = filter isDataTyCon tycons
        -- cg_tycons includes newtypes, for the benefit of External Core,
        -- but we don't generate any code for newtypes

    -------------------
    -- PREPARE FOR CODE GENERATION
    -- Do saturation and convert to A-normal form
    (prepd_binds, _) <- {-# SCC "CorePrep" #-}
                   corePrepPgm hsc_env this_mod location core_binds data_tycons
    -----------------  Generate byte code ------------------
    comp_bc <- byteCodeGen hsc_env this_mod prepd_binds data_tycons mod_breaks
    ------------------ Create f-x-dynamic C-side stuff -----
    (_istub_h_exists, istub_c_exists)
        <- outputForeignStubs dflags this_mod location foreign_stubs
    return (istub_c_exists, comp_bc, spt_entries)

------------------------------

hscCompileCmmFile :: HscEnv -> FilePath -> FilePath -> IO ()
hscCompileCmmFile hsc_env filename output_filename = runHsc hsc_env $ do
    let dflags = hsc_dflags hsc_env
    cmm <- ioMsgMaybe $ parseCmmFile dflags filename
    liftIO $ do
        dumpIfSet_dyn dflags Opt_D_dump_cmm_verbose_by_proc "Parsed Cmm" FormatCMM (ppr cmm)
        let -- Make up a module name to give the NCG. We can't pass bottom here
            -- lest we reproduce #11784.
            mod_name = mkModuleName $ "Cmm$" ++ FilePath.takeFileName filename
            cmm_mod = mkModule (thisPackage dflags) mod_name

        -- Compile decls in Cmm files one decl at a time, to avoid re-ordering
        -- them in SRT analysis.
        --
        -- Re-ordering here causes breakage when booting with C backend because
        -- in C we must declare before use, but SRT algorithm is free to
        -- re-order [A, B] (B refers to A) when A is not CAFFY and return [B, A]
        cmmgroup <-
          concatMapM (\cmm -> snd <$> cmmPipeline hsc_env (emptySRT cmm_mod) [cmm]) cmm

        unless (null cmmgroup) $
          dumpIfSet_dyn dflags Opt_D_dump_cmm "Output Cmm"
            FormatCMM (ppr cmmgroup)
        rawCmms <- lookupHook cmmToRawCmmHook
                     (\dflgs _ -> cmmToRawCmm dflgs) dflags dflags Nothing (Stream.yield cmmgroup)
        _ <- codeOutput dflags cmm_mod output_filename no_loc NoStubs [] []
             rawCmms
        return ()
  where
    no_loc = ModLocation{ ml_hs_file  = Just filename,
                          ml_hi_file  = panic "hscCompileCmmFile: no hi file",
                          ml_obj_file = panic "hscCompileCmmFile: no obj file",
                          ml_hie_file = panic "hscCompileCmmFile: no hie file"}

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

doCodeGen   :: HscEnv -> Module -> [TyCon]
            -> CollectedCCs
            -> [StgTopBinding]
            -> HpcInfo
            -> IO (Stream IO CmmGroupSRTs NameSet)
         -- Note we produce a 'Stream' of CmmGroups, so that the
         -- backend can be run incrementally.  Otherwise it generates all
         -- the C-- up front, which has a significant space cost.
doCodeGen hsc_env this_mod data_tycons
              cost_centre_info stg_binds hpc_info = do
    let dflags = hsc_dflags hsc_env

    let stg_binds_w_fvs = annTopBindingsFreeVars stg_binds

    dumpIfSet_dyn dflags Opt_D_dump_stg_final "Final STG:" FormatSTG (pprGenStgTopBindings stg_binds_w_fvs)

    let cmm_stream :: Stream IO CmmGroup ()
        -- See Note [Forcing of stg_binds]
        cmm_stream = stg_binds_w_fvs `seqList` {-# SCC "StgToCmm" #-}
            lookupHook stgToCmmHook StgToCmm.codeGen dflags dflags this_mod data_tycons
                           cost_centre_info stg_binds_w_fvs hpc_info

        -- codegen consumes a stream of CmmGroup, and produces a new
        -- stream of CmmGroup (not necessarily synchronised: one
        -- CmmGroup on input may produce many CmmGroups on output due
        -- to proc-point splitting).

    let dump1 a = do
          unless (null a) $
            dumpIfSet_dyn dflags Opt_D_dump_cmm_from_stg
              "Cmm produced by codegen" FormatCMM (ppr a)
          return a

        ppr_stream1 = Stream.mapM dump1 cmm_stream

        pipeline_stream =
          {-# SCC "cmmPipeline" #-}
          Stream.mapAccumL (cmmPipeline hsc_env) (emptySRT this_mod) ppr_stream1
            <&> (srtMapNonCAFs . moduleSRTMap)

        dump2 a = do
          unless (null a) $
            dumpIfSet_dyn dflags Opt_D_dump_cmm "Output Cmm" FormatCMM (ppr a)
          return a

    return (Stream.mapM dump2 pipeline_stream)

myCoreToStg :: DynFlags -> Module -> CoreProgram
            -> IO ( [StgTopBinding] -- output program
                  , CollectedCCs )  -- CAF cost centre info (declared and used)
myCoreToStg dflags this_mod prepd_binds = do
    let (stg_binds, cost_centre_info)
         = {-# SCC "Core2Stg" #-}
           coreToStg dflags this_mod prepd_binds

    stg_binds2
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
IO monad as explained in Note [Interactively-bound Ids in GHCi] in GHC.Driver.Types
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
  (ids, tc_expr, fix_env) <- ioMsgMaybe $ tcRnStmt hsc_env stmt

  -- Desugar it
  ds_expr <- ioMsgMaybe $ deSugarExpr hsc_env tc_expr
  liftIO (lintInteractiveExpr "desugar expression" hsc_env ds_expr)
  handleWarnings

  -- Then code-gen, and link it
  -- It's important NOT to have package 'interactive' as thisUnitId
  -- for linking, else we try to link 'main' and can't find it.
  -- Whereas the linker already knows to ignore 'interactive'
  let src_span = srcLocSpan interactiveSrcLoc
  hval <- liftIO $ hscCompileCoreExpr hsc_env src_span ds_expr

  return $ Just (ids, hval, fix_env)

-- | Compile a decls
hscDecls :: HscEnv
         -> String -- ^ The statement
         -> IO ([TyThing], InteractiveContext)
hscDecls hsc_env str = hscDeclsWithLocation hsc_env str "<interactive>" 1

hscParseDeclsWithLocation :: HscEnv -> String -> Int -> String -> IO [LHsDecl GhcPs]
hscParseDeclsWithLocation hsc_env source line_num str = do
    L _ (HsModule{ hsmodDecls = decls }) <-
      runInteractiveHsc hsc_env $
        hscParseThingWithLocation source line_num parseModule str
    return decls

-- | Compile a decls
hscDeclsWithLocation :: HscEnv
                     -> String -- ^ The statement
                     -> String -- ^ The source
                     -> Int    -- ^ Starting line
                     -> IO ([TyThing], InteractiveContext)
hscDeclsWithLocation hsc_env str source linenumber = do
    L _ (HsModule{ hsmodDecls = decls }) <-
      runInteractiveHsc hsc_env $
        hscParseThingWithLocation source linenumber parseModule str
    hscParsedDecls hsc_env decls

hscParsedDecls :: HscEnv -> [LHsDecl GhcPs] -> IO ([TyThing], InteractiveContext)
hscParsedDecls hsc_env decls = runInteractiveHsc hsc_env $ do
    {- Rename and typecheck it -}
    hsc_env <- getHscEnv
    tc_gblenv <- ioMsgMaybe $ tcRnDeclsi hsc_env decls

    {- Grab the new instances -}
    -- We grab the whole environment because of the overlapping that may have
    -- been done. See the notes at the definition of InteractiveContext
    -- (ic_instances) for more details.
    let defaults = tcg_default tc_gblenv

    {- Desugar it -}
    -- We use a basically null location for iNTERACTIVE
    let iNTERACTIVELoc = ModLocation{ ml_hs_file   = Nothing,
                                      ml_hi_file   = panic "hsDeclsWithLocation:ml_hi_file",
                                      ml_obj_file  = panic "hsDeclsWithLocation:ml_obj_file",
                                      ml_hie_file  = panic "hsDeclsWithLocation:ml_hie_file" }
    ds_result <- hscDesugar' iNTERACTIVELoc tc_gblenv

    {- Simplify -}
    simpl_mg <- liftIO $ do
      plugins <- readIORef (tcg_th_coreplugins tc_gblenv)
      hscSimplify hsc_env plugins ds_result

    {- Tidy -}
    (tidy_cg, mod_details) <- liftIO $ tidyProgram hsc_env simpl_mg

    let !CgGuts{ cg_module    = this_mod,
                 cg_binds     = core_binds,
                 cg_tycons    = tycons,
                 cg_modBreaks = mod_breaks } = tidy_cg

        !ModDetails { md_insts     = cls_insts
                    , md_fam_insts = fam_insts } = mod_details
            -- Get the *tidied* cls_insts and fam_insts

        data_tycons = filter isDataTyCon tycons

    {- Prepare For Code Generation -}
    -- Do saturation and convert to A-normal form
    (prepd_binds, _) <- {-# SCC "CorePrep" #-}
      liftIO $ corePrepPgm hsc_env this_mod iNTERACTIVELoc core_binds data_tycons

    {- Generate byte code -}
    cbc <- liftIO $ byteCodeGen hsc_env this_mod
                                prepd_binds data_tycons mod_breaks

    let src_span = srcLocSpan interactiveSrcLoc
    liftIO $ linkDecls hsc_env src_span cbc

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
            --    - DFunIds, which are in 'cls_insts' (see Note [ic_tythings] in GHC.Driver.Types
            --    - Implicit Ids, which are implicit in tcs
            -- c.f. TcRnDriver.runTcInteractive, which reconstructs the TypeEnv

        new_tythings = map AnId ext_ids ++ map ATyCon tcs ++ map (AConLike . PatSynCon) patsyns
        ictxt        = hsc_IC hsc_env
        -- See Note [Fixity declarations in GHCi]
        fix_env      = tcg_fix_env tc_gblenv
        new_ictxt    = extendInteractiveContext ictxt new_tythings cls_insts
                                                fam_insts defaults fix_env
    return (new_tythings, new_ictxt)

-- | Load the given static-pointer table entries into the interpreter.
-- See Note [Grand plan for static forms] in StaticPtrTable.
hscAddSptEntries :: HscEnv -> [SptEntry] -> IO ()
hscAddSptEntries hsc_env entries = do
    let add_spt_entry :: SptEntry -> IO ()
        add_spt_entry (SptEntry i fpr) = do
            val <- getHValue hsc_env (idName i)
            addSptEntry hsc_env fpr val
    mapM_ add_spt_entry entries

{-
  Note [Fixity declarations in GHCi]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  To support fixity declarations on types defined within GHCi (as requested
  in #10018) we record the fixity environment in InteractiveContext.
  When we want to evaluate something TcRnDriver.runTcInteractive pulls out this
  fixity environment and uses it to initialize the global typechecker environment.
  After the typechecker has finished its business, an updated fixity environment
  (reflecting whatever fixity declarations were present in the statements we
  passed it) will be returned from hscParsedStmt. This is passed to
  updateFixityEnv, which will stuff it back into InteractiveContext, to be
  used in evaluating the next statement.

-}

hscImport :: HscEnv -> String -> IO (ImportDecl GhcPs)
hscImport hsc_env str = runInteractiveHsc hsc_env $ do
    (L _ (HsModule{hsmodImports=is})) <-
       hscParseThing parseModule str
    case is of
        [L _ i] -> return i
        _ -> liftIO $ throwOneError $
                 mkPlainErrMsg (hsc_dflags hsc_env) noSrcSpan $
                     text "parse error in import declaration"

-- | Typecheck an expression (but don't run it)
hscTcExpr :: HscEnv
          -> TcRnExprMode
          -> String -- ^ The expression
          -> IO Type
hscTcExpr hsc_env0 mode expr = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  parsed_expr <- hscParseExpr expr
  ioMsgMaybe $ tcRnExpr hsc_env mode parsed_expr

-- | Find the kind of a type, after generalisation
hscKcType
  :: HscEnv
  -> Bool            -- ^ Normalise the type
  -> String          -- ^ The type as a string
  -> IO (Type, Kind) -- ^ Resulting type (possibly normalised) and kind
hscKcType hsc_env0 normalise str = runInteractiveHsc hsc_env0 $ do
    hsc_env <- getHscEnv
    ty <- hscParseType str
    ioMsgMaybe $ tcRnType hsc_env DefaultFlexi normalise ty

hscParseExpr :: String -> Hsc (LHsExpr GhcPs)
hscParseExpr expr = do
  hsc_env <- getHscEnv
  maybe_stmt <- hscParseStmt expr
  case maybe_stmt of
    Just (L _ (BodyStmt _ expr _ _)) -> return expr
    _ -> throwOneError $ mkPlainErrMsg (hsc_dflags hsc_env) noSrcSpan
      (text "not an expression:" <+> quotes (text expr))

hscParseStmt :: String -> Hsc (Maybe (GhciLStmt GhcPs))
hscParseStmt = hscParseThing parseStmt

hscParseStmtWithLocation :: String -> Int -> String
                         -> Hsc (Maybe (GhciLStmt GhcPs))
hscParseStmtWithLocation source linenumber stmt =
    hscParseThingWithLocation source linenumber parseStmt stmt

hscParseType :: String -> Hsc (LHsType GhcPs)
hscParseType = hscParseThing parseType

hscParseIdentifier :: HscEnv -> String -> IO (Located RdrName)
hscParseIdentifier hsc_env str =
    runInteractiveHsc hsc_env $ hscParseThing parseIdentifier str

hscParseThing :: (Outputable thing, Data thing)
              => Lexer.P thing -> String -> Hsc thing
hscParseThing = hscParseThingWithLocation "<interactive>" 1

hscParseThingWithLocation :: (Outputable thing, Data thing) => String -> Int
                          -> Lexer.P thing -> String -> Hsc thing
hscParseThingWithLocation source linenumber parser str
  = withTimingD
               (text "Parser [source]")
               (const ()) $ {-# SCC "Parser" #-} do
    dflags <- getDynFlags

    let buf = stringToStringBuffer str
        loc = mkRealSrcLoc (fsLit source) linenumber 1

    case unP parser (mkPState dflags buf loc) of
        PFailed pst -> do
            handleWarningsThrowErrors (getMessages pst dflags)

        POk pst thing -> do
            logWarningsReportErrors (getMessages pst dflags)
            liftIO $ dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser"
                        FormatHaskell (ppr thing)
            liftIO $ dumpIfSet_dyn dflags Opt_D_dump_parsed_ast "Parser AST"
                        FormatHaskell (showAstData NoBlankSrcSpan thing)
            return thing


{- **********************************************************************
%*                                                                      *
        Desugar, simplify, convert to bytecode, and link an expression
%*                                                                      *
%********************************************************************* -}

hscCompileCoreExpr :: HscEnv -> SrcSpan -> CoreExpr -> IO ForeignHValue
hscCompileCoreExpr hsc_env =
  lookupHook hscCompileCoreExprHook hscCompileCoreExpr' (hsc_dflags hsc_env) hsc_env

hscCompileCoreExpr' :: HscEnv -> SrcSpan -> CoreExpr -> IO ForeignHValue
hscCompileCoreExpr' hsc_env srcspan ds_expr
    = do { let dflags = hsc_dflags hsc_env

           {- Simplify it -}
         ; simpl_expr <- simplifyExpr hsc_env ds_expr

           {- Tidy it (temporary, until coreSat does cloning) -}
         ; let tidy_expr = tidyExpr emptyTidyEnv simpl_expr

           {- Prepare for codegen -}
         ; prepd_expr <- corePrepExpr dflags hsc_env tidy_expr

           {- Lint if necessary -}
         ; lintInteractiveExpr "hscCompileExpr" hsc_env prepd_expr

           {- Convert to BCOs -}
         ; bcos <- coreExprToBCOs hsc_env
                     (icInteractiveModule (hsc_IC hsc_env)) prepd_expr

           {- link it -}
         ; hval <- linkExpr hsc_env srcspan bcos

         ; return hval }


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
