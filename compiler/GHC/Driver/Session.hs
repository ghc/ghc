{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

-------------------------------------------------------------------------------
--
-- | Dynamic flags
--
-- Most flags are dynamic flags, which means they can change from compilation
-- to compilation using @OPTIONS_GHC@ pragmas, and in a multi-session GHC each
-- session can be using different dynamic flags. Dynamic flags can also be set
-- at the prompt in GHCi.
--
-- (c) The University of Glasgow 2005
--
-------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Driver.Session (
        -- * Dynamic flags and associated configuration types
        DumpFlag(..),
        GeneralFlag(..),
        WarningFlag(..), DiagnosticReason(..),
        Language(..),
        FatalMessager, FlushOut(..),
        ProfAuto(..),
        glasgowExtsFlags,
        hasPprDebug, hasNoDebugOutput, hasNoStateHack, hasNoOptCoercion,
        dopt, dopt_set, dopt_unset,
        gopt, gopt_set, gopt_unset, setGeneralFlag', unSetGeneralFlag',
        wopt, wopt_set, wopt_unset,
        wopt_fatal, wopt_set_fatal, wopt_unset_fatal,
        wopt_set_all_custom, wopt_unset_all_custom,
        wopt_set_all_fatal_custom, wopt_unset_all_fatal_custom,
        wopt_set_custom, wopt_unset_custom,
        wopt_set_fatal_custom, wopt_unset_fatal_custom,
        wopt_any_custom,
        xopt, xopt_set, xopt_unset,
        xopt_set_unlessExplSpec,
        xopt_DuplicateRecordFields,
        xopt_FieldSelectors,
        lang_set,
        DynamicTooState(..), dynamicTooState, setDynamicNow,
        sccProfilingEnabled,
        needSourceNotes,
        OnOff(..),
        DynFlags(..),
        ParMakeCount(..),
        outputFile, objectSuf, ways,
        FlagSpec(..),
        HasDynFlags(..), ContainsDynFlags(..),
        RtsOptsEnabled(..),
        GhcMode(..), isOneShot,
        GhcLink(..), isNoLink,
        PackageFlag(..), PackageArg(..), ModRenaming(..),
        packageFlagsChanged,
        IgnorePackageFlag(..), TrustFlag(..),
        PackageDBFlag(..), PkgDbRef(..),
        Option(..), showOpt,
        DynLibLoader(..),
        fFlags, fLangFlags, xFlags,
        wWarningFlags,
        makeDynFlagsConsistent,
        positionIndependent,
        optimisationFlags,
        codeGenFlags,
        setFlagsFromEnvFile,
        pprDynFlagsDiff,
        flagSpecOf,

        targetProfile,

        -- ** Safe Haskell
        safeHaskellOn, safeHaskellModeEnabled,
        safeImportsOn, safeLanguageOn, safeInferOn,
        packageTrustOn,
        safeDirectImpsReq, safeImplicitImpsReq,
        unsafeFlags, unsafeFlagsForInfer,

        -- ** System tool settings and locations
        Settings(..),
        sProgramName,
        sProjectVersion,
        sGhcUsagePath,
        sGhciUsagePath,
        sToolDir,
        sTopDir,
        sGlobalPackageDatabasePath,
        sLdSupportsCompactUnwind,
        sLdSupportsFilelist,
        sLdIsGnuLd,
        sGccSupportsNoPie,
        sPgm_L,
        sPgm_P,
        sPgm_F,
        sPgm_c,
        sPgm_cxx,
        sPgm_cpp,
        sPgm_a,
        sPgm_l,
        sPgm_lm,
        sPgm_windres,
        sPgm_ar,
        sPgm_ranlib,
        sPgm_lo,
        sPgm_lc,
        sPgm_las,
        sPgm_i,
        sOpt_L,
        sOpt_P,
        sOpt_P_fingerprint,
        sOpt_JSP,
        sOpt_JSP_fingerprint,
        sOpt_CmmP,
        sOpt_CmmP_fingerprint,
        sOpt_F,
        sOpt_c,
        sOpt_cxx,
        sOpt_a,
        sOpt_l,
        sOpt_lm,
        sOpt_windres,
        sOpt_lo,
        sOpt_lc,
        sOpt_i,
        sExtraGccViaCFlags,
        sTargetPlatformString,
        sGhcWithInterpreter,
        sLibFFI,
        sTargetRTSLinkerOnlySupportsSharedLibs,
        GhcNameVersion(..),
        FileSettings(..),
        PlatformMisc(..),
        settings,
        programName, projectVersion,
        ghcUsagePath, ghciUsagePath, topDir,
        versionedAppDir, versionedFilePath,
        extraGccViaCFlags, globalPackageDatabasePath,
        pgm_L, pgm_P, pgm_JSP, pgm_CmmP, pgm_F, pgm_c, pgm_cxx, pgm_cpp, pgm_a, pgm_l,
        pgm_lm, pgm_windres, pgm_ar,
        pgm_ranlib, pgm_lo, pgm_lc, pgm_las, pgm_i,
        opt_L, opt_P, opt_JSP, opt_CmmP, opt_F, opt_c, opt_cxx, opt_a, opt_l, opt_lm, opt_i,
        opt_P_signature, opt_JSP_signature, opt_CmmP_signature,
        opt_windres, opt_lo, opt_lc, opt_las,
        updatePlatformConstants,

        -- ** Manipulating DynFlags
        addPluginModuleName,
        defaultDynFlags,                -- Settings -> DynFlags
        initDynFlags,                   -- DynFlags -> IO DynFlags
        defaultFatalMessager,
        defaultFlushOut,
        setOutputFile, setDynOutputFile, setOutputHi, setDynOutputHi,
        augmentByWorkingDirectory,

        getOpts,                        -- DynFlags -> (DynFlags -> [a]) -> [a]
        getVerbFlags,
        updOptLevel,
        setTmpDir,
        setUnitId,

        TurnOnFlag,
        turnOn,
        turnOff,
        impliedGFlags,
        impliedOffGFlags,
        impliedXFlags,

        -- ** State
        CmdLineP(..), runCmdLineP,
        getCmdLineState, putCmdLineState,
        processCmdLineP,

        -- ** Parsing DynFlags
        parseDynamicFlagsCmdLine,
        parseDynamicFilePragma,
        parseDynamicFlagsFull,
        flagSuggestions,

        -- ** Available DynFlags
        allNonDeprecatedFlags,
        flagsAll,
        flagsDynamic,
        flagsPackage,
        flagsForCompletion,

        supportedLanguagesAndExtensions,
        languageExtensions,

        -- ** DynFlags C compiler options
        picCCOpts, picPOpts,

        -- ** DynFlags C linker options
        pieCCLDOpts,

        -- * Compiler configuration suitable for display to the user
        compilerInfo,

        wordAlignment,

        setUnsafeGlobalDynFlags,

        -- * SSE and AVX
        isSse4_2Enabled,
        isBmiEnabled,
        isBmi2Enabled,
        isAvxEnabled,
        isAvx2Enabled,
        isAvx512cdEnabled,
        isAvx512erEnabled,
        isAvx512fEnabled,
        isAvx512pfEnabled,
        isFmaEnabled,

        -- * Linker/compiler information
        useXLinkerRPath,

        -- * Include specifications
        IncludeSpecs(..), addGlobalInclude, addQuoteInclude, flattenIncludes,
        addImplicitQuoteInclude,

        -- * SDoc
        initSDocContext, initDefaultSDocContext,
        initPromotionTickContext,
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Ways
import GHC.Platform.Profile

import GHC.Unit.Types
import GHC.Unit.Parser
import GHC.Unit.Module
import GHC.Unit.Module.Warnings
import GHC.Driver.DynFlags
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Flags
import GHC.Driver.Backend
import GHC.Driver.Errors.Types
import GHC.Driver.Plugins.External
import GHC.Settings.Config
import GHC.Core.Unfold
import GHC.Driver.CmdLine
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.GlobalVars
import GHC.Data.Maybe
import GHC.Data.Bool
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Types.Error
import GHC.Types.Name.Reader (RdrName(..))
import GHC.Types.Name.Occurrence (isVarOcc, occNameString)
import GHC.Utils.Monad
import GHC.Types.SrcLoc
import GHC.Types.SafeHaskell
import GHC.Types.Basic ( treatZeroAsInf )
import GHC.Data.FastString
import GHC.Utils.TmpFs
import GHC.Utils.Fingerprint
import GHC.Utils.Outputable
import GHC.Utils.Error (emptyDiagOpts)
import GHC.Settings
import GHC.CmmToAsm.CFG.Weight
import GHC.Core.Opt.CallerCC
import GHC.Parser (parseIdentifier)
import GHC.Parser.Lexer (mkParserOpts, initParserState, P(..), ParseResult(..))

import GHC.SysTools.BaseDir ( expandToolDir, expandTopDir )

import Data.IORef
import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Trans.State as State
import Data.Functor.Identity

import Data.Ord
import Data.Char
import Data.List (intercalate, sortBy, partition)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word
import System.FilePath
import Text.ParserCombinators.ReadP hiding (char)
import Text.ParserCombinators.ReadP as R

import qualified GHC.Data.EnumSet as EnumSet

import qualified GHC.LanguageExtensions as LangExt


-- Note [Updating flag description in the User's Guide]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- If you modify anything in this file please make sure that your changes are
-- described in the User's Guide. Please update the flag description in the
-- users guide (docs/users_guide) whenever you add or change a flag.
-- Please make sure you add ":since:" information to new flags.

-- Note [Supporting CLI completion]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The command line interface completion (in for example bash) is an easy way
-- for the developer to learn what flags are available from GHC.
-- GHC helps by separating which flags are available when compiling with GHC,
-- and which flags are available when using GHCi.
-- A flag is assumed to either work in both these modes, or only in one of them.
-- When adding or changing a flag, please consider for which mode the flag will
-- have effect, and annotate it accordingly. For Flags use defFlag, defGhcFlag,
-- defGhciFlag, and for FlagSpec use flagSpec or flagGhciSpec.

-- Note [Adding a language extension]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- There are a few steps to adding (or removing) a language extension,
--
--  * Adding the extension to GHC.LanguageExtensions
--
--    The Extension type in libraries/ghc-boot-th/GHC/LanguageExtensions/Type.hs
--    is the canonical list of language extensions known by GHC.
--
--  * Adding a flag to DynFlags.xFlags
--
--    This is fairly self-explanatory. The name should be concise, memorable,
--    and consistent with any previous implementations of the similar idea in
--    other Haskell compilers.
--
--  * Adding the flag to the documentation
--
--    This is the same as any other flag. See
--    Note [Updating flag description in the User's Guide]
--
--  * Adding the flag to Cabal
--
--    The Cabal library has its own list of all language extensions supported
--    by all major compilers. This is the list that user code being uploaded
--    to Hackage is checked against to ensure language extension validity.
--    Consequently, it is very important that this list remains up-to-date.
--
--    To this end, there is a testsuite test (testsuite/tests/driver/T4437.hs)
--    whose job it is to ensure these GHC's extensions are consistent with
--    Cabal.
--
--    The recommended workflow is,
--
--     1. Temporarily add your new language extension to the
--        expectedGhcOnlyExtensions list in T4437 to ensure the test doesn't
--        break while Cabal is updated.
--
--     2. After your GHC change is accepted, submit a Cabal pull request adding
--        your new extension to Cabal's list (found in
--        Cabal/Language/Haskell/Extension.hs).
--
--     3. After your Cabal change is accepted, let the GHC developers know so
--        they can update the Cabal submodule and remove the extensions from
--        expectedGhcOnlyExtensions.
--
--  * Adding the flag to the GHC Wiki
--
--    There is a change log tracking language extension additions and removals
--    on the GHC wiki:  https://gitlab.haskell.org/ghc/ghc/wikis/language-pragma-history
--
--  See #4437 and #8176.

-- -----------------------------------------------------------------------------
-- DynFlags

{- Note [RHS Floating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  We provide both 'Opt_LocalFloatOut' and 'Opt_LocalFloatOutTopLevel' to correspond to
  'doFloatFromRhs'; with this we can control floating out with GHC flags.

  This addresses https://gitlab.haskell.org/ghc/ghc/-/issues/13663 and
  allows for experimentation.
-}

-----------------------------------------------------------------------------
-- Accessors from 'DynFlags'

-- | "unbuild" a 'Settings' from a 'DynFlags'. This shouldn't be needed in the
-- vast majority of code. But GHCi questionably uses this to produce a default
-- 'DynFlags' from which to compute a flags diff for printing.
settings :: DynFlags -> Settings
settings dflags = Settings
  { sGhcNameVersion = ghcNameVersion dflags
  , sFileSettings = fileSettings dflags
  , sTargetPlatform = targetPlatform dflags
  , sToolSettings = toolSettings dflags
  , sPlatformMisc = platformMisc dflags
  , sRawSettings = rawSettings dflags
  }

pgm_L                 :: DynFlags -> String
pgm_L dflags = toolSettings_pgm_L $ toolSettings dflags
pgm_P                 :: DynFlags -> (String,[Option])
pgm_P dflags = toolSettings_pgm_P $ toolSettings dflags
pgm_JSP               :: DynFlags -> (String,[Option])
pgm_JSP dflags = toolSettings_pgm_JSP $ toolSettings dflags
pgm_CmmP              :: DynFlags -> (String,[Option])
pgm_CmmP dflags = toolSettings_pgm_CmmP $ toolSettings dflags
pgm_F                 :: DynFlags -> String
pgm_F dflags = toolSettings_pgm_F $ toolSettings dflags
pgm_c                 :: DynFlags -> String
pgm_c dflags = toolSettings_pgm_c $ toolSettings dflags
pgm_cxx               :: DynFlags -> String
pgm_cxx dflags = toolSettings_pgm_cxx $ toolSettings dflags
pgm_cpp               :: DynFlags -> (String,[Option])
pgm_cpp dflags = toolSettings_pgm_cpp $ toolSettings dflags
pgm_a                 :: DynFlags -> (String,[Option])
pgm_a dflags = toolSettings_pgm_a $ toolSettings dflags
pgm_l                 :: DynFlags -> (String,[Option])
pgm_l dflags = toolSettings_pgm_l $ toolSettings dflags
pgm_lm                 :: DynFlags -> Maybe (String,[Option])
pgm_lm dflags = toolSettings_pgm_lm $ toolSettings dflags
pgm_windres           :: DynFlags -> String
pgm_windres dflags = toolSettings_pgm_windres $ toolSettings dflags
pgm_ar                :: DynFlags -> String
pgm_ar dflags = toolSettings_pgm_ar $ toolSettings dflags
pgm_ranlib            :: DynFlags -> String
pgm_ranlib dflags = toolSettings_pgm_ranlib $ toolSettings dflags
pgm_lo                :: DynFlags -> (String,[Option])
pgm_lo dflags = toolSettings_pgm_lo $ toolSettings dflags
pgm_lc                :: DynFlags -> (String,[Option])
pgm_lc dflags = toolSettings_pgm_lc $ toolSettings dflags
pgm_las               :: DynFlags -> (String,[Option])
pgm_las dflags = toolSettings_pgm_las $ toolSettings dflags
pgm_i                 :: DynFlags -> String
pgm_i dflags = toolSettings_pgm_i $ toolSettings dflags
opt_L                 :: DynFlags -> [String]
opt_L dflags = toolSettings_opt_L $ toolSettings dflags
opt_P                 :: DynFlags -> [String]
opt_P dflags = concatMap (wayOptP (targetPlatform dflags)) (ways dflags)
            ++ toolSettings_opt_P (toolSettings dflags)
opt_JSP               :: DynFlags -> [String]
opt_JSP dflags = concatMap (wayOptP (targetPlatform dflags)) (ways dflags)
            ++ toolSettings_opt_JSP (toolSettings dflags)
opt_CmmP              :: DynFlags -> [String]
opt_CmmP dflags = toolSettings_opt_CmmP $ toolSettings dflags

-- This function packages everything that's needed to fingerprint opt_P
-- flags. See Note [Repeated -optP hashing].
opt_P_signature       :: DynFlags -> ([String], Fingerprint)
opt_P_signature dflags =
  ( concatMap (wayOptP (targetPlatform dflags)) (ways dflags)
  , toolSettings_opt_P_fingerprint $ toolSettings dflags
  )
-- This function packages everything that's needed to fingerprint opt_P
-- flags. See Note [Repeated -optP hashing].
opt_JSP_signature     :: DynFlags -> ([String], Fingerprint)
opt_JSP_signature dflags =
  ( concatMap (wayOptP (targetPlatform dflags)) (ways dflags)
  , toolSettings_opt_JSP_fingerprint $ toolSettings dflags
  )
-- This function packages everything that's needed to fingerprint opt_CmmP
-- flags. See Note [Repeated -optP hashing].
opt_CmmP_signature     :: DynFlags -> Fingerprint
opt_CmmP_signature = toolSettings_opt_CmmP_fingerprint . toolSettings

opt_F                 :: DynFlags -> [String]
opt_F dflags= toolSettings_opt_F $ toolSettings dflags
opt_c                 :: DynFlags -> [String]
opt_c dflags = concatMap (wayOptc (targetPlatform dflags)) (ways dflags)
            ++ toolSettings_opt_c (toolSettings dflags)
opt_cxx               :: DynFlags -> [String]
opt_cxx dflags = concatMap (wayOptcxx (targetPlatform dflags)) (ways dflags)
           ++ toolSettings_opt_cxx (toolSettings dflags)
opt_a                 :: DynFlags -> [String]
opt_a dflags= toolSettings_opt_a $ toolSettings dflags
opt_l                 :: DynFlags -> [String]
opt_l dflags = concatMap (wayOptl (targetPlatform dflags)) (ways dflags)
            ++ toolSettings_opt_l (toolSettings dflags)
opt_lm                :: DynFlags -> [String]
opt_lm dflags= toolSettings_opt_lm $ toolSettings dflags
opt_windres           :: DynFlags -> [String]
opt_windres dflags= toolSettings_opt_windres $ toolSettings dflags
opt_lo                :: DynFlags -> [String]
opt_lo dflags= toolSettings_opt_lo $ toolSettings dflags
opt_lc                :: DynFlags -> [String]
opt_lc dflags= toolSettings_opt_lc $ toolSettings dflags
opt_las               :: DynFlags -> [String]
opt_las dflags = toolSettings_opt_las $ toolSettings dflags
opt_i                 :: DynFlags -> [String]
opt_i dflags= toolSettings_opt_i $ toolSettings dflags

-----------------------------------------------------------------------------

{-
Note [Verbosity levels]
~~~~~~~~~~~~~~~~~~~~~~~
    0   |   print errors & warnings only
    1   |   minimal verbosity: print "compiling M ... done." for each module.
    2   |   equivalent to -dshow-passes
    3   |   equivalent to existing "ghc -v"
    4   |   "ghc -v -ddump-most"
    5   |   "ghc -v -ddump-all"
-}

-- | Set the Haskell language standard to use
setLanguage :: Language -> DynP ()
setLanguage l = upd (`lang_set` Just l)

-- | Is the -fpackage-trust mode on
packageTrustOn :: DynFlags -> Bool
packageTrustOn = gopt Opt_PackageTrust

-- | Is Safe Haskell on in some way (including inference mode)
safeHaskellOn :: DynFlags -> Bool
safeHaskellOn dflags = safeHaskellModeEnabled dflags || safeInferOn dflags

safeHaskellModeEnabled :: DynFlags -> Bool
safeHaskellModeEnabled dflags = safeHaskell dflags `elem` [Sf_Unsafe, Sf_Trustworthy
                                                   , Sf_Safe ]


-- | Is the Safe Haskell safe language in use
safeLanguageOn :: DynFlags -> Bool
safeLanguageOn dflags = safeHaskell dflags == Sf_Safe

-- | Is the Safe Haskell safe inference mode active
safeInferOn :: DynFlags -> Bool
safeInferOn = safeInfer

-- | Test if Safe Imports are on in some form
safeImportsOn :: DynFlags -> Bool
safeImportsOn dflags = safeHaskell dflags == Sf_Unsafe ||
                       safeHaskell dflags == Sf_Trustworthy ||
                       safeHaskell dflags == Sf_Safe

-- | Set a 'Safe Haskell' flag
setSafeHaskell :: SafeHaskellMode -> DynP ()
setSafeHaskell s = updM f
    where f dfs = do
              let sf = safeHaskell dfs
              safeM <- combineSafeFlags sf s
              case s of
                Sf_Safe -> return $ dfs { safeHaskell = safeM, safeInfer = False }
                -- leave safe inference on in Trustworthy mode so we can warn
                -- if it could have been inferred safe.
                Sf_Trustworthy -> do
                  l <- getCurLoc
                  return $ dfs { safeHaskell = safeM, trustworthyOnLoc = l }
                -- leave safe inference on in Unsafe mode as well.
                _ -> return $ dfs { safeHaskell = safeM }

-- | Are all direct imports required to be safe for this Safe Haskell mode?
-- Direct imports are when the code explicitly imports a module
safeDirectImpsReq :: DynFlags -> Bool
safeDirectImpsReq d = safeLanguageOn d

-- | Are all implicit imports required to be safe for this Safe Haskell mode?
-- Implicit imports are things in the prelude. e.g System.IO when print is used.
safeImplicitImpsReq :: DynFlags -> Bool
safeImplicitImpsReq d = safeLanguageOn d

-- | Combine two Safe Haskell modes correctly. Used for dealing with multiple flags.
-- This makes Safe Haskell very much a monoid but for now I prefer this as I don't
-- want to export this functionality from the module but do want to export the
-- type constructors.
combineSafeFlags :: SafeHaskellMode -> SafeHaskellMode -> DynP SafeHaskellMode
combineSafeFlags a b | a == Sf_None         = return b
                     | b == Sf_None         = return a
                     | a == Sf_Ignore || b == Sf_Ignore = return Sf_Ignore
                     | a == b               = return a
                     | otherwise            = addErr errm >> pure a
    where errm = "Incompatible Safe Haskell flags! ("
                    ++ show a ++ ", " ++ show b ++ ")"

-- | A list of unsafe flags under Safe Haskell. Tuple elements are:
--     * name of the flag
--     * function to get srcspan that enabled the flag
--     * function to test if the flag is on
--     * function to turn the flag off
unsafeFlags, unsafeFlagsForInfer
  :: [(LangExt.Extension, DynFlags -> SrcSpan, DynFlags -> Bool, DynFlags -> DynFlags)]
unsafeFlags = [ (LangExt.GeneralizedNewtypeDeriving, newDerivOnLoc,
                    xopt LangExt.GeneralizedNewtypeDeriving,
                    flip xopt_unset LangExt.GeneralizedNewtypeDeriving)
              , (LangExt.DerivingVia, deriveViaOnLoc,
                    xopt LangExt.DerivingVia,
                    flip xopt_unset LangExt.DerivingVia)
              , (LangExt.TemplateHaskell, thOnLoc,
                    xopt LangExt.TemplateHaskell,
                    flip xopt_unset LangExt.TemplateHaskell)
              ]
unsafeFlagsForInfer = unsafeFlags


-- | Retrieve the options corresponding to a particular @opt_*@ field in the correct order
getOpts :: DynFlags             -- ^ 'DynFlags' to retrieve the options from
        -> (DynFlags -> [a])    -- ^ Relevant record accessor: one of the @opt_*@ accessors
        -> [a]                  -- ^ Correctly ordered extracted options
getOpts dflags opts = reverse (opts dflags)
        -- We add to the options from the front, so we need to reverse the list

-- | Gets the verbosity flag for the current verbosity level. This is fed to
-- other tools, so GHC-specific verbosity flags like @-ddump-most@ are not included
getVerbFlags :: DynFlags -> [String]
getVerbFlags dflags
  | verbosity dflags >= 4 = ["-v"]
  | otherwise             = []

setObjectDir, setHiDir, setHieDir, setStubDir, setDumpDir, setOutputDir,
         setDynObjectSuf, setDynHiSuf,
         setDylibInstallName,
         setObjectSuf, setHiSuf, setHieSuf, setHcSuf, parseDynLibLoaderMode,
         setPgmP, setPgmJSP, setPgmCmmP, addOptl, addOptc, addOptcxx, addOptP,
         addOptJSP, addOptCmmP,
         addCmdlineFramework, addHaddockOpts, addGhciScript,
         setInteractivePrint
   :: String -> DynFlags -> DynFlags
setOutputFile, setDynOutputFile, setOutputHi, setDynOutputHi, setDumpPrefixForce
   :: Maybe String -> DynFlags -> DynFlags

setObjectDir  f d = d { objectDir  = Just f}
setHiDir      f d = d { hiDir      = Just f}
setHieDir     f d = d { hieDir     = Just f}
setStubDir    f d = d { stubDir    = Just f
                      , includePaths = addGlobalInclude (includePaths d) [f] }
  -- -stubdir D adds an implicit -I D, so that gcc can find the _stub.h file
  -- \#included from the .hc file when compiling via C (i.e. unregisterised
  -- builds).
setDumpDir    f d = d { dumpDir    = Just f}
setOutputDir  f = setObjectDir f
                . setHieDir f
                . setHiDir f
                . setStubDir f
                . setDumpDir f
setDylibInstallName  f d = d { dylibInstallName = Just f}

setObjectSuf    f d = d { objectSuf_    = f}
setDynObjectSuf f d = d { dynObjectSuf_ = f}
setHiSuf        f d = d { hiSuf_        = f}
setHieSuf       f d = d { hieSuf        = f}
setDynHiSuf     f d = d { dynHiSuf_     = f}
setHcSuf        f d = d { hcSuf         = f}

setOutputFile    f d = d { outputFile_    = f}
setDynOutputFile f d = d { dynOutputFile_ = f}
setOutputHi      f d = d { outputHi       = f}
setDynOutputHi   f d = d { dynOutputHi    = f}

parseUnitInsts :: String -> Instantiations
parseUnitInsts str = case filter ((=="").snd) (readP_to_S parse str) of
    [(r, "")] -> r
    _ -> throwGhcException $ CmdLineError ("Can't parse -instantiated-with: " ++ str)
  where parse = sepBy parseEntry (R.char ',')
        parseEntry = do
            n <- parseModuleName
            _ <- R.char '='
            m <- parseHoleyModule
            return (n, m)

setUnitInstantiations :: String -> DynFlags -> DynFlags
setUnitInstantiations s d =
    d { homeUnitInstantiations_ = parseUnitInsts s }

setUnitInstanceOf :: String -> DynFlags -> DynFlags
setUnitInstanceOf s d =
    d { homeUnitInstanceOf_ = Just (UnitId (fsLit s)) }

addPluginModuleName :: String -> DynFlags -> DynFlags
addPluginModuleName name d = d { pluginModNames = (mkModuleName name) : (pluginModNames d) }

clearPluginModuleNames :: DynFlags -> DynFlags
clearPluginModuleNames d =
    d { pluginModNames = []
      , pluginModNameOpts = []
      }

addPluginModuleNameOption :: String -> DynFlags -> DynFlags
addPluginModuleNameOption optflag d = d { pluginModNameOpts = (mkModuleName m, option) : (pluginModNameOpts d) }
  where (m, rest) = break (== ':') optflag
        option = case rest of
          [] -> "" -- should probably signal an error
          (_:plug_opt) -> plug_opt -- ignore the ':' from break

addExternalPlugin :: String -> DynFlags -> DynFlags
addExternalPlugin optflag d = case parseExternalPluginSpec optflag of
  Just r  -> d { externalPluginSpecs = r : externalPluginSpecs d }
  Nothing -> cmdLineError $ "Couldn't parse external plugin specification: " ++ optflag

addFrontendPluginOption :: String -> DynFlags -> DynFlags
addFrontendPluginOption s d = d { frontendPluginOpts = s : frontendPluginOpts d }

parseDynLibLoaderMode f d =
 case splitAt 8 f of
   ("deploy", "")       -> d { dynLibLoader = Deployable }
   ("sysdep", "")       -> d { dynLibLoader = SystemDependent }
   _                    -> throwGhcException (CmdLineError ("Unknown dynlib loader: " ++ f))

setDumpPrefixForce f d = d { dumpPrefixForce = f}

-- XXX HACK: Prelude> words "'does not' work" ===> ["'does","not'","work"]
-- Config.hs should really use Option.
setPgmP   f = alterToolSettings (\s -> s { toolSettings_pgm_P   = (pgm, map Option args)})
  where (pgm:args) = words f
-- XXX HACK: Prelude> words "'does not' work" ===> ["'does","not'","work"]
-- Config.hs should really use Option.
setPgmJSP   f = alterToolSettings (\s -> s { toolSettings_pgm_JSP   = (pgm, map Option args)})
  where (pgm:args) = words f
-- XXX HACK: Prelude> words "'does not' work" ===> ["'does","not'","work"]
-- Config.hs should really use Option.
setPgmCmmP f = alterToolSettings (\s -> s { toolSettings_pgm_CmmP = (pgm, map Option args)})
  where (pgm:args) = words f
addOptl   f = alterToolSettings (\s -> s { toolSettings_opt_l   = f : toolSettings_opt_l s})
addOptc   f = alterToolSettings (\s -> s { toolSettings_opt_c   = f : toolSettings_opt_c s})
addOptcxx f = alterToolSettings (\s -> s { toolSettings_opt_cxx = f : toolSettings_opt_cxx s})
addOptP   f = alterToolSettings $ \s -> s
          { toolSettings_opt_P   = f : toolSettings_opt_P s
          , toolSettings_opt_P_fingerprint = fingerprintStrings (f : toolSettings_opt_P s)
          }
          -- See Note [Repeated -optP hashing]
addOptJSP f = alterToolSettings $ \s -> s
          { toolSettings_opt_JSP   = f : toolSettings_opt_JSP s
          , toolSettings_opt_JSP_fingerprint = fingerprintStrings (f : toolSettings_opt_JSP s)
          }
          -- See Note [Repeated -optP hashing]
addOptCmmP f = alterToolSettings $ \s -> s
          { toolSettings_opt_CmmP = f : toolSettings_opt_CmmP s
          , toolSettings_opt_CmmP_fingerprint = fingerprintStrings (f : toolSettings_opt_CmmP s)
          }

setDepMakefile :: FilePath -> DynFlags -> DynFlags
setDepMakefile f d = d { depMakefile = f }

setDepIncludeCppDeps :: Bool -> DynFlags -> DynFlags
setDepIncludeCppDeps b d = d { depIncludeCppDeps = b }

setDepIncludePkgDeps :: Bool -> DynFlags -> DynFlags
setDepIncludePkgDeps b d = d { depIncludePkgDeps = b }

addDepExcludeMod :: String -> DynFlags -> DynFlags
addDepExcludeMod m d
    = d { depExcludeMods = mkModuleName m : depExcludeMods d }

addDepSuffix :: FilePath -> DynFlags -> DynFlags
addDepSuffix s d = d { depSuffixes = s : depSuffixes d }

addCmdlineFramework f d = d { cmdlineFrameworks = f : cmdlineFrameworks d}

addGhcVersionFile :: FilePath -> DynFlags -> DynFlags
addGhcVersionFile f d = d { ghcVersionFile = Just f }

addHaddockOpts f d = d { haddockOptions = Just f}

addGhciScript f d = d { ghciScripts = f : ghciScripts d}

setInteractivePrint f d = d { interactivePrint = Just f}

-----------------------------------------------------------------------------
-- Setting the optimisation level

updOptLevelChanged :: Int -> DynFlags -> (DynFlags, Bool)
-- ^ Sets the 'DynFlags' to be appropriate to the optimisation level and signals if any changes took place
updOptLevelChanged n dfs
  = (dfs3, changed1 || changed2 || changed3)
  where
   final_n = max 0 (min 2 n)    -- Clamp to 0 <= n <= 2
   (dfs1, changed1) = foldr unset (dfs , False) remove_gopts
   (dfs2, changed2) = foldr set   (dfs1, False) extra_gopts
   (dfs3, changed3) = setLlvmOptLevel dfs2

   extra_gopts  = [ f | (ns,f) <- optLevelFlags, final_n `elem` ns ]
   remove_gopts = [ f | (ns,f) <- optLevelFlags, final_n `notElem` ns ]

   set f (dfs, changed)
     | gopt f dfs = (dfs, changed)
     | otherwise = (gopt_set dfs f, True)

   unset f (dfs, changed)
     | not (gopt f dfs) = (dfs, changed)
     | otherwise = (gopt_unset dfs f, True)

   setLlvmOptLevel dfs
     | llvmOptLevel dfs /= final_n = (dfs{ llvmOptLevel = final_n }, True)
     | otherwise = (dfs, False)

updOptLevel :: Int -> DynFlags -> DynFlags
-- ^ Sets the 'DynFlags' to be appropriate to the optimisation level
updOptLevel n = fst . updOptLevelChanged n

{- **********************************************************************
%*                                                                      *
                DynFlags parser
%*                                                                      *
%********************************************************************* -}

-- -----------------------------------------------------------------------------
-- Parsing the dynamic flags.


-- | Parse dynamic flags from a list of command line arguments.  Returns
-- the parsed 'DynFlags', the left-over arguments, and a list of warnings.
-- Throws a 'UsageError' if errors occurred during parsing (such as unknown
-- flags or missing arguments).
parseDynamicFlagsCmdLine :: MonadIO m => DynFlags -> [Located String]
                         -> m (DynFlags, [Located String], Messages DriverMessage)
                            -- ^ Updated 'DynFlags', left-over arguments, and
                            -- list of warnings.
parseDynamicFlagsCmdLine = parseDynamicFlagsFull flagsAll True


-- | Like 'parseDynamicFlagsCmdLine' but does not allow the package flags
-- (-package, -hide-package, -ignore-package, -hide-all-packages, -package-db).
-- Used to parse flags set in a modules pragma.
parseDynamicFilePragma :: MonadIO m => DynFlags -> [Located String]
                       -> m (DynFlags, [Located String], Messages DriverMessage)
                          -- ^ Updated 'DynFlags', left-over arguments, and
                          -- list of warnings.
parseDynamicFilePragma = parseDynamicFlagsFull flagsDynamic False

newtype CmdLineP s a = CmdLineP (forall m. (Monad m) => StateT s m a)
  deriving (Functor)

instance Monad (CmdLineP s) where
    CmdLineP k >>= f = CmdLineP (k >>= \x -> case f x of CmdLineP g -> g)
    return = pure

instance Applicative (CmdLineP s) where
    pure x = CmdLineP (pure x)
    (<*>) = ap

getCmdLineState :: CmdLineP s s
getCmdLineState = CmdLineP State.get

putCmdLineState :: s -> CmdLineP s ()
putCmdLineState x = CmdLineP (State.put x)

runCmdLineP :: CmdLineP s a -> s -> (a, s)
runCmdLineP (CmdLineP k) s0 = runIdentity $ runStateT k s0

-- | A helper to parse a set of flags from a list of command-line arguments, handling
-- response files.
processCmdLineP
    :: forall s m. MonadIO m
    => [Flag (CmdLineP s)]  -- ^ valid flags to match against
    -> s                    -- ^ current state
    -> [Located String]     -- ^ arguments to parse
    -> m (([Located String], [Err], [Warn]), s)
                            -- ^ (leftovers, errors, warnings)
processCmdLineP activeFlags s0 args =
    runStateT (processArgs (map (hoistFlag getCmdLineP) activeFlags) args parseResponseFile) s0
  where
    getCmdLineP :: CmdLineP s a -> StateT s m a
    getCmdLineP (CmdLineP k) = k

-- | Parses the dynamically set flags for GHC. This is the most general form of
-- the dynamic flag parser that the other methods simply wrap. It allows
-- saying which flags are valid flags and indicating if we are parsing
-- arguments from the command line or from a file pragma.
parseDynamicFlagsFull
    :: forall m. MonadIO m
    => [Flag (CmdLineP DynFlags)]    -- ^ valid flags to match against
    -> Bool                          -- ^ are the arguments from the command line?
    -> DynFlags                      -- ^ current dynamic flags
    -> [Located String]              -- ^ arguments to parse
    -> m (DynFlags, [Located String], Messages DriverMessage)
parseDynamicFlagsFull activeFlags cmdline dflags0 args = do
  ((leftover, errs, cli_warns), dflags1) <- processCmdLineP activeFlags dflags0 args

  -- See Note [Handling errors when parsing command-line flags]
  let rdr = renderWithContext (initSDocContext dflags0 defaultUserStyle)
  unless (null errs) $ liftIO $ throwGhcExceptionIO $ errorsToGhcException $
    map ((rdr . ppr . getLoc &&& unLoc) . errMsg) $ errs

  -- check for disabled flags in safe haskell
  let (dflags2, sh_warns) = safeFlagCheck cmdline dflags1
      theWays = ways dflags2

  unless (allowed_combination theWays) $ liftIO $
      throwGhcExceptionIO (CmdLineError ("combination not supported: " ++
                               intercalate "/" (map wayDesc (Set.toAscList theWays))))

  let (dflags3, consistency_warnings) = makeDynFlagsConsistent dflags2

  -- Set timer stats & heap size
  when (enableTimeStats dflags3) $ liftIO enableTimingStats
  case (ghcHeapSize dflags3) of
    Just x -> liftIO (setHeapSize x)
    _      -> return ()

  liftIO $ setUnsafeGlobalDynFlags dflags3

  -- create message envelopes using final DynFlags: #23402
  let diag_opts = initDiagOpts dflags3
      warns = warnsToMessages diag_opts $ mconcat [consistency_warnings, sh_warns, cli_warns]

  return (dflags3, leftover, warns)

-- | Check (and potentially disable) any extensions that aren't allowed
-- in safe mode.
--
-- The bool is to indicate if we are parsing command line flags (false means
-- file pragma). This allows us to generate better warnings.
safeFlagCheck :: Bool -> DynFlags -> (DynFlags, [Warn])
safeFlagCheck _ dflags | safeLanguageOn dflags = (dflagsUnset, warns)
  where
    -- Handle illegal flags under safe language.
    (dflagsUnset, warns) = foldl' check_method (dflags, mempty) unsafeFlags

    check_method (df, warns) (ext,loc,test,fix)
        | test df   = (fix df, safeFailure (loc df) ext :  warns)
        | otherwise = (df, warns)

    safeFailure loc ext
       = L loc $ DriverSafeHaskellIgnoredExtension ext

safeFlagCheck cmdl dflags =
  case safeInferOn dflags of
    True   -> (dflags' { safeInferred = safeFlags }, warn)
    False  -> (dflags', warn)

  where
    -- dynflags and warn for when -fpackage-trust by itself with no safe
    -- haskell flag
    (dflags', warn)
      | not (safeHaskellModeEnabled dflags) && not cmdl && packageTrustOn dflags
      = (gopt_unset dflags Opt_PackageTrust, pkgWarnMsg)
      | otherwise = (dflags, mempty)

    pkgWarnMsg :: [Warn]
    pkgWarnMsg = [ L (pkgTrustOnLoc dflags') DriverPackageTrustIgnored ]

    -- Have we inferred Unsafe? See Note [Safe Haskell Inference] in GHC.Driver.Main
    -- Force this to avoid retaining reference to old DynFlags value
    !safeFlags = all (\(_,_,t,_) -> not $ t dflags) unsafeFlagsForInfer

-- | Produce a list of suggestions for a user provided flag that is invalid.
flagSuggestions
  :: [String] -- valid flags to match against
  -> String
  -> [String]
flagSuggestions flags userInput
  -- fixes #11789
  -- If the flag contains '=',
  -- this uses both the whole and the left side of '=' for comparing.
  | elem '=' userInput =
        let (flagsWithEq, flagsWithoutEq) = partition (elem '=') flags
            fName = takeWhile (/= '=') userInput
        in (fuzzyMatch userInput flagsWithEq) ++ (fuzzyMatch fName flagsWithoutEq)
  | otherwise = fuzzyMatch userInput flags

{- **********************************************************************
%*                                                                      *
                DynFlags specifications
%*                                                                      *
%********************************************************************* -}

-- | All dynamic flags option strings without the deprecated ones.
-- These are the user facing strings for enabling and disabling options.
allNonDeprecatedFlags :: [String]
allNonDeprecatedFlags = allFlagsDeps False

-- | All flags with possibility to filter deprecated ones
allFlagsDeps :: Bool -> [String]
allFlagsDeps keepDeprecated = [ '-':flagName flag
                              | (deprecated, flag) <- flagsAllDeps
                              , keepDeprecated || not (isDeprecated deprecated)]
  where isDeprecated Deprecated = True
        isDeprecated _ = False

{-
 - Below we export user facing symbols for GHC dynamic flags for use with the
 - GHC API.
 -}

-- All dynamic flags present in GHC.
flagsAll :: [Flag (CmdLineP DynFlags)]
flagsAll = map snd flagsAllDeps

-- All dynamic flags present in GHC with deprecation information.
flagsAllDeps :: [(Deprecation, Flag (CmdLineP DynFlags))]
flagsAllDeps =  package_flags_deps ++ dynamic_flags_deps


-- All dynamic flags, minus package flags, present in GHC.
flagsDynamic :: [Flag (CmdLineP DynFlags)]
flagsDynamic = map snd dynamic_flags_deps

-- ALl package flags present in GHC.
flagsPackage :: [Flag (CmdLineP DynFlags)]
flagsPackage = map snd package_flags_deps

----------------Helpers to make flags and keep deprecation information----------

type FlagMaker m = String -> OptKind m -> Flag m
type DynFlagMaker = FlagMaker (CmdLineP DynFlags)

-- Make a non-deprecated flag
make_ord_flag :: DynFlagMaker -> String -> OptKind (CmdLineP DynFlags)
              -> (Deprecation, Flag (CmdLineP DynFlags))
make_ord_flag fm name kind = (NotDeprecated, fm name kind)

-- Make a deprecated flag
make_dep_flag :: DynFlagMaker -> String -> OptKind (CmdLineP DynFlags) -> String
                 -> (Deprecation, Flag (CmdLineP DynFlags))
make_dep_flag fm name kind message = (Deprecated,
                                      fm name $ add_dep_message kind message)

add_dep_message :: OptKind (CmdLineP DynFlags) -> String
                -> OptKind (CmdLineP DynFlags)
add_dep_message (NoArg f) message = NoArg $ f >> deprecate message
add_dep_message (HasArg f) message = HasArg $ \s -> f s >> deprecate message
add_dep_message (SepArg f) message = SepArg $ \s -> f s >> deprecate message
add_dep_message (Prefix f) message = Prefix $ \s -> f s >> deprecate message
add_dep_message (OptPrefix f) message =
                                  OptPrefix $ \s -> f s >> deprecate message
add_dep_message (OptIntSuffix f) message =
                               OptIntSuffix $ \oi -> f oi >> deprecate message
add_dep_message (IntSuffix f) message =
                                  IntSuffix $ \i -> f i >> deprecate message
add_dep_message (Word64Suffix f) message =
                                  Word64Suffix $ \i -> f i >> deprecate message
add_dep_message (FloatSuffix f) message =
                                FloatSuffix $ \fl -> f fl >> deprecate message
add_dep_message (PassFlag f) message =
                                   PassFlag $ \s -> f s >> deprecate message
add_dep_message (AnySuffix f) message =
                                  AnySuffix $ \s -> f s >> deprecate message

----------------------- The main flags themselves ------------------------------
-- See Note [Updating flag description in the User's Guide]
-- See Note [Supporting CLI completion]
dynamic_flags_deps :: [(Deprecation, Flag (CmdLineP DynFlags))]
dynamic_flags_deps = [
    make_dep_flag defFlag "n" (NoArg $ return ())
        "The -n flag is deprecated and no longer has any effect"
  , make_ord_flag defFlag "cpp"      (NoArg (setExtensionFlag LangExt.Cpp))
  , make_ord_flag defFlag "F"        (NoArg (setGeneralFlag Opt_Pp))
  , (Deprecated, defFlag "#include"
      (HasArg (\_s ->
         deprecate ("-#include and INCLUDE pragmas are " ++
                    "deprecated: They no longer have any effect"))))
  , make_ord_flag defFlag "v"        (OptIntSuffix setVerbosity)

  , make_ord_flag defGhcFlag "j"     (OptIntSuffix
        (\n -> case n of
                 Just n
                     | n > 0     -> upd (\d -> d { parMakeCount = Just (ParMakeThisMany n) })
                     | otherwise -> addErr "Syntax: -j[n] where n > 0"
                 Nothing -> upd (\d -> d { parMakeCount = Just ParMakeNumProcessors })))
                 -- When the number of parallel builds
                 -- is omitted, it is the same
                 -- as specifying that the number of
                 -- parallel builds is equal to the
                 -- result of getNumProcessors
  , make_ord_flag defGhcFlag "jsem" $ hasArg $ \f d -> d { parMakeCount = Just (ParMakeSemaphore f) }

  , make_ord_flag defFlag "instantiated-with"   (sepArg setUnitInstantiations)
  , make_ord_flag defFlag "this-component-id"   (sepArg setUnitInstanceOf)

    -- RTS options -------------------------------------------------------------
  , make_ord_flag defFlag "H"           (HasArg (\s -> upd (\d ->
          d { ghcHeapSize = Just $ fromIntegral (decodeSize s)})))

  , make_ord_flag defFlag "Rghc-timing" (NoArg (upd (\d ->
                                               d { enableTimeStats = True })))

    ------- ways ---------------------------------------------------------------
  , make_ord_flag defGhcFlag "prof"           (NoArg (addWayDynP WayProf))
  , (Deprecated, defFlag     "eventlog"
     $ noArgM $ \d -> do
         deprecate "the eventlog is now enabled in all runtime system ways"
         return d)
  , make_ord_flag defGhcFlag "debug"          (NoArg (addWayDynP WayDebug))
  , make_ord_flag defGhcFlag "threaded"       (NoArg (addWayDynP WayThreaded))
  , make_ord_flag defGhcFlag "single-threaded" (NoArg (removeWayDynP WayThreaded))

  , make_ord_flag defGhcFlag "ticky"
      (NoArg (setGeneralFlag Opt_Ticky >> addWayDynP WayDebug))

    -- -ticky enables ticky-ticky code generation, and also implies -debug which
    -- is required to get the RTS ticky support.

        ----- Linker --------------------------------------------------------
  , make_ord_flag defGhcFlag "static"         (NoArg (removeWayDynP WayDyn))
  , make_ord_flag defGhcFlag "dynamic"        (NoArg (addWayDynP WayDyn))
  , make_ord_flag defGhcFlag "rdynamic" $ noArg $
#if defined(linux_HOST_OS)
                              addOptl "-rdynamic"
#elif defined(mingw32_HOST_OS)
                              addOptl "-Wl,--export-all-symbols"
#else
    -- ignored for compat w/ gcc:
                              id
#endif
  , make_ord_flag defGhcFlag "relative-dynlib-paths"
      (NoArg (setGeneralFlag Opt_RelativeDynlibPaths))
  , make_ord_flag defGhcFlag "copy-libs-when-linking"
      (NoArg (setGeneralFlag Opt_SingleLibFolder))
  , make_ord_flag defGhcFlag "pie"            (NoArg (setGeneralFlag Opt_PICExecutable))
  , make_ord_flag defGhcFlag "no-pie"         (NoArg (unSetGeneralFlag Opt_PICExecutable))

        ------- Specific phases  --------------------------------------------
    -- need to appear before -pgmL to be parsed as LLVM flags.
  , make_ord_flag defFlag "pgmlo"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_lo  = (f,[]) }
  , make_ord_flag defFlag "pgmlc"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_lc  = (f,[]) }
  , make_ord_flag defFlag "pgmlas"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_las  = (f,[]) }
  , make_ord_flag defFlag "pgmlm"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_lm  =
          if null f then Nothing else Just (f,[]) }
  , make_ord_flag defFlag "pgmi"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_i   =  f }
  , make_ord_flag defFlag "pgmL"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_L   = f }
  , make_ord_flag defFlag "pgmP"
      (hasArg setPgmP)
  , make_ord_flag defFlag "pgmJSP"
      (hasArg setPgmJSP)
  , make_ord_flag defFlag "pgmCmmP"
      (hasArg setPgmCmmP)
  , make_ord_flag defFlag "pgmF"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_F   = f }
  , make_ord_flag defFlag "pgmc"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_c   = f }
  , make_ord_flag defFlag "pgmcxx"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_cxx = f }
  , (Deprecated, defFlag  "pgmc-supports-no-pie"
      $ noArgM  $ \d -> do
        deprecate $ "use -pgml-supports-no-pie instead"
        pure $ alterToolSettings (\s -> s { toolSettings_ccSupportsNoPie = True }) d)
  , make_ord_flag defFlag "pgms"
      (HasArg (\_ -> addWarn "Object splitting was removed in GHC 8.8"))
  , make_ord_flag defFlag "pgma"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_a   = (f,[]) }
  , make_ord_flag defFlag "pgml"
      $ hasArg $ \f -> alterToolSettings $ \s -> s
         { toolSettings_pgm_l   = (f,[])
         , -- Don't pass -no-pie with custom -pgml (see #15319). Note
           -- that this could break when -no-pie is actually needed.
           -- But the CC_SUPPORTS_NO_PIE check only happens at
           -- buildtime, and -pgml is a runtime option. A better
           -- solution would be running this check for each custom
           -- -pgml.
           toolSettings_ccSupportsNoPie = False
         }
  , make_ord_flag defFlag "pgml-supports-no-pie"
      $ noArg $ alterToolSettings $ \s -> s { toolSettings_ccSupportsNoPie = True }
  , make_ord_flag defFlag "pgmwindres"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_windres = f }
  , make_ord_flag defFlag "pgmar"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_ar = f }
  , make_ord_flag defFlag "pgmotool"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_otool = f}
  , make_ord_flag defFlag "pgminstall_name_tool"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_install_name_tool = f}
  , make_ord_flag defFlag "pgmranlib"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_pgm_ranlib = f }


    -- need to appear before -optl/-opta to be parsed as LLVM flags.
  , make_ord_flag defFlag "optlm"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_lm  = f : toolSettings_opt_lm s }
  , make_ord_flag defFlag "optlo"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_lo  = f : toolSettings_opt_lo s }
  , make_ord_flag defFlag "optlc"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_lc  = f : toolSettings_opt_lc s }
  , make_ord_flag defFlag "optlas"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_las  = f : toolSettings_opt_las s }
  , make_ord_flag defFlag "opti"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_i   = f : toolSettings_opt_i s }
  , make_ord_flag defFlag "optL"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_L   = f : toolSettings_opt_L s }
  , make_ord_flag defFlag "optP"
      (hasArg addOptP)
  , make_ord_flag defFlag "optJSP"
      (hasArg addOptJSP)
  , make_ord_flag defFlag "optCmmP"
      (hasArg addOptCmmP)
  , make_ord_flag defFlag "optF"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_F   = f : toolSettings_opt_F s }
  , make_ord_flag defFlag "optc"
      (hasArg addOptc)
  , make_ord_flag defFlag "optcxx"
      (hasArg addOptcxx)
  , make_ord_flag defFlag "opta"
      $ hasArg $ \f -> alterToolSettings $ \s -> s { toolSettings_opt_a   = f : toolSettings_opt_a s }
  , make_ord_flag defFlag "optl"
      (hasArg addOptl)
  , make_ord_flag defFlag "optwindres"
      $ hasArg $ \f ->
        alterToolSettings $ \s -> s { toolSettings_opt_windres = f : toolSettings_opt_windres s }

    -- N.B. We may someday deprecate this in favor of -fsplit-sections,
    -- which has the benefit of also having a negating -fno-split-sections.
  , make_ord_flag defGhcFlag "split-sections"
      (NoArg $ setGeneralFlag Opt_SplitSections)

        -------- ghc -M -----------------------------------------------------
  , make_ord_flag defGhcFlag "dep-suffix"              (hasArg addDepSuffix)
  , make_ord_flag defGhcFlag "dep-makefile"            (hasArg setDepMakefile)
  , make_ord_flag defGhcFlag "include-cpp-deps"
        (noArg (setDepIncludeCppDeps True))
  , make_ord_flag defGhcFlag "include-pkg-deps"
        (noArg (setDepIncludePkgDeps True))
  , make_ord_flag defGhcFlag "exclude-module"          (hasArg addDepExcludeMod)

        -------- Linking ----------------------------------------------------
  , make_ord_flag defGhcFlag "no-link"
        (noArg (\d -> d { ghcLink=NoLink }))
  , make_ord_flag defGhcFlag "shared"
        (noArg (\d -> d { ghcLink=LinkDynLib }))
  , make_ord_flag defGhcFlag "staticlib"
        (noArg (\d -> setGeneralFlag' Opt_LinkRts (d { ghcLink=LinkStaticLib })))
  , make_ord_flag defGhcFlag "-merge-objs"
        (noArg (\d -> d { ghcLink=LinkMergedObj }))
  , make_ord_flag defGhcFlag "dynload"            (hasArg parseDynLibLoaderMode)
  , make_ord_flag defGhcFlag "dylib-install-name" (hasArg setDylibInstallName)

        ------- Libraries ---------------------------------------------------
  , make_ord_flag defFlag "L"   (Prefix addLibraryPath)
  , make_ord_flag defFlag "l"   (hasArg (addLdInputs . Option . ("-l" ++)))

        ------- Frameworks --------------------------------------------------
        -- -framework-path should really be -F ...
  , make_ord_flag defFlag "framework-path" (HasArg addFrameworkPath)
  , make_ord_flag defFlag "framework"      (hasArg addCmdlineFramework)

        ------- Output Redirection ------------------------------------------
  , make_ord_flag defGhcFlag "odir"              (hasArg setObjectDir)
  , make_ord_flag defGhcFlag "o"                 (sepArg (setOutputFile . Just))
  , make_ord_flag defGhcFlag "dyno"
        (sepArg (setDynOutputFile . Just))
  , make_ord_flag defGhcFlag "ohi"
        (hasArg (setOutputHi . Just ))
  , make_ord_flag defGhcFlag "dynohi"
        (hasArg (setDynOutputHi . Just ))
  , make_ord_flag defGhcFlag "osuf"              (hasArg setObjectSuf)
  , make_ord_flag defGhcFlag "dynosuf"           (hasArg setDynObjectSuf)
  , make_ord_flag defGhcFlag "hcsuf"             (hasArg setHcSuf)
  , make_ord_flag defGhcFlag "hisuf"             (hasArg setHiSuf)
  , make_ord_flag defGhcFlag "hiesuf"            (hasArg setHieSuf)
  , make_ord_flag defGhcFlag "dynhisuf"          (hasArg setDynHiSuf)
  , make_ord_flag defGhcFlag "hidir"             (hasArg setHiDir)
  , make_ord_flag defGhcFlag "hiedir"            (hasArg setHieDir)
  , make_ord_flag defGhcFlag "tmpdir"            (hasArg setTmpDir)
  , make_ord_flag defGhcFlag "stubdir"           (hasArg setStubDir)
  , make_ord_flag defGhcFlag "dumpdir"           (hasArg setDumpDir)
  , make_ord_flag defGhcFlag "outputdir"         (hasArg setOutputDir)
  , make_ord_flag defGhcFlag "ddump-file-prefix"
        (hasArg (setDumpPrefixForce . Just . flip (++) "."))

  , make_ord_flag defGhcFlag "dynamic-too"
        (NoArg (setGeneralFlag Opt_BuildDynamicToo))

        ------- Keeping temporary files -------------------------------------
     -- These can be singular (think ghc -c) or plural (think ghc --make)
  , make_ord_flag defGhcFlag "keep-hc-file"
        (NoArg (setGeneralFlag Opt_KeepHcFiles))
  , make_ord_flag defGhcFlag "keep-hc-files"
        (NoArg (setGeneralFlag Opt_KeepHcFiles))
  , make_ord_flag defGhcFlag "keep-hscpp-file"
        (NoArg (setGeneralFlag Opt_KeepHscppFiles))
  , make_ord_flag defGhcFlag "keep-hscpp-files"
        (NoArg (setGeneralFlag Opt_KeepHscppFiles))
  , make_ord_flag defGhcFlag "keep-s-file"
        (NoArg (setGeneralFlag Opt_KeepSFiles))
  , make_ord_flag defGhcFlag "keep-s-files"
        (NoArg (setGeneralFlag Opt_KeepSFiles))
  , make_ord_flag defGhcFlag "keep-llvm-file"
        (NoArg $ setObjBackend llvmBackend >> setGeneralFlag Opt_KeepLlvmFiles)
  , make_ord_flag defGhcFlag "keep-llvm-files"
        (NoArg $ setObjBackend llvmBackend >> setGeneralFlag Opt_KeepLlvmFiles)
     -- This only makes sense as plural
  , make_ord_flag defGhcFlag "keep-tmp-files"
        (NoArg (setGeneralFlag Opt_KeepTmpFiles))
  , make_ord_flag defGhcFlag "keep-hi-file"
        (NoArg (setGeneralFlag Opt_KeepHiFiles))
  , make_ord_flag defGhcFlag "no-keep-hi-file"
        (NoArg (unSetGeneralFlag Opt_KeepHiFiles))
  , make_ord_flag defGhcFlag "keep-hi-files"
        (NoArg (setGeneralFlag Opt_KeepHiFiles))
  , make_ord_flag defGhcFlag "no-keep-hi-files"
        (NoArg (unSetGeneralFlag Opt_KeepHiFiles))
  , make_ord_flag defGhcFlag "keep-o-file"
        (NoArg (setGeneralFlag Opt_KeepOFiles))
  , make_ord_flag defGhcFlag "no-keep-o-file"
        (NoArg (unSetGeneralFlag Opt_KeepOFiles))
  , make_ord_flag defGhcFlag "keep-o-files"
        (NoArg (setGeneralFlag Opt_KeepOFiles))
  , make_ord_flag defGhcFlag "no-keep-o-files"
        (NoArg (unSetGeneralFlag Opt_KeepOFiles))

        ------- Miscellaneous ----------------------------------------------
  , make_ord_flag defGhcFlag "no-auto-link-packages"
        (NoArg (unSetGeneralFlag Opt_AutoLinkPackages))
  , make_ord_flag defGhcFlag "no-hs-main"
        (NoArg (setGeneralFlag Opt_NoHsMain))
  , make_ord_flag defGhcFlag "fno-state-hack"
        (NoArg (setGeneralFlag Opt_G_NoStateHack))
  , make_ord_flag defGhcFlag "fno-opt-coercion"
        (NoArg (setGeneralFlag Opt_G_NoOptCoercion))
  , make_ord_flag defGhcFlag "with-rtsopts"
        (HasArg setRtsOpts)
  , make_ord_flag defGhcFlag "rtsopts"
        (NoArg (setRtsOptsEnabled RtsOptsAll))
  , make_ord_flag defGhcFlag "rtsopts=all"
        (NoArg (setRtsOptsEnabled RtsOptsAll))
  , make_ord_flag defGhcFlag "rtsopts=some"
        (NoArg (setRtsOptsEnabled RtsOptsSafeOnly))
  , make_ord_flag defGhcFlag "rtsopts=none"
        (NoArg (setRtsOptsEnabled RtsOptsNone))
  , make_ord_flag defGhcFlag "rtsopts=ignore"
        (NoArg (setRtsOptsEnabled RtsOptsIgnore))
  , make_ord_flag defGhcFlag "rtsopts=ignoreAll"
        (NoArg (setRtsOptsEnabled RtsOptsIgnoreAll))
  , make_ord_flag defGhcFlag "no-rtsopts"
        (NoArg (setRtsOptsEnabled RtsOptsNone))
  , make_ord_flag defGhcFlag "no-rtsopts-suggestions"
      (noArg (\d -> d {rtsOptsSuggestions = False}))
  , make_ord_flag defGhcFlag "dhex-word-literals"
        (NoArg (setGeneralFlag Opt_HexWordLiterals))

  , make_ord_flag defGhcFlag "ghcversion-file"      (hasArg addGhcVersionFile)
  , make_ord_flag defGhcFlag "main-is"              (SepArg setMainIs)
  , make_ord_flag defGhcFlag "haddock"              (NoArg (setGeneralFlag Opt_Haddock))
  , make_ord_flag defGhcFlag "no-haddock"           (NoArg (unSetGeneralFlag Opt_Haddock))
  , make_ord_flag defGhcFlag "haddock-opts"         (hasArg addHaddockOpts)
  , make_ord_flag defGhcFlag "hpcdir"               (SepArg setOptHpcDir)
  , make_ord_flag defGhciFlag "ghci-script"         (hasArg addGhciScript)
  , make_ord_flag defGhciFlag "interactive-print"   (hasArg setInteractivePrint)
  , make_ord_flag defGhcFlag "ticky-allocd"
        (NoArg (setGeneralFlag Opt_Ticky_Allocd))
  , make_ord_flag defGhcFlag "ticky-LNE"
        (NoArg (setGeneralFlag Opt_Ticky_LNE))
  , make_ord_flag defGhcFlag "ticky-ap-thunk"
        (NoArg (setGeneralFlag Opt_Ticky_AP))
  , make_ord_flag defGhcFlag "ticky-dyn-thunk"
        (NoArg (setGeneralFlag Opt_Ticky_Dyn_Thunk))
  , make_ord_flag defGhcFlag "ticky-tag-checks"
        (NoArg (setGeneralFlag Opt_Ticky_Tag))
        ------- recompilation checker --------------------------------------
  , make_dep_flag defGhcFlag "recomp"
        (NoArg $ unSetGeneralFlag Opt_ForceRecomp)
             "Use -fno-force-recomp instead"
  , make_dep_flag defGhcFlag "no-recomp"
        (NoArg $ setGeneralFlag Opt_ForceRecomp) "Use -fforce-recomp instead"
  , make_ord_flag defFlag "fmax-errors"
      (intSuffix (\n d -> d { maxErrors = Just (max 1 n) }))
  , make_ord_flag defFlag "fno-max-errors"
      (noArg (\d -> d { maxErrors = Nothing }))
  , make_ord_flag defFlag "freverse-errors"
        (noArg (\d -> d {reverseErrors = True} ))
  , make_ord_flag defFlag "fno-reverse-errors"
        (noArg (\d -> d {reverseErrors = False} ))

        ------ HsCpp opts ---------------------------------------------------
  , make_ord_flag defFlag "D"              (AnySuffix (upd . addOptP))
  , make_ord_flag defFlag "U"              (AnySuffix (upd . addOptP))

        ------- Include/Import Paths ----------------------------------------
  , make_ord_flag defFlag "I"              (Prefix    addIncludePath)
  , make_ord_flag defFlag "i"              (OptPrefix addImportPath)

        ------ Output style options -----------------------------------------
  , make_ord_flag defFlag "dppr-user-length" (intSuffix (\n d ->
                                                       d { pprUserLength = n }))
  , make_ord_flag defFlag "dppr-cols"        (intSuffix (\n d ->
                                                             d { pprCols = n }))
  , make_ord_flag defFlag "fdiagnostics-color=auto"
      (NoArg (upd (\d -> d { useColor = Auto })))
  , make_ord_flag defFlag "fdiagnostics-color=always"
      (NoArg (upd (\d -> d { useColor = Always })))
  , make_ord_flag defFlag "fdiagnostics-color=never"
      (NoArg (upd (\d -> d { useColor = Never })))

  , make_ord_flag defFlag "fprint-error-index-links=auto"
      (NoArg (upd (\d -> d { useErrorLinks = Auto })))
  , make_ord_flag defFlag "fprint-error-index-links=always"
      (NoArg (upd (\d -> d { useErrorLinks = Always })))
  , make_ord_flag defFlag "fprint-error-index-links=never"
      (NoArg (upd (\d -> d { useErrorLinks = Never })))

  -- Suppress all that is suppressible in core dumps.
  -- Except for uniques, as some simplifier phases introduce new variables that
  -- have otherwise identical names.
  , make_ord_flag defGhcFlag "dsuppress-all"
      (NoArg $ do setGeneralFlag Opt_SuppressCoercions
                  setGeneralFlag Opt_SuppressCoercionTypes
                  setGeneralFlag Opt_SuppressVarKinds
                  setGeneralFlag Opt_SuppressModulePrefixes
                  setGeneralFlag Opt_SuppressTypeApplications
                  setGeneralFlag Opt_SuppressIdInfo
                  setGeneralFlag Opt_SuppressTicks
                  setGeneralFlag Opt_SuppressStgExts
                  setGeneralFlag Opt_SuppressStgReps
                  setGeneralFlag Opt_SuppressTypeSignatures
                  setGeneralFlag Opt_SuppressCoreSizes
                  setGeneralFlag Opt_SuppressTimestamps)

        ------ Debugging ----------------------------------------------------
  , make_ord_flag defGhcFlag "dstg-stats"
        (NoArg (setGeneralFlag Opt_StgStats))

  , make_ord_flag defGhcFlag "ddump-cmm"
        (setDumpFlag Opt_D_dump_cmm)
  , make_ord_flag defGhcFlag "ddump-cmm-from-stg"
        (setDumpFlag Opt_D_dump_cmm_from_stg)
  , make_ord_flag defGhcFlag "ddump-cmm-raw"
        (setDumpFlag Opt_D_dump_cmm_raw)
  , make_ord_flag defGhcFlag "ddump-cmm-verbose"
        (setDumpFlag Opt_D_dump_cmm_verbose)
  , make_ord_flag defGhcFlag "ddump-cmm-verbose-by-proc"
        (setDumpFlag Opt_D_dump_cmm_verbose_by_proc)
  , make_ord_flag defGhcFlag "ddump-cmm-cfg"
        (setDumpFlag Opt_D_dump_cmm_cfg)
  , make_ord_flag defGhcFlag "ddump-cmm-cbe"
        (setDumpFlag Opt_D_dump_cmm_cbe)
  , make_ord_flag defGhcFlag "ddump-cmm-switch"
        (setDumpFlag Opt_D_dump_cmm_switch)
  , make_ord_flag defGhcFlag "ddump-cmm-proc"
        (setDumpFlag Opt_D_dump_cmm_proc)
  , make_ord_flag defGhcFlag "ddump-cmm-sp"
        (setDumpFlag Opt_D_dump_cmm_sp)
  , make_ord_flag defGhcFlag "ddump-cmm-sink"
        (setDumpFlag Opt_D_dump_cmm_sink)
  , make_ord_flag defGhcFlag "ddump-cmm-caf"
        (setDumpFlag Opt_D_dump_cmm_caf)
  , make_ord_flag defGhcFlag "ddump-cmm-procmap"
        (setDumpFlag Opt_D_dump_cmm_procmap)
  , make_ord_flag defGhcFlag "ddump-cmm-split"
        (setDumpFlag Opt_D_dump_cmm_split)
  , make_ord_flag defGhcFlag "ddump-cmm-info"
        (setDumpFlag Opt_D_dump_cmm_info)
  , make_ord_flag defGhcFlag "ddump-cmm-cps"
        (setDumpFlag Opt_D_dump_cmm_cps)
  , make_ord_flag defGhcFlag "ddump-cmm-opt"
        (setDumpFlag Opt_D_dump_opt_cmm)
  , make_ord_flag defGhcFlag "ddump-cmm-thread-sanitizer"
        (setDumpFlag Opt_D_dump_cmm_thread_sanitizer)
  , make_ord_flag defGhcFlag "ddump-cfg-weights"
        (setDumpFlag Opt_D_dump_cfg_weights)
  , make_ord_flag defGhcFlag "ddump-core-stats"
        (setDumpFlag Opt_D_dump_core_stats)
  , make_ord_flag defGhcFlag "ddump-asm"
        (setDumpFlag Opt_D_dump_asm)
  , make_ord_flag defGhcFlag "ddump-js"
        (setDumpFlag Opt_D_dump_js)
  , make_ord_flag defGhcFlag "ddump-asm-native"
        (setDumpFlag Opt_D_dump_asm_native)
  , make_ord_flag defGhcFlag "ddump-asm-liveness"
        (setDumpFlag Opt_D_dump_asm_liveness)
  , make_ord_flag defGhcFlag "ddump-asm-regalloc"
        (setDumpFlag Opt_D_dump_asm_regalloc)
  , make_ord_flag defGhcFlag "ddump-asm-conflicts"
        (setDumpFlag Opt_D_dump_asm_conflicts)
  , make_ord_flag defGhcFlag "ddump-asm-regalloc-stages"
        (setDumpFlag Opt_D_dump_asm_regalloc_stages)
  , make_ord_flag defGhcFlag "ddump-asm-stats"
        (setDumpFlag Opt_D_dump_asm_stats)
  , make_ord_flag defGhcFlag "ddump-llvm"
        (NoArg $ setDumpFlag' Opt_D_dump_llvm)
  , make_ord_flag defGhcFlag "ddump-c-backend"
        (NoArg $ setDumpFlag' Opt_D_dump_c_backend)
  , make_ord_flag defGhcFlag "ddump-deriv"
        (setDumpFlag Opt_D_dump_deriv)
  , make_ord_flag defGhcFlag "ddump-ds"
        (setDumpFlag Opt_D_dump_ds)
  , make_ord_flag defGhcFlag "ddump-ds-preopt"
        (setDumpFlag Opt_D_dump_ds_preopt)
  , make_ord_flag defGhcFlag "ddump-foreign"
        (setDumpFlag Opt_D_dump_foreign)
  , make_ord_flag defGhcFlag "ddump-inlinings"
        (setDumpFlag Opt_D_dump_inlinings)
  , make_ord_flag defGhcFlag "ddump-verbose-inlinings"
        (setDumpFlag Opt_D_dump_verbose_inlinings)
  , make_ord_flag defGhcFlag "ddump-rule-firings"
        (setDumpFlag Opt_D_dump_rule_firings)
  , make_ord_flag defGhcFlag "ddump-rule-rewrites"
        (setDumpFlag Opt_D_dump_rule_rewrites)
  , make_ord_flag defGhcFlag "ddump-simpl-trace"
        (setDumpFlag Opt_D_dump_simpl_trace)
  , make_ord_flag defGhcFlag "ddump-occur-anal"
        (setDumpFlag Opt_D_dump_occur_anal)
  , make_ord_flag defGhcFlag "ddump-parsed"
        (setDumpFlag Opt_D_dump_parsed)
  , make_ord_flag defGhcFlag "ddump-parsed-ast"
        (setDumpFlag Opt_D_dump_parsed_ast)
  , make_ord_flag defGhcFlag "dkeep-comments"
        (NoArg (setGeneralFlag Opt_KeepRawTokenStream))
  , make_ord_flag defGhcFlag "ddump-rn"
        (setDumpFlag Opt_D_dump_rn)
  , make_ord_flag defGhcFlag "ddump-rn-ast"
        (setDumpFlag Opt_D_dump_rn_ast)
  , make_ord_flag defGhcFlag "ddump-simpl"
        (setDumpFlag Opt_D_dump_simpl)
  , make_ord_flag defGhcFlag "ddump-simpl-iterations"
      (setDumpFlag Opt_D_dump_simpl_iterations)
  , make_ord_flag defGhcFlag "ddump-spec"
        (setDumpFlag Opt_D_dump_spec)
  , make_ord_flag defGhcFlag "ddump-spec-constr"
        (setDumpFlag Opt_D_dump_spec_constr)
  , make_ord_flag defGhcFlag "ddump-prep"
        (setDumpFlag Opt_D_dump_prep)
  , make_ord_flag defGhcFlag "ddump-late-cc"
        (setDumpFlag Opt_D_dump_late_cc)
  , make_ord_flag defGhcFlag "ddump-stg-from-core"
        (setDumpFlag Opt_D_dump_stg_from_core)
  , make_ord_flag defGhcFlag "ddump-stg-unarised"
        (setDumpFlag Opt_D_dump_stg_unarised)
  , make_ord_flag defGhcFlag "ddump-stg-final"
        (setDumpFlag Opt_D_dump_stg_final)
  , make_ord_flag defGhcFlag "ddump-stg-cg"
        (setDumpFlag Opt_D_dump_stg_cg)
  , make_dep_flag defGhcFlag "ddump-stg"
        (setDumpFlag Opt_D_dump_stg_from_core)
        "Use `-ddump-stg-from-core` or `-ddump-stg-final` instead"
  , make_ord_flag defGhcFlag "ddump-stg-tags"
        (setDumpFlag Opt_D_dump_stg_tags)
  , make_ord_flag defGhcFlag "ddump-stg-from-js-sinker"
        (setDumpFlag Opt_D_dump_stg_from_js_sinker)
  , make_ord_flag defGhcFlag "ddump-call-arity"
        (setDumpFlag Opt_D_dump_call_arity)
  , make_ord_flag defGhcFlag "ddump-exitify"
        (setDumpFlag Opt_D_dump_exitify)
  , make_dep_flag defGhcFlag "ddump-stranal"
        (setDumpFlag Opt_D_dump_dmdanal)
        "Use `-ddump-dmdanal` instead"
  , make_dep_flag defGhcFlag "ddump-str-signatures"
        (setDumpFlag Opt_D_dump_dmd_signatures)
        "Use `-ddump-dmd-signatures` instead"
  , make_ord_flag defGhcFlag "ddump-dmdanal"
        (setDumpFlag Opt_D_dump_dmdanal)
  , make_ord_flag defGhcFlag "ddump-dmd-signatures"
        (setDumpFlag Opt_D_dump_dmd_signatures)
  , make_ord_flag defGhcFlag "ddump-cpranal"
        (setDumpFlag Opt_D_dump_cpranal)
  , make_ord_flag defGhcFlag "ddump-cpr-signatures"
        (setDumpFlag Opt_D_dump_cpr_signatures)
  , make_ord_flag defGhcFlag "ddump-tc"
        (setDumpFlag Opt_D_dump_tc)
  , make_ord_flag defGhcFlag "ddump-tc-ast"
        (setDumpFlag Opt_D_dump_tc_ast)
  , make_ord_flag defGhcFlag "ddump-hie"
        (setDumpFlag Opt_D_dump_hie)
  , make_ord_flag defGhcFlag "ddump-types"
        (setDumpFlag Opt_D_dump_types)
  , make_ord_flag defGhcFlag "ddump-rules"
        (setDumpFlag Opt_D_dump_rules)
  , make_ord_flag defGhcFlag "ddump-cse"
        (setDumpFlag Opt_D_dump_cse)
  , make_ord_flag defGhcFlag "ddump-float-out"
        (setDumpFlag Opt_D_dump_float_out)
  , make_ord_flag defGhcFlag "ddump-full-laziness"
        (setDumpFlag Opt_D_dump_float_out)
  , make_ord_flag defGhcFlag "ddump-float-in"
        (setDumpFlag Opt_D_dump_float_in)
  , make_ord_flag defGhcFlag "ddump-liberate-case"
        (setDumpFlag Opt_D_dump_liberate_case)
  , make_ord_flag defGhcFlag "ddump-static-argument-transformation"
        (setDumpFlag Opt_D_dump_static_argument_transformation)
  , make_ord_flag defGhcFlag "ddump-worker-wrapper"
        (setDumpFlag Opt_D_dump_worker_wrapper)
  , make_ord_flag defGhcFlag "ddump-rn-trace"
        (setDumpFlag Opt_D_dump_rn_trace)
  , make_ord_flag defGhcFlag "ddump-if-trace"
        (setDumpFlag Opt_D_dump_if_trace)
  , make_ord_flag defGhcFlag "ddump-cs-trace"
        (setDumpFlag Opt_D_dump_cs_trace)
  , make_ord_flag defGhcFlag "ddump-tc-trace"
        (NoArg (do setDumpFlag' Opt_D_dump_tc_trace
                   setDumpFlag' Opt_D_dump_cs_trace))
  , make_ord_flag defGhcFlag "ddump-ec-trace"
        (setDumpFlag Opt_D_dump_ec_trace)
  , make_ord_flag defGhcFlag "ddump-splices"
        (setDumpFlag Opt_D_dump_splices)
  , make_ord_flag defGhcFlag "dth-dec-file"
        (setDumpFlag Opt_D_th_dec_file)

  , make_ord_flag defGhcFlag "ddump-rn-stats"
        (setDumpFlag Opt_D_dump_rn_stats)
  , make_ord_flag defGhcFlag "ddump-opt-cmm" --old alias for cmm-opt
        (setDumpFlag Opt_D_dump_opt_cmm)
  , make_ord_flag defGhcFlag "ddump-simpl-stats"
        (setDumpFlag Opt_D_dump_simpl_stats)
  , make_ord_flag defGhcFlag "ddump-bcos"
        (setDumpFlag Opt_D_dump_BCOs)
  , make_ord_flag defGhcFlag "dsource-stats"
        (setDumpFlag Opt_D_source_stats)
  , make_ord_flag defGhcFlag "dverbose-core2core"
        (NoArg $ setVerbosity (Just 2) >> setDumpFlag' Opt_D_verbose_core2core)
  , make_ord_flag defGhcFlag "dverbose-stg2stg"
        (setDumpFlag Opt_D_verbose_stg2stg)
  , make_ord_flag defGhcFlag "ddump-hi"
        (setDumpFlag Opt_D_dump_hi)
  , make_ord_flag defGhcFlag "ddump-minimal-imports"
        (NoArg (setGeneralFlag Opt_D_dump_minimal_imports))
  , make_ord_flag defGhcFlag "ddump-hpc"
        (setDumpFlag Opt_D_dump_ticked) -- back compat
  , make_ord_flag defGhcFlag "ddump-ticked"
        (setDumpFlag Opt_D_dump_ticked)
  , make_ord_flag defGhcFlag "ddump-mod-cycles"
        (setDumpFlag Opt_D_dump_mod_cycles)
  , make_ord_flag defGhcFlag "ddump-mod-map"
        (setDumpFlag Opt_D_dump_mod_map)
  , make_ord_flag defGhcFlag "ddump-timings"
        (setDumpFlag Opt_D_dump_timings)
  , make_ord_flag defGhcFlag "ddump-view-pattern-commoning"
        (setDumpFlag Opt_D_dump_view_pattern_commoning)
  , make_ord_flag defGhcFlag "ddump-to-file"
        (NoArg (setGeneralFlag Opt_DumpToFile))
  , make_ord_flag defGhcFlag "ddump-hi-diffs"
        (setDumpFlag Opt_D_dump_hi_diffs)
  , make_ord_flag defGhcFlag "ddump-rtti"
        (setDumpFlag Opt_D_dump_rtti)
  , make_ord_flag defGhcFlag "dlint"
        (NoArg enableDLint)
  , make_ord_flag defGhcFlag "dcore-lint"
        (NoArg (setGeneralFlag Opt_DoCoreLinting))
  , make_ord_flag defGhcFlag "dlinear-core-lint"
        (NoArg (setGeneralFlag Opt_DoLinearCoreLinting))
  , make_ord_flag defGhcFlag "dstg-lint"
        (NoArg (setGeneralFlag Opt_DoStgLinting))
  , make_ord_flag defGhcFlag "dcmm-lint"
        (NoArg (setGeneralFlag Opt_DoCmmLinting))
  , make_ord_flag defGhcFlag "dasm-lint"
        (NoArg (setGeneralFlag Opt_DoAsmLinting))
  , make_ord_flag defGhcFlag "dannot-lint"
        (NoArg (setGeneralFlag Opt_DoAnnotationLinting))
  , make_ord_flag defGhcFlag "dtag-inference-checks"
        (NoArg (setGeneralFlag Opt_DoTagInferenceChecks))
  , make_ord_flag defGhcFlag "dshow-passes"
        (NoArg $ forceRecompile >> (setVerbosity $ Just 2))
  , make_ord_flag defGhcFlag "dipe-stats"
        (setDumpFlag Opt_D_ipe_stats)
  , make_ord_flag defGhcFlag "dfaststring-stats"
        (setDumpFlag Opt_D_faststring_stats)
  , make_ord_flag defGhcFlag "dno-llvm-mangler"
        (NoArg (setGeneralFlag Opt_NoLlvmMangler)) -- hidden flag
  , make_ord_flag defGhcFlag "dno-typeable-binds"
        (NoArg (setGeneralFlag Opt_NoTypeableBinds))
  , make_ord_flag defGhcFlag "ddump-debug"
        (setDumpFlag Opt_D_dump_debug)
  , make_dep_flag defGhcFlag "ddump-json"
        (setDumpFlag Opt_D_dump_json)
        "Use `-fdiagnostics-as-json` instead"
  , make_ord_flag defGhcFlag "dppr-debug"
        (setDumpFlag Opt_D_ppr_debug)
  , make_ord_flag defGhcFlag "ddebug-output"
        (noArg (flip dopt_unset Opt_D_no_debug_output))
  , make_ord_flag defGhcFlag "dno-debug-output"
        (setDumpFlag Opt_D_no_debug_output)
  , make_ord_flag defGhcFlag "ddump-faststrings"
        (setDumpFlag Opt_D_dump_faststrings)

        ------ Machine dependent (-m<blah>) stuff ---------------------------

  , make_ord_flag defGhcFlag "msse"         (noArg (\d ->
                                                  d { sseVersion = Just SSE1 }))
  , make_ord_flag defGhcFlag "msse2"        (noArg (\d ->
                                                  d { sseVersion = Just SSE2 }))
  , make_ord_flag defGhcFlag "msse3"        (noArg (\d ->
                                                  d { sseVersion = Just SSE3 }))
  , make_ord_flag defGhcFlag "msse4"        (noArg (\d ->
                                                  d { sseVersion = Just SSE4 }))
  , make_ord_flag defGhcFlag "msse4.2"      (noArg (\d ->
                                                 d { sseVersion = Just SSE42 }))
  , make_ord_flag defGhcFlag "mbmi"         (noArg (\d ->
                                                 d { bmiVersion = Just BMI1 }))
  , make_ord_flag defGhcFlag "mbmi2"        (noArg (\d ->
                                                 d { bmiVersion = Just BMI2 }))
  , make_ord_flag defGhcFlag "mavx"         (noArg (\d -> d { avx = True }))
  , make_ord_flag defGhcFlag "mavx2"        (noArg (\d -> d { avx2 = True }))
  , make_ord_flag defGhcFlag "mavx512cd"    (noArg (\d ->
                                                         d { avx512cd = True }))
  , make_ord_flag defGhcFlag "mavx512er"    (noArg (\d ->
                                                         d { avx512er = True }))
  , make_ord_flag defGhcFlag "mavx512f"     (noArg (\d -> d { avx512f = True }))
  , make_ord_flag defGhcFlag "mavx512pf"    (noArg (\d ->
                                                         d { avx512pf = True }))
  , make_ord_flag defGhcFlag "mfma"         (noArg (\d -> d { fma = True }))

        ------ Plugin flags ------------------------------------------------
  , make_ord_flag defGhcFlag "fplugin-opt" (hasArg addPluginModuleNameOption)
  , make_ord_flag defGhcFlag "fplugin-trustworthy"
      (NoArg (setGeneralFlag Opt_PluginTrustworthy))
  , make_ord_flag defGhcFlag "fplugin"     (hasArg addPluginModuleName)
  , make_ord_flag defGhcFlag "fclear-plugins" (noArg clearPluginModuleNames)
  , make_ord_flag defGhcFlag "ffrontend-opt" (hasArg addFrontendPluginOption)

  , make_ord_flag defGhcFlag "fplugin-library" (hasArg addExternalPlugin)

        ------ Optimisation flags ------------------------------------------
  , make_dep_flag defGhcFlag "Onot"   (noArgM $ setOptLevel 0 )
                                                            "Use -O0 instead"
  , make_ord_flag defGhcFlag "O"      (optIntSuffixM (\mb_n ->
                                                setOptLevel (mb_n `orElse` 1)))
                -- If the number is missing, use 1

  , make_ord_flag defFlag "fbinary-blob-threshold"
      (intSuffix (\n d -> d { binBlobThreshold = case fromIntegral n of
                                                    0 -> Nothing
                                                    x -> Just x}))
  , make_ord_flag defFlag "fmax-relevant-binds"
      (intSuffix (\n d -> d { maxRelevantBinds = Just n }))
  , make_ord_flag defFlag "fno-max-relevant-binds"
      (noArg (\d -> d { maxRelevantBinds = Nothing }))

  , make_ord_flag defFlag "fmax-valid-hole-fits"
      (intSuffix (\n d -> d { maxValidHoleFits = Just n }))
  , make_ord_flag defFlag "fno-max-valid-hole-fits"
      (noArg (\d -> d { maxValidHoleFits = Nothing }))
  , make_ord_flag defFlag "fmax-refinement-hole-fits"
      (intSuffix (\n d -> d { maxRefHoleFits = Just n }))
  , make_ord_flag defFlag "fno-max-refinement-hole-fits"
      (noArg (\d -> d { maxRefHoleFits = Nothing }))
  , make_ord_flag defFlag "frefinement-level-hole-fits"
      (intSuffix (\n d -> d { refLevelHoleFits = Just n }))
  , make_ord_flag defFlag "fno-refinement-level-hole-fits"
      (noArg (\d -> d { refLevelHoleFits = Nothing }))

  , make_ord_flag defFlag "fwrite-if-compression"
      (intSuffix (\n d -> d { ifCompression = n }))

  , make_dep_flag defGhcFlag "fllvm-pass-vectors-in-regs"
            (noArg id)
            "vectors registers are now passed in registers by default."
  , make_ord_flag defFlag "fmax-uncovered-patterns"
      (intSuffix (\n d -> d { maxUncoveredPatterns = n }))
  , make_ord_flag defFlag "fmax-pmcheck-models"
      (intSuffix (\n d -> d { maxPmCheckModels = n }))
  , make_ord_flag defFlag "fsimplifier-phases"
      (intSuffix (\n d -> d { simplPhases = n }))
  , make_ord_flag defFlag "fmax-simplifier-iterations"
      (intSuffix (\n d -> d { maxSimplIterations = n }))
  , (Deprecated, defFlag "fmax-pmcheck-iterations"
      (intSuffixM (\_ d ->
       do { deprecate $ "use -fmax-pmcheck-models instead"
          ; return d })))
  , make_ord_flag defFlag "fsimpl-tick-factor"
      (intSuffix (\n d -> d { simplTickFactor = n }))
  , make_ord_flag defFlag "fdmd-unbox-width"
      (intSuffix (\n d -> d { dmdUnboxWidth = n }))
  , make_ord_flag defFlag "fspec-constr-threshold"
      (intSuffix (\n d -> d { specConstrThreshold = Just n }))
  , make_ord_flag defFlag "fno-spec-constr-threshold"
      (noArg (\d -> d { specConstrThreshold = Nothing }))
  , make_ord_flag defFlag "fspec-constr-count"
      (intSuffix (\n d -> d { specConstrCount = Just n }))
  , make_ord_flag defFlag "fno-spec-constr-count"
      (noArg (\d -> d { specConstrCount = Nothing }))
  , make_ord_flag defFlag "fspec-constr-recursive"
      (intSuffix (\n d -> d { specConstrRecursive = n }))
  , make_ord_flag defFlag "fliberate-case-threshold"
      (intSuffix (\n d -> d { liberateCaseThreshold = Just n }))
  , make_ord_flag defFlag "fno-liberate-case-threshold"
      (noArg (\d -> d { liberateCaseThreshold = Nothing }))
  , make_ord_flag defFlag "drule-check"
      (sepArg (\s d -> d { ruleCheck = Just s }))
  , make_ord_flag defFlag "dinline-check"
      (sepArg (\s d -> d { unfoldingOpts = updateReportPrefix (Just s) (unfoldingOpts d)}))
  , make_ord_flag defFlag "freduction-depth"
      (intSuffix (\n d -> d { reductionDepth = treatZeroAsInf n }))
  , make_ord_flag defFlag "fconstraint-solver-iterations"
      (intSuffix (\n d -> d { solverIterations = treatZeroAsInf n }))
  , make_ord_flag defFlag "fgivens-expansion-fuel"
      (intSuffix (\n d -> d { givensFuel = n }))
  , make_ord_flag defFlag "fwanteds-expansion-fuel"
      (intSuffix (\n d -> d { wantedsFuel = n }))
  , make_ord_flag defFlag "fqcs-expansion-fuel"
      (intSuffix (\n d -> d { qcsFuel = n }))
  , (Deprecated, defFlag "fcontext-stack"
      (intSuffixM (\n d ->
       do { deprecate $ "use -freduction-depth=" ++ show n ++ " instead"
          ; return $ d { reductionDepth = treatZeroAsInf n } })))
  , (Deprecated, defFlag "ftype-function-depth"
      (intSuffixM (\n d ->
       do { deprecate $ "use -freduction-depth=" ++ show n ++ " instead"
          ; return $ d { reductionDepth = treatZeroAsInf n } })))
  , make_ord_flag defFlag "fstrictness-before"
      (intSuffix (\n d -> d { strictnessBefore = n : strictnessBefore d }))
  , make_ord_flag defFlag "ffloat-lam-args"
      (intSuffix (\n d -> d { floatLamArgs = Just n }))
  , make_ord_flag defFlag "ffloat-all-lams"
      (noArg (\d -> d { floatLamArgs = Nothing }))
  , make_ord_flag defFlag "fstg-lift-lams-rec-args"
      (intSuffix (\n d -> d { liftLamsRecArgs = Just n }))
  , make_ord_flag defFlag "fstg-lift-lams-rec-args-any"
      (noArg (\d -> d { liftLamsRecArgs = Nothing }))
  , make_ord_flag defFlag "fstg-lift-lams-non-rec-args"
      (intSuffix (\n d -> d { liftLamsNonRecArgs = Just n }))
  , make_ord_flag defFlag "fstg-lift-lams-non-rec-args-any"
      (noArg (\d -> d { liftLamsNonRecArgs = Nothing }))
  , make_ord_flag defFlag "fstg-lift-lams-known"
      (noArg (\d -> d { liftLamsKnown = True }))
  , make_ord_flag defFlag "fno-stg-lift-lams-known"
      (noArg (\d -> d { liftLamsKnown = False }))
  , make_ord_flag defFlag "fproc-alignment"
      (intSuffix (\n d -> d { cmmProcAlignment = Just n }))
  , make_ord_flag defFlag "fblock-layout-weights"
        (HasArg (\s ->
            upd (\d -> d { cfgWeights =
                parseWeights s (cfgWeights d)})))
  , make_ord_flag defFlag "fhistory-size"
      (intSuffix (\n d -> d { historySize = n }))

  , make_ord_flag defFlag "funfolding-creation-threshold"
      (intSuffix   (\n d -> d { unfoldingOpts = updateCreationThreshold n (unfoldingOpts d)}))
  , make_ord_flag defFlag "funfolding-use-threshold"
      (intSuffix   (\n d -> d { unfoldingOpts = updateUseThreshold n (unfoldingOpts d)}))
  , make_ord_flag defFlag "funfolding-fun-discount"
      (intSuffix   (\n d -> d { unfoldingOpts = updateFunAppDiscount n (unfoldingOpts d)}))
  , make_ord_flag defFlag "funfolding-dict-discount"
      (intSuffix   (\n d -> d { unfoldingOpts = updateDictDiscount n (unfoldingOpts d)}))

  , make_ord_flag defFlag "funfolding-case-threshold"
      (intSuffix   (\n d -> d { unfoldingOpts = updateCaseThreshold n (unfoldingOpts d)}))
  , make_ord_flag defFlag "funfolding-case-scaling"
      (intSuffix   (\n d -> d { unfoldingOpts = updateCaseScaling n (unfoldingOpts d)}))

  , make_dep_flag defFlag "funfolding-keeness-factor"
      (floatSuffix (\_ d -> d))
      "-funfolding-keeness-factor is no longer respected as of GHC 9.0"

  , make_ord_flag defFlag "fmax-worker-args"
      (intSuffix (\n d -> d {maxWorkerArgs = n}))
  , make_ord_flag defFlag "fmax-forced-spec-args"
      (intSuffix (\n d -> d {maxForcedSpecArgs = n}))
  , make_ord_flag defGhciFlag "fghci-hist-size"
      (intSuffix (\n d -> d {ghciHistSize = n}))
  , make_ord_flag defGhcFlag "fmax-inline-alloc-size"
      (intSuffix (\n d -> d { maxInlineAllocSize = n }))
  , make_ord_flag defGhcFlag "fmax-inline-memcpy-insns"
      (intSuffix (\n d -> d { maxInlineMemcpyInsns = n }))
  , make_ord_flag defGhcFlag "fmax-inline-memset-insns"
      (intSuffix (\n d -> d { maxInlineMemsetInsns = n }))
  , make_ord_flag defGhcFlag "dinitial-unique"
      (word64Suffix (\n d -> d { initialUnique = n }))
  , make_ord_flag defGhcFlag "dunique-increment"
      (intSuffix (\n d -> d { uniqueIncrement = n }))

        ------ Profiling ----------------------------------------------------

        -- OLD profiling flags
  , make_dep_flag defGhcFlag "auto-all"
                    (noArg (\d -> d { profAuto = ProfAutoAll } ))
                    "Use -fprof-auto instead"
  , make_dep_flag defGhcFlag "no-auto-all"
                    (noArg (\d -> d { profAuto = NoProfAuto } ))
                    "Use -fno-prof-auto instead"
  , make_dep_flag defGhcFlag "auto"
                    (noArg (\d -> d { profAuto = ProfAutoExports } ))
                    "Use -fprof-auto-exported instead"
  , make_dep_flag defGhcFlag "no-auto"
            (noArg (\d -> d { profAuto = NoProfAuto } ))
                    "Use -fno-prof-auto instead"
  , make_dep_flag defGhcFlag "caf-all"
            (NoArg (setGeneralFlag Opt_AutoSccsOnIndividualCafs))
                    "Use -fprof-cafs instead"
  , make_dep_flag defGhcFlag "no-caf-all"
            (NoArg (unSetGeneralFlag Opt_AutoSccsOnIndividualCafs))
                    "Use -fno-prof-cafs instead"

        -- NEW profiling flags
  , make_ord_flag defGhcFlag "fprof-auto"
      (noArg (\d -> d { profAuto = ProfAutoAll } ))
  , make_ord_flag defGhcFlag "fprof-auto-top"
      (noArg (\d -> d { profAuto = ProfAutoTop } ))
  , make_ord_flag defGhcFlag "fprof-auto-exported"
      (noArg (\d -> d { profAuto = ProfAutoExports } ))
  , make_ord_flag defGhcFlag "fprof-auto-calls"
      (noArg (\d -> d { profAuto = ProfAutoCalls } ))
  , make_ord_flag defGhcFlag "fno-prof-auto"
      (noArg (\d -> d { profAuto = NoProfAuto } ))

        -- Caller-CC
  , make_ord_flag defGhcFlag "fprof-callers"
         (HasArg setCallerCcFilters)
        ------ Compiler flags -----------------------------------------------

  , make_ord_flag defGhcFlag "fasm"             (NoArg (setObjBackend ncgBackend))
  , make_ord_flag defGhcFlag "fvia-c"           (NoArg
         (deprecate $ "The -fvia-c flag does nothing; " ++
                      "it will be removed in a future GHC release"))
  , make_ord_flag defGhcFlag "fvia-C"           (NoArg
         (deprecate $ "The -fvia-C flag does nothing; " ++
                      "it will be removed in a future GHC release"))
  , make_ord_flag defGhcFlag "fllvm"            (NoArg (setObjBackend llvmBackend))

  , make_ord_flag defFlag "fno-code"         (NoArg ((upd $ \d ->
                  d { ghcLink=NoLink }) >> setBackend noBackend))
  , make_ord_flag defFlag "fbyte-code"
      (noArgM $ \dflags -> do
        setBackend interpreterBackend
        pure $ flip gopt_unset Opt_ByteCodeAndObjectCode (gopt_set dflags Opt_ByteCode))
  , make_ord_flag defFlag "fobject-code"     $ noArgM $ \dflags -> do
      setBackend $ platformDefaultBackend (targetPlatform dflags)
      dflags' <- liftEwM getCmdLineState
      pure $ gopt_unset dflags' Opt_ByteCodeAndObjectCode

  , make_dep_flag defFlag "fglasgow-exts"
      (NoArg enableGlasgowExts) "Use individual extensions instead"
  , make_dep_flag defFlag "fno-glasgow-exts"
      (NoArg disableGlasgowExts) "Use individual extensions instead"

        ------ Safe Haskell flags -------------------------------------------
  , make_ord_flag defFlag "fpackage-trust"   (NoArg setPackageTrust)
  , make_ord_flag defFlag "fno-safe-infer"   (noArg (\d ->
                                                    d { safeInfer = False }))
  , make_ord_flag defFlag "fno-safe-haskell" (NoArg (setSafeHaskell Sf_Ignore))

        ------ position independent flags  ----------------------------------
  , make_ord_flag defGhcFlag "fPIC"          (NoArg (setGeneralFlag Opt_PIC))
  , make_ord_flag defGhcFlag "fno-PIC"       (NoArg (unSetGeneralFlag Opt_PIC))
  , make_ord_flag defGhcFlag "fPIE"          (NoArg (setGeneralFlag Opt_PIE))
  , make_ord_flag defGhcFlag "fno-PIE"       (NoArg (unSetGeneralFlag Opt_PIE))

         ------ Debugging flags ----------------------------------------------
  , make_ord_flag defGhcFlag "g"             (OptIntSuffix setDebugLevel)
 ]
 ++ map (mkFlag turnOn  ""          setGeneralFlag    ) negatableFlagsDeps
 ++ map (mkFlag turnOff "no-"       unSetGeneralFlag  ) negatableFlagsDeps
 ++ map (mkFlag turnOn  "d"         setGeneralFlag    ) dFlagsDeps
 ++ map (mkFlag turnOff "dno-"      unSetGeneralFlag  ) dFlagsDeps
 ++ map (mkFlag turnOn  "f"         setGeneralFlag    ) fFlagsDeps
 ++ map (mkFlag turnOff "fno-"      unSetGeneralFlag  ) fFlagsDeps
 ++

        ------ Warning flags -------------------------------------------------
  [ make_ord_flag defFlag "W"       (NoArg (setWarningGroup W_extra))
  , make_ord_flag defFlag "Werror"
               (NoArg (do { setGeneralFlag Opt_WarnIsError
                          ; setFatalWarningGroup W_everything }))
  , make_ord_flag defFlag "Wwarn"
               (NoArg (do { unSetGeneralFlag Opt_WarnIsError
                          ; unSetFatalWarningGroup W_everything }))
                          -- Opt_WarnIsError is still needed to pass -Werror
                          -- to CPP; see runCpp in SysTools
  , make_dep_flag defFlag "Wnot"    (NoArg (unSetWarningGroup W_everything))
                                             "Use -w or -Wno-everything instead"
  , make_ord_flag defFlag "w"       (NoArg (unSetWarningGroup W_everything))
  ]

     -- New-style uniform warning sets
     --
     -- Note that -Weverything > -Wall > -Wextra > -Wdefault > -Wno-everything
 ++ warningControls setWarningGroup unSetWarningGroup setWErrorWarningGroup unSetFatalWarningGroup warningGroupsDeps
 ++ warningControls setWarningFlag unSetWarningFlag setWErrorFlag unSetFatalWarningFlag wWarningFlagsDeps
 ++ warningControls setCustomWarningFlag unSetCustomWarningFlag setCustomWErrorFlag unSetCustomFatalWarningFlag
      [(NotDeprecated, FlagSpec "warnings-deprecations" defaultWarningCategory nop AllModes)]
      -- See Note [Warning categories] in GHC.Unit.Module.Warnings.

 ++ [ (NotDeprecated, customOrUnrecognisedWarning "Wno-"       unSetCustomWarningFlag)
    , (NotDeprecated, customOrUnrecognisedWarning "Werror="    setCustomWErrorFlag)
    , (NotDeprecated, customOrUnrecognisedWarning "Wwarn="     unSetCustomFatalWarningFlag)
    , (NotDeprecated, customOrUnrecognisedWarning "Wno-error=" unSetCustomFatalWarningFlag)
    , (NotDeprecated, customOrUnrecognisedWarning "W"          setCustomWarningFlag)
    , (Deprecated,    customOrUnrecognisedWarning "fwarn-"     setCustomWarningFlag)
    , (Deprecated,    customOrUnrecognisedWarning "fno-warn-"  unSetCustomWarningFlag)
    ]

     ------ JavaScript flags -----------------------------------------------
 ++ [ make_ord_flag defFlag "ddisable-js-minifier" (NoArg (setGeneralFlag Opt_DisableJsMinifier))
    , make_ord_flag defFlag "ddisable-js-c-sources" (NoArg (setGeneralFlag Opt_DisableJsCsources))
    ]

     ------ Language flags -------------------------------------------------
 ++ map (mkFlag turnOn  "f"         setExtensionFlag  ) fLangFlagsDeps
 ++ map (mkFlag turnOff "fno-"      unSetExtensionFlag) fLangFlagsDeps
 ++ map (mkFlag turnOn  "X"         setExtensionFlag  ) xFlagsDeps
 ++ map (mkFlag turnOff "XNo"       unSetExtensionFlag) xFlagsDeps
 ++ map (mkFlag turnOn  "X"         setLanguage       ) languageFlagsDeps
 ++ map (mkFlag turnOn  "X"         setSafeHaskell    ) safeHaskellFlagsDeps

-- | Warnings have both new-style flags to control their state (@-W@, @-Wno-@,
-- @-Werror=@, @-Wwarn=@) and old-style flags (@-fwarn-@, @-fno-warn-@).  We
-- define these uniformly for individual warning flags and groups of warnings.
warningControls :: (warn_flag -> DynP ()) -- ^ Set the warning
                -> (warn_flag -> DynP ()) -- ^ Unset the warning
                -> (warn_flag -> DynP ()) -- ^ Make the warning an error
                -> (warn_flag -> DynP ()) -- ^ Clear the error status
                -> [(Deprecation, FlagSpec warn_flag)]
                -> [(Deprecation, Flag (CmdLineP DynFlags))]
warningControls set unset set_werror unset_fatal xs =
    map (mkFlag turnOn  "W"          set             ) xs
 ++ map (mkFlag turnOff "Wno-"       unset           ) xs
 ++ map (mkFlag turnOn  "Werror="    set_werror      ) xs
 ++ map (mkFlag turnOn  "Wwarn="     unset_fatal     ) xs
 ++ map (mkFlag turnOn  "Wno-error=" unset_fatal     ) xs
 ++ map (mkFlag turnOn  "fwarn-"     set   . hideFlag) xs
 ++ map (mkFlag turnOff "fno-warn-"  unset . hideFlag) xs

-- | This is where we handle unrecognised warning flags. If the flag is valid as
-- an extended warning category, we call the supplied action. Otherwise, issue a
-- warning if -Wunrecognised-warning-flags is set. See #11429 for context.
-- See Note [Warning categories] in GHC.Unit.Module.Warnings.
customOrUnrecognisedWarning :: String -> (WarningCategory -> DynP ()) -> Flag (CmdLineP DynFlags)
customOrUnrecognisedWarning prefix custom = defHiddenFlag prefix (Prefix action)
  where
    action :: String -> DynP ()
    action flag
      | validWarningCategory cat = custom cat
      | otherwise = unrecognised flag
      where
        cat = mkWarningCategory (mkFastString flag)

    unrecognised flag = do
      -- #23402 and #12056
      -- for unrecognised flags we consider current dynflags, not the final one.
      -- But if final state says to not report unrecognised flags, they won't anyway.
      f <- wopt Opt_WarnUnrecognisedWarningFlags <$> liftEwM getCmdLineState
      when f $ addFlagWarn (DriverUnrecognisedFlag (prefix ++ flag))

-- See Note [Supporting CLI completion]
package_flags_deps :: [(Deprecation, Flag (CmdLineP DynFlags))]
package_flags_deps = [
        ------- Packages ----------------------------------------------------
    make_ord_flag defFlag "package-db"
      (HasArg (addPkgDbRef . PkgDbPath))
  , make_ord_flag defFlag "clear-package-db"      (NoArg clearPkgDb)
  , make_ord_flag defFlag "no-global-package-db"  (NoArg removeGlobalPkgDb)
  , make_ord_flag defFlag "no-user-package-db"    (NoArg removeUserPkgDb)
  , make_ord_flag defFlag "global-package-db"
      (NoArg (addPkgDbRef GlobalPkgDb))
  , make_ord_flag defFlag "user-package-db"
      (NoArg (addPkgDbRef UserPkgDb))
    -- backwards compat with GHC<=7.4 :
  , make_dep_flag defFlag "package-conf"
      (HasArg $ addPkgDbRef . PkgDbPath) "Use -package-db instead"
  , make_dep_flag defFlag "no-user-package-conf"
      (NoArg removeUserPkgDb)              "Use -no-user-package-db instead"
  , make_ord_flag defGhcFlag "package-name"       (HasArg $ \name ->
                                      upd (setUnitId name))
  , make_ord_flag defGhcFlag "this-unit-id"       (hasArg setUnitId)

  , make_ord_flag defGhcFlag "working-dir"       (hasArg setWorkingDirectory)
  , make_ord_flag defGhcFlag "this-package-name"  (hasArg setPackageName)
  , make_ord_flag defGhcFlag "hidden-module"      (HasArg addHiddenModule)
  , make_ord_flag defGhcFlag "reexported-module"  (HasArg addReexportedModule)

  , make_ord_flag defFlag "package"               (HasArg exposePackage)
  , make_ord_flag defFlag "plugin-package-id"     (HasArg exposePluginPackageId)
  , make_ord_flag defFlag "plugin-package"        (HasArg exposePluginPackage)
  , make_ord_flag defFlag "package-id"            (HasArg exposePackageId)
  , make_ord_flag defFlag "hide-package"          (HasArg hidePackage)
  , make_ord_flag defFlag "hide-all-packages"
      (NoArg (setGeneralFlag Opt_HideAllPackages))
  , make_ord_flag defFlag "hide-all-plugin-packages"
      (NoArg (setGeneralFlag Opt_HideAllPluginPackages))
  , make_ord_flag defFlag "package-env"           (HasArg setPackageEnv)
  , make_ord_flag defFlag "ignore-package"        (HasArg ignorePackage)
  , make_dep_flag defFlag "syslib" (HasArg exposePackage) "Use -package instead"
  , make_ord_flag defFlag "distrust-all-packages"
      (NoArg (setGeneralFlag Opt_DistrustAllPackages))
  , make_ord_flag defFlag "trust"                 (HasArg trustPackage)
  , make_ord_flag defFlag "distrust"              (HasArg distrustPackage)
  ]
  where
    setPackageEnv env = upd $ \s -> s { packageEnv = Just env }

-- | Make a list of flags for shell completion.
-- Filter all available flags into two groups, for interactive GHC vs all other.
flagsForCompletion :: Bool -> [String]
flagsForCompletion isInteractive
    = [ '-':flagName flag
      | flag <- flagsAll
      , modeFilter (flagGhcMode flag)
      ]
    where
      modeFilter AllModes = True
      modeFilter OnlyGhci = isInteractive
      modeFilter OnlyGhc = not isInteractive
      modeFilter HiddenFlag = False

data FlagSpec flag
   = FlagSpec
       { flagSpecName :: String   -- ^ Flag in string form
       , flagSpecFlag :: flag     -- ^ Flag in internal form
       , flagSpecAction :: (TurnOnFlag -> DynP ())
           -- ^ Extra action to run when the flag is found
           -- Typically, emit a warning or error
       , flagSpecGhcMode :: GhcFlagMode
           -- ^ In which ghc mode the flag has effect
       }

-- | Define a new flag.
flagSpec :: String -> flag -> (Deprecation, FlagSpec flag)
flagSpec name flag = flagSpec' name flag nop

-- | Define a new flag with an effect.
flagSpec' :: String -> flag -> (TurnOnFlag -> DynP ())
          -> (Deprecation, FlagSpec flag)
flagSpec' name flag act = (NotDeprecated, FlagSpec name flag act AllModes)

-- | Define a warning flag.
warnSpec :: WarningFlag -> [(Deprecation, FlagSpec WarningFlag)]
warnSpec flag = warnSpec' flag nop

-- | Define a warning flag with an effect.
warnSpec' :: WarningFlag -> (TurnOnFlag -> DynP ())
          -> [(Deprecation, FlagSpec WarningFlag)]
warnSpec' flag act = [ (NotDeprecated, FlagSpec name flag act AllModes)
                     | name <- NE.toList (warnFlagNames flag)
                     ]

-- | Define a new deprecated flag with an effect.
depFlagSpecOp :: String -> flag -> (TurnOnFlag -> DynP ()) -> String
            -> (Deprecation, FlagSpec flag)
depFlagSpecOp name flag act dep =
    (Deprecated, snd (flagSpec' name flag (\f -> act f >> deprecate dep)))

-- | Define a new deprecated flag.
depFlagSpec :: String -> flag -> String
            -> (Deprecation, FlagSpec flag)
depFlagSpec name flag dep = depFlagSpecOp name flag nop dep

-- | Define a deprecated warning flag.
depWarnSpec :: WarningFlag -> String
            -> [(Deprecation, FlagSpec WarningFlag)]
depWarnSpec flag dep = [ depFlagSpecOp name flag nop dep
                       | name <- NE.toList (warnFlagNames flag)
                       ]

-- | Define a deprecated warning name substituted by another.
subWarnSpec :: String -> WarningFlag -> String
            -> [(Deprecation, FlagSpec WarningFlag)]
subWarnSpec oldname flag dep = [ depFlagSpecOp oldname flag nop dep ]


-- | Define a new deprecated flag with an effect where the deprecation message
-- depends on the flag value
depFlagSpecOp' :: String
             -> flag
             -> (TurnOnFlag -> DynP ())
             -> (TurnOnFlag -> String)
             -> (Deprecation, FlagSpec flag)
depFlagSpecOp' name flag act dep =
    (Deprecated, FlagSpec name flag (\f -> act f >> (deprecate $ dep f))
                                                                       AllModes)

-- | Define a new deprecated flag where the deprecation message
-- depends on the flag value
depFlagSpec' :: String
             -> flag
             -> (TurnOnFlag -> String)
             -> (Deprecation, FlagSpec flag)
depFlagSpec' name flag dep = depFlagSpecOp' name flag nop dep

-- | Define a new flag for GHCi.
flagGhciSpec :: String -> flag -> (Deprecation, FlagSpec flag)
flagGhciSpec name flag = flagGhciSpec' name flag nop

-- | Define a new flag for GHCi with an effect.
flagGhciSpec' :: String -> flag -> (TurnOnFlag -> DynP ())
              -> (Deprecation, FlagSpec flag)
flagGhciSpec' name flag act = (NotDeprecated, FlagSpec name flag act OnlyGhci)

-- | Define a new flag invisible to CLI completion.
flagHiddenSpec :: String -> flag -> (Deprecation, FlagSpec flag)
flagHiddenSpec name flag = flagHiddenSpec' name flag nop

-- | Define a new flag invisible to CLI completion with an effect.
flagHiddenSpec' :: String -> flag -> (TurnOnFlag -> DynP ())
                -> (Deprecation, FlagSpec flag)
flagHiddenSpec' name flag act = (NotDeprecated, FlagSpec name flag act
                                                                     HiddenFlag)

-- | Hide a 'FlagSpec' from being displayed in @--show-options@.
--
-- This is for example useful for flags that are obsolete, but should not
-- (yet) be deprecated for compatibility reasons.
hideFlag :: (Deprecation, FlagSpec a) -> (Deprecation, FlagSpec a)
hideFlag (dep, fs) = (dep, fs { flagSpecGhcMode = HiddenFlag })

mkFlag :: TurnOnFlag            -- ^ True <=> it should be turned on
       -> String                -- ^ The flag prefix
       -> (flag -> DynP ())     -- ^ What to do when the flag is found
       -> (Deprecation, FlagSpec flag)  -- ^ Specification of
                                        -- this particular flag
       -> (Deprecation, Flag (CmdLineP DynFlags))
mkFlag turn_on flagPrefix f (dep, (FlagSpec name flag extra_action mode))
    = (dep,
       Flag (flagPrefix ++ name) (NoArg (f flag >> extra_action turn_on)) mode)

deprecate :: String -> DynP ()
deprecate s = do
    arg <- getArg
    addFlagWarn (DriverDeprecatedFlag arg s)

deprecatedForExtension :: String -> TurnOnFlag -> String
deprecatedForExtension lang turn_on
    = "use -X" ++ flag ++
      " or pragma {-# LANGUAGE " ++ flag ++ " #-} instead"
    where
      flag | turn_on   = lang
           | otherwise = "No" ++ lang

deprecatedForExtensions :: [String] -> TurnOnFlag -> String
deprecatedForExtensions [] _ = panic "new extension has not been specified"
deprecatedForExtensions [lang] turn_on = deprecatedForExtension lang turn_on
deprecatedForExtensions langExts turn_on
    = "use " ++ xExt flags ++ " instead"
    where
      flags | turn_on = langExts
            | otherwise = ("No" ++) <$> langExts

      xExt fls = intercalate " and "  $ (\flag -> "-X" ++ flag) <$> fls

useInstead :: String -> String -> TurnOnFlag -> String
useInstead prefix flag turn_on
  = "Use " ++ prefix ++ no ++ flag ++ " instead"
  where
    no = if turn_on then "" else "no-"

nop :: TurnOnFlag -> DynP ()
nop _ = return ()

-- | Find the 'FlagSpec' for a 'WarningFlag'.
flagSpecOf :: WarningFlag -> Maybe (FlagSpec WarningFlag)
flagSpecOf = flip Map.lookup wWarningFlagMap

wWarningFlagMap :: Map.Map WarningFlag (FlagSpec WarningFlag)
wWarningFlagMap = Map.fromListWith (\_ x -> x) $ map (flagSpecFlag &&& id) wWarningFlags

-- | These @-W\<blah\>@ flags can all be reversed with @-Wno-\<blah\>@
wWarningFlags :: [FlagSpec WarningFlag]
wWarningFlags = map snd (sortBy (comparing fst) wWarningFlagsDeps)

wWarningFlagsDeps :: [(Deprecation, FlagSpec WarningFlag)]
wWarningFlagsDeps = [minBound..maxBound] >>= \x -> case x of
-- See Note [Updating flag description in the User's Guide]
-- See Note [Supporting CLI completion]
  Opt_WarnAlternativeLayoutRuleTransitional -> warnSpec x
  Opt_WarnAmbiguousFields -> warnSpec x
  Opt_WarnAutoOrphans -> depWarnSpec x "it has no effect"
  Opt_WarnCPPUndef -> warnSpec x
  Opt_WarnBadlyStagedTypes -> warnSpec x
  Opt_WarnUnbangedStrictPatterns -> warnSpec x
  Opt_WarnDeferredTypeErrors -> warnSpec x
  Opt_WarnDeferredOutOfScopeVariables -> warnSpec x
  Opt_WarnDeprecatedFlags -> warnSpec x
  Opt_WarnDerivingDefaults -> warnSpec x
  Opt_WarnDerivingTypeable -> warnSpec x
  Opt_WarnDodgyExports -> warnSpec x
  Opt_WarnDodgyForeignImports -> warnSpec x
  Opt_WarnDodgyImports -> warnSpec x
  Opt_WarnEmptyEnumerations -> warnSpec x
  Opt_WarnDuplicateConstraints
    -> subWarnSpec "duplicate-constraints" x "it is subsumed by -Wredundant-constraints"
  Opt_WarnRedundantConstraints -> warnSpec x
  Opt_WarnDuplicateExports -> warnSpec x
  Opt_WarnHiShadows
    -> depWarnSpec x "it is not used, and was never implemented"
  Opt_WarnInaccessibleCode -> warnSpec x
  Opt_WarnImplicitPrelude -> warnSpec x
  Opt_WarnImplicitKindVars -> depWarnSpec x "it is now an error"
  Opt_WarnIncompletePatterns -> warnSpec x
  Opt_WarnIncompletePatternsRecUpd -> warnSpec x
  Opt_WarnIncompleteUniPatterns -> warnSpec x
  Opt_WarnInconsistentFlags -> warnSpec x
  Opt_WarnInlineRuleShadowing -> warnSpec x
  Opt_WarnIdentities -> warnSpec x
  Opt_WarnLoopySuperclassSolve -> depWarnSpec x "it is now an error"
  Opt_WarnMissingFields -> warnSpec x
  Opt_WarnMissingImportList -> warnSpec x
  Opt_WarnMissingExportList -> warnSpec x
  Opt_WarnMissingLocalSignatures
    -> subWarnSpec "missing-local-sigs" x
                   "it is replaced by -Wmissing-local-signatures"
       ++ warnSpec x
  Opt_WarnMissingMethods -> warnSpec x
  Opt_WarnMissingMonadFailInstances
    -> depWarnSpec x "fail is no longer a method of Monad"
  Opt_WarnSemigroup -> depWarnSpec x "Semigroup is now a superclass of Monoid"
  Opt_WarnMissingSignatures -> warnSpec x
  Opt_WarnMissingKindSignatures -> warnSpec x
  Opt_WarnMissingPolyKindSignatures -> warnSpec x
  Opt_WarnMissingExportedSignatures
    -> subWarnSpec "missing-exported-sigs" x
                   "it is replaced by -Wmissing-exported-signatures"
       ++ warnSpec x
  Opt_WarnMonomorphism -> warnSpec x
  Opt_WarnNameShadowing -> warnSpec x
  Opt_WarnNonCanonicalMonadInstances -> warnSpec x
  Opt_WarnNonCanonicalMonadFailInstances
    -> depWarnSpec x "fail is no longer a method of Monad"
  Opt_WarnNonCanonicalMonoidInstances -> warnSpec x
  Opt_WarnOrphans -> warnSpec x
  Opt_WarnOverflowedLiterals -> warnSpec x
  Opt_WarnOverlappingPatterns -> warnSpec x
  Opt_WarnMissedSpecs -> warnSpec x
  Opt_WarnAllMissedSpecs -> warnSpec x
  Opt_WarnSafe -> warnSpec' x setWarnSafe
  Opt_WarnTrustworthySafe -> warnSpec x
  Opt_WarnInferredSafeImports -> warnSpec x
  Opt_WarnMissingSafeHaskellMode -> warnSpec x
  Opt_WarnTabs -> warnSpec x
  Opt_WarnTypeDefaults -> warnSpec x
  Opt_WarnTypedHoles -> warnSpec x
  Opt_WarnPartialTypeSignatures -> warnSpec x
  Opt_WarnUnrecognisedPragmas -> warnSpec x
  Opt_WarnMisplacedPragmas -> warnSpec x
  Opt_WarnUnsafe -> warnSpec' x setWarnUnsafe
  Opt_WarnUnsupportedCallingConventions -> warnSpec x
  Opt_WarnUnsupportedLlvmVersion -> warnSpec x
  Opt_WarnMissedExtraSharedLib -> warnSpec x
  Opt_WarnUntickedPromotedConstructors -> warnSpec x
  Opt_WarnUnusedDoBind -> warnSpec x
  Opt_WarnUnusedForalls -> warnSpec x
  Opt_WarnUnusedImports -> warnSpec x
  Opt_WarnUnusedLocalBinds -> warnSpec x
  Opt_WarnUnusedMatches -> warnSpec x
  Opt_WarnUnusedPatternBinds -> warnSpec x
  Opt_WarnUnusedTopBinds -> warnSpec x
  Opt_WarnUnusedTypePatterns -> warnSpec x
  Opt_WarnUnusedRecordWildcards -> warnSpec x
  Opt_WarnRedundantBangPatterns -> warnSpec x
  Opt_WarnRedundantRecordWildcards -> warnSpec x
  Opt_WarnRedundantStrictnessFlags -> warnSpec x
  Opt_WarnWrongDoBind -> warnSpec x
  Opt_WarnMissingPatternSynonymSignatures -> warnSpec x
  Opt_WarnMissingDerivingStrategies -> warnSpec x
  Opt_WarnSimplifiableClassConstraints -> warnSpec x
  Opt_WarnMissingHomeModules -> warnSpec x
  Opt_WarnUnrecognisedWarningFlags -> warnSpec x
  Opt_WarnStarBinder -> warnSpec x
  Opt_WarnStarIsType -> warnSpec x
  Opt_WarnSpaceAfterBang
    -> depWarnSpec x "bang patterns can no longer be written with a space"
  Opt_WarnPartialFields -> warnSpec x
  Opt_WarnPrepositiveQualifiedModule -> warnSpec x
  Opt_WarnUnusedPackages -> warnSpec x
  Opt_WarnCompatUnqualifiedImports ->
    depWarnSpec x "This warning no longer does anything; see GHC #24904"
  Opt_WarnInvalidHaddock -> warnSpec x
  Opt_WarnOperatorWhitespaceExtConflict -> warnSpec x
  Opt_WarnOperatorWhitespace -> warnSpec x
  Opt_WarnImplicitLift -> warnSpec x
  Opt_WarnMissingExportedPatternSynonymSignatures -> warnSpec x
  Opt_WarnForallIdentifier
    -> depWarnSpec x "forall is no longer a valid identifier"
  Opt_WarnUnicodeBidirectionalFormatCharacters -> warnSpec x
  Opt_WarnGADTMonoLocalBinds -> warnSpec x
  Opt_WarnTypeEqualityOutOfScope -> warnSpec x
  Opt_WarnTypeEqualityRequiresOperators -> warnSpec x
  Opt_WarnTermVariableCapture -> warnSpec x
  Opt_WarnMissingRoleAnnotations -> warnSpec x
  Opt_WarnImplicitRhsQuantification -> warnSpec x
  Opt_WarnIncompleteExportWarnings -> warnSpec x
  Opt_WarnIncompleteRecordSelectors -> warnSpec x
  Opt_WarnDataKindsTC -> warnSpec x
  Opt_WarnDeprecatedTypeAbstractions -> warnSpec x
  Opt_WarnDefaultedExceptionContext -> warnSpec x
  Opt_WarnViewPatternSignatures -> warnSpec x
  Opt_WarnAmbiguousMainReturn -> warnSpec x

warningGroupsDeps :: [(Deprecation, FlagSpec WarningGroup)]
warningGroupsDeps = map mk warningGroups
  where
    mk g = (NotDeprecated, FlagSpec (warningGroupName g) g nop AllModes)

-- | These @-\<blah\>@ flags can all be reversed with @-no-\<blah\>@
negatableFlagsDeps :: [(Deprecation, FlagSpec GeneralFlag)]
negatableFlagsDeps = [
  flagGhciSpec "ignore-dot-ghci"         Opt_IgnoreDotGhci ]

-- | These @-d\<blah\>@ flags can all be reversed with @-dno-\<blah\>@
dFlagsDeps :: [(Deprecation, FlagSpec GeneralFlag)]
dFlagsDeps = [
-- See Note [Updating flag description in the User's Guide]
-- See Note [Supporting CLI completion]
-- Please keep the list of flags below sorted alphabetically
  flagSpec "ppr-case-as-let"            Opt_PprCaseAsLet,
  depFlagSpec' "ppr-ticks"              Opt_PprShowTicks
     (\turn_on -> useInstead "-d" "suppress-ticks" (not turn_on)),
  flagSpec "suppress-ticks"             Opt_SuppressTicks,
  depFlagSpec' "suppress-stg-free-vars" Opt_SuppressStgExts
     (useInstead "-d" "suppress-stg-exts"),
  flagSpec "suppress-stg-exts"          Opt_SuppressStgExts,
  flagSpec "suppress-stg-reps"          Opt_SuppressStgReps,
  flagSpec "suppress-coercions"         Opt_SuppressCoercions,
  flagSpec "suppress-coercion-types"    Opt_SuppressCoercionTypes,
  flagSpec "suppress-idinfo"            Opt_SuppressIdInfo,
  flagSpec "suppress-unfoldings"        Opt_SuppressUnfoldings,
  flagSpec "suppress-module-prefixes"   Opt_SuppressModulePrefixes,
  flagSpec "suppress-timestamps"        Opt_SuppressTimestamps,
  flagSpec "suppress-type-applications" Opt_SuppressTypeApplications,
  flagSpec "suppress-type-signatures"   Opt_SuppressTypeSignatures,
  flagSpec "suppress-uniques"           Opt_SuppressUniques,
  flagSpec "suppress-var-kinds"         Opt_SuppressVarKinds,
  flagSpec "suppress-core-sizes"        Opt_SuppressCoreSizes
  ]

-- | These @-f\<blah\>@ flags can all be reversed with @-fno-\<blah\>@
fFlags :: [FlagSpec GeneralFlag]
fFlags = map snd fFlagsDeps

fFlagsDeps :: [(Deprecation, FlagSpec GeneralFlag)]
fFlagsDeps = [
-- See Note [Updating flag description in the User's Guide]
-- See Note [Supporting CLI completion]
-- Please keep the list of flags below sorted alphabetically
  flagSpec "asm-shortcutting"                 Opt_AsmShortcutting,
  flagGhciSpec "break-on-error"               Opt_BreakOnError,
  flagGhciSpec "break-on-exception"           Opt_BreakOnException,
  flagSpec "building-cabal-package"           Opt_BuildingCabalPackage,
  flagSpec "call-arity"                       Opt_CallArity,
  flagSpec "exitification"                    Opt_Exitification,
  flagSpec "case-merge"                       Opt_CaseMerge,
  flagSpec "case-folding"                     Opt_CaseFolding,
  flagSpec "cmm-elim-common-blocks"           Opt_CmmElimCommonBlocks,
  flagSpec "cmm-sink"                         Opt_CmmSink,
  flagSpec "cmm-static-pred"                  Opt_CmmStaticPred,
  flagSpec "cse"                              Opt_CSE,
  flagSpec "stg-cse"                          Opt_StgCSE,
  flagSpec "stg-lift-lams"                    Opt_StgLiftLams,
  flagSpec "cpr-anal"                         Opt_CprAnal,
  flagSpec "defer-diagnostics"                Opt_DeferDiagnostics,
  flagSpec "defer-type-errors"                Opt_DeferTypeErrors,
  flagSpec "defer-typed-holes"                Opt_DeferTypedHoles,
  flagSpec "defer-out-of-scope-variables"     Opt_DeferOutOfScopeVariables,
  flagSpec "diagnostics-show-caret"           Opt_DiagnosticsShowCaret,
  flagSpec "diagnostics-as-json"              Opt_DiagnosticsAsJSON,
  -- With-ways needs to be reversible hence why its made via flagSpec unlike
  -- other debugging flags.
  flagSpec "dump-with-ways"                   Opt_DumpWithWays,
  flagSpec "dicts-cheap"                      Opt_DictsCheap,
  flagSpec "dicts-strict"                     Opt_DictsStrict,
  depFlagSpec "dmd-tx-dict-sel"
      Opt_DmdTxDictSel "effect is now unconditionally enabled",
  flagSpec "do-eta-reduction"                 Opt_DoEtaReduction,
  flagSpec "do-lambda-eta-expansion"          Opt_DoLambdaEtaExpansion,
  flagSpec "do-clever-arg-eta-expansion"      Opt_DoCleverArgEtaExpansion, -- See Note [Eta expansion of arguments in CorePrep]
  flagSpec "eager-blackholing"                Opt_EagerBlackHoling,
  flagSpec "orig-thunk-info"                  Opt_OrigThunkInfo,
  flagSpec "embed-manifest"                   Opt_EmbedManifest,
  flagSpec "enable-rewrite-rules"             Opt_EnableRewriteRules,
  flagSpec "enable-th-splice-warnings"        Opt_EnableThSpliceWarnings,
  flagSpec "error-spans"                      Opt_ErrorSpans,
  flagSpec "excess-precision"                 Opt_ExcessPrecision,
  flagSpec "expose-all-unfoldings"            Opt_ExposeAllUnfoldings,
  flagSpec "expose-overloaded-unfoldings"     Opt_ExposeOverloadedUnfoldings,
  flagSpec "keep-auto-rules"                  Opt_KeepAutoRules,
  flagSpec "expose-internal-symbols"          Opt_ExposeInternalSymbols,
  flagSpec "external-dynamic-refs"            Opt_ExternalDynamicRefs,
  flagSpec "external-interpreter"             Opt_ExternalInterpreter,
  flagSpec "family-application-cache"         Opt_FamAppCache,
  flagSpec "float-in"                         Opt_FloatIn,
  flagSpec "force-recomp"                     Opt_ForceRecomp,
  flagSpec "ignore-optim-changes"             Opt_IgnoreOptimChanges,
  flagSpec "ignore-hpc-changes"               Opt_IgnoreHpcChanges,
  flagSpec "full-laziness"                    Opt_FullLaziness,
  depFlagSpec' "fun-to-thunk"                 Opt_FunToThunk
      (useInstead "-f" "full-laziness"),
  flagSpec "local-float-out"                  Opt_LocalFloatOut,
  flagSpec "local-float-out-top-level"        Opt_LocalFloatOutTopLevel,
  flagSpec "gen-manifest"                     Opt_GenManifest,
  flagSpec "ghci-history"                     Opt_GhciHistory,
  flagSpec "ghci-leak-check"                  Opt_GhciLeakCheck,
  flagSpec "validate-ide-info"                Opt_ValidateHie,
  flagGhciSpec "local-ghci-history"           Opt_LocalGhciHistory,
  flagGhciSpec "no-it"                        Opt_NoIt,
  flagSpec "ghci-sandbox"                     Opt_GhciSandbox,
  flagSpec "helpful-errors"                   Opt_HelpfulErrors,
  flagSpec "hpc"                              Opt_Hpc,
  flagSpec "ignore-asserts"                   Opt_IgnoreAsserts,
  flagSpec "ignore-interface-pragmas"         Opt_IgnoreInterfacePragmas,
  flagGhciSpec "implicit-import-qualified"    Opt_ImplicitImportQualified,
  flagSpec "irrefutable-tuples"               Opt_IrrefutableTuples,
  flagSpec "keep-going"                       Opt_KeepGoing,
  flagSpec "late-dmd-anal"                    Opt_LateDmdAnal,
  flagSpec "late-specialise"                  Opt_LateSpecialise,
  flagSpec "liberate-case"                    Opt_LiberateCase,
  flagHiddenSpec "llvm-fill-undef-with-garbage" Opt_LlvmFillUndefWithGarbage,
  flagSpec "loopification"                    Opt_Loopification,
  flagSpec "block-layout-cfg"                 Opt_CfgBlocklayout,
  flagSpec "block-layout-weightless"          Opt_WeightlessBlocklayout,
  flagSpec "omit-interface-pragmas"           Opt_OmitInterfacePragmas,
  flagSpec "omit-yields"                      Opt_OmitYields,
  flagSpec "optimal-applicative-do"           Opt_OptimalApplicativeDo,
  flagSpec "pedantic-bottoms"                 Opt_PedanticBottoms,
  flagSpec "pre-inlining"                     Opt_SimplPreInlining,
  flagGhciSpec "print-bind-contents"          Opt_PrintBindContents,
  flagGhciSpec "print-bind-result"            Opt_PrintBindResult,
  flagGhciSpec "print-evld-with-show"         Opt_PrintEvldWithShow,
  flagSpec "print-explicit-foralls"           Opt_PrintExplicitForalls,
  flagSpec "print-explicit-kinds"             Opt_PrintExplicitKinds,
  flagSpec "print-explicit-coercions"         Opt_PrintExplicitCoercions,
  flagSpec "print-explicit-runtime-reps"      Opt_PrintExplicitRuntimeReps,
  flagSpec "print-equality-relations"         Opt_PrintEqualityRelations,
  flagSpec "print-axiom-incomps"              Opt_PrintAxiomIncomps,
  flagSpec "print-unicode-syntax"             Opt_PrintUnicodeSyntax,
  flagSpec "print-expanded-synonyms"          Opt_PrintExpandedSynonyms,
  flagSpec "print-potential-instances"        Opt_PrintPotentialInstances,
  flagSpec "print-redundant-promotion-ticks"  Opt_PrintRedundantPromotionTicks,
  flagSpec "print-typechecker-elaboration"    Opt_PrintTypecheckerElaboration,
  flagSpec "prof-cafs"                        Opt_AutoSccsOnIndividualCafs,
  flagSpec "prof-count-entries"               Opt_ProfCountEntries,
  flagSpec "prof-late"                        Opt_ProfLateCcs,
  flagSpec "prof-late-overloaded"             Opt_ProfLateOverloadedCcs,
  flagSpec "prof-late-overloaded-calls"       Opt_ProfLateoverloadedCallsCCs,
  flagSpec "prof-manual"                      Opt_ProfManualCcs,
  flagSpec "prof-late-inline"                 Opt_ProfLateInlineCcs,
  flagSpec "regs-graph"                       Opt_RegsGraph,
  flagSpec "regs-iterative"                   Opt_RegsIterative,
  depFlagSpec' "rewrite-rules"                Opt_EnableRewriteRules
   (useInstead "-f" "enable-rewrite-rules"),
  flagSpec "shared-implib"                    Opt_SharedImplib,
  flagSpec "spec-constr"                      Opt_SpecConstr,
  flagSpec "spec-constr-keen"                 Opt_SpecConstrKeen,
  flagSpec "specialise"                       Opt_Specialise,
  flagSpec "specialize"                       Opt_Specialise,
  flagSpec "specialise-aggressively"          Opt_SpecialiseAggressively,
  flagSpec "specialize-aggressively"          Opt_SpecialiseAggressively,
  flagSpec "cross-module-specialise"          Opt_CrossModuleSpecialise,
  flagSpec "cross-module-specialize"          Opt_CrossModuleSpecialise,
  flagSpec "polymorphic-specialisation"       Opt_PolymorphicSpecialisation,
  flagSpec "specialise-incoherents"           Opt_SpecialiseIncoherents,
  flagSpec "inline-generics"                  Opt_InlineGenerics,
  flagSpec "inline-generics-aggressively"     Opt_InlineGenericsAggressively,
  flagSpec "static-argument-transformation"   Opt_StaticArgumentTransformation,
  flagSpec "strictness"                       Opt_Strictness,
  flagSpec "use-rpaths"                       Opt_RPath,
  flagSpec "write-interface"                  Opt_WriteInterface,
  flagSpec "write-if-simplified-core"         Opt_WriteIfSimplifiedCore,
  flagSpec "write-ide-info"                   Opt_WriteHie,
  flagSpec "unbox-small-strict-fields"        Opt_UnboxSmallStrictFields,
  flagSpec "unbox-strict-fields"              Opt_UnboxStrictFields,
  flagSpec "unoptimized-core-for-interpreter" Opt_UnoptimizedCoreForInterpreter,
  flagSpec "version-macros"                   Opt_VersionMacros,
  flagSpec "worker-wrapper"                   Opt_WorkerWrapper,
  flagSpec "worker-wrapper-cbv"               Opt_WorkerWrapperUnlift, -- See Note [Worker/wrapper for strict arguments]
  flagSpec "solve-constant-dicts"             Opt_SolveConstantDicts,
  flagSpec "catch-nonexhaustive-cases"        Opt_CatchNonexhaustiveCases,
  flagSpec "alignment-sanitisation"           Opt_AlignmentSanitisation,
  flagSpec "check-prim-bounds"                Opt_DoBoundsChecking,
  flagSpec "num-constant-folding"             Opt_NumConstantFolding,
  flagSpec "core-constant-folding"            Opt_CoreConstantFolding,
  flagSpec "fast-pap-calls"                   Opt_FastPAPCalls,
  flagSpec "cmm-control-flow"                 Opt_CmmControlFlow,
  flagSpec "show-warning-groups"              Opt_ShowWarnGroups,
  flagSpec "hide-source-paths"                Opt_HideSourcePaths,
  flagSpec "show-loaded-modules"              Opt_ShowLoadedModules,
  flagSpec "whole-archive-hs-libs"            Opt_WholeArchiveHsLibs,
  flagSpec "keep-cafs"                        Opt_KeepCAFs,
  flagSpec "link-rts"                         Opt_LinkRts,
  flagSpec "byte-code-and-object-code"        Opt_ByteCodeAndObjectCode,
  flagSpec "prefer-byte-code"                 Opt_UseBytecodeRatherThanObjects,
  flagSpec "object-determinism"               Opt_ObjectDeterminism,
  flagSpec' "compact-unwind"                  Opt_CompactUnwind
      (\turn_on -> updM (\dflags -> do
        unless (platformOS (targetPlatform dflags) == OSDarwin && turn_on)
               (addWarn "-compact-unwind is only implemented by the darwin platform. Ignoring.")
        return dflags)),
  flagSpec "show-error-context"               Opt_ShowErrorContext,
  flagSpec "cmm-thread-sanitizer"             Opt_CmmThreadSanitizer,
  flagSpec "split-sections"                   Opt_SplitSections,
  flagSpec "break-points"                     Opt_InsertBreakpoints,
  flagSpec "distinct-constructor-tables"      Opt_DistinctConstructorTables,
  flagSpec "info-table-map"                   Opt_InfoTableMap,
  flagSpec "info-table-map-with-stack"        Opt_InfoTableMapWithStack,
  flagSpec "info-table-map-with-fallback"     Opt_InfoTableMapWithFallback
  ]
  ++ fHoleFlags

-- | These @-f\<blah\>@ flags have to do with the typed-hole error message or
-- the valid hole fits in that message. See Note [Valid hole fits include ...]
-- in the "GHC.Tc.Errors.Hole" module. These flags can all be reversed with
-- @-fno-\<blah\>@
fHoleFlags :: [(Deprecation, FlagSpec GeneralFlag)]
fHoleFlags = [
  flagSpec "show-hole-constraints"            Opt_ShowHoleConstraints,
  depFlagSpec' "show-valid-substitutions"     Opt_ShowValidHoleFits
   (useInstead "-f" "show-valid-hole-fits"),
  flagSpec "show-valid-hole-fits"             Opt_ShowValidHoleFits,
  -- Sorting settings
  flagSpec "sort-valid-hole-fits"             Opt_SortValidHoleFits,
  flagSpec "sort-by-size-hole-fits"           Opt_SortBySizeHoleFits,
  flagSpec "sort-by-subsumption-hole-fits"    Opt_SortBySubsumHoleFits,
  flagSpec "abstract-refinement-hole-fits"    Opt_AbstractRefHoleFits,
  -- Output format settings
  flagSpec "show-hole-matches-of-hole-fits"   Opt_ShowMatchesOfHoleFits,
  flagSpec "show-provenance-of-hole-fits"     Opt_ShowProvOfHoleFits,
  flagSpec "show-type-of-hole-fits"           Opt_ShowTypeOfHoleFits,
  flagSpec "show-type-app-of-hole-fits"       Opt_ShowTypeAppOfHoleFits,
  flagSpec "show-type-app-vars-of-hole-fits"  Opt_ShowTypeAppVarsOfHoleFits,
  flagSpec "show-docs-of-hole-fits"           Opt_ShowDocsOfHoleFits,
  flagSpec "unclutter-valid-hole-fits"        Opt_UnclutterValidHoleFits
  ]

-- | These @-f\<blah\>@ flags can all be reversed with @-fno-\<blah\>@
fLangFlags :: [FlagSpec LangExt.Extension]
fLangFlags = map snd fLangFlagsDeps

fLangFlagsDeps :: [(Deprecation, FlagSpec LangExt.Extension)]
fLangFlagsDeps = [
-- See Note [Updating flag description in the User's Guide]
-- See Note [Supporting CLI completion]
  depFlagSpecOp' "th"                           LangExt.TemplateHaskell
    checkTemplateHaskellOk
    (deprecatedForExtension "TemplateHaskell"),
  depFlagSpec' "fi"                             LangExt.ForeignFunctionInterface
    (deprecatedForExtension "ForeignFunctionInterface"),
  depFlagSpec' "ffi"                            LangExt.ForeignFunctionInterface
    (deprecatedForExtension "ForeignFunctionInterface"),
  depFlagSpec' "arrows"                         LangExt.Arrows
    (deprecatedForExtension "Arrows"),
  depFlagSpec' "implicit-prelude"               LangExt.ImplicitPrelude
    (deprecatedForExtension "ImplicitPrelude"),
  depFlagSpec' "bang-patterns"                  LangExt.BangPatterns
    (deprecatedForExtension "BangPatterns"),
  depFlagSpec' "monomorphism-restriction"       LangExt.MonomorphismRestriction
    (deprecatedForExtension "MonomorphismRestriction"),
  depFlagSpec' "extended-default-rules"         LangExt.ExtendedDefaultRules
    (deprecatedForExtension "ExtendedDefaultRules"),
  depFlagSpec' "implicit-params"                LangExt.ImplicitParams
    (deprecatedForExtension "ImplicitParams"),
  depFlagSpec' "scoped-type-variables"          LangExt.ScopedTypeVariables
    (deprecatedForExtension "ScopedTypeVariables"),
  depFlagSpec' "allow-overlapping-instances"    LangExt.OverlappingInstances
    (deprecatedForExtension "OverlappingInstances"),
  depFlagSpec' "allow-undecidable-instances"    LangExt.UndecidableInstances
    (deprecatedForExtension "UndecidableInstances"),
  depFlagSpec' "allow-incoherent-instances"     LangExt.IncoherentInstances
    (deprecatedForExtension "IncoherentInstances")
  ]

supportedLanguages :: [String]
supportedLanguages = map (flagSpecName . snd) languageFlagsDeps

supportedLanguageOverlays :: [String]
supportedLanguageOverlays = map (flagSpecName . snd) safeHaskellFlagsDeps

supportedExtensions :: ArchOS -> [String]
supportedExtensions (ArchOS arch os) = concatMap toFlagSpecNamePair xFlags
  where
    toFlagSpecNamePair flg
      -- IMPORTANT! Make sure that `ghc --supported-extensions` omits
      -- "TemplateHaskell"/"QuasiQuotes" when it's known not to work out of the
      -- box. See also GHC #11102 and #16331 for more details about
      -- the rationale
      | isAIX, flagSpecFlag flg == LangExt.TemplateHaskell  = [noName]
      | isAIX, flagSpecFlag flg == LangExt.QuasiQuotes      = [noName]
      -- "JavaScriptFFI" is only supported on the JavaScript/Wasm backend
      | notJSOrWasm, flagSpecFlag flg == LangExt.JavaScriptFFI = [noName]
      | otherwise = [name, noName]
      where
        isAIX = os == OSAIX
        notJSOrWasm = not $ arch `elem` [ ArchJavaScript, ArchWasm32 ]
        noName = "No" ++ name
        name = flagSpecName flg

supportedLanguagesAndExtensions :: ArchOS -> [String]
supportedLanguagesAndExtensions arch_os =
    supportedLanguages ++ supportedLanguageOverlays ++ supportedExtensions arch_os

-- | These -X<blah> flags cannot be reversed with -XNo<blah>
languageFlagsDeps :: [(Deprecation, FlagSpec Language)]
languageFlagsDeps = [
  flagSpec "Haskell98"   Haskell98,
  flagSpec "Haskell2010" Haskell2010,
  flagSpec "GHC2021"     GHC2021,
  flagSpec "GHC2024"     GHC2024
  ]

-- | These -X<blah> flags cannot be reversed with -XNo<blah>
-- They are used to place hard requirements on what GHC Haskell language
-- features can be used.
safeHaskellFlagsDeps :: [(Deprecation, FlagSpec SafeHaskellMode)]
safeHaskellFlagsDeps = [mkF Sf_Unsafe, mkF Sf_Trustworthy, mkF Sf_Safe]
    where mkF flag = flagSpec (show flag) flag

-- | These -X<blah> flags can all be reversed with -XNo<blah>
xFlags :: [FlagSpec LangExt.Extension]
xFlags = map snd xFlagsDeps

makeExtensionFlags :: LangExt.Extension -> [(Deprecation, FlagSpec LangExt.Extension)]
makeExtensionFlags ext = [ makeExtensionFlag name depr ext | (depr, name) <- extensionNames ext ]

xFlagsDeps :: [(Deprecation, FlagSpec LangExt.Extension)]
xFlagsDeps = concatMap makeExtensionFlags [minBound .. maxBound]

makeExtensionFlag :: String -> ExtensionDeprecation -> LangExt.Extension -> (Deprecation, FlagSpec LangExt.Extension)
makeExtensionFlag name depr ext = (deprecation depr, spec)
  where effect = extensionEffect ext
        spec = FlagSpec name ext (\f -> effect f >> act f) AllModes
        act = case depr of
                ExtensionNotDeprecated -> nop
                ExtensionDeprecatedFor xs
                  -> deprecate . deprecatedForExtensions (map extensionName xs)
                ExtensionFlagDeprecatedCond cond str
                  -> \f -> when (f == cond) (deprecate str)
                ExtensionFlagDeprecated str
                  -> const (deprecate str)

extensionEffect :: LangExt.Extension -> (TurnOnFlag -> DynP ())
extensionEffect = \case
  LangExt.TemplateHaskell
    -> checkTemplateHaskellOk
  LangExt.OverlappingInstances
    -> setOverlappingInsts
  LangExt.GeneralizedNewtypeDeriving
    -> setGenDeriving
  LangExt.IncoherentInstances
    -> setIncoherentInsts
  LangExt.DerivingVia
    -> setDeriveVia
  _ -> nop

-- | Things you get with `-dlint`.
enableDLint :: DynP ()
enableDLint = do
    mapM_ setGeneralFlag dLintFlags
    addWayDynP WayDebug
  where
    dLintFlags :: [GeneralFlag]
    dLintFlags =
        [ Opt_DoCoreLinting
        , Opt_DoStgLinting
        , Opt_DoCmmLinting
        , Opt_DoAsmLinting
        , Opt_CatchNonexhaustiveCases
        , Opt_LlvmFillUndefWithGarbage
        ]

enableGlasgowExts :: DynP ()
enableGlasgowExts = do setGeneralFlag Opt_PrintExplicitForalls
                       mapM_ setExtensionFlag glasgowExtsFlags

disableGlasgowExts :: DynP ()
disableGlasgowExts = do unSetGeneralFlag Opt_PrintExplicitForalls
                        mapM_ unSetExtensionFlag glasgowExtsFlags


setWarnSafe :: Bool -> DynP ()
setWarnSafe True  = getCurLoc >>= \l -> upd (\d -> d { warnSafeOnLoc = l })
setWarnSafe False = return ()

setWarnUnsafe :: Bool -> DynP ()
setWarnUnsafe True  = getCurLoc >>= \l -> upd (\d -> d { warnUnsafeOnLoc = l })
setWarnUnsafe False = return ()

setPackageTrust :: DynP ()
setPackageTrust = do
    setGeneralFlag Opt_PackageTrust
    l <- getCurLoc
    upd $ \d -> d { pkgTrustOnLoc = l }

setGenDeriving :: TurnOnFlag -> DynP ()
setGenDeriving True  = getCurLoc >>= \l -> upd (\d -> d { newDerivOnLoc = l })
setGenDeriving False = return ()

setDeriveVia :: TurnOnFlag -> DynP ()
setDeriveVia True  = getCurLoc >>= \l -> upd (\d -> d { deriveViaOnLoc = l })
setDeriveVia False = return ()

setOverlappingInsts :: TurnOnFlag -> DynP ()
setOverlappingInsts False = return ()
setOverlappingInsts True = do
  l <- getCurLoc
  upd (\d -> d { overlapInstLoc = l })

setIncoherentInsts :: TurnOnFlag -> DynP ()
setIncoherentInsts False = return ()
setIncoherentInsts True = do
  l <- getCurLoc
  upd (\d -> d { incoherentOnLoc = l })

checkTemplateHaskellOk :: TurnOnFlag -> DynP ()
checkTemplateHaskellOk _turn_on
  = getCurLoc >>= \l -> upd (\d -> d { thOnLoc = l })

{- **********************************************************************
%*                                                                      *
                DynFlags constructors
%*                                                                      *
%********************************************************************* -}

type DynP = EwM (CmdLineP DynFlags)

upd :: (DynFlags -> DynFlags) -> DynP ()
upd f = liftEwM (do dflags <- getCmdLineState
                    putCmdLineState $! f dflags)

updM :: (DynFlags -> DynP DynFlags) -> DynP ()
updM f = do dflags <- liftEwM getCmdLineState
            dflags' <- f dflags
            liftEwM $ putCmdLineState $! dflags'

--------------- Constructor functions for OptKind -----------------
noArg :: (DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
noArg fn = NoArg (upd fn)

noArgM :: (DynFlags -> DynP DynFlags) -> OptKind (CmdLineP DynFlags)
noArgM fn = NoArg (updM fn)

hasArg :: (String -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
hasArg fn = HasArg (upd . fn)

sepArg :: (String -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
sepArg fn = SepArg (upd . fn)

intSuffix :: (Int -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
intSuffix fn = IntSuffix (\n -> upd (fn n))

intSuffixM :: (Int -> DynFlags -> DynP DynFlags) -> OptKind (CmdLineP DynFlags)
intSuffixM fn = IntSuffix (\n -> updM (fn n))

word64Suffix :: (Word64 -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
word64Suffix fn = Word64Suffix (\n -> upd (fn n))

floatSuffix :: (Float -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
floatSuffix fn = FloatSuffix (\n -> upd (fn n))

optIntSuffixM :: (Maybe Int -> DynFlags -> DynP DynFlags)
              -> OptKind (CmdLineP DynFlags)
optIntSuffixM fn = OptIntSuffix (\mi -> updM (fn mi))

setDumpFlag :: DumpFlag -> OptKind (CmdLineP DynFlags)
setDumpFlag dump_flag = NoArg (setDumpFlag' dump_flag)

--------------------------
addWayDynP :: Way -> DynP ()
addWayDynP = upd . addWay'

addWay' :: Way -> DynFlags -> DynFlags
addWay' w dflags0 =
   let platform = targetPlatform dflags0
       dflags1 = dflags0 { targetWays_ = addWay w (targetWays_ dflags0) }
       dflags2 = foldr setGeneralFlag' dflags1
                       (wayGeneralFlags platform w)
       dflags3 = foldr unSetGeneralFlag' dflags2
                       (wayUnsetGeneralFlags platform w)
   in dflags3

removeWayDynP :: Way -> DynP ()
removeWayDynP w = upd (\dfs -> dfs { targetWays_ = removeWay w (targetWays_ dfs) })

--------------------------
setGeneralFlag, unSetGeneralFlag :: GeneralFlag -> DynP ()
setGeneralFlag   f = upd (setGeneralFlag' f)
unSetGeneralFlag f = upd (unSetGeneralFlag' f)

setGeneralFlag' :: GeneralFlag -> DynFlags -> DynFlags
setGeneralFlag' f dflags = foldr ($) (gopt_set dflags f) deps
  where
    deps = [ if turn_on then setGeneralFlag'   d
                        else unSetGeneralFlag' d
           | (f', turn_on, d) <- impliedGFlags, f' == f ]
        -- When you set f, set the ones it implies
        -- NB: use setGeneralFlag recursively, in case the implied flags
        --     implies further flags

unSetGeneralFlag' :: GeneralFlag -> DynFlags -> DynFlags
unSetGeneralFlag' f dflags = foldr ($) (gopt_unset dflags f) deps
  where
    deps = [ if turn_on then setGeneralFlag' d
                        else unSetGeneralFlag' d
           | (f', turn_on, d) <- impliedOffGFlags, f' == f ]
   -- In general, when you un-set f, we don't un-set the things it implies.
   -- There are however some exceptions, e.g., -fno-strictness implies
   -- -fno-worker-wrapper.
   --
   -- NB: use unSetGeneralFlag' recursively, in case the implied off flags
   --     imply further flags.

--------------------------
setWarningGroup :: WarningGroup -> DynP ()
setWarningGroup g = do
    mapM_ setWarningFlag (warningGroupFlags g)
    when (warningGroupIncludesExtendedWarnings g) $ upd wopt_set_all_custom

unSetWarningGroup :: WarningGroup -> DynP ()
unSetWarningGroup g = do
    mapM_ unSetWarningFlag (warningGroupFlags g)
    when (warningGroupIncludesExtendedWarnings g) $ upd wopt_unset_all_custom

setWErrorWarningGroup :: WarningGroup -> DynP ()
setWErrorWarningGroup g =
  do { setWarningGroup g
     ; setFatalWarningGroup g }

setFatalWarningGroup :: WarningGroup -> DynP ()
setFatalWarningGroup g = do
    mapM_ setFatalWarningFlag (warningGroupFlags g)
    when (warningGroupIncludesExtendedWarnings g) $ upd wopt_set_all_fatal_custom

unSetFatalWarningGroup :: WarningGroup -> DynP ()
unSetFatalWarningGroup g = do
    mapM_ unSetFatalWarningFlag (warningGroupFlags g)
    when (warningGroupIncludesExtendedWarnings g) $ upd wopt_unset_all_fatal_custom


setWarningFlag, unSetWarningFlag :: WarningFlag -> DynP ()
setWarningFlag   f = upd (\dfs -> wopt_set dfs f)
unSetWarningFlag f = upd (\dfs -> wopt_unset dfs f)

setFatalWarningFlag, unSetFatalWarningFlag :: WarningFlag -> DynP ()
setFatalWarningFlag   f = upd (\dfs -> wopt_set_fatal dfs f)
unSetFatalWarningFlag f = upd (\dfs -> wopt_unset_fatal dfs f)

setWErrorFlag :: WarningFlag -> DynP ()
setWErrorFlag flag =
  do { setWarningFlag flag
     ; setFatalWarningFlag flag }


setCustomWarningFlag, unSetCustomWarningFlag :: WarningCategory -> DynP ()
setCustomWarningFlag   f = upd (\dfs -> wopt_set_custom dfs f)
unSetCustomWarningFlag f = upd (\dfs -> wopt_unset_custom dfs f)

setCustomFatalWarningFlag, unSetCustomFatalWarningFlag :: WarningCategory -> DynP ()
setCustomFatalWarningFlag   f = upd (\dfs -> wopt_set_fatal_custom dfs f)
unSetCustomFatalWarningFlag f = upd (\dfs -> wopt_unset_fatal_custom dfs f)

setCustomWErrorFlag :: WarningCategory -> DynP ()
setCustomWErrorFlag flag =
  do { setCustomWarningFlag flag
     ; setCustomFatalWarningFlag flag }


--------------------------
setExtensionFlag, unSetExtensionFlag :: LangExt.Extension -> DynP ()
setExtensionFlag f = upd (setExtensionFlag' f)
unSetExtensionFlag f = upd (unSetExtensionFlag' f)

setExtensionFlag', unSetExtensionFlag' :: LangExt.Extension -> DynFlags -> DynFlags
setExtensionFlag' f dflags = foldr ($) (xopt_set dflags f) deps
  where
    deps = [ if turn_on then setExtensionFlag'   d
                        else unSetExtensionFlag' d
           | (f', turn_on, d) <- impliedXFlags, f' == f ]
        -- When you set f, set the ones it implies
        -- NB: use setExtensionFlag recursively, in case the implied flags
        --     implies further flags

unSetExtensionFlag' f dflags = xopt_unset dflags f
   -- When you un-set f, however, we don't un-set the things it implies
   --      (except for -fno-glasgow-exts, which is treated specially)

--------------------------

alterToolSettings :: (ToolSettings -> ToolSettings) -> DynFlags -> DynFlags
alterToolSettings f dynFlags = dynFlags { toolSettings = f (toolSettings dynFlags) }

--------------------------
setDumpFlag' :: DumpFlag -> DynP ()
setDumpFlag' dump_flag
  = do upd (\dfs -> dopt_set dfs dump_flag)
       when want_recomp forceRecompile
    where -- Certain dumpy-things are really interested in what's going
          -- on during recompilation checking, so in those cases we
          -- don't want to turn it off.
          want_recomp = dump_flag `notElem` [Opt_D_dump_if_trace,
                                             Opt_D_dump_hi_diffs,
                                             Opt_D_no_debug_output]

forceRecompile :: DynP ()
-- Whenever we -ddump, force recompilation (by switching off the
-- recompilation checker), else you don't see the dump! However,
-- don't switch it off in --make mode, else *everything* gets
-- recompiled which probably isn't what you want
forceRecompile = do dfs <- liftEwM getCmdLineState
                    when (force_recomp dfs) (setGeneralFlag Opt_ForceRecomp)
        where
          force_recomp dfs = isOneShot (ghcMode dfs)


setVerbosity :: Maybe Int -> DynP ()
setVerbosity mb_n = upd (\dfs -> dfs{ verbosity = mb_n `orElse` 3 })

setDebugLevel :: Maybe Int -> DynP ()
setDebugLevel mb_n =
  upd (\dfs -> exposeSyms $ dfs{ debugLevel = n })
  where
    n = mb_n `orElse` 2
    exposeSyms
      | n > 2     = setGeneralFlag' Opt_ExposeInternalSymbols
      | otherwise = id

addPkgDbRef :: PkgDbRef -> DynP ()
addPkgDbRef p = upd $ \s ->
  s { packageDBFlags = PackageDB p : packageDBFlags s }

removeUserPkgDb :: DynP ()
removeUserPkgDb = upd $ \s ->
  s { packageDBFlags = NoUserPackageDB : packageDBFlags s }

removeGlobalPkgDb :: DynP ()
removeGlobalPkgDb = upd $ \s ->
 s { packageDBFlags = NoGlobalPackageDB : packageDBFlags s }

clearPkgDb :: DynP ()
clearPkgDb = upd $ \s ->
  s { packageDBFlags = ClearPackageDBs : packageDBFlags s }

parsePackageFlag :: String                 -- the flag
                 -> ReadP PackageArg       -- type of argument
                 -> String                 -- string to parse
                 -> PackageFlag
parsePackageFlag flag arg_parse str
 = case filter ((=="").snd) (readP_to_S parse str) of
    [(r, "")] -> r
    _ -> throwGhcException $ CmdLineError ("Can't parse package flag: " ++ str)
  where doc = flag ++ " " ++ str
        parse = do
            pkg_arg <- tok arg_parse
            let mk_expose = ExposePackage doc pkg_arg
            ( do _ <- tok $ string "with"
                 fmap (mk_expose . ModRenaming True) parseRns
             <++ fmap (mk_expose . ModRenaming False) parseRns
             <++ return (mk_expose (ModRenaming True [])))
        parseRns = do _ <- tok $ R.char '('
                      rns <- tok $ sepBy parseItem (tok $ R.char ',')
                      _ <- tok $ R.char ')'
                      return rns
        parseItem = do
            orig <- tok $ parseModuleName
            (do _ <- tok $ string "as"
                new <- tok $ parseModuleName
                return (orig, new)
              +++
             return (orig, orig))
        tok m = m >>= \x -> skipSpaces >> return x

exposePackage, exposePackageId, hidePackage,
        exposePluginPackage, exposePluginPackageId,
        ignorePackage,
        trustPackage, distrustPackage :: String -> DynP ()
exposePackage p = upd (exposePackage' p)
exposePackageId p =
  upd (\s -> s{ packageFlags =
    parsePackageFlag "-package-id" parseUnitArg p : packageFlags s })
exposePluginPackage p =
  upd (\s -> s{ pluginPackageFlags =
    parsePackageFlag "-plugin-package" parsePackageArg p : pluginPackageFlags s })
exposePluginPackageId p =
  upd (\s -> s{ pluginPackageFlags =
    parsePackageFlag "-plugin-package-id" parseUnitArg p : pluginPackageFlags s })
hidePackage p =
  upd (\s -> s{ packageFlags = HidePackage p : packageFlags s })
ignorePackage p =
  upd (\s -> s{ ignorePackageFlags = IgnorePackage p : ignorePackageFlags s })

trustPackage p = exposePackage p >> -- both trust and distrust also expose a package
  upd (\s -> s{ trustFlags = TrustPackage p : trustFlags s })
distrustPackage p = exposePackage p >>
  upd (\s -> s{ trustFlags = DistrustPackage p : trustFlags s })

exposePackage' :: String -> DynFlags -> DynFlags
exposePackage' p dflags
    = dflags { packageFlags =
            parsePackageFlag "-package" parsePackageArg p : packageFlags dflags }

parsePackageArg :: ReadP PackageArg
parsePackageArg =
    fmap PackageArg (munch1 (\c -> isAlphaNum c || c `elem` ":-_."))

parseUnitArg :: ReadP PackageArg
parseUnitArg =
    fmap UnitIdArg parseUnit

setUnitId :: String -> DynFlags -> DynFlags
setUnitId p d = d { homeUnitId_ = stringToUnitId p }

setWorkingDirectory :: String -> DynFlags -> DynFlags
setWorkingDirectory p d = d { workingDirectory =  Just p }

{-
Note [Filepaths and Multiple Home Units]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is common to assume that a package is compiled in the directory where its
cabal file resides. Thus, all paths used in the compiler are assumed to be relative
to this directory. When there are multiple home units the compiler is often
not operating in the standard directory and instead where the cabal.project
file is located. In this case the `-working-dir` option can be passed which specifies
the path from the current directory to the directory the unit assumes to be it's root,
normally the directory which contains the cabal file.

When the flag is passed, any relative paths used by the compiler are offset
by the working directory. Notably this includes `-i`, `-I⟨dir⟩`, `-hidir`, `-odir` etc and
the location of input files.

-}

augmentByWorkingDirectory :: DynFlags -> FilePath -> FilePath
augmentByWorkingDirectory dflags fp | isRelative fp, Just offset <- workingDirectory dflags = offset </> fp
augmentByWorkingDirectory _ fp = fp

setPackageName :: String -> DynFlags -> DynFlags
setPackageName p d = d { thisPackageName =  Just p }

addHiddenModule :: String -> DynP ()
addHiddenModule p =
  upd (\s -> s{ hiddenModules  = Set.insert (mkModuleName p) (hiddenModules s) })

addReexportedModule :: String -> DynP ()
addReexportedModule p =
  upd (\s -> s{ reexportedModules  = (parseReexportedModule p) : (reexportedModules s) })

parseReexportedModule :: String                 -- string to parse
                      -> ReexportedModule
parseReexportedModule str
 = case filter ((=="").snd) (readP_to_S parseItem str) of
    [(r, "")] -> r
    _ -> throwGhcException $ CmdLineError ("Can't parse reexported module flag: " ++ str)
  where
        parseItem = do
            orig <- tok $ parseModuleName
            (do _ <- tok $ string "as"
                new <- tok $ parseModuleName
                return (ReexportedModule orig new))
              +++
             return (ReexportedModule orig orig)

        tok m = m >>= \x -> skipSpaces >> return x


-- If we're linking a binary, then only backends that produce object
-- code are allowed (requests for other target types are ignored).
setBackend :: Backend -> DynP ()
setBackend l = upd $ \ dfs ->
  if ghcLink dfs /= LinkBinary || backendWritesFiles l
  then dfs{ backend = l }
  else dfs

-- Changes the target only if we're compiling object code.  This is
-- used by -fasm and -fllvm, which switch from one to the other, but
-- not from bytecode to object-code.  The idea is that -fasm/-fllvm
-- can be safely used in an OPTIONS_GHC pragma.
setObjBackend :: Backend -> DynP ()
setObjBackend l = updM set
  where
   set dflags
     | backendWritesFiles (backend dflags)
       = return $ dflags { backend = l }
     | otherwise = return dflags

setOptLevel :: Int -> DynFlags -> DynP DynFlags
setOptLevel n dflags = return (updOptLevel n dflags)

setCallerCcFilters :: String -> DynP ()
setCallerCcFilters arg =
  case parseCallerCcFilter arg of
    Right filt -> upd $ \d -> d { callerCcFilters = filt : callerCcFilters d }
    Left err -> addErr err

setMainIs :: String -> DynP ()
setMainIs arg = parse parse_main_f arg
  where
    parse callback str = case unP parseIdentifier (p_state str) of
      PFailed _      -> addErr $ "Can't parse -main-is \"" ++ arg ++ "\" as an identifier or module."
      POk _ (L _ re) -> callback re

    -- dummy parser state.
    p_state str = initParserState
              (mkParserOpts mempty emptyDiagOpts [] False False False True)
              (stringToStringBuffer str)
              (mkRealSrcLoc (mkFastString []) 1 1)

    parse_main_f (Unqual occ)
      | isVarOcc occ = upd $ \d -> d { mainFunIs = main_f occ }
    parse_main_f (Qual (ModuleName mod) occ)
      | isVarOcc occ = upd $ \d -> d { mainModuleNameIs = mkModuleNameFS mod
                                     , mainFunIs = main_f occ }
    -- append dummy "function" to parse A.B as the module A.B
    -- and not the Data constructor B from the module A
    parse_main_f _ = parse parse_mod (arg ++ ".main")

    main_f = Just . occNameString

    parse_mod (Qual (ModuleName mod) _) = upd $ \d -> d { mainModuleNameIs = mkModuleNameFS mod }
    -- we appended ".m" and any parse error was caught. We are Qual or something went very wrong
    parse_mod _ = error "unreachable"

addLdInputs :: Option -> DynFlags -> DynFlags
addLdInputs p dflags = dflags{ldInputs = ldInputs dflags ++ [p]}

-- -----------------------------------------------------------------------------
-- Load dynflags from environment files.

setFlagsFromEnvFile :: FilePath -> String -> DynP ()
setFlagsFromEnvFile envfile content = do
  setGeneralFlag Opt_HideAllPackages
  parseEnvFile envfile content

parseEnvFile :: FilePath -> String -> DynP ()
parseEnvFile envfile = mapM_ parseEntry . lines
  where
    parseEntry str = case words str of
      ("package-db": _)     -> addPkgDbRef (PkgDbPath (envdir </> db))
        -- relative package dbs are interpreted relative to the env file
        where envdir = takeDirectory envfile
              db     = drop 11 str
      ["clear-package-db"]  -> clearPkgDb
      ["hide-package", pkg]  -> hidePackage pkg
      ["global-package-db"] -> addPkgDbRef GlobalPkgDb
      ["user-package-db"]   -> addPkgDbRef UserPkgDb
      ["package-id", pkgid] -> exposePackageId pkgid
      (('-':'-':_):_)       -> return () -- comments
      -- and the original syntax introduced in 7.10:
      [pkgid]               -> exposePackageId pkgid
      []                    -> return ()
      _                     -> throwGhcException $ CmdLineError $
                                    "Can't parse environment file entry: "
                                 ++ envfile ++ ": " ++ str


-----------------------------------------------------------------------------
-- Paths & Libraries

addImportPath, addLibraryPath, addIncludePath, addFrameworkPath :: FilePath -> DynP ()

-- -i on its own deletes the import paths
addImportPath "" = upd (\s -> s{importPaths = []})
addImportPath p  = upd (\s -> s{importPaths = importPaths s ++ splitPathList p})

addLibraryPath p =
  upd (\s -> s{libraryPaths = libraryPaths s ++ splitPathList p})

addIncludePath p =
  upd (\s -> s{includePaths =
                  addGlobalInclude (includePaths s) (splitPathList p)})

addFrameworkPath p =
  upd (\s -> s{frameworkPaths = frameworkPaths s ++ splitPathList p})

#if !defined(mingw32_HOST_OS)
split_marker :: Char
split_marker = ':'   -- not configurable (ToDo)
#endif

splitPathList :: String -> [String]
splitPathList s = filter notNull (splitUp s)
                -- empty paths are ignored: there might be a trailing
                -- ':' in the initial list, for example.  Empty paths can
                -- cause confusion when they are translated into -I options
                -- for passing to gcc.
  where
#if !defined(mingw32_HOST_OS)
    splitUp xs = split split_marker xs
#else
     -- Windows: 'hybrid' support for DOS-style paths in directory lists.
     --
     -- That is, if "foo:bar:baz" is used, this interpreted as
     -- consisting of three entries, 'foo', 'bar', 'baz'.
     -- However, with "c:/foo:c:\\foo;x:/bar", this is interpreted
     -- as 3 elts, "c:/foo", "c:\\foo", "x:/bar"
     --
     -- Notice that no attempt is made to fully replace the 'standard'
     -- split marker ':' with the Windows / DOS one, ';'. The reason being
     -- that this will cause too much breakage for users & ':' will
     -- work fine even with DOS paths, if you're not insisting on being silly.
     -- So, use either.
    splitUp []             = []
    splitUp (x:':':div:xs) | div `elem` dir_markers
                           = ((x:':':div:p): splitUp rs)
                           where
                              (p,rs) = findNextPath xs
          -- we used to check for existence of the path here, but that
          -- required the IO monad to be threaded through the command-line
          -- parser which is quite inconvenient.  The
    splitUp xs = cons p (splitUp rs)
               where
                 (p,rs) = findNextPath xs

                 cons "" xs = xs
                 cons x  xs = x:xs

    -- will be called either when we've consumed nought or the
    -- "<Drive>:/" part of a DOS path, so splitting is just a Q of
    -- finding the next split marker.
    findNextPath xs =
        case break (`elem` split_markers) xs of
           (p, _:ds) -> (p, ds)
           (p, xs)   -> (p, xs)

    split_markers :: [Char]
    split_markers = [':', ';']

    dir_markers :: [Char]
    dir_markers = ['/', '\\']
#endif

-- -----------------------------------------------------------------------------
-- tmpDir, where we store temporary files.

setTmpDir :: FilePath -> DynFlags -> DynFlags
setTmpDir dir d = d { tmpDir = TempDir (normalise dir) }
  -- we used to fix /cygdrive/c/.. on Windows, but this doesn't
  -- seem necessary now --SDM 7/2/2008

-----------------------------------------------------------------------------
-- RTS opts

setRtsOpts :: String -> DynP ()
setRtsOpts arg  = upd $ \ d -> d {rtsOpts = Just arg}

setRtsOptsEnabled :: RtsOptsEnabled -> DynP ()
setRtsOptsEnabled arg  = upd $ \ d -> d {rtsOptsEnabled = arg}

-----------------------------------------------------------------------------
-- Hpc stuff

setOptHpcDir :: String -> DynP ()
setOptHpcDir arg  = upd $ \ d -> d {hpcDir = arg}

-----------------------------------------------------------------------------
-- Via-C compilation stuff

-- There are some options that we need to pass to gcc when compiling
-- Haskell code via C, but are only supported by recent versions of
-- gcc.  The configure script decides which of these options we need,
-- and puts them in the "settings" file in $topdir. The advantage of
-- having these in a separate file is that the file can be created at
-- install-time depending on the available gcc version, and even
-- re-generated later if gcc is upgraded.
--
-- The options below are not dependent on the version of gcc, only the
-- platform.

picCCOpts :: DynFlags -> [String]
picCCOpts dflags =
      case platformOS (targetPlatform dflags) of
      OSDarwin
          -- Apple prefers to do things the other way round.
          -- PIC is on by default.
          -- -mdynamic-no-pic:
          --     Turn off PIC code generation.
          -- -fno-common:
          --     Don't generate "common" symbols - these are unwanted
          --     in dynamic libraries.

       | gopt Opt_PIC dflags -> ["-fno-common", "-U__PIC__", "-D__PIC__"]
       | otherwise           -> ["-mdynamic-no-pic"]
      OSMinGW32 -- no -fPIC for Windows
       | gopt Opt_PIC dflags -> ["-U__PIC__", "-D__PIC__"]
       | otherwise           -> []
      _
      -- we need -fPIC for C files when we are compiling with -dynamic,
      -- otherwise things like stub.c files don't get compiled
      -- correctly.  They need to reference data in the Haskell
      -- objects, but can't without -fPIC.  See
      -- https://gitlab.haskell.org/ghc/ghc/wikis/commentary/position-independent-code
       | gopt Opt_PIC dflags || ways dflags `hasWay` WayDyn ->
          ["-fPIC", "-U__PIC__", "-D__PIC__"] ++
          -- Clang defaults to -fvisibility=hidden for wasm targets,
          -- but we need these compile-time flags to generate PIC
          -- objects that can be properly linked by wasm-ld using
          -- --export-dynamic; without these flags we would need
          -- -Wl,--export-all at .so link-time which will export
          -- internal symbols as well, and that severely pollutes the
          -- global symbol namespace.
          (if platformArch (targetPlatform dflags) == ArchWasm32
            then [ "-fvisibility=default", "-fvisibility-inlines-hidden" ]
            else [])
      -- gcc may be configured to have PIC on by default, let's be
      -- explicit here, see #15847
       | otherwise -> ["-fno-PIC"]

pieCCLDOpts :: DynFlags -> [String]
pieCCLDOpts dflags
      | gopt Opt_PICExecutable dflags       = ["-pie"]
        -- See Note [No PIE when linking]
      | toolSettings_ccSupportsNoPie (toolSettings dflags) = ["-no-pie"]
      | otherwise                           = []


{-
Note [No PIE when linking]
~~~~~~~~~~~~~~~~~~~~~~~~~~
As of 2016 some Linux distributions (e.g. Debian) have started enabling -pie by
default in their gcc builds. This is incompatible with -r as it implies that we
are producing an executable. Consequently, we must manually pass -no-pie to gcc
when joining object files or linking dynamic libraries. Unless, of course, the
user has explicitly requested a PIE executable with -pie. See #12759.
-}

picPOpts :: DynFlags -> [String]
picPOpts dflags
 | gopt Opt_PIC dflags = ["-U__PIC__", "-D__PIC__"]
 | otherwise           = []

-- -----------------------------------------------------------------------------
-- Compiler Info

compilerInfo :: DynFlags -> [(String, String)]
compilerInfo dflags
    = -- We always make "Project name" be first to keep parsing in
      -- other languages simple, i.e. when looking for other fields,
      -- you don't have to worry whether there is a leading '[' or not
      ("Project name",                 cProjectName)
      -- Next come the settings, so anything else can be overridden
      -- in the settings file (as "lookup" uses the first match for the
      -- key)
    : map (fmap $ expandDirectories (topDir dflags) (toolDir dflags))
          (rawSettings dflags)
   ++ [("Project version",             projectVersion dflags),
       ("Project Git commit id",       cProjectGitCommitId),
       ("Project Version Int",         cProjectVersionInt),
       ("Project Patch Level",         cProjectPatchLevel),
       ("Project Patch Level1",        cProjectPatchLevel1),
       ("Project Patch Level2",        cProjectPatchLevel2),
       ("Project Unit Id",             cProjectUnitId),
       ("Booter version",              cBooterVersion),
       ("Stage",                       cStage),
       ("Build platform",              cBuildPlatformString),
       ("Host platform",               cHostPlatformString),
       ("Target platform",             platformMisc_targetPlatformString $ platformMisc dflags),
       ("Have interpreter",            showBool $ platformMisc_ghcWithInterpreter $ platformMisc dflags),
       ("Object splitting supported",  showBool False),
       ("Have native code generator",  showBool $ platformNcgSupported platform),
       ("target has RTS linker",       showBool $ platformHasRTSLinker platform),
       ("Target default backend",      show     $ platformDefaultBackend platform),
       -- Whether or not we support @-dynamic-too@
       ("Support dynamic-too",         showBool $ not isWindows),
       -- Whether or not we support the @-j@ flag with @--make@.
       ("Support parallel --make",     "YES"),
       -- Whether or not we support "Foo from foo-0.1-XXX:Foo" syntax in
       -- installed package info.
       ("Support reexported-modules",  "YES"),
       -- Whether or not we support extended @-package foo (Foo)@ syntax.
       ("Support thinning and renaming package flags", "YES"),
       -- Whether or not we support Backpack.
       ("Support Backpack", "YES"),
       -- If true, we require that the 'id' field in installed package info
       -- match what is passed to the @-this-unit-id@ flag for modules
       -- built in it
       ("Requires unified installed package IDs", "YES"),
       -- Whether or not we support the @-this-package-key@ flag.  Prefer
       -- "Uses unit IDs" over it. We still say yes even if @-this-package-key@
       -- flag has been removed, otherwise it breaks Cabal...
       ("Uses package keys",           "YES"),
       -- Whether or not we support the @-this-unit-id@ flag
       ("Uses unit IDs",               "YES"),
       -- Whether or not GHC was compiled using -dynamic
       ("GHC Dynamic",                 showBool hostIsDynamic),
       -- Whether or not GHC was compiled using -prof
       ("GHC Profiled",                showBool hostIsProfiled),
       ("Debug on",                    showBool debugIsOn),
       ("LibDir",                      topDir dflags),
       -- This is always an absolute path, unlike "Relative Global Package DB" which is
       -- in the settings file.
       ("Global Package DB",           globalPackageDatabasePath dflags)
      ]
  where
    showBool True  = "YES"
    showBool False = "NO"
    platform  = targetPlatform dflags
    isWindows = platformOS platform == OSMinGW32
    useInplaceMinGW = toolSettings_useInplaceMinGW $ toolSettings dflags
    expandDirectories :: FilePath -> Maybe FilePath -> String -> String
    expandDirectories topd mtoold = expandToolDir useInplaceMinGW mtoold . expandTopDir topd

{- -----------------------------------------------------------------------------
Note [DynFlags consistency]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are a number of number of DynFlags configurations which either
do not make sense or lead to unimplemented or buggy codepaths in the
compiler. makeDynFlagsConsistent is responsible for verifying the validity
of a set of DynFlags, fixing any issues, and reporting them back to the
caller.

GHCi and -O
---------------

When using optimization, the compiler can introduce several things
(such as unboxed tuples) into the intermediate code, which GHCi later
chokes on since the bytecode interpreter can't handle this (and while
this is arguably a bug these aren't handled, there are no plans to fix
it.)

While the driver pipeline always checks for this particular erroneous
combination when parsing flags, we also need to check when we update
the flags; this is because API clients may parse flags but update the
DynFlags afterwords, before finally running code inside a session (see
T10052 and #10052).
-}

-- | Resolve any internal inconsistencies in a set of 'DynFlags'.
-- Returns the consistent 'DynFlags' as well as a list of warnings
-- to report to the user.
makeDynFlagsConsistent :: DynFlags -> (DynFlags, [Warn])
-- Whenever makeDynFlagsConsistent does anything, it starts over, to
-- ensure that a later change doesn't invalidate an earlier check.
-- Be careful not to introduce potential loops!
makeDynFlagsConsistent dflags
 -- Disable -dynamic-too on Windows (#8228, #7134, #5987)
 | os == OSMinGW32 && gopt Opt_BuildDynamicToo dflags
    = let dflags' = gopt_unset dflags Opt_BuildDynamicToo
          warn    = "-dynamic-too is not supported on Windows"
      in loop dflags' warn
 -- Disable -dynamic-too if we are are compiling with -dynamic already, otherwise
 -- you get two dynamic object files (.o and .dyn_o). (#20436)
 | ways dflags `hasWay` WayDyn && gopt Opt_BuildDynamicToo dflags
    = let dflags' = gopt_unset dflags Opt_BuildDynamicToo
          warn = "-dynamic-too is ignored when using -dynamic"
      in loop dflags' warn

 | gopt Opt_SplitSections dflags
 , platformHasSubsectionsViaSymbols (targetPlatform dflags)
    = let dflags' = gopt_unset dflags Opt_SplitSections
          warn = "-fsplit-sections is not useful on this platform " ++
                 "since it uses subsections-via-symbols. Ignoring."
      in loop dflags' warn

   -- Via-C backend only supports unregisterised ABI. Switch to a backend
   -- supporting it if possible.
 | backendUnregisterisedAbiOnly (backend dflags) &&
   not (platformUnregisterised (targetPlatform dflags))
    = let b = platformDefaultBackend (targetPlatform dflags)
      in if backendSwappableWithViaC b then
           let dflags' = dflags { backend = b }
               warn = "Target platform doesn't use unregisterised ABI, so using " ++
                      backendDescription b ++ " rather than " ++
                      backendDescription (backend dflags)
           in loop dflags' warn
         else
           pgmError (backendDescription (backend dflags) ++
                     " supports only unregisterised ABI but target platform doesn't use it.")

 | gopt Opt_Hpc dflags && not (backendSupportsHpc (backend dflags))
    = let dflags' = gopt_unset dflags Opt_Hpc
          warn = "Hpc can't be used with " ++ backendDescription (backend dflags) ++
                 ". Ignoring -fhpc."
      in loop dflags' warn

 | backendSwappableWithViaC (backend dflags) &&
   platformUnregisterised (targetPlatform dflags)
    = loop (dflags { backend = viaCBackend })
           "Target platform uses unregisterised ABI, so compiling via C"

 | backendNeedsPlatformNcgSupport (backend dflags) &&
   not (platformNcgSupported $ targetPlatform dflags)
      = let dflags' = dflags { backend = llvmBackend }
            warn = "Native code generator doesn't support target platform, so using LLVM"
        in loop dflags' warn

 | not (osElfTarget os) && gopt Opt_PIE dflags
    = loop (gopt_unset dflags Opt_PIE)
           "Position-independent only supported on ELF platforms"
 | os == OSDarwin &&
   arch == ArchX86_64 &&
   not (gopt Opt_PIC dflags)
    = loop (gopt_set dflags Opt_PIC)
           "Enabling -fPIC as it is always on for this platform"

 | backendForcesOptimization0 (backend dflags)
 , gopt Opt_UnoptimizedCoreForInterpreter dflags
 , let (dflags', changed) = updOptLevelChanged 0 dflags
 , changed
    = loop dflags' $
      "Ignoring optimization flags since they are experimental for the " ++
      backendDescription (backend dflags) ++
      ". Pass -fno-unoptimized-core-for-interpreter to enable this feature."

 | LinkInMemory <- ghcLink dflags
 , not (gopt Opt_ExternalInterpreter dflags)
 , hostIsProfiled
 , backendWritesFiles (backend dflags)
 , ways dflags `hasNotWay` WayProf
    = loop dflags{targetWays_ = addWay WayProf (targetWays_ dflags)}
         "Enabling -prof, because -fobject-code is enabled and GHCi is profiled"

 | gopt Opt_ByteCode dflags || gopt Opt_ByteCodeAndObjectCode dflags
 , not (gopt Opt_ExternalInterpreter dflags)
 , hostIsProfiled
 , ways dflags `hasNotWay` WayProf
    = loop (gopt_set dflags Opt_ExternalInterpreter)
         "Enabling external interpreter, because GHC is profiled and bytecode is being used for TH"

 | LinkMergedObj <- ghcLink dflags
 , Nothing <- outputFile dflags
 = pgmError "--output must be specified when using --merge-objs"

 | otherwise = (dflags, mempty)
    where loc = mkGeneralSrcSpan (fsLit "when making flags consistent")
          loop updated_dflags warning
              = case makeDynFlagsConsistent updated_dflags of
                (dflags', ws) -> (dflags', L loc (DriverInconsistentDynFlags warning) : ws)
          platform = targetPlatform dflags
          arch = platformArch platform
          os   = platformOS   platform


setUnsafeGlobalDynFlags :: DynFlags -> IO ()
setUnsafeGlobalDynFlags dflags = do
   writeIORef v_unsafeHasPprDebug (hasPprDebug dflags)
   writeIORef v_unsafeHasNoDebugOutput (hasNoDebugOutput dflags)
   writeIORef v_unsafeHasNoStateHack (hasNoStateHack dflags)


-- -----------------------------------------------------------------------------

-- | Indicate if cost-centre profiling is enabled
sccProfilingEnabled :: DynFlags -> Bool
sccProfilingEnabled dflags = profileIsProfiling (targetProfile dflags)

-- | Indicate whether we need to generate source notes
needSourceNotes :: DynFlags -> Bool
needSourceNotes dflags = debugLevel dflags > 0
                       || gopt Opt_InfoTableMap dflags

                       -- Source ticks are used to approximate the location of
                       -- overloaded call cost centers
                       || gopt Opt_ProfLateoverloadedCallsCCs dflags

-- -----------------------------------------------------------------------------
-- Linker/compiler information

-- | Should we use `-XLinker -rpath` when linking or not?
-- See Note [-fno-use-rpaths]
useXLinkerRPath :: DynFlags -> OS -> Bool
-- wasm shared libs don't have RPATH at all and wasm-ld doesn't accept
-- any RPATH-related flags
useXLinkerRPath dflags _ | ArchWasm32 <- platformArch $ targetPlatform dflags = False
useXLinkerRPath _ OSDarwin = False -- See Note [Dynamic linking on macOS]
useXLinkerRPath dflags _ = gopt Opt_RPath dflags

{-
Note [-fno-use-rpaths]
~~~~~~~~~~~~~~~~~~~~~~

First read, Note [Dynamic linking on macOS] to understand why on darwin we never
use `-XLinker -rpath`.

The specification of `Opt_RPath` is as follows:

The default case `-fuse-rpaths`:
* On darwin, never use `-Xlinker -rpath -Xlinker`, always inject the rpath
  afterwards, see `runInjectRPaths`. There is no way to use `-Xlinker` on darwin
  as things stand but it wasn't documented in the user guide before this patch how
  `-fuse-rpaths` should behave and the fact it was always disabled on darwin.
* Otherwise, use `-Xlinker -rpath -Xlinker` to set the rpath of the executable,
  this is the normal way you should set the rpath.

The case of `-fno-use-rpaths`
* Never inject anything into the rpath.

When this was first implemented, `Opt_RPath` was disabled on darwin, but
the rpath was still always augmented by `runInjectRPaths`, and there was no way to
stop this. This was problematic because you couldn't build an executable in CI
with a clean rpath.

-}

-- -----------------------------------------------------------------------------
-- RTS hooks

-- Convert sizes like "3.5M" into integers
decodeSize :: String -> Integer
decodeSize str
  | c == ""      = truncate n
  | c == "K" || c == "k" = truncate (n * 1000)
  | c == "M" || c == "m" = truncate (n * 1000 * 1000)
  | c == "G" || c == "g" = truncate (n * 1000 * 1000 * 1000)
  | otherwise            = throwGhcException (CmdLineError ("can't decode size: " ++ str))
  where (m, c) = span pred str
        n      = readRational m
        pred c = isDigit c || c == '.'

foreign import ccall unsafe "setHeapSize"       setHeapSize       :: Int -> IO ()
foreign import ccall unsafe "enableTimingStats" enableTimingStats :: IO ()

outputFile :: DynFlags -> Maybe String
outputFile dflags
   | dynamicNow dflags = dynOutputFile_ dflags
   | otherwise         = outputFile_    dflags

objectSuf :: DynFlags -> String
objectSuf dflags
   | dynamicNow dflags = dynObjectSuf_ dflags
   | otherwise         = objectSuf_    dflags

-- | Pretty-print the difference between 2 DynFlags.
--
-- For now only their general flags but it could be extended.
-- Useful mostly for debugging.
pprDynFlagsDiff :: DynFlags -> DynFlags -> SDoc
pprDynFlagsDiff d1 d2 =
   let gf_removed  = EnumSet.difference (generalFlags d1) (generalFlags d2)
       gf_added    = EnumSet.difference (generalFlags d2) (generalFlags d1)
       ext_removed = EnumSet.difference (extensionFlags d1) (extensionFlags d2)
       ext_added   = EnumSet.difference (extensionFlags d2) (extensionFlags d1)
   in vcat
      [ text "Added general flags:"
      , text $ show $ EnumSet.toList $ gf_added
      , text "Removed general flags:"
      , text $ show $ EnumSet.toList $ gf_removed
      , text "Added extension flags:"
      , text $ show $ EnumSet.toList $ ext_added
      , text "Removed extension flags:"
      , text $ show $ EnumSet.toList $ ext_removed
      ]

updatePlatformConstants :: DynFlags -> Maybe PlatformConstants -> IO DynFlags
updatePlatformConstants dflags mconstants = do
  let platform1 = (targetPlatform dflags) { platform_constants = mconstants }
  let dflags1   = dflags { targetPlatform = platform1 }
  return dflags1
