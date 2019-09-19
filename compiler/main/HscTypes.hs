{-
(c) The University of Glasgow, 2006

\section[HscTypes]{Types for the per-module compiler}
-}

{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

-- | Types for the per-module compiler
module HscTypes (
        -- * compilation state
        HscEnv(..), hscEPS,
        FinderCache, FindResult(..), InstalledFindResult(..),
        Target(..), TargetId(..), InputFileBuffer, pprTarget, pprTargetId,
        HscStatus(..),
        IServ(..),

        -- * ModuleGraph
        ModuleGraph, emptyMG, mkModuleGraph, extendMG, mapMG,
        mgModSummaries, mgElemModule, mgLookupModule,
        needsTemplateHaskellOrQQ, mgBootModules,

        -- * Hsc monad
        Hsc(..), runHsc, mkInteractiveHscEnv, runInteractiveHsc,

        -- * Information about modules
        ModDetails(..), emptyModDetails,
        ModGuts(..), CgGuts(..), ForeignStubs(..), appendStubC,
        ImportedMods, ImportedBy(..), importedByUser, ImportedModsVal(..), SptEntry(..),
        ForeignSrcLang(..),
        phaseForeignLanguage,

        ModSummary(..), ms_imps, ms_installed_mod, ms_mod_name, ms_home_imps,
        home_imps, ms_home_allimps, ms_home_srcimps, showModMsg, isBootSummary,
        msHsFilePath, msHiFilePath, msObjFilePath,
        SourceModified(..), isTemplateHaskellOrQQNonBoot,

        -- * Information about the module being compiled
        -- (re-exported from DriverPhases)
        HscSource(..), isHsBootOrSig, isHsigFile, hscSourceString,


        -- * State relating to modules in this package
        HomePackageTable, HomeModInfo(..), emptyHomePackageTable,
        lookupHpt, eltsHpt, filterHpt, allHpt, mapHpt, delFromHpt,
        addToHpt, addListToHpt, lookupHptDirectly, listToHpt,
        hptCompleteSigs,
        hptInstances, hptRules, pprHPT,

        -- * State relating to known packages
        ExternalPackageState(..), EpsStats(..), addEpsInStats,
        PackageTypeEnv, PackageIfaceTable, emptyPackageIfaceTable,
        lookupIfaceByModule, emptyPartialModIface, emptyFullModIface, lookupHptByModule,

        PackageInstEnv, PackageFamInstEnv, PackageRuleBase,
        PackageCompleteMatchMap,

        mkSOName, mkHsSOName, soExt,

        -- * Metaprogramming
        MetaRequest(..),
        MetaResult, -- data constructors not exported to ensure correct response type
        metaRequestE, metaRequestP, metaRequestT, metaRequestD, metaRequestAW,
        MetaHook,

        -- * Annotations
        prepareAnnotations,

        -- * Interactive context
        InteractiveContext(..), emptyInteractiveContext,
        icPrintUnqual, icInScopeTTs, icExtendGblRdrEnv,
        extendInteractiveContext, extendInteractiveContextWithIds,
        substInteractiveContext,
        setInteractivePrintName, icInteractiveModule,
        InteractiveImport(..), setInteractivePackage,
        mkPrintUnqualified, pprModulePrefix,
        mkQualPackage, mkQualModule, pkgQual,

        -- * Interfaces
        ModIface, PartialModIface, ModIface_(..), ModIfaceBackend(..),
        mkIfaceWarnCache, mkIfaceHashCache, mkIfaceFixCache,
        emptyIfaceWarnCache, mi_boot, mi_fix,
        mi_semantic_module,
        mi_free_holes,
        renameFreeHoles,

        -- * Fixity
        FixityEnv, FixItem(..), lookupFixity, emptyFixityEnv,

        -- * TyThings and type environments
        TyThing(..),  tyThingAvailInfo,
        tyThingTyCon, tyThingDataCon, tyThingConLike,
        tyThingId, tyThingCoAxiom, tyThingParent_maybe, tyThingsTyCoVars,
        implicitTyThings, implicitTyConThings, implicitClassThings,
        isImplicitTyThing,

        TypeEnv, lookupType, lookupTypeHscEnv, mkTypeEnv, emptyTypeEnv,
        typeEnvFromEntities, mkTypeEnvWithImplicits,
        extendTypeEnv, extendTypeEnvList,
        extendTypeEnvWithIds, plusTypeEnv,
        lookupTypeEnv,
        typeEnvElts, typeEnvTyCons, typeEnvIds, typeEnvPatSyns,
        typeEnvDataCons, typeEnvCoAxioms, typeEnvClasses,

        -- * MonadThings
        MonadThings(..),

        -- * Information on imports and exports
        WhetherHasOrphans, IsBootInterface, Usage(..),
        Dependencies(..), noDependencies,
        updNameCache,
        IfaceExport,

        -- * Warnings
        Warnings(..), WarningTxt(..), plusWarns,

        -- * Linker stuff
        Linkable(..), isObjectLinkable, linkableObjs,
        Unlinked(..), CompiledByteCode,
        isObject, nameOfObject, isInterpretable, byteCodeOfObject,

        -- * Program coverage
        HpcInfo(..), emptyHpcInfo, isHpcUsed, AnyHpcUsage,

        -- * Breakpoints
        ModBreaks (..), emptyModBreaks,

        -- * Safe Haskell information
        IfaceTrustInfo, getSafeMode, setSafeMode, noIfaceTrustInfo,
        trustInfoToNum, numToTrustInfo, IsSafeImport,

        -- * result of the parser
        HsParsedModule(..),

        -- * Compilation errors and warnings
        SourceError, GhcApiError, mkSrcErr, srcErrorMessages, mkApiErr,
        throwOneError, throwErrors, handleSourceError,
        handleFlagWarnings, printOrThrowWarnings,

        -- * COMPLETE signature
        CompleteMatch(..), CompleteMatchMap,
        mkCompleteMatchMap, extendCompleteMatchMap
    ) where

#include "HsVersions.h"

import GhcPrelude

import ByteCodeTypes
import InteractiveEvalTypes ( Resume )
import GHCi.Message         ( Pipe )
import GHCi.RemoteTypes
import GHC.ForeignSrcLang

import UniqFM
import GHC.Hs
import RdrName
import Avail
import Module
import InstEnv          ( InstEnv, ClsInst, identicalClsInstHead )
import FamInstEnv
import CoreSyn          ( CoreProgram, RuleBase, CoreRule )
import Name
import NameEnv
import VarSet
import Var
import Id
import IdInfo           ( IdDetails(..), RecSelParent(..))
import Type

import ApiAnnotation    ( ApiAnns )
import Annotations      ( Annotation, AnnEnv, mkAnnEnv, plusAnnEnv )
import Class
import TyCon
import CoAxiom
import ConLike
import DataCon
import PatSyn
import PrelNames        ( gHC_PRIM, ioTyConName, printName, mkInteractiveModule )
import TysWiredIn
import Packages hiding  ( Version(..) )
import CmdLineParser
import DynFlags
import LinkerTypes      ( DynLinker, Linkable(..), Unlinked(..), SptEntry(..) )
import DriverPhases     ( Phase, HscSource(..), hscSourceString
                        , isHsBootOrSig, isHsigFile )
import qualified DriverPhases as Phase
import BasicTypes
import GHC.Iface.Syntax
import Maybes
import Outputable
import SrcLoc
import Unique
import UniqDFM
import FastString
import StringBuffer     ( StringBuffer )
import Fingerprint
import MonadUtils
import Bag
import Binary
import ErrUtils
import NameCache
import GHC.Platform
import Util
import UniqDSet
import GHC.Serialized   ( Serialized )
import qualified GHC.LanguageExtensions as LangExt

import Foreign
import Control.Monad    ( guard, liftM, ap )
import Data.IORef
import Data.Time
import Exception
import System.FilePath
import Control.Concurrent
import System.Process   ( ProcessHandle )
import Control.DeepSeq
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

-- -----------------------------------------------------------------------------
-- Compilation state
-- -----------------------------------------------------------------------------

-- | Status of a compilation to hard-code
data HscStatus
    -- | Nothing to do.
    = HscNotGeneratingCode ModIface ModDetails
    -- | Nothing to do because code already exists.
    | HscUpToDate ModIface ModDetails
    -- | Update boot file result.
    | HscUpdateBoot ModIface ModDetails
    -- | Generate signature file (backpack)
    | HscUpdateSig ModIface ModDetails
    -- | Recompile this module.
    | HscRecomp
        { hscs_guts       :: CgGuts
          -- ^ Information for the code generator.
        , hscs_mod_location :: !ModLocation
          -- ^ Module info
        , hscs_mod_details :: !ModDetails
        , hscs_partial_iface  :: !PartialModIface
          -- ^ Partial interface
        , hscs_old_iface_hash :: !(Maybe Fingerprint)
          -- ^ Old interface hash for this compilation, if an old interface file
          -- exists. Pass to `hscMaybeWriteIface` when writing the interface to
          -- avoid updating the existing interface when the interface isn't
          -- changed.
        , hscs_iface_dflags :: !DynFlags
          -- ^ Generate final iface using this DynFlags.
          -- FIXME (osa): I don't understand why this is necessary, but I spent
          -- almost two days trying to figure this out and I couldn't .. perhaps
          -- someone who understands this code better will remove this later.
        }
-- Should HscStatus contain the HomeModInfo?
-- All places where we return a status we also return a HomeModInfo.

-- -----------------------------------------------------------------------------
-- The Hsc monad: Passing an environment and warning state

newtype Hsc a = Hsc (HscEnv -> WarningMessages -> IO (a, WarningMessages))
    deriving (Functor)

instance Applicative Hsc where
    pure a = Hsc $ \_ w -> return (a, w)
    (<*>) = ap

instance Monad Hsc where
    Hsc m >>= k = Hsc $ \e w -> do (a, w1) <- m e w
                                   case k a of
                                       Hsc k' -> k' e w1

instance MonadIO Hsc where
    liftIO io = Hsc $ \_ w -> do a <- io; return (a, w)

instance HasDynFlags Hsc where
    getDynFlags = Hsc $ \e w -> return (hsc_dflags e, w)

runHsc :: HscEnv -> Hsc a -> IO a
runHsc hsc_env (Hsc hsc) = do
    (a, w) <- hsc hsc_env emptyBag
    printOrThrowWarnings (hsc_dflags hsc_env) w
    return a

mkInteractiveHscEnv :: HscEnv -> HscEnv
mkInteractiveHscEnv hsc_env = hsc_env{ hsc_dflags = interactive_dflags }
  where
    interactive_dflags = ic_dflags (hsc_IC hsc_env)

runInteractiveHsc :: HscEnv -> Hsc a -> IO a
-- A variant of runHsc that switches in the DynFlags from the
-- InteractiveContext before running the Hsc computation.
runInteractiveHsc hsc_env = runHsc (mkInteractiveHscEnv hsc_env)

-- -----------------------------------------------------------------------------
-- Source Errors

-- When the compiler (HscMain) discovers errors, it throws an
-- exception in the IO monad.

mkSrcErr :: ErrorMessages -> SourceError
mkSrcErr = SourceError

srcErrorMessages :: SourceError -> ErrorMessages
srcErrorMessages (SourceError msgs) = msgs

mkApiErr :: DynFlags -> SDoc -> GhcApiError
mkApiErr dflags msg = GhcApiError (showSDoc dflags msg)

throwErrors :: MonadIO io => ErrorMessages -> io a
throwErrors = liftIO . throwIO . mkSrcErr

throwOneError :: MonadIO io => ErrMsg -> io a
throwOneError = throwErrors . unitBag

-- | A source error is an error that is caused by one or more errors in the
-- source code.  A 'SourceError' is thrown by many functions in the
-- compilation pipeline.  Inside GHC these errors are merely printed via
-- 'log_action', but API clients may treat them differently, for example,
-- insert them into a list box.  If you want the default behaviour, use the
-- idiom:
--
-- > handleSourceError printExceptionAndWarnings $ do
-- >   ... api calls that may fail ...
--
-- The 'SourceError's error messages can be accessed via 'srcErrorMessages'.
-- This list may be empty if the compiler failed due to @-Werror@
-- ('Opt_WarnIsError').
--
-- See 'printExceptionAndWarnings' for more information on what to take care
-- of when writing a custom error handler.
newtype SourceError = SourceError ErrorMessages

instance Show SourceError where
  show (SourceError msgs) = unlines . map show . bagToList $ msgs

instance Exception SourceError

-- | Perform the given action and call the exception handler if the action
-- throws a 'SourceError'.  See 'SourceError' for more information.
handleSourceError :: (ExceptionMonad m) =>
                     (SourceError -> m a) -- ^ exception handler
                  -> m a -- ^ action to perform
                  -> m a
handleSourceError handler act =
  gcatch act (\(e :: SourceError) -> handler e)

-- | An error thrown if the GHC API is used in an incorrect fashion.
newtype GhcApiError = GhcApiError String

instance Show GhcApiError where
  show (GhcApiError msg) = msg

instance Exception GhcApiError

-- | Given a bag of warnings, turn them into an exception if
-- -Werror is enabled, or print them out otherwise.
printOrThrowWarnings :: DynFlags -> Bag WarnMsg -> IO ()
printOrThrowWarnings dflags warns = do
  let (make_error, warns') =
        mapAccumBagL
          (\make_err warn ->
            case isWarnMsgFatal dflags warn of
              Nothing ->
                (make_err, warn)
              Just err_reason ->
                (True, warn{ errMsgSeverity = SevError
                           , errMsgReason = ErrReason err_reason
                           }))
          False warns
  if make_error
    then throwIO (mkSrcErr warns')
    else printBagOfErrors dflags warns

handleFlagWarnings :: DynFlags -> [Warn] -> IO ()
handleFlagWarnings dflags warns = do
  let warns' = filter (shouldPrintWarning dflags . warnReason)  warns

      -- It would be nicer if warns :: [Located MsgDoc], but that
      -- has circular import problems.
      bag = listToBag [ mkPlainWarnMsg dflags loc (text warn)
                      | Warn _ (L loc warn) <- warns' ]

  printOrThrowWarnings dflags bag

-- Given a warn reason, check to see if it's associated -W opt is enabled
shouldPrintWarning :: DynFlags -> CmdLineParser.WarnReason -> Bool
shouldPrintWarning dflags ReasonDeprecatedFlag
  = wopt Opt_WarnDeprecatedFlags dflags
shouldPrintWarning dflags ReasonUnrecognisedFlag
  = wopt Opt_WarnUnrecognisedWarningFlags dflags
shouldPrintWarning _ _
  = True

{-
************************************************************************
*                                                                      *
\subsection{HscEnv}
*                                                                      *
************************************************************************
-}

-- | HscEnv is like 'Session', except that some of the fields are immutable.
-- An HscEnv is used to compile a single module from plain Haskell source
-- code (after preprocessing) to either C, assembly or C--. It's also used
-- to store the dynamic linker state to allow for multiple linkers in the
-- same address space.
-- Things like the module graph don't change during a single compilation.
--
-- Historical note: \"hsc\" used to be the name of the compiler binary,
-- when there was a separate driver and compiler.  To compile a single
-- module, the driver would invoke hsc on the source code... so nowadays
-- we think of hsc as the layer of the compiler that deals with compiling
-- a single module.
data HscEnv
  = HscEnv {
        hsc_dflags :: DynFlags,
                -- ^ The dynamic flag settings

        hsc_targets :: [Target],
                -- ^ The targets (or roots) of the current session

        hsc_mod_graph :: ModuleGraph,
                -- ^ The module graph of the current session

        hsc_IC :: InteractiveContext,
                -- ^ The context for evaluating interactive statements

        hsc_HPT    :: HomePackageTable,
                -- ^ The home package table describes already-compiled
                -- home-package modules, /excluding/ the module we
                -- are compiling right now.
                -- (In one-shot mode the current module is the only
                -- home-package module, so hsc_HPT is empty.  All other
                -- modules count as \"external-package\" modules.
                -- However, even in GHCi mode, hi-boot interfaces are
                -- demand-loaded into the external-package table.)
                --
                -- 'hsc_HPT' is not mutable because we only demand-load
                -- external packages; the home package is eagerly
                -- loaded, module by module, by the compilation manager.
                --
                -- The HPT may contain modules compiled earlier by @--make@
                -- but not actually below the current module in the dependency
                -- graph.
                --
                -- (This changes a previous invariant: changed Jan 05.)

        hsc_EPS :: {-# UNPACK #-} !(IORef ExternalPackageState),
                -- ^ Information about the currently loaded external packages.
                -- This is mutable because packages will be demand-loaded during
                -- a compilation run as required.

        hsc_NC  :: {-# UNPACK #-} !(IORef NameCache),
                -- ^ As with 'hsc_EPS', this is side-effected by compiling to
                -- reflect sucking in interface files.  They cache the state of
                -- external interface files, in effect.

        hsc_FC   :: {-# UNPACK #-} !(IORef FinderCache),
                -- ^ The cached result of performing finding in the file system

        hsc_type_env_var :: Maybe (Module, IORef TypeEnv)
                -- ^ Used for one-shot compilation only, to initialise
                -- the 'IfGblEnv'. See 'TcRnTypes.tcg_type_env_var' for
                -- 'TcRnTypes.TcGblEnv'.  See also Note [hsc_type_env_var hack]

        , hsc_iserv :: MVar (Maybe IServ)
                -- ^ interactive server process.  Created the first
                -- time it is needed.

        , hsc_dynLinker :: DynLinker
                -- ^ dynamic linker.

 }

-- Note [hsc_type_env_var hack]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- hsc_type_env_var is used to initialize tcg_type_env_var, and
-- eventually it is the mutable variable that is queried from
-- if_rec_types to get a TypeEnv.  So, clearly, it's something
-- related to knot-tying (see Note [Tying the knot]).
-- hsc_type_env_var is used in two places: initTcRn (where
-- it initializes tcg_type_env_var) and initIfaceCheck
-- (where it initializes if_rec_types).
--
-- But why do we need a way to feed a mutable variable in?  Why
-- can't we just initialize tcg_type_env_var when we start
-- typechecking?  The problem is we need to knot-tie the
-- EPS, and we may start adding things to the EPS before type
-- checking starts.
--
-- Here is a concrete example. Suppose we are running
-- "ghc -c A.hs", and we have this file system state:
--
--  A.hs-boot   A.hi-boot **up to date**
--  B.hs        B.hi      **up to date**
--  A.hs        A.hi      **stale**
--
-- The first thing we do is run checkOldIface on A.hi.
-- checkOldIface will call loadInterface on B.hi so it can
-- get its hands on the fingerprints, to find out if A.hi
-- needs recompilation.  But loadInterface also populates
-- the EPS!  And so if compilation turns out to be necessary,
-- as it is in this case, the thunks we put into the EPS for
-- B.hi need to have the correct if_rec_types mutable variable
-- to query.
--
-- If the mutable variable is only allocated WHEN we start
-- typechecking, then that's too late: we can't get the
-- information to the thunks.  So we need to pre-commit
-- to a type variable in 'hscIncrementalCompile' BEFORE we
-- check the old interface.
--
-- This is all a massive hack because arguably checkOldIface
-- should not populate the EPS. But that's a refactor for
-- another day.


data IServ = IServ
  { iservPipe :: Pipe
  , iservProcess :: ProcessHandle
  , iservLookupSymbolCache :: IORef (UniqFM (Ptr ()))
  , iservPendingFrees :: [HValueRef]
  }

-- | Retrieve the ExternalPackageState cache.
hscEPS :: HscEnv -> IO ExternalPackageState
hscEPS hsc_env = readIORef (hsc_EPS hsc_env)

-- | A compilation target.
--
-- A target may be supplied with the actual text of the
-- module.  If so, use this instead of the file contents (this
-- is for use in an IDE where the file hasn't been saved by
-- the user yet).
data Target
  = Target {
      targetId           :: TargetId, -- ^ module or filename
      targetAllowObjCode :: Bool,     -- ^ object code allowed?
      targetContents     :: Maybe (InputFileBuffer, UTCTime)
      -- ^ Optional in-memory buffer containing the source code GHC should
      -- use for this target instead of reading it from disk.
      --
      -- Since GHC version 8.10 modules which require preprocessors such as
      -- Literate Haskell or CPP to run are also supported.
      --
      -- If a corresponding source file does not exist on disk this will
      -- result in a 'SourceError' exception if @targetId = TargetModule _@
      -- is used. However together with @targetId = TargetFile _@ GHC will
      -- not complain about the file missing.
    }

data TargetId
  = TargetModule ModuleName
        -- ^ A module name: search for the file
  | TargetFile FilePath (Maybe Phase)
        -- ^ A filename: preprocess & parse it to find the module name.
        -- If specified, the Phase indicates how to compile this file
        -- (which phase to start from).  Nothing indicates the starting phase
        -- should be determined from the suffix of the filename.
  deriving Eq

type InputFileBuffer = StringBuffer

pprTarget :: Target -> SDoc
pprTarget (Target id obj _) =
    (if obj then char '*' else empty) <> pprTargetId id

instance Outputable Target where
    ppr = pprTarget

pprTargetId :: TargetId -> SDoc
pprTargetId (TargetModule m) = ppr m
pprTargetId (TargetFile f _) = text f

instance Outputable TargetId where
    ppr = pprTargetId

{-
************************************************************************
*                                                                      *
\subsection{Package and Module Tables}
*                                                                      *
************************************************************************
-}

-- | Helps us find information about modules in the home package
type HomePackageTable  = DModuleNameEnv HomeModInfo
        -- Domain = modules in the home package that have been fully compiled
        -- "home" unit id cached here for convenience

-- | Helps us find information about modules in the imported packages
type PackageIfaceTable = ModuleEnv ModIface
        -- Domain = modules in the imported packages

-- | Constructs an empty HomePackageTable
emptyHomePackageTable :: HomePackageTable
emptyHomePackageTable  = emptyUDFM

-- | Constructs an empty PackageIfaceTable
emptyPackageIfaceTable :: PackageIfaceTable
emptyPackageIfaceTable = emptyModuleEnv

pprHPT :: HomePackageTable -> SDoc
-- A bit arbitrary for now
pprHPT hpt = pprUDFM hpt $ \hms ->
    vcat [ hang (ppr (mi_module (hm_iface hm)))
              2 (ppr (md_types (hm_details hm)))
         | hm <- hms ]

lookupHpt :: HomePackageTable -> ModuleName -> Maybe HomeModInfo
lookupHpt = lookupUDFM

lookupHptDirectly :: HomePackageTable -> Unique -> Maybe HomeModInfo
lookupHptDirectly = lookupUDFM_Directly

eltsHpt :: HomePackageTable -> [HomeModInfo]
eltsHpt = eltsUDFM

filterHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> HomePackageTable
filterHpt = filterUDFM

allHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> Bool
allHpt = allUDFM

mapHpt :: (HomeModInfo -> HomeModInfo) -> HomePackageTable -> HomePackageTable
mapHpt = mapUDFM

delFromHpt :: HomePackageTable -> ModuleName -> HomePackageTable
delFromHpt = delFromUDFM

addToHpt :: HomePackageTable -> ModuleName -> HomeModInfo -> HomePackageTable
addToHpt = addToUDFM

addListToHpt
  :: HomePackageTable -> [(ModuleName, HomeModInfo)] -> HomePackageTable
addListToHpt = addListToUDFM

listToHpt :: [(ModuleName, HomeModInfo)] -> HomePackageTable
listToHpt = listToUDFM

lookupHptByModule :: HomePackageTable -> Module -> Maybe HomeModInfo
-- The HPT is indexed by ModuleName, not Module,
-- we must check for a hit on the right Module
lookupHptByModule hpt mod
  = case lookupHpt hpt (moduleName mod) of
      Just hm | mi_module (hm_iface hm) == mod -> Just hm
      _otherwise                               -> Nothing

-- | Information about modules in the package being compiled
data HomeModInfo
  = HomeModInfo {
      hm_iface    :: !ModIface,
        -- ^ The basic loaded interface file: every loaded module has one of
        -- these, even if it is imported from another package
      hm_details  :: !ModDetails,
        -- ^ Extra information that has been created from the 'ModIface' for
        -- the module, typically during typechecking
      hm_linkable :: !(Maybe Linkable)
        -- ^ The actual artifact we would like to link to access things in
        -- this module.
        --
        -- 'hm_linkable' might be Nothing:
        --
        --   1. If this is an .hs-boot module
        --
        --   2. Temporarily during compilation if we pruned away
        --      the old linkable because it was out of date.
        --
        -- After a complete compilation ('GHC.load'), all 'hm_linkable' fields
        -- in the 'HomePackageTable' will be @Just@.
        --
        -- When re-linking a module ('HscMain.HscNoRecomp'), we construct the
        -- 'HomeModInfo' by building a new 'ModDetails' from the old
        -- 'ModIface' (only).
    }

-- | Find the 'ModIface' for a 'Module', searching in both the loaded home
-- and external package module information
lookupIfaceByModule
        :: HomePackageTable
        -> PackageIfaceTable
        -> Module
        -> Maybe ModIface
lookupIfaceByModule hpt pit mod
  = case lookupHptByModule hpt mod of
       Just hm -> Just (hm_iface hm)
       Nothing -> lookupModuleEnv pit mod

-- If the module does come from the home package, why do we look in the PIT as well?
-- (a) In OneShot mode, even home-package modules accumulate in the PIT
-- (b) Even in Batch (--make) mode, there is *one* case where a home-package
--     module is in the PIT, namely GHC.Prim when compiling the base package.
-- We could eliminate (b) if we wanted, by making GHC.Prim belong to a package
-- of its own, but it doesn't seem worth the bother.

hptCompleteSigs :: HscEnv -> [CompleteMatch]
hptCompleteSigs = hptAllThings  (md_complete_sigs . hm_details)

-- | Find all the instance declarations (of classes and families) from
-- the Home Package Table filtered by the provided predicate function.
-- Used in @tcRnImports@, to select the instances that are in the
-- transitive closure of imports from the currently compiled module.
hptInstances :: HscEnv -> (ModuleName -> Bool) -> ([ClsInst], [FamInst])
hptInstances hsc_env want_this_module
  = let (insts, famInsts) = unzip $ flip hptAllThings hsc_env $ \mod_info -> do
                guard (want_this_module (moduleName (mi_module (hm_iface mod_info))))
                let details = hm_details mod_info
                return (md_insts details, md_fam_insts details)
    in (concat insts, concat famInsts)

-- | Get rules from modules "below" this one (in the dependency sense)
hptRules :: HscEnv -> [ModuleNameWithIsBoot] -> [CoreRule]
hptRules = hptSomeThingsBelowUs (md_rules . hm_details) False


-- | Get annotations from modules "below" this one (in the dependency sense)
hptAnns :: HscEnv -> Maybe [ModuleNameWithIsBoot] -> [Annotation]
hptAnns hsc_env (Just deps) = hptSomeThingsBelowUs (md_anns . hm_details) False hsc_env deps
hptAnns hsc_env Nothing = hptAllThings (md_anns . hm_details) hsc_env

hptAllThings :: (HomeModInfo -> [a]) -> HscEnv -> [a]
hptAllThings extract hsc_env = concatMap extract (eltsHpt (hsc_HPT hsc_env))

-- | Get things from modules "below" this one (in the dependency sense)
-- C.f Inst.hptInstances
hptSomeThingsBelowUs :: (HomeModInfo -> [a]) -> Bool -> HscEnv -> [ModuleNameWithIsBoot] -> [a]
hptSomeThingsBelowUs extract include_hi_boot hsc_env deps
  | isOneShot (ghcMode (hsc_dflags hsc_env)) = []

  | otherwise
  = let hpt = hsc_HPT hsc_env
    in
    [ thing
    |   -- Find each non-hi-boot module below me
      ModuleNameWithIsBoot mod is_boot_mod <- deps
    , include_hi_boot || not is_boot_mod

        -- unsavoury: when compiling the base package with --make, we
        -- sometimes try to look up RULES etc for GHC.Prim. GHC.Prim won't
        -- be in the HPT, because we never compile it; it's in the EPT
        -- instead. ToDo: clean up, and remove this slightly bogus filter:
    , mod /= moduleName gHC_PRIM

        -- Look it up in the HPT
    , let things = case lookupHpt hpt mod of
                    Just info -> extract info
                    Nothing -> pprTrace "WARNING in hptSomeThingsBelowUs" msg []
          msg = vcat [text "missing module" <+> ppr mod,
                      text "Probable cause: out-of-date interface files"]
                        -- This really shouldn't happen, but see #962

        -- And get its dfuns
    , thing <- things ]


{-
************************************************************************
*                                                                      *
\subsection{Metaprogramming}
*                                                                      *
************************************************************************
-}

-- | The supported metaprogramming result types
data MetaRequest
  = MetaE  (LHsExpr GhcPs   -> MetaResult)
  | MetaP  (LPat GhcPs      -> MetaResult)
  | MetaT  (LHsType GhcPs   -> MetaResult)
  | MetaD  ([LHsDecl GhcPs] -> MetaResult)
  | MetaAW (Serialized     -> MetaResult)

-- | data constructors not exported to ensure correct result type
data MetaResult
  = MetaResE  { unMetaResE  :: LHsExpr GhcPs   }
  | MetaResP  { unMetaResP  :: LPat GhcPs      }
  | MetaResT  { unMetaResT  :: LHsType GhcPs   }
  | MetaResD  { unMetaResD  :: [LHsDecl GhcPs] }
  | MetaResAW { unMetaResAW :: Serialized        }

type MetaHook f = MetaRequest -> LHsExpr GhcTc -> f MetaResult

metaRequestE :: Functor f => MetaHook f -> LHsExpr GhcTc -> f (LHsExpr GhcPs)
metaRequestE h = fmap unMetaResE . h (MetaE MetaResE)

metaRequestP :: Functor f => MetaHook f -> LHsExpr GhcTc -> f (LPat GhcPs)
metaRequestP h = fmap unMetaResP . h (MetaP MetaResP)

metaRequestT :: Functor f => MetaHook f -> LHsExpr GhcTc -> f (LHsType GhcPs)
metaRequestT h = fmap unMetaResT . h (MetaT MetaResT)

metaRequestD :: Functor f => MetaHook f -> LHsExpr GhcTc -> f [LHsDecl GhcPs]
metaRequestD h = fmap unMetaResD . h (MetaD MetaResD)

metaRequestAW :: Functor f => MetaHook f -> LHsExpr GhcTc -> f Serialized
metaRequestAW h = fmap unMetaResAW . h (MetaAW MetaResAW)

{-
************************************************************************
*                                                                      *
\subsection{Dealing with Annotations}
*                                                                      *
************************************************************************
-}

-- | Deal with gathering annotations in from all possible places
--   and combining them into a single 'AnnEnv'
prepareAnnotations :: HscEnv -> Maybe ModGuts -> IO AnnEnv
prepareAnnotations hsc_env mb_guts = do
    eps <- hscEPS hsc_env
    let -- Extract annotations from the module being compiled if supplied one
        mb_this_module_anns = fmap (mkAnnEnv . mg_anns) mb_guts
        -- Extract dependencies of the module if we are supplied one,
        -- otherwise load annotations from all home package table
        -- entries regardless of dependency ordering.
        home_pkg_anns  = (mkAnnEnv . hptAnns hsc_env) $ fmap (dep_mods . mg_deps) mb_guts
        other_pkg_anns = eps_ann_env eps
        ann_env        = foldl1' plusAnnEnv $ catMaybes [mb_this_module_anns,
                                                         Just home_pkg_anns,
                                                         Just other_pkg_anns]
    return ann_env

{-
************************************************************************
*                                                                      *
\subsection{The Finder cache}
*                                                                      *
************************************************************************
-}

-- | The 'FinderCache' maps modules to the result of
-- searching for that module. It records the results of searching for
-- modules along the search path. On @:load@, we flush the entire
-- contents of this cache.
--
type FinderCache = InstalledModuleEnv InstalledFindResult

data InstalledFindResult
  = InstalledFound ModLocation InstalledModule
  | InstalledNoPackage InstalledUnitId
  | InstalledNotFound [FilePath] (Maybe InstalledUnitId)

-- | The result of searching for an imported module.
--
-- NB: FindResult manages both user source-import lookups
-- (which can result in 'Module') as well as direct imports
-- for interfaces (which always result in 'InstalledModule').
data FindResult
  = Found ModLocation Module
        -- ^ The module was found
  | NoPackage UnitId
        -- ^ The requested package was not found
  | FoundMultiple [(Module, ModuleOrigin)]
        -- ^ _Error_: both in multiple packages

        -- | Not found
  | NotFound
      { fr_paths       :: [FilePath]       -- Places where I looked

      , fr_pkg         :: Maybe UnitId  -- Just p => module is in this package's
                                           --           manifest, but couldn't find
                                           --           the .hi file

      , fr_mods_hidden :: [UnitId]      -- Module is in these packages,
                                           --   but the *module* is hidden

      , fr_pkgs_hidden :: [UnitId]      -- Module is in these packages,
                                           --   but the *package* is hidden

        -- Modules are in these packages, but it is unusable
      , fr_unusables   :: [(UnitId, UnusablePackageReason)]

      , fr_suggestions :: [ModuleSuggestion] -- Possible mis-spelled modules
      }

{-
************************************************************************
*                                                                      *
\subsection{Symbol tables and Module details}
*                                                                      *
************************************************************************
-}

{- Note [Interface file stages]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Interface files have two possible stages.

* A partial stage built from the result of the core pipeline.
* A fully instantiated form. Which also includes fingerprints and
  potentially information provided by backends.

We can build a full interface file two ways:
* Directly from a partial one:
  Then we omit backend information and mostly compute fingerprints.
* From a partial one + information produced by a backend.
  Then we store the provided information and fingerprint both.
-}

type PartialModIface = ModIface_ 'ModIfaceCore
type ModIface = ModIface_ 'ModIfaceFinal

-- | Extends a PartialModIface with information which is either:
-- * Computed after codegen
-- * Or computed just before writing the iface to disk. (Hashes)
-- In order to fully instantiate it.
data ModIfaceBackend = ModIfaceBackend
  { mi_iface_hash :: !Fingerprint
    -- ^ Hash of the whole interface
  , mi_mod_hash :: !Fingerprint
    -- ^ Hash of the ABI only
  , mi_flag_hash :: !Fingerprint
    -- ^ Hash of the important flags used when compiling the module, excluding
    -- optimisation flags
  , mi_opt_hash :: !Fingerprint
    -- ^ Hash of optimisation flags
  , mi_hpc_hash :: !Fingerprint
    -- ^ Hash of hpc flags
  , mi_plugin_hash :: !Fingerprint
    -- ^ Hash of plugins
  , mi_orphan :: !WhetherHasOrphans
    -- ^ Whether this module has orphans
  , mi_finsts :: !WhetherHasFamInst
    -- ^ Whether this module has family instances. See Note [The type family
    -- instance consistency story].
  , mi_exp_hash :: !Fingerprint
    -- ^ Hash of export list
  , mi_orphan_hash :: !Fingerprint
    -- ^ Hash for orphan rules, class and family instances combined

    -- Cached environments for easy lookup. These are computed (lazily) from
    -- other fields and are not put into the interface file.
    -- Not really produced by the backend but there is no need to create them
    -- any earlier.
  , mi_warn_fn :: !(OccName -> Maybe WarningTxt)
    -- ^ Cached lookup for 'mi_warns'
  , mi_fix_fn :: !(OccName -> Maybe Fixity)
    -- ^ Cached lookup for 'mi_fixities'
  , mi_hash_fn :: !(OccName -> Maybe (OccName, Fingerprint))
    -- ^ Cached lookup for 'mi_decls'. The @Nothing@ in 'mi_hash_fn' means that
    -- the thing isn't in decls. It's useful to know that when seeing if we are
    -- up to date wrt. the old interface. The 'OccName' is the parent of the
    -- name, if it has one.
  }

data ModIfacePhase
  = ModIfaceCore
  -- ^ Partial interface built based on output of core pipeline.
  | ModIfaceFinal

-- | Selects a IfaceDecl representation.
-- For fully instantiated interfaces we also maintain
-- a fingerprint, which is used for recompilation checks.
type family IfaceDeclExts (phase :: ModIfacePhase) where
  IfaceDeclExts 'ModIfaceCore = IfaceDecl
  IfaceDeclExts 'ModIfaceFinal = (Fingerprint, IfaceDecl)

type family IfaceBackendExts (phase :: ModIfacePhase) where
  IfaceBackendExts 'ModIfaceCore = ()
  IfaceBackendExts 'ModIfaceFinal = ModIfaceBackend



-- | A 'ModIface' plus a 'ModDetails' summarises everything we know
-- about a compiled module.  The 'ModIface' is the stuff *before* linking,
-- and can be written out to an interface file. The 'ModDetails is after
-- linking and can be completely recovered from just the 'ModIface'.
--
-- When we read an interface file, we also construct a 'ModIface' from it,
-- except that we explicitly make the 'mi_decls' and a few other fields empty;
-- as when reading we consolidate the declarations etc. into a number of indexed
-- maps and environments in the 'ExternalPackageState'.
data ModIface_ (phase :: ModIfacePhase)
  = ModIface {
        mi_module     :: !Module,             -- ^ Name of the module we are for
        mi_sig_of     :: !(Maybe Module),     -- ^ Are we a sig of another mod?

        mi_hsc_src    :: !HscSource,          -- ^ Boot? Signature?

        mi_deps     :: Dependencies,
                -- ^ The dependencies of the module.  This is
                -- consulted for directly-imported modules, but not
                -- for anything else (hence lazy)

        mi_usages   :: [Usage],
                -- ^ Usages; kept sorted so that it's easy to decide
                -- whether to write a new iface file (changing usages
                -- doesn't affect the hash of this module)
                -- NOT STRICT!  we read this field lazily from the interface file
                -- It is *only* consulted by the recompilation checker

        mi_exports  :: ![IfaceExport],
                -- ^ Exports
                -- Kept sorted by (mod,occ), to make version comparisons easier
                -- Records the modules that are the declaration points for things
                -- exported by this module, and the 'OccName's of those things


        mi_used_th  :: !Bool,
                -- ^ Module required TH splices when it was compiled.
                -- This disables recompilation avoidance (see #481).

        mi_fixities :: [(OccName,Fixity)],
                -- ^ Fixities
                -- NOT STRICT!  we read this field lazily from the interface file

        mi_warns    :: Warnings,
                -- ^ Warnings
                -- NOT STRICT!  we read this field lazily from the interface file

        mi_anns     :: [IfaceAnnotation],
                -- ^ Annotations
                -- NOT STRICT!  we read this field lazily from the interface file


        mi_decls    :: [IfaceDeclExts phase],
                -- ^ Type, class and variable declarations
                -- The hash of an Id changes if its fixity or deprecations change
                --      (as well as its type of course)
                -- Ditto data constructors, class operations, except that
                -- the hash of the parent class/tycon changes

        mi_globals  :: !(Maybe GlobalRdrEnv),
                -- ^ Binds all the things defined at the top level in
                -- the /original source/ code for this module. which
                -- is NOT the same as mi_exports, nor mi_decls (which
                -- may contains declarations for things not actually
                -- defined by the user).  Used for GHCi and for inspecting
                -- the contents of modules via the GHC API only.
                --
                -- (We need the source file to figure out the
                -- top-level environment, if we didn't compile this module
                -- from source then this field contains @Nothing@).
                --
                -- Strictly speaking this field should live in the
                -- 'HomeModInfo', but that leads to more plumbing.

                -- Instance declarations and rules
        mi_insts       :: [IfaceClsInst],     -- ^ Sorted class instance
        mi_fam_insts   :: [IfaceFamInst],  -- ^ Sorted family instances
        mi_rules       :: [IfaceRule],     -- ^ Sorted rules

        mi_hpc       :: !AnyHpcUsage,
                -- ^ True if this program uses Hpc at any point in the program.

        mi_trust     :: !IfaceTrustInfo,
                -- ^ Safe Haskell Trust information for this module.

        mi_trust_pkg :: !Bool,
                -- ^ Do we require the package this module resides in be trusted
                -- to trust this module? This is used for the situation where a
                -- module is Safe (so doesn't require the package be trusted
                -- itself) but imports some trustworthy modules from its own
                -- package (which does require its own package be trusted).
                -- See Note [Trust Own Package] in GHC.Rename.Names
        mi_complete_sigs :: [IfaceCompleteMatch],

        mi_doc_hdr :: Maybe HsDocString,
                -- ^ Module header.

        mi_decl_docs :: DeclDocMap,
                -- ^ Docs on declarations.

        mi_arg_docs :: ArgDocMap,
                -- ^ Docs on arguments.

        mi_final_exts :: !(IfaceBackendExts phase)
                -- ^ Either `()` or `ModIfaceBackend` for
                -- a fully instantiated interface.
     }

-- | Old-style accessor for whether or not the ModIface came from an hs-boot
-- file.
mi_boot :: ModIface -> Bool
mi_boot iface = mi_hsc_src iface == HsBootFile

-- | Lookups up a (possibly cached) fixity from a 'ModIface'. If one cannot be
-- found, 'defaultFixity' is returned instead.
mi_fix :: ModIface -> OccName -> Fixity
mi_fix iface name = mi_fix_fn (mi_final_exts iface) name `orElse` defaultFixity

-- | The semantic module for this interface; e.g., if it's a interface
-- for a signature, if 'mi_module' is @p[A=<A>]:A@, 'mi_semantic_module'
-- will be @<A>@.
mi_semantic_module :: ModIface_ a -> Module
mi_semantic_module iface = case mi_sig_of iface of
                            Nothing -> mi_module iface
                            Just mod -> mod

-- | The "precise" free holes, e.g., the signatures that this
-- 'ModIface' depends on.
mi_free_holes :: ModIface -> UniqDSet ModuleName
mi_free_holes iface =
  case splitModuleInsts (mi_module iface) of
    (_, Just indef)
        -- A mini-hack: we rely on the fact that 'renameFreeHoles'
        -- drops things that aren't holes.
        -> renameFreeHoles (mkUniqDSet cands) (indefUnitIdInsts (indefModuleUnitId indef))
    _   -> emptyUniqDSet
  where
    cands = map mnwib_moduleName $ dep_mods $ mi_deps iface

-- | Given a set of free holes, and a unit identifier, rename
-- the free holes according to the instantiation of the unit
-- identifier.  For example, if we have A and B free, and
-- our unit identity is @p[A=<C>,B=impl:B]@, the renamed free
-- holes are just C.
renameFreeHoles :: UniqDSet ModuleName -> [(ModuleName, Module)] -> UniqDSet ModuleName
renameFreeHoles fhs insts =
    unionManyUniqDSets (map lookup_impl (uniqDSetToList fhs))
  where
    hmap = listToUFM insts
    lookup_impl mod_name
        | Just mod <- lookupUFM hmap mod_name = moduleFreeHoles mod
        -- It wasn't actually a hole
        | otherwise                           = emptyUniqDSet

instance Binary ModIface where
   put_ bh (ModIface {
                 mi_module    = mod,
                 mi_sig_of    = sig_of,
                 mi_hsc_src   = hsc_src,
                 mi_deps      = deps,
                 mi_usages    = usages,
                 mi_exports   = exports,
                 mi_used_th   = used_th,
                 mi_fixities  = fixities,
                 mi_warns     = warns,
                 mi_anns      = anns,
                 mi_decls     = decls,
                 mi_insts     = insts,
                 mi_fam_insts = fam_insts,
                 mi_rules     = rules,
                 mi_hpc       = hpc_info,
                 mi_trust     = trust,
                 mi_trust_pkg = trust_pkg,
                 mi_complete_sigs = complete_sigs,
                 mi_doc_hdr   = doc_hdr,
                 mi_decl_docs = decl_docs,
                 mi_arg_docs  = arg_docs,
                 mi_final_exts = ModIfaceBackend {
                   mi_iface_hash = iface_hash,
                   mi_mod_hash = mod_hash,
                   mi_flag_hash = flag_hash,
                   mi_opt_hash = opt_hash,
                   mi_hpc_hash = hpc_hash,
                   mi_plugin_hash = plugin_hash,
                   mi_orphan = orphan,
                   mi_finsts = hasFamInsts,
                   mi_exp_hash = exp_hash,
                   mi_orphan_hash = orphan_hash
                 }}) = do
        put_ bh mod
        put_ bh sig_of
        put_ bh hsc_src
        put_ bh iface_hash
        put_ bh mod_hash
        put_ bh flag_hash
        put_ bh opt_hash
        put_ bh hpc_hash
        put_ bh plugin_hash
        put_ bh orphan
        put_ bh hasFamInsts
        lazyPut bh deps
        lazyPut bh usages
        put_ bh exports
        put_ bh exp_hash
        put_ bh used_th
        put_ bh fixities
        lazyPut bh warns
        lazyPut bh anns
        put_ bh decls
        put_ bh insts
        put_ bh fam_insts
        lazyPut bh rules
        put_ bh orphan_hash
        put_ bh hpc_info
        put_ bh trust
        put_ bh trust_pkg
        put_ bh complete_sigs
        lazyPut bh doc_hdr
        lazyPut bh decl_docs
        lazyPut bh arg_docs

   get bh = do
        mod         <- get bh
        sig_of      <- get bh
        hsc_src     <- get bh
        iface_hash  <- get bh
        mod_hash    <- get bh
        flag_hash   <- get bh
        opt_hash    <- get bh
        hpc_hash    <- get bh
        plugin_hash <- get bh
        orphan      <- get bh
        hasFamInsts <- get bh
        deps        <- lazyGet bh
        usages      <- {-# SCC "bin_usages" #-} lazyGet bh
        exports     <- {-# SCC "bin_exports" #-} get bh
        exp_hash    <- get bh
        used_th     <- get bh
        fixities    <- {-# SCC "bin_fixities" #-} get bh
        warns       <- {-# SCC "bin_warns" #-} lazyGet bh
        anns        <- {-# SCC "bin_anns" #-} lazyGet bh
        decls       <- {-# SCC "bin_tycldecls" #-} get bh
        insts       <- {-# SCC "bin_insts" #-} get bh
        fam_insts   <- {-# SCC "bin_fam_insts" #-} get bh
        rules       <- {-# SCC "bin_rules" #-} lazyGet bh
        orphan_hash <- get bh
        hpc_info    <- get bh
        trust       <- get bh
        trust_pkg   <- get bh
        complete_sigs <- get bh
        doc_hdr     <- lazyGet bh
        decl_docs   <- lazyGet bh
        arg_docs    <- lazyGet bh
        return (ModIface {
                 mi_module      = mod,
                 mi_sig_of      = sig_of,
                 mi_hsc_src     = hsc_src,
                 mi_deps        = deps,
                 mi_usages      = usages,
                 mi_exports     = exports,
                 mi_used_th     = used_th,
                 mi_anns        = anns,
                 mi_fixities    = fixities,
                 mi_warns       = warns,
                 mi_decls       = decls,
                 mi_globals     = Nothing,
                 mi_insts       = insts,
                 mi_fam_insts   = fam_insts,
                 mi_rules       = rules,
                 mi_hpc         = hpc_info,
                 mi_trust       = trust,
                 mi_trust_pkg   = trust_pkg,
                        -- And build the cached values
                 mi_complete_sigs = complete_sigs,
                 mi_doc_hdr     = doc_hdr,
                 mi_decl_docs   = decl_docs,
                 mi_arg_docs    = arg_docs,
                 mi_final_exts = ModIfaceBackend {
                   mi_iface_hash = iface_hash,
                   mi_mod_hash = mod_hash,
                   mi_flag_hash = flag_hash,
                   mi_opt_hash = opt_hash,
                   mi_hpc_hash = hpc_hash,
                   mi_plugin_hash = plugin_hash,
                   mi_orphan = orphan,
                   mi_finsts = hasFamInsts,
                   mi_exp_hash = exp_hash,
                   mi_orphan_hash = orphan_hash,
                   mi_warn_fn = mkIfaceWarnCache warns,
                   mi_fix_fn = mkIfaceFixCache fixities,
                   mi_hash_fn = mkIfaceHashCache decls
                 }})

-- | The original names declared of a certain module that are exported
type IfaceExport = AvailInfo

emptyPartialModIface :: Module -> PartialModIface
emptyPartialModIface mod
  = ModIface { mi_module      = mod,
               mi_sig_of      = Nothing,
               mi_hsc_src     = HsSrcFile,
               mi_deps        = noDependencies,
               mi_usages      = [],
               mi_exports     = [],
               mi_used_th     = False,
               mi_fixities    = [],
               mi_warns       = NoWarnings,
               mi_anns        = [],
               mi_insts       = [],
               mi_fam_insts   = [],
               mi_rules       = [],
               mi_decls       = [],
               mi_globals     = Nothing,
               mi_hpc         = False,
               mi_trust       = noIfaceTrustInfo,
               mi_trust_pkg   = False,
               mi_complete_sigs = [],
               mi_doc_hdr     = Nothing,
               mi_decl_docs   = emptyDeclDocMap,
               mi_arg_docs    = emptyArgDocMap,
               mi_final_exts        = () }

emptyFullModIface :: Module -> ModIface
emptyFullModIface mod =
    (emptyPartialModIface mod)
      { mi_decls = []
      , mi_final_exts = ModIfaceBackend
        { mi_iface_hash = fingerprint0,
          mi_mod_hash = fingerprint0,
          mi_flag_hash = fingerprint0,
          mi_opt_hash = fingerprint0,
          mi_hpc_hash = fingerprint0,
          mi_plugin_hash = fingerprint0,
          mi_orphan = False,
          mi_finsts = False,
          mi_exp_hash = fingerprint0,
          mi_orphan_hash = fingerprint0,
          mi_warn_fn = emptyIfaceWarnCache,
          mi_fix_fn = emptyIfaceFixCache,
          mi_hash_fn = emptyIfaceHashCache } }

-- | Constructs cache for the 'mi_hash_fn' field of a 'ModIface'
mkIfaceHashCache :: [(Fingerprint,IfaceDecl)]
                 -> (OccName -> Maybe (OccName, Fingerprint))
mkIfaceHashCache pairs
  = \occ -> lookupOccEnv env occ
  where
    env = foldl' add_decl emptyOccEnv pairs
    add_decl env0 (v,d) = foldl' add env0 (ifaceDeclFingerprints v d)
      where
        add env0 (occ,hash) = extendOccEnv env0 occ (occ,hash)

emptyIfaceHashCache :: OccName -> Maybe (OccName, Fingerprint)
emptyIfaceHashCache _occ = Nothing


-- | The 'ModDetails' is essentially a cache for information in the 'ModIface'
-- for home modules only. Information relating to packages will be loaded into
-- global environments in 'ExternalPackageState'.
data ModDetails
  = ModDetails {
        -- The next two fields are created by the typechecker
        md_exports   :: [AvailInfo],
        md_types     :: !TypeEnv,       -- ^ Local type environment for this particular module
                                        -- Includes Ids, TyCons, PatSyns
        md_insts     :: ![ClsInst],     -- ^ 'DFunId's for the instances in this module
        md_fam_insts :: ![FamInst],
        md_rules     :: ![CoreRule],    -- ^ Domain may include 'Id's from other modules
        md_anns      :: ![Annotation],  -- ^ Annotations present in this module: currently
                                        -- they only annotate things also declared in this module
        md_complete_sigs :: [CompleteMatch]
          -- ^ Complete match pragmas for this module
     }

-- | Constructs an empty ModDetails
emptyModDetails :: ModDetails
emptyModDetails
  = ModDetails { md_types     = emptyTypeEnv,
                 md_exports   = [],
                 md_insts     = [],
                 md_rules     = [],
                 md_fam_insts = [],
                 md_anns      = [],
                 md_complete_sigs = [] }

-- | Records the modules directly imported by a module for extracting e.g.
-- usage information, and also to give better error message
type ImportedMods = ModuleEnv [ImportedBy]

-- | If a module was "imported" by the user, we associate it with
-- more detailed usage information 'ImportedModsVal'; a module
-- imported by the system only gets used for usage information.
data ImportedBy
    = ImportedByUser ImportedModsVal
    | ImportedBySystem

importedByUser :: [ImportedBy] -> [ImportedModsVal]
importedByUser (ImportedByUser imv : bys) = imv : importedByUser bys
importedByUser (ImportedBySystem   : bys) =       importedByUser bys
importedByUser [] = []

data ImportedModsVal
 = ImportedModsVal {
        imv_name :: ModuleName,          -- ^ The name the module is imported with
        imv_span :: SrcSpan,             -- ^ the source span of the whole import
        imv_is_safe :: IsSafeImport,     -- ^ whether this is a safe import
        imv_is_hiding :: Bool,           -- ^ whether this is an "hiding" import
        imv_all_exports :: !GlobalRdrEnv, -- ^ all the things the module could provide
          -- NB. BangPattern here: otherwise this leaks. (#15111)
        imv_qualified :: Bool            -- ^ whether this is a qualified import
        }

-- | A ModGuts is carried through the compiler, accumulating stuff as it goes
-- There is only one ModGuts at any time, the one for the module
-- being compiled right now.  Once it is compiled, a 'ModIface' and
-- 'ModDetails' are extracted and the ModGuts is discarded.
data ModGuts
  = ModGuts {
        mg_module    :: !Module,         -- ^ Module being compiled
        mg_hsc_src   :: HscSource,       -- ^ Whether it's an hs-boot module
        mg_loc       :: SrcSpan,         -- ^ For error messages from inner passes
        mg_exports   :: ![AvailInfo],    -- ^ What it exports
        mg_deps      :: !Dependencies,   -- ^ What it depends on, directly or
                                         -- otherwise
        mg_usages    :: ![Usage],        -- ^ What was used?  Used for interfaces.

        mg_used_th   :: !Bool,           -- ^ Did we run a TH splice?
        mg_rdr_env   :: !GlobalRdrEnv,   -- ^ Top-level lexical environment

        -- These fields all describe the things **declared in this module**
        mg_fix_env   :: !FixityEnv,      -- ^ Fixities declared in this module.
                                         -- Used for creating interface files.
        mg_tcs       :: ![TyCon],        -- ^ TyCons declared in this module
                                         -- (includes TyCons for classes)
        mg_insts     :: ![ClsInst],      -- ^ Class instances declared in this module
        mg_fam_insts :: ![FamInst],
                                         -- ^ Family instances declared in this module
        mg_patsyns   :: ![PatSyn],       -- ^ Pattern synonyms declared in this module
        mg_rules     :: ![CoreRule],     -- ^ Before the core pipeline starts, contains
                                         -- See Note [Overall plumbing for rules] in Rules.hs
        mg_binds     :: !CoreProgram,    -- ^ Bindings for this module
        mg_foreign   :: !ForeignStubs,   -- ^ Foreign exports declared in this module
        mg_foreign_files :: ![(ForeignSrcLang, FilePath)],
        -- ^ Files to be compiled with the C compiler
        mg_warns     :: !Warnings,       -- ^ Warnings declared in the module
        mg_anns      :: [Annotation],    -- ^ Annotations declared in this module
        mg_complete_sigs :: [CompleteMatch], -- ^ Complete Matches
        mg_hpc_info  :: !HpcInfo,        -- ^ Coverage tick boxes in the module
        mg_modBreaks :: !(Maybe ModBreaks), -- ^ Breakpoints for the module

                        -- The next two fields are unusual, because they give instance
                        -- environments for *all* modules in the home package, including
                        -- this module, rather than for *just* this module.
                        -- Reason: when looking up an instance we don't want to have to
                        --         look at each module in the home package in turn
        mg_inst_env     :: InstEnv,             -- ^ Class instance environment for
                                                -- /home-package/ modules (including this
                                                -- one); c.f. 'tcg_inst_env'
        mg_fam_inst_env :: FamInstEnv,          -- ^ Type-family instance environment for
                                                -- /home-package/ modules (including this
                                                -- one); c.f. 'tcg_fam_inst_env'

        mg_safe_haskell :: SafeHaskellMode,     -- ^ Safe Haskell mode
        mg_trust_pkg    :: Bool,                -- ^ Do we need to trust our
                                                -- own package for Safe Haskell?
                                                -- See Note [Trust Own Package]
                                                -- in GHC.Rename.Names

        mg_doc_hdr       :: !(Maybe HsDocString), -- ^ Module header.
        mg_decl_docs     :: !DeclDocMap,     -- ^ Docs on declarations.
        mg_arg_docs      :: !ArgDocMap       -- ^ Docs on arguments.
    }

-- The ModGuts takes on several slightly different forms:
--
-- After simplification, the following fields change slightly:
--      mg_rules        Orphan rules only (local ones now attached to binds)
--      mg_binds        With rules attached

---------------------------------------------------------
-- The Tidy pass forks the information about this module:
--      * one lot goes to interface file generation (ModIface)
--        and later compilations (ModDetails)
--      * the other lot goes to code generation (CgGuts)

-- | A restricted form of 'ModGuts' for code generation purposes
data CgGuts
  = CgGuts {
        cg_module    :: !Module,
                -- ^ Module being compiled

        cg_tycons    :: [TyCon],
                -- ^ Algebraic data types (including ones that started
                -- life as classes); generate constructors and info
                -- tables. Includes newtypes, just for the benefit of
                -- External Core

        cg_binds     :: CoreProgram,
                -- ^ The tidied main bindings, including
                -- previously-implicit bindings for record and class
                -- selectors, and data constructor wrappers.  But *not*
                -- data constructor workers; reason: we regard them
                -- as part of the code-gen of tycons

        cg_foreign   :: !ForeignStubs,   -- ^ Foreign export stubs
        cg_foreign_files :: ![(ForeignSrcLang, FilePath)],
        cg_dep_pkgs  :: ![InstalledUnitId], -- ^ Dependent packages, used to
                                            -- generate #includes for C code gen
        cg_hpc_info  :: !HpcInfo,           -- ^ Program coverage tick box information
        cg_modBreaks :: !(Maybe ModBreaks), -- ^ Module breakpoints
        cg_spt_entries :: [SptEntry]
                -- ^ Static pointer table entries for static forms defined in
                -- the module.
                -- See Note [Grand plan for static forms] in StaticPtrTable
    }

-----------------------------------
-- | Foreign export stubs
data ForeignStubs
  = NoStubs
      -- ^ We don't have any stubs
  | ForeignStubs SDoc SDoc
      -- ^ There are some stubs. Parameters:
      --
      --  1) Header file prototypes for
      --     "foreign exported" functions
      --
      --  2) C stubs to use when calling
      --     "foreign exported" functions

appendStubC :: ForeignStubs -> SDoc -> ForeignStubs
appendStubC NoStubs            c_code = ForeignStubs empty c_code
appendStubC (ForeignStubs h c) c_code = ForeignStubs h (c $$ c_code)

{-
************************************************************************
*                                                                      *
                The interactive context
*                                                                      *
************************************************************************

Note [The interactive package]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type, class, and value declarations at the command prompt are treated
as if they were defined in modules
   interactive:Ghci1
   interactive:Ghci2
   ...etc...
with each bunch of declarations using a new module, all sharing a
common package 'interactive' (see Module.interactiveUnitId, and
PrelNames.mkInteractiveModule).

This scheme deals well with shadowing.  For example:

   ghci> data T = A
   ghci> data T = B
   ghci> :i A
   data Ghci1.T = A  -- Defined at <interactive>:2:10

Here we must display info about constructor A, but its type T has been
shadowed by the second declaration.  But it has a respectable
qualified name (Ghci1.T), and its source location says where it was
defined.

So the main invariant continues to hold, that in any session an
original name M.T only refers to one unique thing.  (In a previous
iteration both the T's above were called :Interactive.T, albeit with
different uniques, which gave rise to all sorts of trouble.)

The details are a bit tricky though:

 * The field ic_mod_index counts which Ghci module we've got up to.
   It is incremented when extending ic_tythings

 * ic_tythings contains only things from the 'interactive' package.

 * Module from the 'interactive' package (Ghci1, Ghci2 etc) never go
   in the Home Package Table (HPT).  When you say :load, that's when we
   extend the HPT.

 * The 'thisPackage' field of DynFlags is *not* set to 'interactive'.
   It stays as 'main' (or whatever -this-unit-id says), and is the
   package to which :load'ed modules are added to.

 * So how do we arrange that declarations at the command prompt get to
   be in the 'interactive' package?  Simply by setting the tcg_mod
   field of the TcGblEnv to "interactive:Ghci1".  This is done by the
   call to initTc in initTcInteractive, which in turn get the module
   from it 'icInteractiveModule' field of the interactive context.

   The 'thisPackage' field stays as 'main' (or whatever -this-unit-id says.

 * The main trickiness is that the type environment (tcg_type_env) and
   fixity envt (tcg_fix_env), now contain entities from all the
   interactive-package modules (Ghci1, Ghci2, ...) together, rather
   than just a single module as is usually the case.  So you can't use
   "nameIsLocalOrFrom" to decide whether to look in the TcGblEnv vs
   the HPT/PTE.  This is a change, but not a problem provided you
   know.

* However, the tcg_binds, tcg_sigs, tcg_insts, tcg_fam_insts, etc fields
  of the TcGblEnv, which collect "things defined in this module", all
  refer to stuff define in a single GHCi command, *not* all the commands
  so far.

  In contrast, tcg_inst_env, tcg_fam_inst_env, have instances from
  all GhciN modules, which makes sense -- they are all "home package"
  modules.


Note [Interactively-bound Ids in GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Ids bound by previous Stmts in GHCi are currently
        a) GlobalIds, with
        b) An External Name, like Ghci4.foo
           See Note [The interactive package] above
        c) A tidied type

 (a) They must be GlobalIds (not LocalIds) otherwise when we come to
     compile an expression using these ids later, the byte code
     generator will consider the occurrences to be free rather than
     global.

 (b) Having an External Name is important because of Note
     [GlobalRdrEnv shadowing] in RdrName

 (c) Their types are tidied. This is important, because :info may ask
     to look at them, and :info expects the things it looks up to have
     tidy types

Where do interactively-bound Ids come from?

  - GHCi REPL Stmts   e.g.
         ghci> let foo x = x+1
    These start with an Internal Name because a Stmt is a local
    construct, so the renamer naturally builds an Internal name for
    each of its binders.  Then in tcRnStmt they are externalised via
    TcRnDriver.externaliseAndTidyId, so they get Names like Ghic4.foo.

  - Ids bound by the debugger etc have Names constructed by
    GHC.Iface.Env.newInteractiveBinder; at the call sites it is followed by
    mkVanillaGlobal or mkVanillaGlobalWithInfo.  So again, they are
    all Global, External.

  - TyCons, Classes, and Ids bound by other top-level declarations in
    GHCi (eg foreign import, record selectors) also get External
    Names, with Ghci9 (or 8, or 7, etc) as the module name.


Note [ic_tythings]
~~~~~~~~~~~~~~~~~~
The ic_tythings field contains
  * The TyThings declared by the user at the command prompt
    (eg Ids, TyCons, Classes)

  * The user-visible Ids that arise from such things, which
    *don't* come from 'implicitTyThings', notably:
       - record selectors
       - class ops
    The implicitTyThings are readily obtained from the TyThings
    but record selectors etc are not

It does *not* contain
  * DFunIds (they can be gotten from ic_instances)
  * CoAxioms (ditto)

See also Note [Interactively-bound Ids in GHCi]

Note [Override identical instances in GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If you declare a new instance in GHCi that is identical to a previous one,
we simply override the previous one; we don't regard it as overlapping.
e.g.    Prelude> data T = A | B
        Prelude> instance Eq T where ...
        Prelude> instance Eq T where ...   -- This one overrides

It's exactly the same for type-family instances.  See #7102
-}

-- | Interactive context, recording information about the state of the
-- context in which statements are executed in a GHCi session.
data InteractiveContext
  = InteractiveContext {
         ic_dflags     :: DynFlags,
             -- ^ The 'DynFlags' used to evaluate interactive expressions
             -- and statements.

         ic_mod_index :: Int,
             -- ^ Each GHCi stmt or declaration brings some new things into
             -- scope. We give them names like interactive:Ghci9.T,
             -- where the ic_index is the '9'.  The ic_mod_index is
             -- incremented whenever we add something to ic_tythings
             -- See Note [The interactive package]

         ic_imports :: [InteractiveImport],
             -- ^ The GHCi top-level scope (ic_rn_gbl_env) is extended with
             -- these imports
             --
             -- This field is only stored here so that the client
             -- can retrieve it with GHC.getContext. GHC itself doesn't
             -- use it, but does reset it to empty sometimes (such
             -- as before a GHC.load). The context is set with GHC.setContext.

         ic_tythings   :: [TyThing],
             -- ^ TyThings defined by the user, in reverse order of
             -- definition (ie most recent at the front)
             -- See Note [ic_tythings]

         ic_rn_gbl_env :: GlobalRdrEnv,
             -- ^ The cached 'GlobalRdrEnv', built by
             -- 'InteractiveEval.setContext' and updated regularly
             -- It contains everything in scope at the command line,
             -- including everything in ic_tythings

         ic_instances  :: ([ClsInst], [FamInst]),
             -- ^ All instances and family instances created during
             -- this session.  These are grabbed en masse after each
             -- update to be sure that proper overlapping is retained.
             -- That is, rather than re-check the overlapping each
             -- time we update the context, we just take the results
             -- from the instance code that already does that.

         ic_fix_env :: FixityEnv,
            -- ^ Fixities declared in let statements

         ic_default :: Maybe [Type],
             -- ^ The current default types, set by a 'default' declaration

          ic_resume :: [Resume],
             -- ^ The stack of breakpoint contexts

         ic_monad      :: Name,
             -- ^ The monad that GHCi is executing in

         ic_int_print  :: Name,
             -- ^ The function that is used for printing results
             -- of expressions in ghci and -e mode.

         ic_cwd :: Maybe FilePath
             -- virtual CWD of the program
    }

data InteractiveImport
  = IIDecl (ImportDecl GhcPs)
      -- ^ Bring the exports of a particular module
      -- (filtered by an import decl) into scope

  | IIModule ModuleName
      -- ^ Bring into scope the entire top-level envt of
      -- of this module, including the things imported
      -- into it.


-- | Constructs an empty InteractiveContext.
emptyInteractiveContext :: DynFlags -> InteractiveContext
emptyInteractiveContext dflags
  = InteractiveContext {
       ic_dflags     = dflags,
       ic_imports    = [],
       ic_rn_gbl_env = emptyGlobalRdrEnv,
       ic_mod_index  = 1,
       ic_tythings   = [],
       ic_instances  = ([],[]),
       ic_fix_env    = emptyNameEnv,
       ic_monad      = ioTyConName,  -- IO monad by default
       ic_int_print  = printName,    -- System.IO.print by default
       ic_default    = Nothing,
       ic_resume     = [],
       ic_cwd        = Nothing }

icInteractiveModule :: InteractiveContext -> Module
icInteractiveModule (InteractiveContext { ic_mod_index = index })
  = mkInteractiveModule index

-- | This function returns the list of visible TyThings (useful for
-- e.g. showBindings)
icInScopeTTs :: InteractiveContext -> [TyThing]
icInScopeTTs = ic_tythings

-- | Get the PrintUnqualified function based on the flags and this InteractiveContext
icPrintUnqual :: DynFlags -> InteractiveContext -> PrintUnqualified
icPrintUnqual dflags InteractiveContext{ ic_rn_gbl_env = grenv } =
    mkPrintUnqualified dflags grenv

-- | extendInteractiveContext is called with new TyThings recently defined to update the
-- InteractiveContext to include them.  Ids are easily removed when shadowed,
-- but Classes and TyCons are not.  Some work could be done to determine
-- whether they are entirely shadowed, but as you could still have references
-- to them (e.g. instances for classes or values of the type for TyCons), it's
-- not clear whether removing them is even the appropriate behavior.
extendInteractiveContext :: InteractiveContext
                         -> [TyThing]
                         -> [ClsInst] -> [FamInst]
                         -> Maybe [Type]
                         -> FixityEnv
                         -> InteractiveContext
extendInteractiveContext ictxt new_tythings new_cls_insts new_fam_insts defaults fix_env
  = ictxt { ic_mod_index  = ic_mod_index ictxt + 1
                            -- Always bump this; even instances should create
                            -- a new mod_index (#9426)
          , ic_tythings   = new_tythings ++ old_tythings
          , ic_rn_gbl_env = ic_rn_gbl_env ictxt `icExtendGblRdrEnv` new_tythings
          , ic_instances  = ( new_cls_insts ++ old_cls_insts
                            , new_fam_insts ++ fam_insts )
                            -- we don't shadow old family instances (#7102),
                            -- so don't need to remove them here
          , ic_default    = defaults
          , ic_fix_env    = fix_env  -- See Note [Fixity declarations in GHCi]
          }
  where
    new_ids = [id | AnId id <- new_tythings]
    old_tythings = filterOut (shadowed_by new_ids) (ic_tythings ictxt)

    -- Discard old instances that have been fully overridden
    -- See Note [Override identical instances in GHCi]
    (cls_insts, fam_insts) = ic_instances ictxt
    old_cls_insts = filterOut (\i -> any (identicalClsInstHead i) new_cls_insts) cls_insts

extendInteractiveContextWithIds :: InteractiveContext -> [Id] -> InteractiveContext
-- Just a specialised version
extendInteractiveContextWithIds ictxt new_ids
  | null new_ids = ictxt
  | otherwise    = ictxt { ic_mod_index  = ic_mod_index ictxt + 1
                         , ic_tythings   = new_tythings ++ old_tythings
                         , ic_rn_gbl_env = ic_rn_gbl_env ictxt `icExtendGblRdrEnv` new_tythings }
  where
    new_tythings = map AnId new_ids
    old_tythings = filterOut (shadowed_by new_ids) (ic_tythings ictxt)

shadowed_by :: [Id] -> TyThing -> Bool
shadowed_by ids = shadowed
  where
    shadowed id = getOccName id `elemOccSet` new_occs
    new_occs = mkOccSet (map getOccName ids)

setInteractivePackage :: HscEnv -> HscEnv
-- Set the 'thisPackage' DynFlag to 'interactive'
setInteractivePackage hsc_env
   = hsc_env { hsc_dflags = (hsc_dflags hsc_env)
                { thisInstalledUnitId = toInstalledUnitId interactiveUnitId } }

setInteractivePrintName :: InteractiveContext -> Name -> InteractiveContext
setInteractivePrintName ic n = ic{ic_int_print = n}

    -- ToDo: should not add Ids to the gbl env here

-- | Add TyThings to the GlobalRdrEnv, earlier ones in the list shadowing
-- later ones, and shadowing existing entries in the GlobalRdrEnv.
icExtendGblRdrEnv :: GlobalRdrEnv -> [TyThing] -> GlobalRdrEnv
icExtendGblRdrEnv env tythings
  = foldr add env tythings  -- Foldr makes things in the front of
                            -- the list shadow things at the back
  where
    -- One at a time, to ensure each shadows the previous ones
    add thing env
       | is_sub_bndr thing
       = env
       | otherwise
       = foldl' extendGlobalRdrEnv env1 (concatMap localGREsFromAvail avail)
       where
          env1  = shadowNames env (concatMap availNames avail)
          avail = tyThingAvailInfo thing

    -- Ugh! The new_tythings may include record selectors, since they
    -- are not implicit-ids, and must appear in the TypeEnv.  But they
    -- will also be brought into scope by the corresponding (ATyCon
    -- tc).  And we want the latter, because that has the correct
    -- parent (#10520)
    is_sub_bndr (AnId f) = case idDetails f of
                             RecSelId {}  -> True
                             ClassOpId {} -> True
                             _            -> False
    is_sub_bndr _ = False

substInteractiveContext :: InteractiveContext -> TCvSubst -> InteractiveContext
substInteractiveContext ictxt@InteractiveContext{ ic_tythings = tts } subst
  | isEmptyTCvSubst subst = ictxt
  | otherwise             = ictxt { ic_tythings = map subst_ty tts }
  where
    subst_ty (AnId id)
      = AnId $ id `setIdType` substTyAddInScope subst (idType id)
      -- Variables in the interactive context *can* mention free type variables
      -- because of the runtime debugger. Otherwise you'd expect all
      -- variables bound in the interactive context to be closed.
    subst_ty tt
      = tt

instance Outputable InteractiveImport where
  ppr (IIModule m) = char '*' <> ppr m
  ppr (IIDecl d)   = ppr d

{-
************************************************************************
*                                                                      *
        Building a PrintUnqualified
*                                                                      *
************************************************************************

Note [Printing original names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Deciding how to print names is pretty tricky.  We are given a name
P:M.T, where P is the package name, M is the defining module, and T is
the occurrence name, and we have to decide in which form to display
the name given a GlobalRdrEnv describing the current scope.

Ideally we want to display the name in the form in which it is in
scope.  However, the name might not be in scope at all, and that's
where it gets tricky.  Here are the cases:

 1. T uniquely maps to  P:M.T      --->  "T"      NameUnqual
 2. There is an X for which X.T
       uniquely maps to  P:M.T     --->  "X.T"    NameQual X
 3. There is no binding for "M.T"  --->  "M.T"    NameNotInScope1
 4. Otherwise                      --->  "P:M.T"  NameNotInScope2

(3) and (4) apply when the entity P:M.T is not in the GlobalRdrEnv at
all. In these cases we still want to refer to the name as "M.T", *but*
"M.T" might mean something else in the current scope (e.g. if there's
an "import X as M"), so to avoid confusion we avoid using "M.T" if
there's already a binding for it.  Instead we write P:M.T.

There's one further subtlety: in case (3), what if there are two
things around, P1:M.T and P2:M.T?  Then we don't want to print both of
them as M.T!  However only one of the modules P1:M and P2:M can be
exposed (say P2), so we use M.T for that, and P1:M.T for the other one.
This is handled by the qual_mod component of PrintUnqualified, inside
the (ppr mod) of case (3), in Name.pprModulePrefix

Note [Printing unit ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the old days, original names were tied to PackageIds, which directly
corresponded to the entities that users wrote in Cabal files, and were perfectly
suitable for printing when we need to disambiguate packages.  However, with
UnitId, the situation can be different: if the key is instantiated with
some holes, we should try to give the user some more useful information.
-}

-- | Creates some functions that work out the best ways to format
-- names for the user according to a set of heuristics.
mkPrintUnqualified :: DynFlags -> GlobalRdrEnv -> PrintUnqualified
mkPrintUnqualified dflags env = QueryQualify qual_name
                                             (mkQualModule dflags)
                                             (mkQualPackage dflags)
  where
  qual_name mod occ
        | [gre] <- unqual_gres
        , right_name gre
        = NameUnqual   -- If there's a unique entity that's in scope
                       -- unqualified with 'occ' AND that entity is
                       -- the right one, then we can use the unqualified name

        | [] <- unqual_gres
        , any is_name forceUnqualNames
        , not (isDerivedOccName occ)
        = NameUnqual   -- Don't qualify names that come from modules
                       -- that come with GHC, often appear in error messages,
                       -- but aren't typically in scope. Doing this does not
                       -- cause ambiguity, and it reduces the amount of
                       -- qualification in error messages thus improving
                       -- readability.
                       --
                       -- A motivating example is 'Constraint'. It's often not
                       -- in scope, but printing GHC.Prim.Constraint seems
                       -- overkill.

        | [gre] <- qual_gres
        = NameQual (greQualModName gre)

        | null qual_gres
        = if null (lookupGRE_RdrName (mkRdrQual (moduleName mod) occ) env)
          then NameNotInScope1
          else NameNotInScope2

        | otherwise
        = NameNotInScope1   -- Can happen if 'f' is bound twice in the module
                            -- Eg  f = True; g = 0; f = False
      where
        is_name :: Name -> Bool
        is_name name = ASSERT2( isExternalName name, ppr name )
                       nameModule name == mod && nameOccName name == occ

        forceUnqualNames :: [Name]
        forceUnqualNames =
          map tyConName [ constraintKindTyCon, heqTyCon, coercibleTyCon ]
          ++ [ eqTyConName ]

        right_name gre = nameModule_maybe (gre_name gre) == Just mod

        unqual_gres = lookupGRE_RdrName (mkRdrUnqual occ) env
        qual_gres   = filter right_name (lookupGlobalRdrEnv env occ)

    -- we can mention a module P:M without the P: qualifier iff
    -- "import M" would resolve unambiguously to P:M.  (if P is the
    -- current package we can just assume it is unqualified).

-- | Creates a function for formatting modules based on two heuristics:
-- (1) if the module is the current module, don't qualify, and (2) if there
-- is only one exposed package which exports this module, don't qualify.
mkQualModule :: DynFlags -> QueryQualifyModule
mkQualModule dflags mod
     | moduleUnitId mod == thisPackage dflags = False

     | [(_, pkgconfig)] <- lookup,
       packageConfigId pkgconfig == moduleUnitId mod
        -- this says: we are given a module P:M, is there just one exposed package
        -- that exposes a module M, and is it package P?
     = False

     | otherwise = True
     where lookup = lookupModuleInAllPackages dflags (moduleName mod)

-- | Creates a function for formatting packages based on two heuristics:
-- (1) don't qualify if the package in question is "main", and (2) only qualify
-- with a unit id if the package ID would be ambiguous.
mkQualPackage :: DynFlags -> QueryQualifyPackage
mkQualPackage dflags uid
     | uid == mainUnitId || uid == interactiveUnitId
        -- Skip the lookup if it's main, since it won't be in the package
        -- database!
     = False
     | Just pkgid <- mb_pkgid
     , searchPackageId dflags pkgid `lengthIs` 1
        -- this says: we are given a package pkg-0.1@MMM, are there only one
        -- exposed packages whose package ID is pkg-0.1?
     = False
     | otherwise
     = True
     where mb_pkgid = fmap sourcePackageId (lookupUnit dflags uid)

-- | A function which only qualifies package names if necessary; but
-- qualifies all other identifiers.
pkgQual :: DynFlags -> PrintUnqualified
pkgQual dflags = alwaysQualify {
        queryQualifyPackage = mkQualPackage dflags
    }

{-
************************************************************************
*                                                                      *
                Implicit TyThings
*                                                                      *
************************************************************************

Note [Implicit TyThings]
~~~~~~~~~~~~~~~~~~~~~~~~
  DEFINITION: An "implicit" TyThing is one that does not have its own
  IfaceDecl in an interface file.  Instead, its binding in the type
  environment is created as part of typechecking the IfaceDecl for
  some other thing.

Examples:
  * All DataCons are implicit, because they are generated from the
    IfaceDecl for the data/newtype.  Ditto class methods.

  * Record selectors are *not* implicit, because they get their own
    free-standing IfaceDecl.

  * Associated data/type families are implicit because they are
    included in the IfaceDecl of the parent class.  (NB: the
    IfaceClass decl happens to use IfaceDecl recursively for the
    associated types, but that's irrelevant here.)

  * Dictionary function Ids are not implicit.

  * Axioms for newtypes are implicit (same as above), but axioms
    for data/type family instances are *not* implicit (like DFunIds).
-}

-- | Determine the 'TyThing's brought into scope by another 'TyThing'
-- /other/ than itself. For example, Id's don't have any implicit TyThings
-- as they just bring themselves into scope, but classes bring their
-- dictionary datatype, type constructor and some selector functions into
-- scope, just for a start!

-- N.B. the set of TyThings returned here *must* match the set of
-- names returned by GHC.Iface.Load.ifaceDeclImplicitBndrs, in the sense that
-- TyThing.getOccName should define a bijection between the two lists.
-- This invariant is used in GHC.Iface.Load.loadDecl (see note [Tricky iface loop])
-- The order of the list does not matter.
implicitTyThings :: TyThing -> [TyThing]
implicitTyThings (AnId _)       = []
implicitTyThings (ACoAxiom _cc) = []
implicitTyThings (ATyCon tc)    = implicitTyConThings tc
implicitTyThings (AConLike cl)  = implicitConLikeThings cl

implicitConLikeThings :: ConLike -> [TyThing]
implicitConLikeThings (RealDataCon dc)
  = dataConImplicitTyThings dc

implicitConLikeThings (PatSynCon {})
  = []  -- Pattern synonyms have no implicit Ids; the wrapper and matcher
        -- are not "implicit"; they are simply new top-level bindings,
        -- and they have their own declaration in an interface file
        -- Unless a record pat syn when there are implicit selectors
        -- They are still not included here as `implicitConLikeThings` is
        -- used by `tcTyClsDecls` whilst pattern synonyms are typed checked
        -- by `tcTopValBinds`.

implicitClassThings :: Class -> [TyThing]
implicitClassThings cl
  = -- Does not include default methods, because those Ids may have
    --    their own pragmas, unfoldings etc, not derived from the Class object

    -- associated types
    --    No recursive call for the classATs, because they
    --    are only the family decls; they have no implicit things
    map ATyCon (classATs cl) ++

    -- superclass and operation selectors
    map AnId (classAllSelIds cl)

implicitTyConThings :: TyCon -> [TyThing]
implicitTyConThings tc
  = class_stuff ++
      -- fields (names of selectors)

      -- (possibly) implicit newtype axioms
      -- or type family axioms
    implicitCoTyCon tc ++

      -- for each data constructor in order,
      --   the constructor, worker, and (possibly) wrapper
    [ thing | dc    <- tyConDataCons tc
            , thing <- AConLike (RealDataCon dc) : dataConImplicitTyThings dc ]
      -- NB. record selectors are *not* implicit, they have fully-fledged
      -- bindings that pass through the compilation pipeline as normal.
  where
    class_stuff = case tyConClass_maybe tc of
        Nothing -> []
        Just cl -> implicitClassThings cl

-- For newtypes and closed type families (only) add the implicit coercion tycon
implicitCoTyCon :: TyCon -> [TyThing]
implicitCoTyCon tc
  | Just co <- newTyConCo_maybe tc = [ACoAxiom $ toBranchedAxiom co]
  | Just co <- isClosedSynFamilyTyConWithAxiom_maybe tc
                                   = [ACoAxiom co]
  | otherwise                      = []

-- | Returns @True@ if there should be no interface-file declaration
-- for this thing on its own: either it is built-in, or it is part
-- of some other declaration, or it is generated implicitly by some
-- other declaration.
isImplicitTyThing :: TyThing -> Bool
isImplicitTyThing (AConLike cl) = case cl of
                                    RealDataCon {} -> True
                                    PatSynCon {}   -> False
isImplicitTyThing (AnId id)     = isImplicitId id
isImplicitTyThing (ATyCon tc)   = isImplicitTyCon tc
isImplicitTyThing (ACoAxiom ax) = isImplicitCoAxiom ax

-- | tyThingParent_maybe x returns (Just p)
-- when pprTyThingInContext should print a declaration for p
-- (albeit with some "..." in it) when asked to show x
-- It returns the *immediate* parent.  So a datacon returns its tycon
-- but the tycon could be the associated type of a class, so it in turn
-- might have a parent.
tyThingParent_maybe :: TyThing -> Maybe TyThing
tyThingParent_maybe (AConLike cl) = case cl of
    RealDataCon dc  -> Just (ATyCon (dataConTyCon dc))
    PatSynCon{}     -> Nothing
tyThingParent_maybe (ATyCon tc)   = case tyConAssoc_maybe tc of
                                      Just tc -> Just (ATyCon tc)
                                      Nothing -> Nothing
tyThingParent_maybe (AnId id)     = case idDetails id of
                                      RecSelId { sel_tycon = RecSelData tc } ->
                                          Just (ATyCon tc)
                                      ClassOpId cls               ->
                                          Just (ATyCon (classTyCon cls))
                                      _other                      -> Nothing
tyThingParent_maybe _other = Nothing

tyThingsTyCoVars :: [TyThing] -> TyCoVarSet
tyThingsTyCoVars tts =
    unionVarSets $ map ttToVarSet tts
    where
        ttToVarSet (AnId id)     = tyCoVarsOfType $ idType id
        ttToVarSet (AConLike cl) = case cl of
            RealDataCon dc  -> tyCoVarsOfType $ dataConRepType dc
            PatSynCon{}     -> emptyVarSet
        ttToVarSet (ATyCon tc)
          = case tyConClass_maybe tc of
              Just cls -> (mkVarSet . fst . classTvsFds) cls
              Nothing  -> tyCoVarsOfType $ tyConKind tc
        ttToVarSet (ACoAxiom _)  = emptyVarSet

-- | The Names that a TyThing should bring into scope.  Used to build
-- the GlobalRdrEnv for the InteractiveContext.
tyThingAvailInfo :: TyThing -> [AvailInfo]
tyThingAvailInfo (ATyCon t)
   = case tyConClass_maybe t of
        Just c  -> [AvailTC n (n : map getName (classMethods c)
                                 ++ map getName (classATs c))
                             [] ]
             where n = getName c
        Nothing -> [AvailTC n (n : map getName dcs) flds]
             where n    = getName t
                   dcs  = tyConDataCons t
                   flds = tyConFieldLabels t
tyThingAvailInfo (AConLike (PatSynCon p))
  = map avail ((getName p) : map flSelector (patSynFieldLabels p))
tyThingAvailInfo t
   = [avail (getName t)]

{-
************************************************************************
*                                                                      *
                TypeEnv
*                                                                      *
************************************************************************
-}

-- | A map from 'Name's to 'TyThing's, constructed by typechecking
-- local declarations or interface files
type TypeEnv = NameEnv TyThing

emptyTypeEnv    :: TypeEnv
typeEnvElts     :: TypeEnv -> [TyThing]
typeEnvTyCons   :: TypeEnv -> [TyCon]
typeEnvCoAxioms :: TypeEnv -> [CoAxiom Branched]
typeEnvIds      :: TypeEnv -> [Id]
typeEnvPatSyns  :: TypeEnv -> [PatSyn]
typeEnvDataCons :: TypeEnv -> [DataCon]
typeEnvClasses  :: TypeEnv -> [Class]
lookupTypeEnv   :: TypeEnv -> Name -> Maybe TyThing

emptyTypeEnv        = emptyNameEnv
typeEnvElts     env = nameEnvElts env
typeEnvTyCons   env = [tc | ATyCon tc   <- typeEnvElts env]
typeEnvCoAxioms env = [ax | ACoAxiom ax <- typeEnvElts env]
typeEnvIds      env = [id | AnId id     <- typeEnvElts env]
typeEnvPatSyns  env = [ps | AConLike (PatSynCon ps) <- typeEnvElts env]
typeEnvDataCons env = [dc | AConLike (RealDataCon dc) <- typeEnvElts env]
typeEnvClasses  env = [cl | tc <- typeEnvTyCons env,
                            Just cl <- [tyConClass_maybe tc]]

mkTypeEnv :: [TyThing] -> TypeEnv
mkTypeEnv things = extendTypeEnvList emptyTypeEnv things

mkTypeEnvWithImplicits :: [TyThing] -> TypeEnv
mkTypeEnvWithImplicits things =
  mkTypeEnv things
    `plusNameEnv`
  mkTypeEnv (concatMap implicitTyThings things)

typeEnvFromEntities :: [Id] -> [TyCon] -> [FamInst] -> TypeEnv
typeEnvFromEntities ids tcs famInsts =
  mkTypeEnv (   map AnId ids
             ++ map ATyCon all_tcs
             ++ concatMap implicitTyConThings all_tcs
             ++ map (ACoAxiom . toBranchedAxiom . famInstAxiom) famInsts
            )
 where
  all_tcs = tcs ++ famInstsRepTyCons famInsts

lookupTypeEnv = lookupNameEnv

-- Extend the type environment
extendTypeEnv :: TypeEnv -> TyThing -> TypeEnv
extendTypeEnv env thing = extendNameEnv env (getName thing) thing

extendTypeEnvList :: TypeEnv -> [TyThing] -> TypeEnv
extendTypeEnvList env things = foldl' extendTypeEnv env things

extendTypeEnvWithIds :: TypeEnv -> [Id] -> TypeEnv
extendTypeEnvWithIds env ids
  = extendNameEnvList env [(getName id, AnId id) | id <- ids]

plusTypeEnv :: TypeEnv -> TypeEnv -> TypeEnv
plusTypeEnv env1 env2 = plusNameEnv env1 env2

-- | Find the 'TyThing' for the given 'Name' by using all the resources
-- at our disposal: the compiled modules in the 'HomePackageTable' and the
-- compiled modules in other packages that live in 'PackageTypeEnv'. Note
-- that this does NOT look up the 'TyThing' in the module being compiled: you
-- have to do that yourself, if desired
lookupType :: DynFlags
           -> HomePackageTable
           -> PackageTypeEnv
           -> Name
           -> Maybe TyThing

lookupType dflags hpt pte name
  | isOneShot (ghcMode dflags)  -- in one-shot, we don't use the HPT
  = lookupNameEnv pte name
  | otherwise
  = case lookupHptByModule hpt mod of
       Just hm -> lookupNameEnv (md_types (hm_details hm)) name
       Nothing -> lookupNameEnv pte name
  where
    mod = ASSERT2( isExternalName name, ppr name )
          if isHoleName name
            then mkModule (thisPackage dflags) (moduleName (nameModule name))
            else nameModule name

-- | As 'lookupType', but with a marginally easier-to-use interface
-- if you have a 'HscEnv'
lookupTypeHscEnv :: HscEnv -> Name -> IO (Maybe TyThing)
lookupTypeHscEnv hsc_env name = do
    eps <- readIORef (hsc_EPS hsc_env)
    return $! lookupType dflags hpt (eps_PTE eps) name
  where
    dflags = hsc_dflags hsc_env
    hpt = hsc_HPT hsc_env

-- | Get the 'TyCon' from a 'TyThing' if it is a type constructor thing. Panics otherwise
tyThingTyCon :: TyThing -> TyCon
tyThingTyCon (ATyCon tc) = tc
tyThingTyCon other       = pprPanic "tyThingTyCon" (ppr other)

-- | Get the 'CoAxiom' from a 'TyThing' if it is a coercion axiom thing. Panics otherwise
tyThingCoAxiom :: TyThing -> CoAxiom Branched
tyThingCoAxiom (ACoAxiom ax) = ax
tyThingCoAxiom other         = pprPanic "tyThingCoAxiom" (ppr other)

-- | Get the 'DataCon' from a 'TyThing' if it is a data constructor thing. Panics otherwise
tyThingDataCon :: TyThing -> DataCon
tyThingDataCon (AConLike (RealDataCon dc)) = dc
tyThingDataCon other                       = pprPanic "tyThingDataCon" (ppr other)

-- | Get the 'ConLike' from a 'TyThing' if it is a data constructor thing.
-- Panics otherwise
tyThingConLike :: TyThing -> ConLike
tyThingConLike (AConLike dc) = dc
tyThingConLike other         = pprPanic "tyThingConLike" (ppr other)

-- | Get the 'Id' from a 'TyThing' if it is a id *or* data constructor thing. Panics otherwise
tyThingId :: TyThing -> Id
tyThingId (AnId id)                   = id
tyThingId (AConLike (RealDataCon dc)) = dataConWrapId dc
tyThingId other                       = pprPanic "tyThingId" (ppr other)

{-
************************************************************************
*                                                                      *
\subsection{MonadThings and friends}
*                                                                      *
************************************************************************
-}

-- | Class that abstracts out the common ability of the monads in GHC
-- to lookup a 'TyThing' in the monadic environment by 'Name'. Provides
-- a number of related convenience functions for accessing particular
-- kinds of 'TyThing'
class Monad m => MonadThings m where
        lookupThing :: Name -> m TyThing

        lookupId :: Name -> m Id
        lookupId = liftM tyThingId . lookupThing

        lookupDataCon :: Name -> m DataCon
        lookupDataCon = liftM tyThingDataCon . lookupThing

        lookupTyCon :: Name -> m TyCon
        lookupTyCon = liftM tyThingTyCon . lookupThing

-- Instance used in DsMeta
instance MonadThings m => MonadThings (ReaderT s m) where
  lookupThing = lift . lookupThing

{-
************************************************************************
*                                                                      *
\subsection{Auxiliary types}
*                                                                      *
************************************************************************

These types are defined here because they are mentioned in ModDetails,
but they are mostly elaborated elsewhere
-}

------------------ Warnings -------------------------
-- | Warning information for a module
data Warnings
  = NoWarnings                          -- ^ Nothing deprecated
  | WarnAll WarningTxt                  -- ^ Whole module deprecated
  | WarnSome [(OccName,WarningTxt)]     -- ^ Some specific things deprecated

     -- Only an OccName is needed because
     --    (1) a deprecation always applies to a binding
     --        defined in the module in which the deprecation appears.
     --    (2) deprecations are only reported outside the defining module.
     --        this is important because, otherwise, if we saw something like
     --
     --        {-# DEPRECATED f "" #-}
     --        f = ...
     --        h = f
     --        g = let f = undefined in f
     --
     --        we'd need more information than an OccName to know to say something
     --        about the use of f in h but not the use of the locally bound f in g
     --
     --        however, because we only report about deprecations from the outside,
     --        and a module can only export one value called f,
     --        an OccName suffices.
     --
     --        this is in contrast with fixity declarations, where we need to map
     --        a Name to its fixity declaration.
  deriving( Eq )

instance Binary Warnings where
    put_ bh NoWarnings     = putByte bh 0
    put_ bh (WarnAll t) = do
            putByte bh 1
            put_ bh t
    put_ bh (WarnSome ts) = do
            putByte bh 2
            put_ bh ts

    get bh = do
            h <- getByte bh
            case h of
              0 -> return NoWarnings
              1 -> do aa <- get bh
                      return (WarnAll aa)
              _ -> do aa <- get bh
                      return (WarnSome aa)

-- | Constructs the cache for the 'mi_warn_fn' field of a 'ModIface'
mkIfaceWarnCache :: Warnings -> OccName -> Maybe WarningTxt
mkIfaceWarnCache NoWarnings  = \_ -> Nothing
mkIfaceWarnCache (WarnAll t) = \_ -> Just t
mkIfaceWarnCache (WarnSome pairs) = lookupOccEnv (mkOccEnv pairs)

emptyIfaceWarnCache :: OccName -> Maybe WarningTxt
emptyIfaceWarnCache _ = Nothing

plusWarns :: Warnings -> Warnings -> Warnings
plusWarns d NoWarnings = d
plusWarns NoWarnings d = d
plusWarns _ (WarnAll t) = WarnAll t
plusWarns (WarnAll t) _ = WarnAll t
plusWarns (WarnSome v1) (WarnSome v2) = WarnSome (v1 ++ v2)

-- | Creates cached lookup for the 'mi_fix_fn' field of 'ModIface'
mkIfaceFixCache :: [(OccName, Fixity)] -> OccName -> Maybe Fixity
mkIfaceFixCache pairs
  = \n -> lookupOccEnv env n
  where
   env = mkOccEnv pairs

emptyIfaceFixCache :: OccName -> Maybe Fixity
emptyIfaceFixCache _ = Nothing

-- | Fixity environment mapping names to their fixities
type FixityEnv = NameEnv FixItem

-- | Fixity information for an 'Name'. We keep the OccName in the range
-- so that we can generate an interface from it
data FixItem = FixItem OccName Fixity

instance Outputable FixItem where
  ppr (FixItem occ fix) = ppr fix <+> ppr occ

emptyFixityEnv :: FixityEnv
emptyFixityEnv = emptyNameEnv

lookupFixity :: FixityEnv -> Name -> Fixity
lookupFixity env n = case lookupNameEnv env n of
                        Just (FixItem _ fix) -> fix
                        Nothing         -> defaultFixity

{-
************************************************************************
*                                                                      *
\subsection{WhatsImported}
*                                                                      *
************************************************************************
-}

-- | Records whether a module has orphans. An \"orphan\" is one of:
--
-- * An instance declaration in a module other than the definition
--   module for one of the type constructors or classes in the instance head
--
-- * A transformation rule in a module other than the one defining
--   the function in the head of the rule
--
type WhetherHasOrphans   = Bool

-- | Does this module define family instances?
type WhetherHasFamInst = Bool

-- | Dependency information about ALL modules and packages below this one
-- in the import hierarchy.
--
-- Invariant: the dependencies of a module @M@ never includes @M@.
--
-- Invariant: none of the lists contain duplicates.
data Dependencies
  = Deps { dep_mods   :: [ModuleNameWithIsBoot]
                        -- ^ All home-package modules transitively below this one
                        -- I.e. modules that this one imports, or that are in the
                        --      dep_mods of those directly-imported modules

         , dep_pkgs   :: [(InstalledUnitId, Bool)]
                        -- ^ All packages transitively below this module
                        -- I.e. packages to which this module's direct imports belong,
                        --      or that are in the dep_pkgs of those modules
                        -- The bool indicates if the package is required to be
                        -- trusted when the module is imported as a safe import
                        -- (Safe Haskell). See Note [Tracking Trust Transitively] in GHC.Rename.Names

         , dep_orphs  :: [Module]
                        -- ^ Transitive closure of orphan modules (whether
                        -- home or external pkg).
                        --
                        -- (Possible optimization: don't include family
                        -- instance orphans as they are anyway included in
                        -- 'dep_finsts'.  But then be careful about code
                        -- which relies on dep_orphs having the complete list!)
                        -- This does NOT include us, unlike 'imp_orphs'.

         , dep_finsts :: [Module]
                        -- ^ Transitive closure of depended upon modules which
                        -- contain family instances (whether home or external).
                        -- This is used by 'checkFamInstConsistency'.  This
                        -- does NOT include us, unlike 'imp_finsts'. See Note
                        -- [The type family instance consistency story].

         , dep_plgins :: [ModuleName]
                        -- ^ All the plugins used while compiling this module.
         }
  deriving( Eq )
        -- Equality used only for old/new comparison in GHC.Iface.Utils.addFingerprints
        -- See 'TcRnTypes.ImportAvails' for details on dependencies.

instance Binary Dependencies where
    put_ bh deps = do put_ bh (dep_mods deps)
                      put_ bh (dep_pkgs deps)
                      put_ bh (dep_orphs deps)
                      put_ bh (dep_finsts deps)
                      put_ bh (dep_plgins deps)

    get bh = do ms <- get bh
                ps <- get bh
                os <- get bh
                fis <- get bh
                pl <- get bh
                return (Deps { dep_mods = ms, dep_pkgs = ps, dep_orphs = os,
                               dep_finsts = fis, dep_plgins = pl })

noDependencies :: Dependencies
noDependencies = Deps [] [] [] [] []

-- | Records modules for which changes may force recompilation of this module
-- See wiki: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/recompilation-avoidance
--
-- This differs from Dependencies.  A module X may be in the dep_mods of this
-- module (via an import chain) but if we don't use anything from X it won't
-- appear in our Usage
data Usage
  -- | Module from another package
  = UsagePackageModule {
        usg_mod      :: Module,
           -- ^ External package module depended on
        usg_mod_hash :: Fingerprint,
            -- ^ Cached module fingerprint
        usg_safe :: IsSafeImport
            -- ^ Was this module imported as a safe import
    }
  -- | Module from the current package
  | UsageHomeModule {
        usg_mod_name :: ModuleName,
            -- ^ Name of the module
        usg_mod_hash :: Fingerprint,
            -- ^ Cached module fingerprint
        usg_entities :: [(OccName,Fingerprint)],
            -- ^ Entities we depend on, sorted by occurrence name and fingerprinted.
            -- NB: usages are for parent names only, e.g. type constructors
            -- but not the associated data constructors.
        usg_exports  :: Maybe Fingerprint,
            -- ^ Fingerprint for the export list of this module,
            -- if we directly imported it (and hence we depend on its export list)
        usg_safe :: IsSafeImport
            -- ^ Was this module imported as a safe import
    }                                           -- ^ Module from the current package
  -- | A file upon which the module depends, e.g. a CPP #include, or using TH's
  -- 'addDependentFile'
  | UsageFile {
        usg_file_path  :: FilePath,
        -- ^ External file dependency. From a CPP #include or TH
        -- addDependentFile. Should be absolute.
        usg_file_hash  :: Fingerprint
        -- ^ 'Fingerprint' of the file contents.

        -- Note: We don't consider things like modification timestamps
        -- here, because there's no reason to recompile if the actual
        -- contents don't change.  This previously lead to odd
        -- recompilation behaviors; see #8114
  }
  -- | A requirement which was merged into this one.
  | UsageMergedRequirement {
        usg_mod :: Module,
        usg_mod_hash :: Fingerprint
  }
    deriving( Eq )
        -- The export list field is (Just v) if we depend on the export list:
        --      i.e. we imported the module directly, whether or not we
        --           enumerated the things we imported, or just imported
        --           everything
        -- We need to recompile if M's exports change, because
        -- if the import was    import M,       we might now have a name clash
        --                                      in the importing module.
        -- if the import was    import M(x)     M might no longer export x
        -- The only way we don't depend on the export list is if we have
        --                      import M()
        -- And of course, for modules that aren't imported directly we don't
        -- depend on their export lists

instance Binary Usage where
    put_ bh usg@UsagePackageModule{} = do
        putByte bh 0
        put_ bh (usg_mod usg)
        put_ bh (usg_mod_hash usg)
        put_ bh (usg_safe     usg)

    put_ bh usg@UsageHomeModule{} = do
        putByte bh 1
        put_ bh (usg_mod_name usg)
        put_ bh (usg_mod_hash usg)
        put_ bh (usg_exports  usg)
        put_ bh (usg_entities usg)
        put_ bh (usg_safe     usg)

    put_ bh usg@UsageFile{} = do
        putByte bh 2
        put_ bh (usg_file_path usg)
        put_ bh (usg_file_hash usg)

    put_ bh usg@UsageMergedRequirement{} = do
        putByte bh 3
        put_ bh (usg_mod      usg)
        put_ bh (usg_mod_hash usg)

    get bh = do
        h <- getByte bh
        case h of
          0 -> do
            nm    <- get bh
            mod   <- get bh
            safe  <- get bh
            return UsagePackageModule { usg_mod = nm, usg_mod_hash = mod, usg_safe = safe }
          1 -> do
            nm    <- get bh
            mod   <- get bh
            exps  <- get bh
            ents  <- get bh
            safe  <- get bh
            return UsageHomeModule { usg_mod_name = nm, usg_mod_hash = mod,
                     usg_exports = exps, usg_entities = ents, usg_safe = safe }
          2 -> do
            fp   <- get bh
            hash <- get bh
            return UsageFile { usg_file_path = fp, usg_file_hash = hash }
          3 -> do
            mod <- get bh
            hash <- get bh
            return UsageMergedRequirement { usg_mod = mod, usg_mod_hash = hash }
          i -> error ("Binary.get(Usage): " ++ show i)

{-
************************************************************************
*                                                                      *
                The External Package State
*                                                                      *
************************************************************************
-}

type PackageTypeEnv          = TypeEnv
type PackageRuleBase         = RuleBase
type PackageInstEnv          = InstEnv
type PackageFamInstEnv       = FamInstEnv
type PackageAnnEnv           = AnnEnv
type PackageCompleteMatchMap = CompleteMatchMap

-- | Information about other packages that we have slurped in by reading
-- their interface files
data ExternalPackageState
  = EPS {
        eps_is_boot :: !(ModuleNameEnv ModuleNameWithIsBoot),
                -- ^ In OneShot mode (only), home-package modules
                -- accumulate in the external package state, and are
                -- sucked in lazily.  For these home-pkg modules
                -- (only) we need to record which are boot modules.
                -- We set this field after loading all the
                -- explicitly-imported interfaces, but before doing
                -- anything else
                --
                -- The 'ModuleName' part is not necessary, but it's useful for
                -- debug prints, and it's convenient because this field comes
                -- direct from 'TcRnTypes.imp_dep_mods'

        eps_PIT :: !PackageIfaceTable,
                -- ^ The 'ModIface's for modules in external packages
                -- whose interfaces we have opened.
                -- The declarations in these interface files are held in the
                -- 'eps_decls', 'eps_inst_env', 'eps_fam_inst_env' and 'eps_rules'
                -- fields of this record, not in the 'mi_decls' fields of the
                -- interface we have sucked in.
                --
                -- What /is/ in the PIT is:
                --
                -- * The Module
                --
                -- * Fingerprint info
                --
                -- * Its exports
                --
                -- * Fixities
                --
                -- * Deprecations and warnings

        eps_free_holes :: InstalledModuleEnv (UniqDSet ModuleName),
                -- ^ Cache for 'mi_free_holes'.  Ordinarily, we can rely on
                -- the 'eps_PIT' for this information, EXCEPT that when
                -- we do dependency analysis, we need to look at the
                -- 'Dependencies' of our imports to determine what their
                -- precise free holes are ('moduleFreeHolesPrecise').  We
                -- don't want to repeatedly reread in the interface
                -- for every import, so cache it here.  When the PIT
                -- gets filled in we can drop these entries.

        eps_PTE :: !PackageTypeEnv,
                -- ^ Result of typechecking all the external package
                -- interface files we have sucked in. The domain of
                -- the mapping is external-package modules

        eps_inst_env     :: !PackageInstEnv,   -- ^ The total 'InstEnv' accumulated
                                               -- from all the external-package modules
        eps_fam_inst_env :: !PackageFamInstEnv,-- ^ The total 'FamInstEnv' accumulated
                                               -- from all the external-package modules
        eps_rule_base    :: !PackageRuleBase,  -- ^ The total 'RuleEnv' accumulated
                                               -- from all the external-package modules
        eps_ann_env      :: !PackageAnnEnv,    -- ^ The total 'AnnEnv' accumulated
                                               -- from all the external-package modules
        eps_complete_matches :: !PackageCompleteMatchMap,
                                  -- ^ The total 'CompleteMatchMap' accumulated
                                  -- from all the external-package modules

        eps_mod_fam_inst_env :: !(ModuleEnv FamInstEnv), -- ^ The family instances accumulated from external
                                                         -- packages, keyed off the module that declared them

        eps_stats :: !EpsStats                 -- ^ Stastics about what was loaded from external packages
  }

-- | Accumulated statistics about what we are putting into the 'ExternalPackageState'.
-- \"In\" means stuff that is just /read/ from interface files,
-- \"Out\" means actually sucked in and type-checked
data EpsStats = EpsStats { n_ifaces_in
                         , n_decls_in, n_decls_out
                         , n_rules_in, n_rules_out
                         , n_insts_in, n_insts_out :: !Int }

addEpsInStats :: EpsStats -> Int -> Int -> Int -> EpsStats
-- ^ Add stats for one newly-read interface
addEpsInStats stats n_decls n_insts n_rules
  = stats { n_ifaces_in = n_ifaces_in stats + 1
          , n_decls_in  = n_decls_in stats + n_decls
          , n_insts_in  = n_insts_in stats + n_insts
          , n_rules_in  = n_rules_in stats + n_rules }

{-
Names in a NameCache are always stored as a Global, and have the SrcLoc
of their binding locations.

Actually that's not quite right.  When we first encounter the original
name, we might not be at its binding site (e.g. we are reading an
interface file); so we give it 'noSrcLoc' then.  Later, when we find
its binding site, we fix it up.
-}

updNameCache :: IORef NameCache
             -> (NameCache -> (NameCache, c))  -- The updating function
             -> IO c
updNameCache ncRef upd_fn
  = atomicModifyIORef' ncRef upd_fn

mkSOName :: Platform -> FilePath -> FilePath
mkSOName platform root
    = case platformOS platform of
      OSMinGW32 ->           root  <.> soExt platform
      _         -> ("lib" ++ root) <.> soExt platform

mkHsSOName :: Platform -> FilePath -> FilePath
mkHsSOName platform root = ("lib" ++ root) <.> soExt platform

soExt :: Platform -> FilePath
soExt platform
    = case platformOS platform of
      OSDarwin  -> "dylib"
      OSMinGW32 -> "dll"
      _         -> "so"

{-
************************************************************************
*                                                                      *
                The module graph and ModSummary type
        A ModSummary is a node in the compilation manager's
        dependency graph, and it's also passed to hscMain
*                                                                      *
************************************************************************
-}

-- | A ModuleGraph contains all the nodes from the home package (only).
-- There will be a node for each source module, plus a node for each hi-boot
-- module.
--
-- The graph is not necessarily stored in topologically-sorted order.  Use
-- 'GHC.topSortModuleGraph' and 'Digraph.flattenSCC' to achieve this.
data ModuleGraph = ModuleGraph
  { mg_mss :: [ModSummary]
  , mg_non_boot :: ModuleEnv ModSummary
    -- a map of all non-boot ModSummaries keyed by Modules
  , mg_boot :: ModuleSet
    -- a set of boot Modules
  , mg_needs_th_or_qq :: !Bool
    -- does any of the modules in mg_mss require TemplateHaskell or
    -- QuasiQuotes?
  }

-- | Determines whether a set of modules requires Template Haskell or
-- Quasi Quotes
--
-- Note that if the session's 'DynFlags' enabled Template Haskell when
-- 'depanal' was called, then each module in the returned module graph will
-- have Template Haskell enabled whether it is actually needed or not.
needsTemplateHaskellOrQQ :: ModuleGraph -> Bool
needsTemplateHaskellOrQQ mg = mg_needs_th_or_qq mg

-- | Map a function 'f' over all the 'ModSummaries'.
-- To preserve invariants 'f' can't change the isBoot status.
mapMG :: (ModSummary -> ModSummary) -> ModuleGraph -> ModuleGraph
mapMG f mg@ModuleGraph{..} = mg
  { mg_mss = map f mg_mss
  , mg_non_boot = mapModuleEnv f mg_non_boot
  }

mgBootModules :: ModuleGraph -> ModuleSet
mgBootModules ModuleGraph{..} = mg_boot

mgModSummaries :: ModuleGraph -> [ModSummary]
mgModSummaries = mg_mss

mgElemModule :: ModuleGraph -> Module -> Bool
mgElemModule ModuleGraph{..} m = elemModuleEnv m mg_non_boot

-- | Look up a ModSummary in the ModuleGraph
mgLookupModule :: ModuleGraph -> Module -> Maybe ModSummary
mgLookupModule ModuleGraph{..} m = lookupModuleEnv mg_non_boot m

emptyMG :: ModuleGraph
emptyMG = ModuleGraph [] emptyModuleEnv emptyModuleSet False

isTemplateHaskellOrQQNonBoot :: ModSummary -> Bool
isTemplateHaskellOrQQNonBoot ms =
  (xopt LangExt.TemplateHaskell (ms_hspp_opts ms)
    || xopt LangExt.QuasiQuotes (ms_hspp_opts ms)) &&
  not (isBootSummary ms)

-- | Add a ModSummary to ModuleGraph. Assumes that the new ModSummary is
-- not an element of the ModuleGraph.
extendMG :: ModuleGraph -> ModSummary -> ModuleGraph
extendMG ModuleGraph{..} ms = ModuleGraph
  { mg_mss = ms:mg_mss
  , mg_non_boot = if isBootSummary ms
      then mg_non_boot
      else extendModuleEnv mg_non_boot (ms_mod ms) ms
  , mg_boot = if isBootSummary ms
      then extendModuleSet mg_boot (ms_mod ms)
      else mg_boot
  , mg_needs_th_or_qq = mg_needs_th_or_qq || isTemplateHaskellOrQQNonBoot ms
  }

mkModuleGraph :: [ModSummary] -> ModuleGraph
mkModuleGraph = foldr (flip extendMG) emptyMG

-- | A single node in a 'ModuleGraph'. The nodes of the module graph
-- are one of:
--
-- * A regular Haskell source module
-- * A hi-boot source module
--
data ModSummary
   = ModSummary {
        ms_mod          :: Module,
          -- ^ Identity of the module
        ms_hsc_src      :: HscSource,
          -- ^ The module source either plain Haskell or hs-boot
        ms_location     :: ModLocation,
          -- ^ Location of the various files belonging to the module
        ms_hs_date      :: UTCTime,
          -- ^ Timestamp of source file
        ms_obj_date     :: Maybe UTCTime,
          -- ^ Timestamp of object, if we have one
        ms_iface_date   :: Maybe UTCTime,
          -- ^ Timestamp of hi file, if we *only* are typechecking (it is
          -- 'Nothing' otherwise.
          -- See Note [Recompilation checking in -fno-code mode] and #9243
        ms_hie_date   :: Maybe UTCTime,
          -- ^ Timestamp of hie file, if we have one
        ms_srcimps      :: [(Maybe FastString, Located ModuleName)],
          -- ^ Source imports of the module
        ms_textual_imps :: [(Maybe FastString, Located ModuleName)],
          -- ^ Non-source imports of the module from the module *text*
        ms_parsed_mod   :: Maybe HsParsedModule,
          -- ^ The parsed, nonrenamed source, if we have it.  This is also
          -- used to support "inline module syntax" in Backpack files.
        ms_hspp_file    :: FilePath,
          -- ^ Filename of preprocessed source file
        ms_hspp_opts    :: DynFlags,
          -- ^ Cached flags from @OPTIONS@, @INCLUDE@ and @LANGUAGE@
          -- pragmas in the modules source code
        ms_hspp_buf     :: Maybe StringBuffer
          -- ^ The actual preprocessed source, if we have it
     }

ms_installed_mod :: ModSummary -> InstalledModule
ms_installed_mod = fst . splitModuleInsts . ms_mod

ms_mod_name :: ModSummary -> ModuleName
ms_mod_name = moduleName . ms_mod

ms_imps :: ModSummary -> [(Maybe FastString, Located ModuleName)]
ms_imps ms =
  ms_textual_imps ms ++
  map mk_additional_import (dynFlagDependencies (ms_hspp_opts ms))
  where
    mk_additional_import mod_nm = (Nothing, noLoc mod_nm)

home_imps :: [(Maybe FastString, Located ModuleName)] -> [Located ModuleName]
home_imps imps = [ lmodname |  (mb_pkg, lmodname) <- imps,
                                  isLocal mb_pkg ]
  where isLocal Nothing = True
        isLocal (Just pkg) | pkg == fsLit "this" = True -- "this" is special
        isLocal _ = False

ms_home_allimps :: ModSummary -> [ModuleName]
ms_home_allimps ms = map unLoc (ms_home_srcimps ms ++ ms_home_imps ms)

-- | Like 'ms_home_imps', but for SOURCE imports.
ms_home_srcimps :: ModSummary -> [Located ModuleName]
ms_home_srcimps = home_imps . ms_srcimps

-- | All of the (possibly) home module imports from a
-- 'ModSummary'; that is to say, each of these module names
-- could be a home import if an appropriately named file
-- existed.  (This is in contrast to package qualified
-- imports, which are guaranteed not to be home imports.)
ms_home_imps :: ModSummary -> [Located ModuleName]
ms_home_imps = home_imps . ms_imps

-- The ModLocation contains both the original source filename and the
-- filename of the cleaned-up source file after all preprocessing has been
-- done.  The point is that the summariser will have to cpp/unlit/whatever
-- all files anyway, and there's no point in doing this twice -- just
-- park the result in a temp file, put the name of it in the location,
-- and let @compile@ read from that file on the way back up.

-- The ModLocation is stable over successive up-sweeps in GHCi, wheres
-- the ms_hs_date and imports can, of course, change

msHsFilePath, msHiFilePath, msObjFilePath :: ModSummary -> FilePath
msHsFilePath  ms = expectJust "msHsFilePath" (ml_hs_file  (ms_location ms))
msHiFilePath  ms = ml_hi_file  (ms_location ms)
msObjFilePath ms = ml_obj_file (ms_location ms)

msDynObjFilePath :: ModSummary -> DynFlags -> FilePath
msDynObjFilePath ms dflags = dynamicOutputFile dflags (msObjFilePath ms)

-- | Did this 'ModSummary' originate from a hs-boot file?
isBootSummary :: ModSummary -> Bool
isBootSummary ms = ms_hsc_src ms == HsBootFile

instance Outputable ModSummary where
   ppr ms
      = sep [text "ModSummary {",
             nest 3 (sep [text "ms_hs_date = " <> text (show (ms_hs_date ms)),
                          text "ms_mod =" <+> ppr (ms_mod ms)
                                <> text (hscSourceString (ms_hsc_src ms)) <> comma,
                          text "ms_textual_imps =" <+> ppr (ms_textual_imps ms),
                          text "ms_srcimps =" <+> ppr (ms_srcimps ms)]),
             char '}'
            ]

showModMsg :: DynFlags -> HscTarget -> Bool -> ModSummary -> String
showModMsg dflags target recomp mod_summary = showSDoc dflags $
   if gopt Opt_HideSourcePaths dflags
      then text mod_str
      else hsep $
         [ text (mod_str ++ replicate (max 0 (16 - length mod_str)) ' ')
         , char '('
         , text (op $ msHsFilePath mod_summary) <> char ','
         ] ++
         if gopt Opt_BuildDynamicToo dflags
            then [ text obj_file <> char ','
                 , text dyn_file
                 , char ')'
                 ]
            else [ text obj_file, char ')' ]
  where
    op       = normalise
    mod      = moduleName (ms_mod mod_summary)
    mod_str  = showPpr dflags mod ++ hscSourceString (ms_hsc_src mod_summary)
    dyn_file = op $ msDynObjFilePath mod_summary dflags
    obj_file = case target of
                HscInterpreted | recomp -> "interpreted"
                HscNothing              -> "nothing"
                _                       -> (op $ msObjFilePath mod_summary)

{-
************************************************************************
*                                                                      *
\subsection{Recompilation}
*                                                                      *
************************************************************************
-}

-- | Indicates whether a given module's source has been modified since it
-- was last compiled.
data SourceModified
  = SourceModified
       -- ^ the source has been modified
  | SourceUnmodified
       -- ^ the source has not been modified.  Compilation may or may
       -- not be necessary, depending on whether any dependencies have
       -- changed since we last compiled.
  | SourceUnmodifiedAndStable
       -- ^ the source has not been modified, and furthermore all of
       -- its (transitive) dependencies are up to date; it definitely
       -- does not need to be recompiled.  This is important for two
       -- reasons: (a) we can omit the version check in checkOldIface,
       -- and (b) if the module used TH splices we don't need to force
       -- recompilation.

{-
************************************************************************
*                                                                      *
\subsection{Hpc Support}
*                                                                      *
************************************************************************
-}

-- | Information about a modules use of Haskell Program Coverage
data HpcInfo
  = HpcInfo
     { hpcInfoTickCount :: Int
     , hpcInfoHash      :: Int
     }
  | NoHpcInfo
     { hpcUsed          :: AnyHpcUsage  -- ^ Is hpc used anywhere on the module \*tree\*?
     }

-- | This is used to signal if one of my imports used HPC instrumentation
-- even if there is no module-local HPC usage
type AnyHpcUsage = Bool

emptyHpcInfo :: AnyHpcUsage -> HpcInfo
emptyHpcInfo = NoHpcInfo

-- | Find out if HPC is used by this module or any of the modules
-- it depends upon
isHpcUsed :: HpcInfo -> AnyHpcUsage
isHpcUsed (HpcInfo {})                   = True
isHpcUsed (NoHpcInfo { hpcUsed = used }) = used

{-
************************************************************************
*                                                                      *
\subsection{Safe Haskell Support}
*                                                                      *
************************************************************************

This stuff here is related to supporting the Safe Haskell extension,
primarily about storing under what trust type a module has been compiled.
-}

-- | Is an import a safe import?
type IsSafeImport = Bool

-- | Safe Haskell information for 'ModIface'
-- Simply a wrapper around SafeHaskellMode to sepperate iface and flags
newtype IfaceTrustInfo = TrustInfo SafeHaskellMode

getSafeMode :: IfaceTrustInfo -> SafeHaskellMode
getSafeMode (TrustInfo x) = x

setSafeMode :: SafeHaskellMode -> IfaceTrustInfo
setSafeMode = TrustInfo

noIfaceTrustInfo :: IfaceTrustInfo
noIfaceTrustInfo = setSafeMode Sf_None

trustInfoToNum :: IfaceTrustInfo -> Word8
trustInfoToNum it
  = case getSafeMode it of
            Sf_None         -> 0
            Sf_Unsafe       -> 1
            Sf_Trustworthy  -> 2
            Sf_Safe         -> 3
            Sf_SafeInferred -> 4
            Sf_Ignore       -> 0

numToTrustInfo :: Word8 -> IfaceTrustInfo
numToTrustInfo 0 = setSafeMode Sf_None
numToTrustInfo 1 = setSafeMode Sf_Unsafe
numToTrustInfo 2 = setSafeMode Sf_Trustworthy
numToTrustInfo 3 = setSafeMode Sf_Safe
numToTrustInfo 4 = setSafeMode Sf_SafeInferred
numToTrustInfo n = error $ "numToTrustInfo: bad input number! (" ++ show n ++ ")"

instance Outputable IfaceTrustInfo where
    ppr (TrustInfo Sf_None)          = text "none"
    ppr (TrustInfo Sf_Ignore)        = text "none"
    ppr (TrustInfo Sf_Unsafe)        = text "unsafe"
    ppr (TrustInfo Sf_Trustworthy)   = text "trustworthy"
    ppr (TrustInfo Sf_Safe)          = text "safe"
    ppr (TrustInfo Sf_SafeInferred)  = text "safe-inferred"

instance Binary IfaceTrustInfo where
    put_ bh iftrust = putByte bh $ trustInfoToNum iftrust
    get bh = getByte bh >>= (return . numToTrustInfo)

{-
************************************************************************
*                                                                      *
\subsection{Parser result}
*                                                                      *
************************************************************************
-}

data HsParsedModule = HsParsedModule {
    hpm_module    :: Located HsModule,
    hpm_src_files :: [FilePath],
       -- ^ extra source files (e.g. from #includes).  The lexer collects
       -- these from '# <file> <line>' pragmas, which the C preprocessor
       -- leaves behind.  These files and their timestamps are stored in
       -- the .hi file, so that we can force recompilation if any of
       -- them change (#3589)
    hpm_annotations :: ApiAnns
    -- See note [Api annotations] in ApiAnnotation.hs
  }

{-
************************************************************************
*                                                                      *
\subsection{Linkable stuff}
*                                                                      *
************************************************************************

This stuff is in here, rather than (say) in Linker.hs, because the Linker.hs
stuff is the *dynamic* linker, and isn't present in a stage-1 compiler
-}

isObjectLinkable :: Linkable -> Bool
isObjectLinkable l = not (null unlinked) && all isObject unlinked
  where unlinked = linkableUnlinked l
        -- A linkable with no Unlinked's is treated as a BCO.  We can
        -- generate a linkable with no Unlinked's as a result of
        -- compiling a module in HscNothing mode, and this choice
        -- happens to work well with checkStability in module GHC.

linkableObjs :: Linkable -> [FilePath]
linkableObjs l = [ f | DotO f <- linkableUnlinked l ]

-------------------------------------------

-- | Is this an actual file on disk we can link in somehow?
isObject :: Unlinked -> Bool
isObject (DotO _)   = True
isObject (DotA _)   = True
isObject (DotDLL _) = True
isObject _          = False

-- | Is this a bytecode linkable with no file on disk?
isInterpretable :: Unlinked -> Bool
isInterpretable = not . isObject

-- | Retrieve the filename of the linkable if possible. Panic if it is a byte-code object
nameOfObject :: Unlinked -> FilePath
nameOfObject (DotO fn)   = fn
nameOfObject (DotA fn)   = fn
nameOfObject (DotDLL fn) = fn
nameOfObject other       = pprPanic "nameOfObject" (ppr other)

-- | Retrieve the compiled byte-code if possible. Panic if it is a file-based linkable
byteCodeOfObject :: Unlinked -> CompiledByteCode
byteCodeOfObject (BCOs bc _) = bc
byteCodeOfObject other       = pprPanic "byteCodeOfObject" (ppr other)


-------------------------------------------

-- | A list of conlikes which represents a complete pattern match.
-- These arise from @COMPLETE@ signatures.

-- See Note [Implementation of COMPLETE signatures]
data CompleteMatch = CompleteMatch {
                            completeMatchConLikes :: [Name]
                            -- ^ The ConLikes that form a covering family
                            -- (e.g. Nothing, Just)
                          , completeMatchTyCon :: Name
                            -- ^ The TyCon that they cover (e.g. Maybe)
                          }

instance Outputable CompleteMatch where
  ppr (CompleteMatch cl ty) = text "CompleteMatch:" <+> ppr cl
                                                    <+> dcolon <+> ppr ty

-- | A map keyed by the 'completeMatchTyCon'.

-- See Note [Implementation of COMPLETE signatures]
type CompleteMatchMap = UniqFM [CompleteMatch]

mkCompleteMatchMap :: [CompleteMatch] -> CompleteMatchMap
mkCompleteMatchMap = extendCompleteMatchMap emptyUFM

extendCompleteMatchMap :: CompleteMatchMap -> [CompleteMatch]
                       -> CompleteMatchMap
extendCompleteMatchMap = foldl' insertMatch
  where
    insertMatch :: CompleteMatchMap -> CompleteMatch -> CompleteMatchMap
    insertMatch ufm c@(CompleteMatch _ t) = addToUFM_C (++) ufm t [c]

{-
Note [Implementation of COMPLETE signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A COMPLETE signature represents a set of conlikes (i.e., constructors or
pattern synonyms) such that if they are all pattern-matched against in a
function, it gives rise to a total function. An example is:

  newtype Boolean = Boolean Int
  pattern F, T :: Boolean
  pattern F = Boolean 0
  pattern T = Boolean 1
  {-# COMPLETE F, T #-}

  -- This is a total function
  booleanToInt :: Boolean -> Int
  booleanToInt F = 0
  booleanToInt T = 1

COMPLETE sets are represented internally in GHC with the CompleteMatch data
type. For example, {-# COMPLETE F, T #-} would be represented as:

  CompleteMatch { complateMatchConLikes = [F, T]
                , completeMatchTyCon    = Boolean }

Note that GHC was able to infer the completeMatchTyCon (Boolean), but for the
cases in which it's ambiguous, you can also explicitly specify it in the source
language by writing this:

  {-# COMPLETE F, T :: Boolean #-}

For efficiency purposes, GHC collects all of the CompleteMatches that it knows
about into a CompleteMatchMap, which is a map that is keyed by the
completeMatchTyCon. In other words, you could have a multiple COMPLETE sets
for the same TyCon:

  {-# COMPLETE F, T1 :: Boolean #-}
  {-# COMPLETE F, T2 :: Boolean #-}

And looking up the values in the CompleteMatchMap associated with Boolean
would give you [CompleteMatch [F, T1] Boolean, CompleteMatch [F, T2] Boolean].
dsGetCompleteMatches in DsMeta accomplishes this lookup.

Also see Note [Typechecking Complete Matches] in TcBinds for a more detailed
explanation for how GHC ensures that all the conlikes in a COMPLETE set are
consistent.
-}

-- | Foreign language of the phase if the phase deals with a foreign code
phaseForeignLanguage :: Phase -> Maybe ForeignSrcLang
phaseForeignLanguage phase = case phase of
  Phase.Cc           -> Just LangC
  Phase.Ccxx         -> Just LangCxx
  Phase.Cobjc        -> Just LangObjc
  Phase.Cobjcxx      -> Just LangObjcxx
  Phase.HCc          -> Just LangC
  Phase.As _         -> Just LangAsm
  Phase.MergeForeign -> Just RawObject
  _                  -> Nothing

-------------------------------------------

-- Take care, this instance only forces to the degree necessary to
-- avoid major space leaks.
instance (NFData (IfaceBackendExts (phase :: ModIfacePhase)), NFData (IfaceDeclExts (phase :: ModIfacePhase))) => NFData (ModIface_ phase) where
  rnf (ModIface f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12
                f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23) =
    rnf f1 `seq` rnf f2 `seq` f3 `seq` f4 `seq` f5 `seq` f6 `seq` rnf f7 `seq` f8 `seq`
    f9 `seq` rnf f10 `seq` rnf f11 `seq` f12 `seq` rnf f13 `seq` rnf f14 `seq` rnf f15 `seq`
    rnf f16 `seq` f17 `seq` rnf f18 `seq` rnf f19 `seq` f20 `seq` f21 `seq` f22 `seq` rnf f23
