%
% (c) The University of Glasgow, 2006
%
\section[HscTypes]{Types for the per-module compiler}

\begin{code}

-- | Types for the per-module compiler
module HscTypes (
        -- * compilation state
        HscEnv(..), hscEPS,
        FinderCache, FindResult(..), ModLocationCache,
        Target(..), TargetId(..), pprTarget, pprTargetId,
        ModuleGraph, emptyMG,
        HscStatus(..),

        -- * Hsc monad
        Hsc(..), runHsc, runInteractiveHsc,

        -- * Information about modules
        ModDetails(..), emptyModDetails,
        ModGuts(..), CgGuts(..), ForeignStubs(..), appendStubC,
        ImportedMods, ImportedModsVal,

        ModSummary(..), ms_imps, ms_mod_name, showModMsg, isBootSummary,
        msHsFilePath, msHiFilePath, msObjFilePath,
        SourceModified(..),

        -- * Information about the module being compiled
        HscSource(..), isHsBoot, hscSourceString,       -- Re-exported from DriverPhases

        -- * State relating to modules in this package
        HomePackageTable, HomeModInfo(..), emptyHomePackageTable,
        hptInstances, hptRules, hptVectInfo,
        hptObjs,

        -- * State relating to known packages
        ExternalPackageState(..), EpsStats(..), addEpsInStats,
        PackageTypeEnv, PackageIfaceTable, emptyPackageIfaceTable,
        lookupIfaceByModule, emptyModIface,

        PackageInstEnv, PackageRuleBase,

        mkSOName, mkHsSOName, soExt,

        -- * Annotations
        prepareAnnotations,

        -- * Interactive context
        InteractiveContext(..), emptyInteractiveContext,
        icPrintUnqual, icInScopeTTs, icPlusGblRdrEnv,
        extendInteractiveContext, substInteractiveContext,
        setInteractivePrintName,
        InteractiveImport(..),
        mkPrintUnqualified, pprModulePrefix,

        -- * Interfaces
        ModIface(..), mkIfaceWarnCache, mkIfaceHashCache, mkIfaceFixCache,
        emptyIfaceWarnCache, mkIfaceAnnCache, emptyIfaceAnnCache,

        -- * Fixity
        FixityEnv, FixItem(..), lookupFixity, emptyFixityEnv,

        -- * TyThings and type environments
        TyThing(..),  tyThingAvailInfo,
        tyThingTyCon, tyThingDataCon,
        tyThingId, tyThingCoAxiom, tyThingParent_maybe, tyThingsTyVars,
        implicitTyThings, implicitTyConThings, implicitClassThings,
        isImplicitTyThing,

        TypeEnv, lookupType, lookupTypeHscEnv, mkTypeEnv, emptyTypeEnv,
        typeEnvFromEntities, mkTypeEnvWithImplicits,
        extendTypeEnv, extendTypeEnvList, extendTypeEnvWithIds, lookupTypeEnv,
        typeEnvElts, typeEnvTyCons, typeEnvIds,
        typeEnvDataCons, typeEnvCoAxioms, typeEnvClasses,

        -- * MonadThings
        MonadThings(..),

        -- * Information on imports and exports
        WhetherHasOrphans, IsBootInterface, Usage(..),
        Dependencies(..), noDependencies,
        NameCache(..), OrigNameCache,
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
        ModBreaks (..), BreakIndex, emptyModBreaks,

        -- * Vectorisation information
        VectInfo(..), IfaceVectInfo(..), noVectInfo, plusVectInfo,
        noIfaceVectInfo, isNoIfaceVectInfo,

        -- * Safe Haskell information
        IfaceTrustInfo, getSafeMode, setSafeMode, noIfaceTrustInfo,
        trustInfoToNum, numToTrustInfo, IsSafeImport,

        -- * result of the parser
        HsParsedModule(..),

        -- * Compilation errors and warnings
        SourceError, GhcApiError, mkSrcErr, srcErrorMessages, mkApiErr,
        throwOneError, handleSourceError,
        handleFlagWarnings, printOrThrowWarnings,
    ) where

#include "HsVersions.h"

#ifdef GHCI
import ByteCodeAsm      ( CompiledByteCode )
import InteractiveEvalTypes ( Resume )
#endif

import HsSyn
import RdrName
import Avail
import Module
import InstEnv          ( InstEnv, ClsInst )
import FamInstEnv
import Rules            ( RuleBase )
import CoreSyn          ( CoreProgram )
import Name
import NameEnv
import NameSet
import VarEnv
import VarSet
import Var
import Id
import IdInfo           ( IdDetails(..) )
import Type

import Annotations
import Class
import TyCon
import CoAxiom
import DataCon
import PrelNames        ( gHC_PRIM, ioTyConName, printName )
import Packages hiding  ( Version(..) )
import DynFlags
import DriverPhases     ( Phase, HscSource(..), isHsBoot, hscSourceString )
import BasicTypes
import IfaceSyn
import CoreSyn          ( CoreRule, CoreVect )
import Maybes
import Outputable
import BreakArray
import SrcLoc
import Unique
import UniqFM
import UniqSupply
import FastString
import StringBuffer     ( StringBuffer )
import Fingerprint
import MonadUtils
import Bag
import Binary
import ErrUtils
import Platform
import Util
import Serialized

import Control.Monad    ( mplus, guard, liftM, when, ap )
import Data.Array       ( Array, array )
import Data.IORef
import Data.Time
import Data.Word
import Data.Typeable    ( Typeable )
import Exception
import System.FilePath

-- -----------------------------------------------------------------------------
-- Compilation state
-- -----------------------------------------------------------------------------

-- | Status of a compilation to hard-code
data HscStatus
    = HscNotGeneratingCode
    | HscUpToDate
    | HscUpdateBoot
    | HscRecomp CgGuts ModSummary

-- -----------------------------------------------------------------------------
-- The Hsc monad: Passing an environment and warning state

newtype Hsc a = Hsc (HscEnv -> WarningMessages -> IO (a, WarningMessages))

instance Functor Hsc where
    fmap = liftM

instance Applicative Hsc where
    pure = return
    (<*>) = ap

instance Monad Hsc where
    return a    = Hsc $ \_ w -> return (a, w)
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

-- A variant of runHsc that switches in the DynFlags from the
-- InteractiveContext before running the Hsc computation.
--
runInteractiveHsc :: HscEnv -> Hsc a -> IO a
runInteractiveHsc hsc_env =
  runHsc (hsc_env { hsc_dflags = ic_dflags (hsc_IC hsc_env) })

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

throwOneError :: MonadIO m => ErrMsg -> m ab
throwOneError err = liftIO $ throwIO $ mkSrcErr $ unitBag err

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
  deriving Typeable

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
  deriving Typeable

instance Show GhcApiError where
  show (GhcApiError msg) = msg

instance Exception GhcApiError

-- | Given a bag of warnings, turn them into an exception if
-- -Werror is enabled, or print them out otherwise.
printOrThrowWarnings :: DynFlags -> Bag WarnMsg -> IO ()
printOrThrowWarnings dflags warns
  | gopt Opt_WarnIsError dflags
  = when (not (isEmptyBag warns)) $ do
      throwIO $ mkSrcErr $ warns `snocBag` warnIsErrorMsg dflags
  | otherwise
  = printBagOfErrors dflags warns

handleFlagWarnings :: DynFlags -> [Located String] -> IO ()
handleFlagWarnings dflags warns
 = when (wopt Opt_WarnDeprecatedFlags dflags) $ do
        -- It would be nicer if warns :: [Located MsgDoc], but that
        -- has circular import problems.
      let bag = listToBag [ mkPlainWarnMsg dflags loc (text warn)
                          | L loc warn <- warns ]

      printOrThrowWarnings dflags bag
\end{code}

%************************************************************************
%*                                                                      *
\subsection{HscEnv}
%*                                                                      *
%************************************************************************

\begin{code}

-- | Hscenv is like 'Session', except that some of the fields are immutable.
-- An HscEnv is used to compile a single module from plain Haskell source
-- code (after preprocessing) to either C, assembly or C--.  Things like
-- the module graph don't change during a single compilation.
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
        hsc_MLC  :: {-# UNPACK #-} !(IORef ModLocationCache),
                -- ^ This caches the location of modules, so we don't have to
                -- search the filesystem multiple times. See also 'hsc_FC'.

        hsc_type_env_var :: Maybe (Module, IORef TypeEnv)
                -- ^ Used for one-shot compilation only, to initialise
                -- the 'IfGblEnv'. See 'TcRnTypes.tcg_type_env_var' for
                -- 'TcRunTypes.TcGblEnv'
 }

instance ContainsDynFlags HscEnv where
    extractDynFlags env = hsc_dflags env
    replaceDynFlags env dflags = env {hsc_dflags = dflags}

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
      targetContents     :: Maybe (StringBuffer,UTCTime)
                                        -- ^ in-memory text buffer?
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
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Package and Module Tables}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Helps us find information about modules in the home package
type HomePackageTable  = ModuleNameEnv HomeModInfo
        -- Domain = modules in the home package that have been fully compiled
        -- "home" package name cached here for convenience

-- | Helps us find information about modules in the imported packages
type PackageIfaceTable = ModuleEnv ModIface
        -- Domain = modules in the imported packages

-- | Constructs an empty HomePackageTable
emptyHomePackageTable :: HomePackageTable
emptyHomePackageTable  = emptyUFM

-- | Constructs an empty PackageIfaceTable
emptyPackageIfaceTable :: PackageIfaceTable
emptyPackageIfaceTable = emptyModuleEnv

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
        :: DynFlags
        -> HomePackageTable
        -> PackageIfaceTable
        -> Module
        -> Maybe ModIface
lookupIfaceByModule dflags hpt pit mod
  | modulePackageId mod == thisPackage dflags
        -- The module comes from the home package, so look first
        -- in the HPT.  If it's not from the home package it's wrong to look
        -- in the HPT, because the HPT is indexed by *ModuleName* not Module
  = fmap hm_iface (lookupUFM hpt (moduleName mod))
    `mplus` lookupModuleEnv pit mod

  | otherwise = lookupModuleEnv pit mod         -- Look in PIT only

-- If the module does come from the home package, why do we look in the PIT as well?
-- (a) In OneShot mode, even home-package modules accumulate in the PIT
-- (b) Even in Batch (--make) mode, there is *one* case where a home-package
--     module is in the PIT, namely GHC.Prim when compiling the base package.
-- We could eliminate (b) if we wanted, by making GHC.Prim belong to a package
-- of its own, but it doesn't seem worth the bother.


-- | Find all the instance declarations (of classes and families) that are in
-- modules imported by this one, directly or indirectly, and are in the Home
-- Package Table.  This ensures that we don't see instances from modules @--make@
-- compiled before this one, but which are not below this one.
hptInstances :: HscEnv -> (ModuleName -> Bool) -> ([ClsInst], [FamInst])
hptInstances hsc_env want_this_module
  = let (insts, famInsts) = unzip $ flip hptAllThings hsc_env $ \mod_info -> do
                guard (want_this_module (moduleName (mi_module (hm_iface mod_info))))
                let details = hm_details mod_info
                return (md_insts details, md_fam_insts details)
    in (concat insts, concat famInsts)

-- | Get the combined VectInfo of all modules in the home package table. In
-- contrast to instances and rules, we don't care whether the modules are
-- "below" us in the dependency sense. The VectInfo of those modules not "below"
-- us does not affect the compilation of the current module.
hptVectInfo :: HscEnv -> VectInfo
hptVectInfo = concatVectInfo . hptAllThings ((: []) . md_vect_info . hm_details)

-- | Get rules from modules "below" this one (in the dependency sense)
hptRules :: HscEnv -> [(ModuleName, IsBootInterface)] -> [CoreRule]
hptRules = hptSomeThingsBelowUs (md_rules . hm_details) False


-- | Get annotations from modules "below" this one (in the dependency sense)
hptAnns :: HscEnv -> Maybe [(ModuleName, IsBootInterface)] -> [Annotation]
hptAnns hsc_env (Just deps) = hptSomeThingsBelowUs (md_anns . hm_details) False hsc_env deps
hptAnns hsc_env Nothing = hptAllThings (md_anns . hm_details) hsc_env

hptAllThings :: (HomeModInfo -> [a]) -> HscEnv -> [a]
hptAllThings extract hsc_env = concatMap extract (eltsUFM (hsc_HPT hsc_env))

-- | Get things from modules "below" this one (in the dependency sense)
-- C.f Inst.hptInstances
hptSomeThingsBelowUs :: (HomeModInfo -> [a]) -> Bool -> HscEnv -> [(ModuleName, IsBootInterface)] -> [a]
hptSomeThingsBelowUs extract include_hi_boot hsc_env deps
  | isOneShot (ghcMode (hsc_dflags hsc_env)) = []

  | otherwise
  = let hpt = hsc_HPT hsc_env
    in
    [ thing
    |   -- Find each non-hi-boot module below me
      (mod, is_boot_mod) <- deps
    , include_hi_boot || not is_boot_mod

        -- unsavoury: when compiling the base package with --make, we
        -- sometimes try to look up RULES etc for GHC.Prim. GHC.Prim won't
        -- be in the HPT, because we never compile it; it's in the EPT
        -- instead. ToDo: clean up, and remove this slightly bogus filter:
    , mod /= moduleName gHC_PRIM

        -- Look it up in the HPT
    , let things = case lookupUFM hpt mod of
                    Just info -> extract info
                    Nothing -> pprTrace "WARNING in hptSomeThingsBelowUs" msg []
          msg = vcat [ptext (sLit "missing module") <+> ppr mod,
                      ptext (sLit "Probable cause: out-of-date interface files")]
                        -- This really shouldn't happen, but see Trac #962

        -- And get its dfuns
    , thing <- things ]

hptObjs :: HomePackageTable -> [FilePath]
hptObjs hpt = concat (map (maybe [] linkableObjs . hm_linkable) (eltsUFM hpt))
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Dealing with Annotations}
%*                                                                      *
%************************************************************************

\begin{code}
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
\end{code}

%************************************************************************
%*                                                                      *
\subsection{The Finder cache}
%*                                                                      *
%************************************************************************

\begin{code}
-- | The 'FinderCache' maps home module names to the result of
-- searching for that module. It records the results of searching for
-- modules along the search path. On @:load@, we flush the entire
-- contents of this cache.
--
-- Although the @FinderCache@ range is 'FindResult' for convenience,
-- in fact it will only ever contain 'Found' or 'NotFound' entries.
--
type FinderCache = ModuleNameEnv FindResult

-- | The result of searching for an imported module.
data FindResult
  = Found ModLocation Module
        -- ^ The module was found
  | NoPackage PackageId
        -- ^ The requested package was not found
  | FoundMultiple [PackageId]
        -- ^ _Error_: both in multiple packages

        -- | Not found
  | NotFound
      { fr_paths       :: [FilePath]       -- Places where I looked

      , fr_pkg         :: Maybe PackageId  -- Just p => module is in this package's
                                           --           manifest, but couldn't find
                                           --           the .hi file

      , fr_mods_hidden :: [PackageId]      -- Module is in these packages,
                                           --   but the *module* is hidden

      , fr_pkgs_hidden :: [PackageId]      -- Module is in these packages,
                                           --   but the *package* is hidden

      , fr_suggestions :: [Module]         -- Possible mis-spelled modules
      }

-- | Cache that remembers where we found a particular module.  Contains both
-- home modules and package modules.  On @:load@, only home modules are
-- purged from this cache.
type ModLocationCache = ModuleEnv ModLocation
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Symbol tables and Module details}
%*                                                                      *
%************************************************************************

\begin{code}
-- | A 'ModIface' plus a 'ModDetails' summarises everything we know
-- about a compiled module.  The 'ModIface' is the stuff *before* linking,
-- and can be written out to an interface file. The 'ModDetails is after
-- linking and can be completely recovered from just the 'ModIface'.
--
-- When we read an interface file, we also construct a 'ModIface' from it,
-- except that we explicitly make the 'mi_decls' and a few other fields empty;
-- as when reading we consolidate the declarations etc. into a number of indexed
-- maps and environments in the 'ExternalPackageState'.
data ModIface
  = ModIface {
        mi_module     :: !Module,             -- ^ Name of the module we are for
        mi_iface_hash :: !Fingerprint,        -- ^ Hash of the whole interface
        mi_mod_hash   :: !Fingerprint,        -- ^ Hash of the ABI only
        mi_flag_hash  :: !Fingerprint,        -- ^ Hash of the important flags
                                              -- used when compiling this module

        mi_orphan     :: !WhetherHasOrphans,  -- ^ Whether this module has orphans
        mi_finsts     :: !WhetherHasFamInst,  -- ^ Whether this module has family instances
        mi_boot       :: !IsBootInterface,    -- ^ Read from an hi-boot file?

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

        mi_exp_hash :: !Fingerprint,
                -- ^ Hash of export list

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


        mi_decls    :: [(Fingerprint,IfaceDecl)],
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
        mi_orphan_hash :: !Fingerprint,    -- ^ Hash for orphan rules, class and family
                                           -- instances, and vectorise pragmas combined

        mi_vect_info :: !IfaceVectInfo,    -- ^ Vectorisation information

                -- Cached environments for easy lookup
                -- These are computed (lazily) from other fields
                -- and are not put into the interface file
        mi_warn_fn   :: Name -> Maybe WarningTxt,        -- ^ Cached lookup for 'mi_warns'
        mi_fix_fn    :: OccName -> Fixity,               -- ^ Cached lookup for 'mi_fixities'
        mi_ann_fn    :: OccName -> [Serialized],         -- ^ Cached lookup for 'mi_anns'
        mi_hash_fn   :: OccName -> Maybe (OccName, Fingerprint),
                -- ^ Cached lookup for 'mi_decls'.
                -- The @Nothing@ in 'mi_hash_fn' means that the thing
                -- isn't in decls. It's useful to know that when
                -- seeing if we are up to date wrt. the old interface.
                -- The 'OccName' is the parent of the name, if it has one.

        mi_hpc       :: !AnyHpcUsage,
                -- ^ True if this program uses Hpc at any point in the program.

        mi_trust     :: !IfaceTrustInfo,
                -- ^ Safe Haskell Trust information for this module.

        mi_trust_pkg :: !Bool
                -- ^ Do we require the package this module resides in be trusted
                -- to trust this module? This is used for the situation where a
                -- module is Safe (so doesn't require the package be trusted
                -- itself) but imports some trustworthy modules from its own
                -- package (which does require its own package be trusted).
                -- See Note [RnNames . Trust Own Package]
     }

instance Binary ModIface where
   put_ bh (ModIface {
                 mi_module    = mod,
                 mi_boot      = is_boot,
                 mi_iface_hash= iface_hash,
                 mi_mod_hash  = mod_hash,
                 mi_flag_hash = flag_hash,
                 mi_orphan    = orphan,
                 mi_finsts    = hasFamInsts,
                 mi_deps      = deps,
                 mi_usages    = usages,
                 mi_exports   = exports,
                 mi_exp_hash  = exp_hash,
                 mi_used_th   = used_th,
                 mi_fixities  = fixities,
                 mi_warns     = warns,
                 mi_anns      = anns,
                 mi_decls     = decls,
                 mi_insts     = insts,
                 mi_fam_insts = fam_insts,
                 mi_rules     = rules,
                 mi_orphan_hash = orphan_hash,
                 mi_vect_info = vect_info,
                 mi_hpc       = hpc_info,
                 mi_trust     = trust,
                 mi_trust_pkg = trust_pkg }) = do
        put_ bh mod
        put_ bh is_boot
        put_ bh iface_hash
        put_ bh mod_hash
        put_ bh flag_hash
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
        put_ bh vect_info
        put_ bh hpc_info
        put_ bh trust
        put_ bh trust_pkg

   get bh = do
        mod_name    <- get bh
        is_boot     <- get bh
        iface_hash  <- get bh
        mod_hash    <- get bh
        flag_hash   <- get bh
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
        vect_info   <- get bh
        hpc_info    <- get bh
        trust       <- get bh
        trust_pkg   <- get bh
        return (ModIface {
                 mi_module      = mod_name,
                 mi_boot        = is_boot,
                 mi_iface_hash  = iface_hash,
                 mi_mod_hash    = mod_hash,
                 mi_flag_hash   = flag_hash,
                 mi_orphan      = orphan,
                 mi_finsts      = hasFamInsts,
                 mi_deps        = deps,
                 mi_usages      = usages,
                 mi_exports     = exports,
                 mi_exp_hash    = exp_hash,
                 mi_used_th     = used_th,
                 mi_anns        = anns,
                 mi_fixities    = fixities,
                 mi_warns       = warns,
                 mi_decls       = decls,
                 mi_globals     = Nothing,
                 mi_insts       = insts,
                 mi_fam_insts   = fam_insts,
                 mi_rules       = rules,
                 mi_orphan_hash = orphan_hash,
                 mi_vect_info   = vect_info,
                 mi_hpc         = hpc_info,
                 mi_trust       = trust,
                 mi_trust_pkg   = trust_pkg,
                        -- And build the cached values
                 mi_warn_fn     = mkIfaceWarnCache warns,
                 mi_fix_fn      = mkIfaceFixCache fixities,
                 mi_ann_fn      = mkIfaceAnnCache anns,
                 mi_hash_fn     = mkIfaceHashCache decls })

-- | The original names declared of a certain module that are exported
type IfaceExport = AvailInfo

-- | Constructs an empty ModIface
emptyModIface :: Module -> ModIface
emptyModIface mod
  = ModIface { mi_module      = mod,
               mi_iface_hash  = fingerprint0,
               mi_mod_hash    = fingerprint0,
               mi_flag_hash   = fingerprint0,
               mi_orphan      = False,
               mi_finsts      = False,
               mi_boot        = False,
               mi_deps        = noDependencies,
               mi_usages      = [],
               mi_exports     = [],
               mi_exp_hash    = fingerprint0,
               mi_used_th     = False,
               mi_fixities    = [],
               mi_warns       = NoWarnings,
               mi_anns        = [],
               mi_insts       = [],
               mi_fam_insts   = [],
               mi_rules       = [],
               mi_decls       = [],
               mi_globals     = Nothing,
               mi_orphan_hash = fingerprint0,
               mi_vect_info   = noIfaceVectInfo,
               mi_warn_fn     = emptyIfaceWarnCache,
               mi_fix_fn      = emptyIfaceFixCache,
               mi_ann_fn      = emptyIfaceAnnCache,
               mi_hash_fn     = emptyIfaceHashCache,
               mi_hpc         = False,
               mi_trust       = noIfaceTrustInfo,
               mi_trust_pkg   = False }


-- | Constructs cache for the 'mi_hash_fn' field of a 'ModIface'
mkIfaceHashCache :: [(Fingerprint,IfaceDecl)]
                 -> (OccName -> Maybe (OccName, Fingerprint))
mkIfaceHashCache pairs
  = \occ -> lookupOccEnv env occ
  where
    env = foldr add_decl emptyOccEnv pairs
    add_decl (v,d) env0 = foldr add env0 (ifaceDeclFingerprints v d)
      where
        add (occ,hash) env0 = extendOccEnv env0 occ (occ,hash)

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
        md_insts     :: ![ClsInst],    -- ^ 'DFunId's for the instances in this module
        md_fam_insts :: ![FamInst],
        md_rules     :: ![CoreRule],    -- ^ Domain may include 'Id's from other modules
        md_anns      :: ![Annotation],  -- ^ Annotations present in this module: currently
                                        -- they only annotate things also declared in this module
        md_vect_info :: !VectInfo       -- ^ Module vectorisation information
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
                 md_vect_info = noVectInfo }

-- | Records the modules directly imported by a module for extracting e.g. usage information
type ImportedMods = ModuleEnv [ImportedModsVal]
type ImportedModsVal = (ModuleName, Bool, SrcSpan, IsSafeImport)

-- | A ModGuts is carried through the compiler, accumulating stuff as it goes
-- There is only one ModGuts at any time, the one for the module
-- being compiled right now.  Once it is compiled, a 'ModIface' and
-- 'ModDetails' are extracted and the ModGuts is discarded.
data ModGuts
  = ModGuts {
        mg_module    :: !Module,         -- ^ Module being compiled
        mg_boot      :: IsBootInterface, -- ^ Whether it's an hs-boot module
        mg_exports   :: ![AvailInfo],    -- ^ What it exports
        mg_deps      :: !Dependencies,   -- ^ What it depends on, directly or
                                         -- otherwise
        mg_dir_imps  :: !ImportedMods,   -- ^ Directly-imported modules; used to
                                         -- generate initialisation code
        mg_used_names:: !NameSet,        -- ^ What the module needed (used in 'MkIface.mkIface')

        mg_used_th   :: !Bool,           -- ^ Did we run a TH splice?
        mg_rdr_env   :: !GlobalRdrEnv,   -- ^ Top-level lexical environment

        -- These fields all describe the things **declared in this module**
        mg_fix_env   :: !FixityEnv,      -- ^ Fixities declared in this module
                                         -- ToDo: I'm unconvinced this is actually used anywhere
        mg_tcs       :: ![TyCon],        -- ^ TyCons declared in this module
                                         -- (includes TyCons for classes)
        mg_insts     :: ![ClsInst],      -- ^ Class instances declared in this module
        mg_fam_insts :: ![FamInst],
                                         -- ^ Family instances declared in this module
        mg_rules     :: ![CoreRule],     -- ^ Before the core pipeline starts, contains
                                         -- See Note [Overall plumbing for rules] in Rules.lhs
        mg_binds     :: !CoreProgram,    -- ^ Bindings for this module
        mg_foreign   :: !ForeignStubs,   -- ^ Foreign exports declared in this module
        mg_warns     :: !Warnings,       -- ^ Warnings declared in the module
        mg_anns      :: [Annotation],    -- ^ Annotations declared in this module
        mg_hpc_info  :: !HpcInfo,        -- ^ Coverage tick boxes in the module
        mg_modBreaks :: !ModBreaks,      -- ^ Breakpoints for the module
        mg_vect_decls:: ![CoreVect],     -- ^ Vectorisation declarations in this module
                                         --   (produced by desugarer & consumed by vectoriser)
        mg_vect_info :: !VectInfo,       -- ^ Pool of vectorised declarations in the module

        -- The next two fields are unusual, because they give instance
        -- environments for *all* modules in the home package, including
        -- this module, rather than for *just* this module.
        -- Reason: when looking up an instance we don't want to have to
        --        look at each module in the home package in turn
        mg_inst_env     :: InstEnv,
        -- ^ Class instance environment from /home-package/ modules (including
        -- this one); c.f. 'tcg_inst_env'
        mg_fam_inst_env :: FamInstEnv,
        -- ^ Type-family instance enviroment for /home-package/ modules
        -- (including this one); c.f. 'tcg_fam_inst_env'
        mg_safe_haskell :: SafeHaskellMode,
        -- ^ Safe Haskell mode
        mg_trust_pkg    :: Bool,
        -- ^ Do we need to trust our own package for Safe Haskell?
        -- See Note [RnNames . Trust Own Package]
        mg_dependent_files :: [FilePath] -- ^ dependencies from addDependentFile
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
                -- data constructor workers; reason: we we regard them
                -- as part of the code-gen of tycons

        cg_foreign   :: !ForeignStubs,   -- ^ Foreign export stubs
        cg_dep_pkgs  :: ![PackageId],    -- ^ Dependent packages, used to
                                         -- generate #includes for C code gen
        cg_hpc_info  :: !HpcInfo,        -- ^ Program coverage tick box information
        cg_modBreaks :: !ModBreaks       -- ^ Module breakpoints
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
\end{code}

%************************************************************************
%*                                                                      *
\subsection{The interactive context}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Interactive context, recording information about the state of the
-- context in which statements are executed in a GHC session.
data InteractiveContext
  = InteractiveContext {
         ic_dflags     :: DynFlags,
             -- ^ The 'DynFlags' used to evaluate interative expressions
             -- and statements.

         ic_monad      :: Name,
             -- ^ The monad that GHCi is executing in

         ic_imports    :: [InteractiveImport],
             -- ^ The GHCi context is extended with these imports
             --
             -- This field is only stored here so that the client
             -- can retrieve it with GHC.getContext. GHC itself doesn't
             -- use it, but does reset it to empty sometimes (such
             -- as before a GHC.load). The context is set with GHC.setContext.

         ic_rn_gbl_env :: GlobalRdrEnv,
             -- ^ The cached 'GlobalRdrEnv', built by
             -- 'InteractiveEval.setContext' and updated regularly

         ic_tythings   :: [TyThing],
             -- ^ TyThings defined by the user, in reverse order of
             -- definition.  At a breakpoint, this list includes the
             -- local variables in scope at that point

         ic_sys_vars   :: [Id],
             -- ^ Variables defined automatically by the system (e.g.
             -- record field selectors).  See Notes [ic_sys_vars]

         ic_instances  :: ([ClsInst], [FamInst]),
             -- ^ All instances and family instances created during
             -- this session.  These are grabbed en masse after each
             -- update to be sure that proper overlapping is retained.
             -- That is, rather than re-check the overlapping each
             -- time we update the context, we just take the results
             -- from the instance code that already does that.

         ic_fix_env :: FixityEnv,
            -- ^ Fixities declared in let statements

         ic_int_print  :: Name,
             -- ^ The function that is used for printing results
             -- of expressions in ghci and -e mode.

         ic_default :: Maybe [Type],
             -- ^ The current default types, set by a 'default' declaration

#ifdef GHCI
          ic_resume :: [Resume],
             -- ^ The stack of breakpoint contexts
#endif

          ic_cwd :: Maybe FilePath
             -- virtual CWD of the program
    }

{-
Note [ic_sys_vars]
~~~~~~~~~~~~~~~~~~
This list constains any Ids that arise from TyCons, Classes or
instances defined interactively, but that are not given by
'implicitTyThings'.  This includes record selectors, default methods,
and dfuns.

We *could* get rid of this list and generate these Ids from
ic_tythings:

   - dfuns come from Instances
   - record selectors from TyCons
   - default methods from Classes

For record selectors the TyCon gives the Name, but in order to make an
Id we would have to construct the type ourselves.  Similarly for
default methods.  So for now we collect the Ids after tidying (see
hscDeclsWithLocation) and save them in ic_sys_vars.
-}

-- | Constructs an empty InteractiveContext.
emptyInteractiveContext :: DynFlags -> InteractiveContext
emptyInteractiveContext dflags
  = InteractiveContext { ic_dflags     = dflags,
                         -- IO monad by default
                         ic_monad      = ioTyConName,
                         ic_imports    = [],
                         ic_rn_gbl_env = emptyGlobalRdrEnv,
                         ic_tythings   = [],
                         ic_sys_vars   = [],
                         ic_instances  = ([],[]),
                         ic_fix_env    = emptyNameEnv,
                         -- System.IO.print by default
                         ic_int_print  = printName,
                         ic_default    = Nothing,
#ifdef GHCI
                         ic_resume     = [],
#endif
                         ic_cwd        = Nothing }

-- | This function returns the list of visible TyThings (useful for
-- e.g. showBindings)
icInScopeTTs :: InteractiveContext -> [TyThing]
icInScopeTTs = ic_tythings

-- | Get the PrintUnqualified function based on the flags and this InteractiveContext
icPrintUnqual :: DynFlags -> InteractiveContext -> PrintUnqualified
icPrintUnqual dflags InteractiveContext{ ic_rn_gbl_env = grenv } =
    mkPrintUnqualified dflags grenv

-- | This function is called with new TyThings recently defined to update the
-- InteractiveContext to include them.  Ids are easily removed when shadowed,
-- but Classes and TyCons are not.  Some work could be done to determine
-- whether they are entirely shadowed, but as you could still have references
-- to them (e.g. instances for classes or values of the type for TyCons), it's
-- not clear whether removing them is even the appropriate behavior.
extendInteractiveContext :: InteractiveContext -> [TyThing] -> InteractiveContext
extendInteractiveContext ictxt new_tythings
  = ictxt { ic_tythings   = new_tythings ++ old_tythings
          , ic_rn_gbl_env = new_tythings `icPlusGblRdrEnv` ic_rn_gbl_env ictxt
          }
  where
    old_tythings = filter (not . shadowed) (ic_tythings ictxt)

    shadowed (AnId id) = ((`elem` new_names) . nameOccName . idName) id
    shadowed _         = False

    new_names = [ nameOccName (getName id) | AnId id <- new_tythings ]

setInteractivePrintName :: InteractiveContext -> Name -> InteractiveContext
setInteractivePrintName ic n = ic{ic_int_print = n}

    -- ToDo: should not add Ids to the gbl env here

-- | Add TyThings to the GlobalRdrEnv, earlier ones in the list shadowing
-- later ones, and shadowing existing entries in the GlobalRdrEnv.
icPlusGblRdrEnv :: [TyThing] -> GlobalRdrEnv -> GlobalRdrEnv
icPlusGblRdrEnv tythings env = extendOccEnvList env list
  where new_gres = gresFromAvails LocalDef (map tyThingAvailInfo tythings)
        list = [ (nameOccName (gre_name gre), [gre]) | gre <- new_gres ]

substInteractiveContext :: InteractiveContext -> TvSubst -> InteractiveContext
substInteractiveContext ictxt subst
    | isEmptyTvSubst subst = ictxt

substInteractiveContext ictxt@InteractiveContext{ ic_tythings = tts } subst
    = ictxt { ic_tythings = map subst_ty tts }
  where subst_ty (AnId id) = AnId $ id `setIdType` substTy subst (idType id)
        subst_ty tt        = tt

data InteractiveImport
  = IIDecl (ImportDecl RdrName)
      -- ^ Bring the exports of a particular module
      -- (filtered by an import decl) into scope

  | IIModule ModuleName
      -- ^ Bring into scope the entire top-level envt of
      -- of this module, including the things imported
      -- into it.

instance Outputable InteractiveImport where
  ppr (IIModule m) = char '*' <> ppr m
  ppr (IIDecl d)   = ppr d

\end{code}

%************************************************************************
%*                                                                      *
        Building a PrintUnqualified
%*                                                                      *
%************************************************************************

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

\begin{code}
-- | Creates some functions that work out the best ways to format
-- names for the user according to a set of heuristics
mkPrintUnqualified :: DynFlags -> GlobalRdrEnv -> PrintUnqualified
mkPrintUnqualified dflags env = (qual_name, qual_mod)
  where
  qual_name name
        | [gre] <- unqual_gres, right_name gre = NameUnqual
                -- If there's a unique entity that's in scope unqualified with 'occ'
                -- AND that entity is the right one, then we can use the unqualified name

        | [gre] <- qual_gres = NameQual (get_qual_mod (gre_prov gre))

        | null qual_gres =
              if null (lookupGRE_RdrName (mkRdrQual (moduleName mod) occ) env)
                   then NameNotInScope1
                   else NameNotInScope2

        | otherwise = NameNotInScope1   -- Can happen if 'f' is bound twice in the module
                                        -- Eg  f = True; g = 0; f = False
      where
        mod = nameModule name
        occ = nameOccName name

        is_rdr_orig = nameUnique name == mkUniqueGrimily 0
         -- Note [Outputable Orig RdrName]

        right_name gre
          | is_rdr_orig = nameModule_maybe (gre_name gre) == Just mod
          | otherwise   = gre_name gre == name

        unqual_gres = lookupGRE_RdrName (mkRdrUnqual occ) env
        qual_gres   = filter right_name (lookupGlobalRdrEnv env occ)

        get_qual_mod LocalDef      = moduleName mod
        get_qual_mod (Imported is) = ASSERT( not (null is) ) is_as (is_decl (head is))

    -- we can mention a module P:M without the P: qualifier iff
    -- "import M" would resolve unambiguously to P:M.  (if P is the
    -- current package we can just assume it is unqualified).

  qual_mod mod
     | modulePackageId mod == thisPackage dflags = False

     | [pkgconfig] <- [pkg | (pkg,exposed_module) <- lookup,
                             exposed pkg && exposed_module],
       packageConfigId pkgconfig == modulePackageId mod
        -- this says: we are given a module P:M, is there just one exposed package
        -- that exposes a module M, and is it package P?
     = False

     | otherwise = True
     where lookup = lookupModuleInAllPackages dflags (moduleName mod)

-- Note [Outputable Orig RdrName]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This is a Grotesque Hack.  The Outputable instance for RdrEnv wants
-- to print Orig names, which are just pairs of (Module,OccName).  But
-- we want to use full Names here, because in GHCi we might have Ids
-- that have the same (Module,OccName) pair but a different Unique
-- (this happens when you shadow a TyCon or Class in GHCi).
--
-- So in Outputable RdrName we just use a dummy Unique (0), and check
-- for it here.
--
-- Arguably GHCi is invalidating the assumption that (Module,OccName)
-- uniquely identifies an entity.  But we do want to be able to shadow
-- old declarations with new ones in GHCi, and it would be hard to
-- delete all references to the old declaration when that happened.
-- See also Note [interactive name cache] in IfaceEnv for somewhere
-- else that this broken assumption bites.
--
\end{code}


%************************************************************************
%*                                                                      *
                Implicit TyThings
%*                                                                      *
%************************************************************************

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

  * Dictionary function Ids are not implict.

  * Axioms for newtypes are implicit (same as above), but axioms
    for data/type family instances are *not* implicit (like DFunIds).

\begin{code}
-- | Determine the 'TyThing's brought into scope by another 'TyThing'
-- /other/ than itself. For example, Id's don't have any implicit TyThings
-- as they just bring themselves into scope, but classes bring their
-- dictionary datatype, type constructor and some selector functions into
-- scope, just for a start!

-- N.B. the set of TyThings returned here *must* match the set of
-- names returned by LoadIface.ifaceDeclImplicitBndrs, in the sense that
-- TyThing.getOccName should define a bijection between the two lists.
-- This invariant is used in LoadIface.loadDecl (see note [Tricky iface loop])
-- The order of the list does not matter.
implicitTyThings :: TyThing -> [TyThing]
implicitTyThings (AnId _)       = []
implicitTyThings (ACoAxiom _cc) = []
implicitTyThings (ATyCon tc)    = implicitTyConThings tc
implicitTyThings (ADataCon dc)  = map AnId (dataConImplicitIds dc)
    -- For data cons add the worker and (possibly) wrapper

implicitClassThings :: Class -> [TyThing]
implicitClassThings cl
  = -- Does not include default methods, because those Ids may have
    --    their own pragmas, unfoldings etc, not derived from the Class object
    -- associated types
    --    No extras_plus (recursive call) for the classATs, because they
    --    are only the family decls; they have no implicit things
    map ATyCon (classATs cl) ++
    -- superclass and operation selectors
    map AnId (classAllSelIds cl)

implicitTyConThings :: TyCon -> [TyThing]
implicitTyConThings tc
  = class_stuff ++
      -- fields (names of selectors)

      -- (possibly) implicit newtype coercion
    implicitCoTyCon tc ++

      -- for each data constructor in order,
      --   the contructor, worker, and (possibly) wrapper
    concatMap (extras_plus . ADataCon) (tyConDataCons tc)
      -- NB. record selectors are *not* implicit, they have fully-fledged
      -- bindings that pass through the compilation pipeline as normal.
  where
    class_stuff = case tyConClass_maybe tc of
        Nothing -> []
        Just cl -> implicitClassThings cl

-- add a thing and recursive call
extras_plus :: TyThing -> [TyThing]
extras_plus thing = thing : implicitTyThings thing

-- For newtypes and closed type families (only) add the implicit coercion tycon
implicitCoTyCon :: TyCon -> [TyThing]
implicitCoTyCon tc
  | Just co <- newTyConCo_maybe tc = [ACoAxiom $ toBranchedAxiom co]
  | Just co <- isClosedSynFamilyTyCon_maybe tc
                                   = [ACoAxiom co]
  | otherwise                      = []

-- | Returns @True@ if there should be no interface-file declaration
-- for this thing on its own: either it is built-in, or it is part
-- of some other declaration, or it is generated implicitly by some
-- other declaration.
isImplicitTyThing :: TyThing -> Bool
isImplicitTyThing (ADataCon {}) = True
isImplicitTyThing (AnId id)     = isImplicitId id
isImplicitTyThing (ATyCon tc)   = isImplicitTyCon tc
isImplicitTyThing (ACoAxiom ax) = isImplicitCoAxiom ax

-- | tyThingParent_maybe x returns (Just p)
-- when pprTyThingInContext sould print a declaration for p
-- (albeit with some "..." in it) when asked to show x
-- It returns the *immediate* parent.  So a datacon returns its tycon
-- but the tycon could be the associated type of a class, so it in turn
-- might have a parent.
tyThingParent_maybe :: TyThing -> Maybe TyThing
tyThingParent_maybe (ADataCon dc) = Just (ATyCon (dataConTyCon dc))
tyThingParent_maybe (ATyCon tc)   = case tyConAssoc_maybe tc of
                                      Just cls -> Just (ATyCon (classTyCon cls))
                                      Nothing  -> Nothing
tyThingParent_maybe (AnId id)     = case idDetails id of
                                         RecSelId { sel_tycon = tc } -> Just (ATyCon tc)
                                         ClassOpId cls               -> Just (ATyCon (classTyCon cls))
                                         _other                      -> Nothing
tyThingParent_maybe _other = Nothing

tyThingsTyVars :: [TyThing] -> TyVarSet
tyThingsTyVars tts =
    unionVarSets $ map ttToVarSet tts
    where
        ttToVarSet (AnId id)     = tyVarsOfType $ idType id
        ttToVarSet (ADataCon dc) = tyVarsOfType $ dataConRepType dc
        ttToVarSet (ATyCon tc)
          = case tyConClass_maybe tc of
              Just cls -> (mkVarSet . fst . classTvsFds) cls
              Nothing  -> tyVarsOfType $ tyConKind tc
        ttToVarSet _             = emptyVarSet

-- | The Names that a TyThing should bring into scope.  Used to build
-- the GlobalRdrEnv for the InteractiveContext.
tyThingAvailInfo :: TyThing -> AvailInfo
tyThingAvailInfo (ATyCon t)
   = case tyConClass_maybe t of
        Just c  -> AvailTC n (n : map getName (classMethods c)
                  ++ map getName (classATs c))
             where n = getName c
        Nothing -> AvailTC n (n : map getName dcs ++
                                   concatMap dataConFieldLabels dcs)
             where n = getName t
                   dcs = tyConDataCons t
tyThingAvailInfo t
   = Avail (getName t)
\end{code}

%************************************************************************
%*                                                                      *
                TypeEnv
%*                                                                      *
%************************************************************************

\begin{code}
-- | A map from 'Name's to 'TyThing's, constructed by typechecking
-- local declarations or interface files
type TypeEnv = NameEnv TyThing

emptyTypeEnv    :: TypeEnv
typeEnvElts     :: TypeEnv -> [TyThing]
typeEnvTyCons   :: TypeEnv -> [TyCon]
typeEnvCoAxioms :: TypeEnv -> [CoAxiom Branched]
typeEnvIds      :: TypeEnv -> [Id]
typeEnvDataCons :: TypeEnv -> [DataCon]
typeEnvClasses  :: TypeEnv -> [Class]
lookupTypeEnv   :: TypeEnv -> Name -> Maybe TyThing

emptyTypeEnv        = emptyNameEnv
typeEnvElts     env = nameEnvElts env
typeEnvTyCons   env = [tc | ATyCon tc   <- typeEnvElts env]
typeEnvCoAxioms env = [ax | ACoAxiom ax <- typeEnvElts env]
typeEnvIds      env = [id | AnId id     <- typeEnvElts env]
typeEnvDataCons env = [dc | ADataCon dc <- typeEnvElts env]
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
extendTypeEnvList env things = foldl extendTypeEnv env things

extendTypeEnvWithIds :: TypeEnv -> [Id] -> TypeEnv
extendTypeEnvWithIds env ids
  = extendNameEnvList env [(getName id, AnId id) | id <- ids]

\end{code}

\begin{code}
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
  -- in one-shot, we don't use the HPT
  | not (isOneShot (ghcMode dflags)) && modulePackageId mod == this_pkg
  = do hm <- lookupUFM hpt (moduleName mod) -- Maybe monad
       x <- lookupNameEnv (md_types (hm_details hm)) name
       return x
  | otherwise
  = lookupNameEnv pte name
  where
    mod = ASSERT2( isExternalName name, ppr name ) nameModule name
    this_pkg = thisPackage dflags

-- | As 'lookupType', but with a marginally easier-to-use interface
-- if you have a 'HscEnv'
lookupTypeHscEnv :: HscEnv -> Name -> IO (Maybe TyThing)
lookupTypeHscEnv hsc_env name = do
    eps <- readIORef (hsc_EPS hsc_env)
    return $! lookupType dflags hpt (eps_PTE eps) name
  where
    dflags = hsc_dflags hsc_env
    hpt = hsc_HPT hsc_env
\end{code}

\begin{code}
-- | Get the 'TyCon' from a 'TyThing' if it is a type constructor thing. Panics otherwise
tyThingTyCon :: TyThing -> TyCon
tyThingTyCon (ATyCon tc) = tc
tyThingTyCon other       = pprPanic "tyThingTyCon" (pprTyThing other)

-- | Get the 'CoAxiom' from a 'TyThing' if it is a coercion axiom thing. Panics otherwise
tyThingCoAxiom :: TyThing -> CoAxiom Branched
tyThingCoAxiom (ACoAxiom ax) = ax
tyThingCoAxiom other         = pprPanic "tyThingCoAxiom" (pprTyThing other)

-- | Get the 'DataCon' from a 'TyThing' if it is a data constructor thing. Panics otherwise
tyThingDataCon :: TyThing -> DataCon
tyThingDataCon (ADataCon dc) = dc
tyThingDataCon other         = pprPanic "tyThingDataCon" (pprTyThing other)

-- | Get the 'Id' from a 'TyThing' if it is a id *or* data constructor thing. Panics otherwise
tyThingId :: TyThing -> Id
tyThingId (AnId id)     = id
tyThingId (ADataCon dc) = dataConWrapId dc
tyThingId other         = pprPanic "tyThingId" (pprTyThing other)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{MonadThings and friends}
%*                                                                      *
%************************************************************************

\begin{code}
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
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Auxiliary types}
%*                                                                      *
%************************************************************************

These types are defined here because they are mentioned in ModDetails,
but they are mostly elaborated elsewhere

\begin{code}
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
mkIfaceWarnCache :: Warnings -> Name -> Maybe WarningTxt
mkIfaceWarnCache NoWarnings  = \_ -> Nothing
mkIfaceWarnCache (WarnAll t) = \_ -> Just t
mkIfaceWarnCache (WarnSome pairs) = lookupOccEnv (mkOccEnv pairs) . nameOccName

emptyIfaceWarnCache :: Name -> Maybe WarningTxt
emptyIfaceWarnCache _ = Nothing

plusWarns :: Warnings -> Warnings -> Warnings
plusWarns d NoWarnings = d
plusWarns NoWarnings d = d
plusWarns _ (WarnAll t) = WarnAll t
plusWarns (WarnAll t) _ = WarnAll t
plusWarns (WarnSome v1) (WarnSome v2) = WarnSome (v1 ++ v2)
\end{code}

\begin{code}
-- | Creates cached lookup for the 'mi_fix_fn' field of 'ModIface'
mkIfaceFixCache :: [(OccName, Fixity)] -> OccName -> Fixity
mkIfaceFixCache pairs
  = \n -> lookupOccEnv env n `orElse` defaultFixity
  where
   env = mkOccEnv pairs

emptyIfaceFixCache :: OccName -> Fixity
emptyIfaceFixCache _ = defaultFixity

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
\end{code}

\begin{code}
-- | Creates cached lookup for the 'mi_anns' field of ModIface
mkIfaceAnnCache :: [IfaceAnnotation] -> OccName -> [Serialized]
mkIfaceAnnCache anns
  = \n -> lookupOccEnv env n `orElse` []
  where
    pair (IfaceAnnotation target value) =
      (case target of
          NamedTarget occn -> occn
          ModuleTarget _   -> mkVarOcc "module"
      , [value])
    -- flipping (++), so the first argument is always short
    env = mkOccEnv_C (flip (++)) (map pair anns)

emptyIfaceAnnCache :: OccName -> [Serialized]
emptyIfaceAnnCache _ = []
\end{code}

%************************************************************************
%*                                                                      *
\subsection{WhatsImported}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Records whether a module has orphans. An \"orphan\" is one of:
--
-- * An instance declaration in a module other than the definition
--   module for one of the type constructors or classes in the instance head
--
-- * A transformation rule in a module other than the one defining
--   the function in the head of the rule
--
-- * A vectorisation pragma
type WhetherHasOrphans   = Bool

-- | Does this module define family instances?
type WhetherHasFamInst = Bool

-- | Did this module originate from a *-boot file?
type IsBootInterface = Bool

-- | Dependency information about modules and packages below this one
-- in the import hierarchy.
--
-- Invariant: the dependencies of a module @M@ never includes @M@.
--
-- Invariant: none of the lists contain duplicates.
data Dependencies
  = Deps { dep_mods   :: [(ModuleName, IsBootInterface)]
                        -- ^ Home-package module dependencies
         , dep_pkgs   :: [(PackageId, Bool)]
                       -- ^ External package dependencies. The bool indicates
                        -- if the package is required to be trusted when the
                        -- module is imported as a safe import (Safe Haskell).
                        -- See Note [RnNames . Tracking Trust Transitively]
         , dep_orphs  :: [Module]
                        -- ^ Orphan modules (whether home or external pkg),
                        -- *not* including family instance orphans as they
                        -- are anyway included in 'dep_finsts'
         , dep_finsts :: [Module]
                        -- ^ Modules that contain family instances (whether the
                        -- instances are from the home or an external package)
         }
  deriving( Eq )
        -- Equality used only for old/new comparison in MkIface.addFingerprints
        -- See 'TcRnTypes.ImportAvails' for details on dependencies.

instance Binary Dependencies where
    put_ bh deps = do put_ bh (dep_mods deps)
                      put_ bh (dep_pkgs deps)
                      put_ bh (dep_orphs deps)
                      put_ bh (dep_finsts deps)

    get bh = do ms <- get bh
                ps <- get bh
                os <- get bh
                fis <- get bh
                return (Deps { dep_mods = ms, dep_pkgs = ps, dep_orphs = os,
                               dep_finsts = fis })

noDependencies :: Dependencies
noDependencies = Deps [] [] [] []

-- | Records modules that we depend on by making a direct import from
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
            -- ^ Fingerprint for the export list we used to depend on this module,
            -- if we depend on the export list
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
          i -> error ("Binary.get(Usage): " ++ show i)

\end{code}


%************************************************************************
%*                                                                      *
                The External Package State
%*                                                                      *
%************************************************************************

\begin{code}
type PackageTypeEnv    = TypeEnv
type PackageRuleBase   = RuleBase
type PackageInstEnv    = InstEnv
type PackageFamInstEnv = FamInstEnv
type PackageVectInfo   = VectInfo
type PackageAnnEnv     = AnnEnv

-- | Information about other packages that we have slurped in by reading
-- their interface files
data ExternalPackageState
  = EPS {
        eps_is_boot :: !(ModuleNameEnv (ModuleName, IsBootInterface)),
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
        eps_vect_info    :: !PackageVectInfo,  -- ^ The total 'VectInfo' accumulated
                                               -- from all the external-package modules
        eps_ann_env      :: !PackageAnnEnv,    -- ^ The total 'AnnEnv' accumulated
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
\end{code}

Names in a NameCache are always stored as a Global, and have the SrcLoc
of their binding locations.

Actually that's not quite right.  When we first encounter the original
name, we might not be at its binding site (e.g. we are reading an
interface file); so we give it 'noSrcLoc' then.  Later, when we find
its binding site, we fix it up.

\begin{code}
-- | The NameCache makes sure that there is just one Unique assigned for
-- each original name; i.e. (module-name, occ-name) pair and provides
-- something of a lookup mechanism for those names.
data NameCache
 = NameCache {  nsUniqs :: UniqSupply,
                -- ^ Supply of uniques
                nsNames :: OrigNameCache
                -- ^ Ensures that one original name gets one unique
   }

-- | Per-module cache of original 'OccName's given 'Name's
type OrigNameCache   = ModuleEnv (OccEnv Name)
\end{code}


\begin{code}
mkSOName :: Platform -> FilePath -> FilePath
mkSOName platform root
    = case platformOS platform of
      OSDarwin  -> ("lib" ++ root) <.> "dylib"
      OSMinGW32 ->           root  <.> "dll"
      _         -> ("lib" ++ root) <.> "so"

mkHsSOName :: Platform -> FilePath -> FilePath
mkHsSOName platform root = ("lib" ++ root) <.> soExt platform

soExt :: Platform -> FilePath
soExt platform
    = case platformOS platform of
      OSDarwin  -> "dylib"
      OSMinGW32 -> "dll"
      _         -> "so"
\end{code}


%************************************************************************
%*                                                                      *
                The module graph and ModSummary type
        A ModSummary is a node in the compilation manager's
        dependency graph, and it's also passed to hscMain
%*                                                                      *
%************************************************************************

\begin{code}
-- | A ModuleGraph contains all the nodes from the home package (only).
-- There will be a node for each source module, plus a node for each hi-boot
-- module.
--
-- The graph is not necessarily stored in topologically-sorted order.  Use
-- 'GHC.topSortModuleGraph' and 'Digraph.flattenSCC' to achieve this.
type ModuleGraph = [ModSummary]

emptyMG :: ModuleGraph
emptyMG = []

-- | A single node in a 'ModuleGraph. The nodes of the module graph are one of:
--
-- * A regular Haskell source module
--
-- * A hi-boot source module
--
-- * An external-core source module
data ModSummary
   = ModSummary {
        ms_mod          :: Module,              -- ^ Identity of the module
        ms_hsc_src      :: HscSource,           -- ^ The module source either plain Haskell, hs-boot or external core
        ms_location     :: ModLocation,         -- ^ Location of the various files belonging to the module
        ms_hs_date      :: UTCTime,             -- ^ Timestamp of source file
        ms_obj_date     :: Maybe UTCTime,       -- ^ Timestamp of object, if we have one
        ms_srcimps      :: [Located (ImportDecl RdrName)],      -- ^ Source imports of the module
        ms_textual_imps :: [Located (ImportDecl RdrName)],      -- ^ Non-source imports of the module from the module *text*
        ms_hspp_file    :: FilePath,            -- ^ Filename of preprocessed source file
        ms_hspp_opts    :: DynFlags,            -- ^ Cached flags from @OPTIONS@, @INCLUDE@
                                                -- and @LANGUAGE@ pragmas in the modules source code
        ms_hspp_buf     :: Maybe StringBuffer   -- ^ The actual preprocessed source, if we have it
     }

ms_mod_name :: ModSummary -> ModuleName
ms_mod_name = moduleName . ms_mod

ms_imps :: ModSummary -> [Located (ImportDecl RdrName)]
ms_imps ms = ms_textual_imps ms ++ map mk_additional_import (dynFlagDependencies (ms_hspp_opts ms))
  where
    -- This is a not-entirely-satisfactory means of creating an import that corresponds to an
    -- import that did not occur in the program text, such as those induced by the use of
    -- plugins (the -plgFoo flag)
    mk_additional_import mod_nm = noLoc $ ImportDecl {
      ideclName      = noLoc mod_nm,
      ideclPkgQual   = Nothing,
      ideclSource    = False,
      ideclImplicit  = True, -- Maybe implicit because not "in the program text"
      ideclQualified = False,
      ideclAs        = Nothing,
      ideclHiding    = Nothing,
      ideclSafe      = False
    }

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

-- | Did this 'ModSummary' originate from a hs-boot file?
isBootSummary :: ModSummary -> Bool
isBootSummary ms = isHsBoot (ms_hsc_src ms)

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
showModMsg dflags target recomp mod_summary
  = showSDoc dflags $
        hsep [text (mod_str ++ replicate (max 0 (16 - length mod_str)) ' '),
              char '(', text (normalise $ msHsFilePath mod_summary) <> comma,
              case target of
                  HscInterpreted | recomp
                             -> text "interpreted"
                  HscNothing -> text "nothing"
                  _          -> text (normalise $ msObjFilePath mod_summary),
              char ')']
 where
    mod     = moduleName (ms_mod mod_summary)
    mod_str = showPpr dflags mod ++ hscSourceString (ms_hsc_src mod_summary)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Recmpilation}
%*                                                                      *
%************************************************************************

\begin{code}
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
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Hpc Support}
%*                                                                      *
%************************************************************************

\begin{code}
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
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Vectorisation Support}
%*                                                                      *
%************************************************************************

The following information is generated and consumed by the vectorisation
subsystem.  It communicates the vectorisation status of declarations from one
module to another.

Why do we need both f and f_v in the ModGuts/ModDetails/EPS version VectInfo
below?  We need to know `f' when converting to IfaceVectInfo.  However, during
vectorisation, we need to know `f_v', whose `Var' we cannot lookup based
on just the OccName easily in a Core pass.

\begin{code}
-- |Vectorisation information for 'ModGuts', 'ModDetails' and 'ExternalPackageState'; see also
-- documentation at 'Vectorise.Env.GlobalEnv'.
--
-- NB: The following tables may also include 'Var's, 'TyCon's and 'DataCon's from imported modules,
--     which have been subsequently vectorised in the current module.
--
data VectInfo
  = VectInfo
    { vectInfoVar            :: VarEnv  (Var    , Var  )    -- ^ @(f, f_v)@ keyed on @f@
    , vectInfoTyCon          :: NameEnv (TyCon  , TyCon)    -- ^ @(T, T_v)@ keyed on @T@
    , vectInfoDataCon        :: NameEnv (DataCon, DataCon)  -- ^ @(C, C_v)@ keyed on @C@
    , vectInfoParallelVars   :: VarSet                      -- ^ set of parallel variables
    , vectInfoParallelTyCons :: NameSet                     -- ^ set of parallel type constructors
    }

-- |Vectorisation information for 'ModIface'; i.e, the vectorisation information propagated
-- across module boundaries.
--
-- NB: The field 'ifaceVectInfoVar' explicitly contains the workers of data constructors as well as
--     class selectors — i.e., their mappings are /not/ implicitly generated from the data types.
--     Moreover, whether the worker of a data constructor is in 'ifaceVectInfoVar' determines
--     whether that data constructor was vectorised (or is part of an abstractly vectorised type
--     constructor).
--
data IfaceVectInfo
  = IfaceVectInfo
    { ifaceVectInfoVar            :: [Name]  -- ^ All variables in here have a vectorised variant
    , ifaceVectInfoTyCon          :: [Name]  -- ^ All 'TyCon's in here have a vectorised variant;
                                             -- the name of the vectorised variant and those of its
                                             -- data constructors are determined by
                                             -- 'OccName.mkVectTyConOcc' and
                                             -- 'OccName.mkVectDataConOcc'; the names of the
                                             -- isomorphisms are determined by 'OccName.mkVectIsoOcc'
    , ifaceVectInfoTyConReuse     :: [Name]  -- ^ The vectorised form of all the 'TyCon's in here
                                             -- coincides with the unconverted form; the name of the
                                             -- isomorphisms is determined by 'OccName.mkVectIsoOcc'
    , ifaceVectInfoParallelVars   :: [Name]  -- iface version of 'vectInfoParallelVar'
    , ifaceVectInfoParallelTyCons :: [Name]  -- iface version of 'vectInfoParallelTyCon'
    }

noVectInfo :: VectInfo
noVectInfo
  = VectInfo emptyVarEnv emptyNameEnv emptyNameEnv emptyVarSet emptyNameSet

plusVectInfo :: VectInfo -> VectInfo -> VectInfo
plusVectInfo vi1 vi2 =
  VectInfo (vectInfoVar            vi1 `plusVarEnv`    vectInfoVar            vi2)
           (vectInfoTyCon          vi1 `plusNameEnv`   vectInfoTyCon          vi2)
           (vectInfoDataCon        vi1 `plusNameEnv`   vectInfoDataCon        vi2)
           (vectInfoParallelVars   vi1 `unionVarSet`   vectInfoParallelVars   vi2)
           (vectInfoParallelTyCons vi1 `unionNameSets` vectInfoParallelTyCons vi2)

concatVectInfo :: [VectInfo] -> VectInfo
concatVectInfo = foldr plusVectInfo noVectInfo

noIfaceVectInfo :: IfaceVectInfo
noIfaceVectInfo = IfaceVectInfo [] [] [] [] []

isNoIfaceVectInfo :: IfaceVectInfo -> Bool
isNoIfaceVectInfo (IfaceVectInfo l1 l2 l3 l4 l5)
  = null l1 && null l2 && null l3 && null l4 && null l5

instance Outputable VectInfo where
  ppr info = vcat
             [ ptext (sLit "variables       :") <+> ppr (vectInfoVar            info)
             , ptext (sLit "tycons          :") <+> ppr (vectInfoTyCon          info)
             , ptext (sLit "datacons        :") <+> ppr (vectInfoDataCon        info)
             , ptext (sLit "parallel vars   :") <+> ppr (vectInfoParallelVars   info)
             , ptext (sLit "parallel tycons :") <+> ppr (vectInfoParallelTyCons info)
             ]

instance Binary IfaceVectInfo where
    put_ bh (IfaceVectInfo a1 a2 a3 a4 a5) = do
        put_ bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
    get bh = do
        a1 <- get bh
        a2 <- get bh
        a3 <- get bh
        a4 <- get bh
        a5 <- get bh
        return (IfaceVectInfo a1 a2 a3 a4 a5)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Safe Haskell Support}
%*                                                                      *
%************************************************************************

This stuff here is related to supporting the Safe Haskell extension,
primarily about storing under what trust type a module has been compiled.

\begin{code}
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

numToTrustInfo :: Word8 -> IfaceTrustInfo
numToTrustInfo 0 = setSafeMode Sf_None
numToTrustInfo 1 = setSafeMode Sf_Unsafe
numToTrustInfo 2 = setSafeMode Sf_Trustworthy
numToTrustInfo 3 = setSafeMode Sf_Safe
numToTrustInfo 4 = setSafeMode Sf_SafeInferred
numToTrustInfo n = error $ "numToTrustInfo: bad input number! (" ++ show n ++ ")"

instance Outputable IfaceTrustInfo where
    ppr (TrustInfo Sf_None)          = ptext $ sLit "none"
    ppr (TrustInfo Sf_Unsafe)        = ptext $ sLit "unsafe"
    ppr (TrustInfo Sf_Trustworthy)   = ptext $ sLit "trustworthy"
    ppr (TrustInfo Sf_Safe)          = ptext $ sLit "safe"
    ppr (TrustInfo Sf_SafeInferred)  = ptext $ sLit "safe-inferred"

instance Binary IfaceTrustInfo where
    put_ bh iftrust = putByte bh $ trustInfoToNum iftrust
    get bh = getByte bh >>= (return . numToTrustInfo)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Parser result}
%*                                                                      *
%************************************************************************

\begin{code}
data HsParsedModule = HsParsedModule {
    hpm_module    :: Located (HsModule RdrName),
    hpm_src_files :: [FilePath]
       -- ^ extra source files (e.g. from #includes).  The lexer collects
       -- these from '# <file> <line>' pragmas, which the C preprocessor
       -- leaves behind.  These files and their timestamps are stored in
       -- the .hi file, so that we can force recompilation if any of
       -- them change (#3589)
  }
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Linkable stuff}
%*                                                                      *
%************************************************************************

This stuff is in here, rather than (say) in Linker.lhs, because the Linker.lhs
stuff is the *dynamic* linker, and isn't present in a stage-1 compiler

\begin{code}
-- | Information we can use to dynamically link modules into the compiler
data Linkable = LM {
  linkableTime     :: UTCTime,          -- ^ Time at which this linkable was built
                                        -- (i.e. when the bytecodes were produced,
                                        --       or the mod date on the files)
  linkableModule   :: Module,           -- ^ The linkable module itself
  linkableUnlinked :: [Unlinked]
    -- ^ Those files and chunks of code we have yet to link.
    --
    -- INVARIANT: A valid linkable always has at least one 'Unlinked' item.
    -- If this list is empty, the Linkable represents a fake linkable, which
    -- is generated in HscNothing mode to avoid recompiling modules.
    --
    -- ToDo: Do items get removed from this list when they get linked?
 }

isObjectLinkable :: Linkable -> Bool
isObjectLinkable l = not (null unlinked) && all isObject unlinked
  where unlinked = linkableUnlinked l
        -- A linkable with no Unlinked's is treated as a BCO.  We can
        -- generate a linkable with no Unlinked's as a result of
        -- compiling a module in HscNothing mode, and this choice
        -- happens to work well with checkStability in module GHC.

linkableObjs :: Linkable -> [FilePath]
linkableObjs l = [ f | DotO f <- linkableUnlinked l ]

instance Outputable Linkable where
   ppr (LM when_made mod unlinkeds)
      = (text "LinkableM" <+> parens (text (show when_made)) <+> ppr mod)
        $$ nest 3 (ppr unlinkeds)

-------------------------------------------

-- | Objects which have yet to be linked by the compiler
data Unlinked
   = DotO FilePath      -- ^ An object file (.o)
   | DotA FilePath      -- ^ Static archive file (.a)
   | DotDLL FilePath    -- ^ Dynamically linked library file (.so, .dll, .dylib)
   | BCOs CompiledByteCode ModBreaks    -- ^ A byte-code object, lives only in memory

#ifndef GHCI
data CompiledByteCode = CompiledByteCodeUndefined
_unused :: CompiledByteCode
_unused = CompiledByteCodeUndefined
#endif

instance Outputable Unlinked where
   ppr (DotO path)   = text "DotO" <+> text path
   ppr (DotA path)   = text "DotA" <+> text path
   ppr (DotDLL path) = text "DotDLL" <+> text path
#ifdef GHCI
   ppr (BCOs bcos _) = text "BCOs" <+> ppr bcos
#else
   ppr (BCOs _ _)    = text "No byte code"
#endif

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
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Breakpoint Support}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Breakpoint index
type BreakIndex = Int

-- | All the information about the breakpoints for a given module
data ModBreaks
   = ModBreaks
   { modBreaks_flags :: BreakArray
        -- ^ The array of flags, one per breakpoint,
        -- indicating which breakpoints are enabled.
   , modBreaks_locs :: !(Array BreakIndex SrcSpan)
        -- ^ An array giving the source span of each breakpoint.
   , modBreaks_vars :: !(Array BreakIndex [OccName])
        -- ^ An array giving the names of the free variables at each breakpoint.
   , modBreaks_decls :: !(Array BreakIndex [String])
        -- ^ An array giving the names of the declarations enclosing each breakpoint.
   }

-- | Construct an empty ModBreaks
emptyModBreaks :: ModBreaks
emptyModBreaks = ModBreaks
   { modBreaks_flags = error "ModBreaks.modBreaks_array not initialised"
         -- ToDo: can we avoid this?
   , modBreaks_locs  = array (0,-1) []
   , modBreaks_vars  = array (0,-1) []
   , modBreaks_decls = array (0,-1) []
   }
\end{code}
