-- (c) The University of Glasgow, 2006

{-# LANGUAGE CPP, ScopedTypeVariables, BangPatterns, FlexibleContexts #-}

-- | Unit manipulation
module GHC.Unit.State (
        module GHC.Unit.Info,

        -- * Reading the package config, and processing cmdline args
        UnitState(..),
        UnitDatabase (..),
        emptyUnitState,
        initUnits,
        readUnitDatabases,
        readUnitDatabase,
        getUnitDbRefs,
        resolveUnitDatabase,
        listUnitInfo,

        -- * Querying the package config
        lookupUnit,
        lookupUnit',
        unsafeLookupUnit,
        lookupUnitId,
        lookupUnitId',
        unsafeLookupUnitId,

        lookupPackageName,
        improveUnit,
        searchPackageId,
        displayUnitId,
        listVisibleModuleNames,
        lookupModuleInAllUnits,
        lookupModuleWithSuggestions,
        lookupPluginModuleWithSuggestions,
        LookupResult(..),
        ModuleSuggestion(..),
        ModuleOrigin(..),
        UnusableUnitReason(..),
        pprReason,

        -- * Inspecting the set of packages in scope
        getUnitIncludePath,
        getUnitLibraryPath,
        getUnitLinkOpts,
        getUnitExtraCcOpts,
        getUnitFrameworkPath,
        getUnitFrameworks,
        getPreloadUnitsAnd,

        collectArchives,
        collectIncludeDirs, collectLibraryPaths, collectLinkOpts,
        packageHsLibs, getLibs,

        -- * Module hole substitution
        ShHoleSubst,
        renameHoleUnit,
        renameHoleModule,
        renameHoleUnit',
        renameHoleModule',
        instUnitToUnit,
        instModuleToModule,

        -- * Utils
        mkIndefUnitId,
        updateIndefUnitId,
        unwireUnit,
        pprFlag,
        pprUnits,
        pprUnitsSimple,
        pprModuleMap,
        homeUnitIsIndefinite,
        homeUnitIsDefinite,
    )
where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Platform
import GHC.Unit.Database
import GHC.Unit.Info
import GHC.Unit.Types
import GHC.Unit.Module
import GHC.Driver.Session
import GHC.Driver.Ways
import GHC.Types.Unique.FM
import GHC.Types.Unique.DFM
import GHC.Types.Unique.Set
import GHC.Types.Unique.DSet
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable as Outputable
import GHC.Data.Maybe

import System.Environment ( getEnv )
import GHC.Data.FastString
import GHC.Utils.Error  ( debugTraceMsg, MsgDoc, dumpIfSet_dyn,
                          withTiming, DumpFormat (..) )
import GHC.Utils.Exception

import System.Directory
import System.FilePath as FilePath
import Control.Monad
import Data.Graph (stronglyConnComp, SCC(..))
import Data.Char ( toUpper )
import Data.List as List
import Data.Map (Map)
import Data.Set (Set)
import Data.Monoid (First(..))
import qualified Data.Semigroup as Semigroup
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapStrict
import qualified Data.Set as Set

-- ---------------------------------------------------------------------------
-- The Unit state

-- | Unit state is all stored in 'DynFlags', including the details of
-- all units, which units are exposed, and which modules they
-- provide.
--
-- The unit state is computed by 'initUnits', and kept in DynFlags.
-- It is influenced by various command-line flags:
--
--   * @-package \<pkg>@ and @-package-id \<pkg>@ cause @\<pkg>@ to become exposed.
--     If @-hide-all-packages@ was not specified, these commands also cause
--      all other packages with the same name to become hidden.
--
--   * @-hide-package \<pkg>@ causes @\<pkg>@ to become hidden.
--
--   * (there are a few more flags, check below for their semantics)
--
-- The unit state has the following properties.
--
--   * Let @exposedUnits@ be the set of packages thus exposed.
--     Let @depExposedUnits@ be the transitive closure from @exposedUnits@ of
--     their dependencies.
--
--   * When searching for a module from a preload import declaration,
--     only the exposed modules in @exposedUnits@ are valid.
--
--   * When searching for a module from an implicit import, all modules
--     from @depExposedUnits@ are valid.
--
--   * When linking in a compilation manager mode, we link in packages the
--     program depends on (the compiler knows this list by the
--     time it gets to the link step).  Also, we link in all packages
--     which were mentioned with preload @-package@ flags on the command-line,
--     or are a transitive dependency of same, or are \"base\"\/\"rts\".
--     The reason for this is that we might need packages which don't
--     contain any Haskell modules, and therefore won't be discovered
--     by the normal mechanism of dependency tracking.

-- Notes on DLLs
-- ~~~~~~~~~~~~~
-- When compiling module A, which imports module B, we need to
-- know whether B will be in the same DLL as A.
--      If it's in the same DLL, we refer to B_f_closure
--      If it isn't, we refer to _imp__B_f_closure
-- When compiling A, we record in B's Module value whether it's
-- in a different DLL, by setting the DLL flag.

-- | Given a module name, there may be multiple ways it came into scope,
-- possibly simultaneously.  This data type tracks all the possible ways
-- it could have come into scope.  Warning: don't use the record functions,
-- they're partial!
data ModuleOrigin =
    -- | Module is hidden, and thus never will be available for import.
    -- (But maybe the user didn't realize), so we'll still keep track
    -- of these modules.)
    ModHidden
    -- | Module is unavailable because the package is unusable.
  | ModUnusable UnusableUnitReason
    -- | Module is public, and could have come from some places.
  | ModOrigin {
        -- | @Just False@ means that this module is in
        -- someone's @exported-modules@ list, but that package is hidden;
        -- @Just True@ means that it is available; @Nothing@ means neither
        -- applies.
        fromOrigUnit :: Maybe Bool
        -- | Is the module available from a reexport of an exposed package?
        -- There could be multiple.
      , fromExposedReexport :: [UnitInfo]
        -- | Is the module available from a reexport of a hidden package?
      , fromHiddenReexport :: [UnitInfo]
        -- | Did the module export come from a package flag? (ToDo: track
        -- more information.
      , fromPackageFlag :: Bool
      }

instance Outputable ModuleOrigin where
    ppr ModHidden = text "hidden module"
    ppr (ModUnusable _) = text "unusable module"
    ppr (ModOrigin e res rhs f) = sep (punctuate comma (
        (case e of
            Nothing -> []
            Just False -> [text "hidden package"]
            Just True -> [text "exposed package"]) ++
        (if null res
            then []
            else [text "reexport by" <+>
                    sep (map (ppr . mkUnit) res)]) ++
        (if null rhs
            then []
            else [text "hidden reexport by" <+>
                    sep (map (ppr . mkUnit) res)]) ++
        (if f then [text "package flag"] else [])
        ))

-- | Smart constructor for a module which is in @exposed-modules@.  Takes
-- as an argument whether or not the defining package is exposed.
fromExposedModules :: Bool -> ModuleOrigin
fromExposedModules e = ModOrigin (Just e) [] [] False

-- | Smart constructor for a module which is in @reexported-modules@.  Takes
-- as an argument whether or not the reexporting package is exposed, and
-- also its 'UnitInfo'.
fromReexportedModules :: Bool -> UnitInfo -> ModuleOrigin
fromReexportedModules True pkg = ModOrigin Nothing [pkg] [] False
fromReexportedModules False pkg = ModOrigin Nothing [] [pkg] False

-- | Smart constructor for a module which was bound by a package flag.
fromFlag :: ModuleOrigin
fromFlag = ModOrigin Nothing [] [] True

instance Semigroup ModuleOrigin where
    ModOrigin e res rhs f <> ModOrigin e' res' rhs' f' =
        ModOrigin (g e e') (res ++ res') (rhs ++ rhs') (f || f')
      where g (Just b) (Just b')
                | b == b'   = Just b
                | otherwise = panic "ModOrigin: package both exposed/hidden"
            g Nothing x = x
            g x Nothing = x
    _x <> _y = panic "ModOrigin: hidden module redefined"

instance Monoid ModuleOrigin where
    mempty = ModOrigin Nothing [] [] False
    mappend = (Semigroup.<>)

-- | Is the name from the import actually visible? (i.e. does it cause
-- ambiguity, or is it only relevant when we're making suggestions?)
originVisible :: ModuleOrigin -> Bool
originVisible ModHidden = False
originVisible (ModUnusable _) = False
originVisible (ModOrigin b res _ f) = b == Just True || not (null res) || f

-- | Are there actually no providers for this module?  This will never occur
-- except when we're filtering based on package imports.
originEmpty :: ModuleOrigin -> Bool
originEmpty (ModOrigin Nothing [] [] False) = True
originEmpty _ = False

type PreloadUnitClosure = UniqSet UnitId

-- | 'UniqFM' map from 'Unit' to a 'UnitVisibility'.
type VisibilityMap = Map Unit UnitVisibility

-- | 'UnitVisibility' records the various aspects of visibility of a particular
-- 'Unit'.
data UnitVisibility = UnitVisibility
    { uv_expose_all :: Bool
      --  ^ Should all modules in exposed-modules should be dumped into scope?
    , uv_renamings :: [(ModuleName, ModuleName)]
      -- ^ Any custom renamings that should bring extra 'ModuleName's into
      -- scope.
    , uv_package_name :: First FastString
      -- ^ The package name associated with the 'Unit'.  This is used
      -- to implement legacy behavior where @-package foo-0.1@ implicitly
      -- hides any packages named @foo@
    , uv_requirements :: Map ModuleName (Set InstantiatedModule)
      -- ^ The signatures which are contributed to the requirements context
      -- from this unit ID.
    , uv_explicit :: Bool
      -- ^ Whether or not this unit was explicitly brought into scope,
      -- as opposed to implicitly via the 'exposed' fields in the
      -- package database (when @-hide-all-packages@ is not passed.)
    }

instance Outputable UnitVisibility where
    ppr (UnitVisibility {
        uv_expose_all = b,
        uv_renamings = rns,
        uv_package_name = First mb_pn,
        uv_requirements = reqs,
        uv_explicit = explicit
    }) = ppr (b, rns, mb_pn, reqs, explicit)

instance Semigroup UnitVisibility where
    uv1 <> uv2
        = UnitVisibility
          { uv_expose_all = uv_expose_all uv1 || uv_expose_all uv2
          , uv_renamings = uv_renamings uv1 ++ uv_renamings uv2
          , uv_package_name = mappend (uv_package_name uv1) (uv_package_name uv2)
          , uv_requirements = Map.unionWith Set.union (uv_requirements uv1) (uv_requirements uv2)
          , uv_explicit = uv_explicit uv1 || uv_explicit uv2
          }

instance Monoid UnitVisibility where
    mempty = UnitVisibility
             { uv_expose_all = False
             , uv_renamings = []
             , uv_package_name = First Nothing
             , uv_requirements = Map.empty
             , uv_explicit = False
             }
    mappend = (Semigroup.<>)


-- | Unit configuration
data UnitConfig = UnitConfig
   { unitConfigPlatformArchOs :: !PlatformMini  -- ^ Platform
   , unitConfigWays           :: !(Set Way)     -- ^ Ways to use
   , unitConfigProgramName    :: !String
      -- ^ Name of the compiler (e.g. "GHC", "GHCJS"). Used to fetch environment
      -- variables such as "GHC[JS]_PACKAGE_PATH".

   , unitConfigGlobalDB :: !FilePath    -- ^ Path to global DB
   , unitConfigGHCDir   :: !FilePath    -- ^ Main GHC dir: contains settings, etc.
   , unitConfigDBName   :: !String      -- ^ User DB name (e.g. "package.conf.d")

   , unitConfigAutoLink       :: ![UnitId] -- ^ Units to link automatically (e.g. base, rts)
   , unitConfigDistrustAll    :: !Bool     -- ^ Distrust all units by default
   , unitConfigHideAll        :: !Bool     -- ^ Hide all units by default
   , unitConfigHideAllPlugins :: !Bool     -- ^ Hide all plugins units by default

   , unitConfigAllowVirtualUnits :: !Bool
      -- ^ Allow the use of virtual units instantiated on-the-fly (see Note
      -- [About units] in GHC.Unit). This should only be used when we are
      -- type-checking an indefinite unit (not producing any code).

   , unitConfigDBCache      :: Maybe [UnitDatabase UnitId]
      -- ^ Cache of databases to use, in the order they were specified on the
      -- command line (later databases shadow earlier ones).
      -- If Nothing, databases will be found using `unitConfigFlagsDB`.

   -- command-line flags
   , unitConfigFlagsDB      :: [PackageDBFlag]     -- ^ Unit databases flags
   , unitConfigFlagsExposed :: [PackageFlag]       -- ^ Exposed units
   , unitConfigFlagsIgnored :: [IgnorePackageFlag] -- ^ Ignored units
   , unitConfigFlagsTrusted :: [TrustFlag]         -- ^ Trusted units
   , unitConfigFlagsPlugins :: [PackageFlag]       -- ^ Plugins exposed units
   }

initUnitConfig :: DynFlags -> UnitConfig
initUnitConfig dflags =
   let autoLink
         | not (gopt Opt_AutoLinkPackages dflags) = []
         -- By default we add base & rts to the preload units (when they are
         -- found in the unit database) except when we are building them
         | otherwise = filter (/= homeUnitId dflags) [baseUnitId, rtsUnitId]

   in UnitConfig
      { unitConfigPlatformArchOs = platformMini (targetPlatform dflags)
      , unitConfigProgramName    = programName dflags
      , unitConfigWays           = ways dflags

      , unitConfigGlobalDB       = globalPackageDatabasePath dflags
      , unitConfigGHCDir         = topDir dflags
      , unitConfigDBName         = "package.conf.d"

      , unitConfigAutoLink       = autoLink
      , unitConfigDistrustAll    = gopt Opt_DistrustAllPackages dflags
      , unitConfigHideAll        = gopt Opt_HideAllPackages dflags
      , unitConfigHideAllPlugins = gopt Opt_HideAllPluginPackages dflags

        -- when the home unit is indefinite, it means we are type-checking it
        -- only (not producing any code). Hence we can use virtual units
        -- instantiated on-the-fly (see Note [About units] in GHC.Unit)
      , unitConfigAllowVirtualUnits = homeUnitIsIndefinite dflags

      , unitConfigDBCache      = unitDatabases dflags
      , unitConfigFlagsDB      = packageDBFlags dflags
      , unitConfigFlagsExposed = packageFlags dflags
      , unitConfigFlagsIgnored = ignorePackageFlags dflags
      , unitConfigFlagsTrusted = trustFlags dflags
      , unitConfigFlagsPlugins = pluginPackageFlags dflags

      }

-- | Map from 'ModuleName' to a set of of module providers (i.e. a 'Module' and
-- its 'ModuleOrigin').
--
-- NB: the set is in fact a 'Map Module ModuleOrigin', probably to keep only one
-- origin for a given 'Module'
type ModuleNameProvidersMap =
    Map ModuleName (Map Module ModuleOrigin)

data UnitState = UnitState {
  -- | A mapping of 'Unit' to 'UnitInfo'.  This list is adjusted
  -- so that only valid units are here.  'UnitInfo' reflects
  -- what was stored *on disk*, except for the 'trusted' flag, which
  -- is adjusted at runtime.  (In particular, some units in this map
  -- may have the 'exposed' flag be 'False'.)
  unitInfoMap :: UnitInfoMap,

  -- | The set of transitively reachable units according
  -- to the explicitly provided command line arguments.
  -- A fully instantiated VirtUnit may only be replaced by a RealUnit from
  -- this set.
  -- See Note [VirtUnit to RealUnit improvement]
  preloadClosure :: PreloadUnitClosure,

  -- | A mapping of 'PackageName' to 'IndefUnitId'.  This is used when
  -- users refer to packages in Backpack includes.
  packageNameMap            :: Map PackageName IndefUnitId,

  -- | A mapping from database unit keys to wired in unit ids.
  wireMap :: Map UnitId UnitId,

  -- | A mapping from wired in unit ids to unit keys from the database.
  unwireMap :: Map UnitId UnitId,

  -- | The units we're going to link in eagerly.  This list
  -- should be in reverse dependency order; that is, a unit
  -- is always mentioned before the units it depends on.
  preloadUnits      :: [UnitId],

  -- | Units which we explicitly depend on (from a command line flag).
  -- We'll use this to generate version macros.
  explicitUnits      :: [Unit],

  -- | This is a full map from 'ModuleName' to all modules which may possibly
  -- be providing it.  These providers may be hidden (but we'll still want
  -- to report them in error messages), or it may be an ambiguous import.
  moduleNameProvidersMap    :: !ModuleNameProvidersMap,

  -- | A map, like 'moduleNameProvidersMap', but controlling plugin visibility.
  pluginModuleNameProvidersMap    :: !ModuleNameProvidersMap,

  -- | A map saying, for each requirement, what interfaces must be merged
  -- together when we use them.  For example, if our dependencies
  -- are @p[A=\<A>]@ and @q[A=\<A>,B=r[C=\<A>]:B]@, then the interfaces
  -- to merge for A are @p[A=\<A>]:A@, @q[A=\<A>,B=r[C=\<A>]:B]:A@
  -- and @r[C=\<A>]:C@.
  --
  -- There's an entry in this map for each hole in our home library.
  requirementContext :: Map ModuleName [InstantiatedModule],

  -- | Indicate if we can instantiate units on-the-fly.
  --
  -- This should only be true when we are type-checking an indefinite unit.
  -- See Note [About units] in GHC.Unit.
  allowVirtualUnits :: !Bool
  }

emptyUnitState :: UnitState
emptyUnitState = UnitState {
    unitInfoMap = Map.empty,
    preloadClosure = emptyUniqSet,
    packageNameMap = Map.empty,
    wireMap   = Map.empty,
    unwireMap = Map.empty,
    preloadUnits = [],
    explicitUnits = [],
    moduleNameProvidersMap = Map.empty,
    pluginModuleNameProvidersMap = Map.empty,
    requirementContext = Map.empty,
    allowVirtualUnits = False
    }

-- | Unit database
data UnitDatabase unit = UnitDatabase
   { unitDatabasePath  :: FilePath
   , unitDatabaseUnits :: [GenUnitInfo unit]
   }

type UnitInfoMap = Map UnitId UnitInfo

-- | Find the unit we know about with the given unit, if any
lookupUnit :: UnitState -> Unit -> Maybe UnitInfo
lookupUnit pkgs = lookupUnit' (allowVirtualUnits pkgs) (unitInfoMap pkgs) (preloadClosure pkgs)

-- | A more specialized interface, which doesn't require a 'UnitState' (so it
-- can be used while we're initializing 'DynFlags')
--
-- Parameters:
--    * a boolean specifying whether or not to look for on-the-fly renamed interfaces
--    * a 'UnitInfoMap'
--    * a 'PreloadUnitClosure'
lookupUnit' :: Bool -> UnitInfoMap -> PreloadUnitClosure -> Unit -> Maybe UnitInfo
lookupUnit' allowOnTheFlyInst pkg_map closure u = case u of
   HoleUnit   -> error "Hole unit"
   RealUnit i -> Map.lookup (unDefinite i) pkg_map
   VirtUnit i
      | allowOnTheFlyInst
      -> -- lookup UnitInfo of the indefinite unit to be instantiated and
         -- instantiate it on-the-fly
         fmap (renameUnitInfo pkg_map closure (instUnitInsts i))
           (Map.lookup (indefUnit (instUnitInstanceOf i)) pkg_map)

      | otherwise
      -> -- lookup UnitInfo by virtual UnitId. This is used to find indefinite
         -- units. Even if they are real, installed units, they can't use the
         -- `RealUnit` constructor (it is reserved for definite units) so we use
         -- the `VirtUnit` constructor.
         Map.lookup (virtualUnitId i) pkg_map

-- | Find the unit we know about with the given unit id, if any
lookupUnitId :: UnitState -> UnitId -> Maybe UnitInfo
lookupUnitId state uid = lookupUnitId' (unitInfoMap state) uid

-- | Find the unit we know about with the given unit id, if any
lookupUnitId' :: UnitInfoMap -> UnitId -> Maybe UnitInfo
lookupUnitId' db uid = Map.lookup uid db


-- | Looks up the given unit in the unit state, panicing if it is not found
unsafeLookupUnit :: HasDebugCallStack => UnitState -> Unit -> UnitInfo
unsafeLookupUnit state u = case lookupUnit state u of
   Just info -> info
   Nothing   -> pprPanic "unsafeLookupUnit" (ppr u)

-- | Looks up the given unit id in the unit state, panicing if it is not found
unsafeLookupUnitId :: HasDebugCallStack => UnitState -> UnitId -> UnitInfo
unsafeLookupUnitId state uid = case lookupUnitId state uid of
   Just info -> info
   Nothing   -> pprPanic "unsafeLookupUnitId" (ppr uid)


-- | Find the unit we know about with the given package name (e.g. @foo@), if any
-- (NB: there might be a locally defined unit name which overrides this)
lookupPackageName :: UnitState -> PackageName -> Maybe IndefUnitId
lookupPackageName pkgstate n = Map.lookup n (packageNameMap pkgstate)

-- | Search for units with a given package ID (e.g. \"foo-0.1\")
searchPackageId :: UnitState -> PackageId -> [UnitInfo]
searchPackageId pkgstate pid = filter ((pid ==) . unitPackageId)
                               (listUnitInfo pkgstate)

-- | Create a Map UnitId UnitInfo
--
-- For each instantiated unit, we add two map keys:
--    * the real unit id
--    * the virtual unit id made from its instantiation
--
-- We do the same thing for fully indefinite units (which are "instantiated"
-- with module holes).
--
mkUnitInfoMap :: [UnitInfo] -> UnitInfoMap
mkUnitInfoMap infos = foldl' add Map.empty infos
  where
   mkVirt      p = virtualUnitId (mkInstantiatedUnit (unitInstanceOf p) (unitInstantiations p))
   add pkg_map p
      | not (null (unitInstantiations p))
      = Map.insert (mkVirt p) p
         $ Map.insert (unitId p) p
         $ pkg_map
      | otherwise
      = Map.insert (unitId p) p pkg_map

-- | Get a list of entries from the unit database.  NB: be careful with
-- this function, although all units in this map are "visible", this
-- does not imply that the exposed-modules of the unit are available
-- (they may have been thinned or renamed).
listUnitInfo :: UnitState -> [UnitInfo]
listUnitInfo state = Map.elems (unitInfoMap state)

-- ----------------------------------------------------------------------------
-- Loading the unit db files and building up the unit state

-- | Read the unit database files, and sets up various internal tables of
-- unit information, according to the unit-related flags on the
-- command-line (@-package@, @-hide-package@ etc.)
--
-- 'initUnits' can be called again subsequently after updating the
-- 'packageFlags' field of the 'DynFlags', and it will update the
-- 'unitState' in 'DynFlags'.
initUnits :: DynFlags -> IO DynFlags
initUnits dflags = do

  let forceUnitInfoMap (state, _) = unitInfoMap state `seq` ()
  let ctx     = initSDocContext dflags defaultUserStyle -- SDocContext used to render exception messages
  let printer = debugTraceMsg dflags                    -- printer for trace messages

  (state,dbs) <- withTiming dflags (text "initializing unit database")
                   forceUnitInfoMap
                   (mkUnitState ctx printer (initUnitConfig dflags))

  dumpIfSet_dyn (dflags { pprCols = 200 }) Opt_D_dump_mod_map "Module Map"
    FormatText (pprModuleMap (moduleNameProvidersMap state))

  let dflags'  = dflags
                  { unitDatabases = Just dbs -- databases are cached and never read again
                  , unitState     = state
                  }
      dflags'' = upd_wired_in_home_instantiations dflags'

  return dflags''

-- -----------------------------------------------------------------------------
-- Reading the unit database(s)

readUnitDatabases :: (Int -> SDoc -> IO ()) -> UnitConfig -> IO [UnitDatabase UnitId]
readUnitDatabases printer cfg = do
  conf_refs <- getUnitDbRefs cfg
  confs     <- liftM catMaybes $ mapM (resolveUnitDatabase cfg) conf_refs
  mapM (readUnitDatabase printer cfg) confs


getUnitDbRefs :: UnitConfig -> IO [PkgDbRef]
getUnitDbRefs cfg = do
  let system_conf_refs = [UserPkgDb, GlobalPkgDb]

  e_pkg_path <- tryIO (getEnv $ map toUpper (unitConfigProgramName cfg) ++ "_PACKAGE_PATH")
  let base_conf_refs = case e_pkg_path of
        Left _ -> system_conf_refs
        Right path
         | not (null path) && isSearchPathSeparator (last path)
         -> map PkgDbPath (splitSearchPath (init path)) ++ system_conf_refs
         | otherwise
         -> map PkgDbPath (splitSearchPath path)

  -- Apply the package DB-related flags from the command line to get the
  -- final list of package DBs.
  --
  -- Notes on ordering:
  --  * The list of flags is reversed (later ones first)
  --  * We work with the package DB list in "left shadows right" order
  --  * and finally reverse it at the end, to get "right shadows left"
  --
  return $ reverse (foldr doFlag base_conf_refs (unitConfigFlagsDB cfg))
 where
  doFlag (PackageDB p) dbs = p : dbs
  doFlag NoUserPackageDB dbs = filter isNotUser dbs
  doFlag NoGlobalPackageDB dbs = filter isNotGlobal dbs
  doFlag ClearPackageDBs _ = []

  isNotUser UserPkgDb = False
  isNotUser _ = True

  isNotGlobal GlobalPkgDb = False
  isNotGlobal _ = True

-- | Return the path of a package database from a 'PkgDbRef'. Return 'Nothing'
-- when the user database filepath is expected but the latter doesn't exist.
--
-- NB: This logic is reimplemented in Cabal, so if you change it,
-- make sure you update Cabal. (Or, better yet, dump it in the
-- compiler info so Cabal can use the info.)
resolveUnitDatabase :: UnitConfig -> PkgDbRef -> IO (Maybe FilePath)
resolveUnitDatabase cfg GlobalPkgDb = return $ Just (unitConfigGlobalDB cfg)
resolveUnitDatabase cfg UserPkgDb = runMaybeT $ do
  dir <- versionedAppDir (unitConfigProgramName cfg) (unitConfigPlatformArchOs cfg)
  let pkgconf = dir </> unitConfigDBName cfg
  exist <- tryMaybeT $ doesDirectoryExist pkgconf
  if exist then return pkgconf else mzero
resolveUnitDatabase _ (PkgDbPath name) = return $ Just name

readUnitDatabase :: (Int -> SDoc -> IO ()) -> UnitConfig -> FilePath -> IO (UnitDatabase UnitId)
readUnitDatabase printer cfg conf_file = do
  isdir <- doesDirectoryExist conf_file

  proto_pkg_configs <-
    if isdir
       then readDirStyleUnitInfo conf_file
       else do
            isfile <- doesFileExist conf_file
            if isfile
               then do
                 mpkgs <- tryReadOldFileStyleUnitInfo
                 case mpkgs of
                   Just pkgs -> return pkgs
                   Nothing   -> throwGhcExceptionIO $ InstallationError $
                      "ghc no longer supports single-file style package " ++
                      "databases (" ++ conf_file ++
                      ") use 'ghc-pkg init' to create the database with " ++
                      "the correct format."
               else throwGhcExceptionIO $ InstallationError $
                      "can't find a package database at " ++ conf_file

  let
      -- Fix #16360: remove trailing slash from conf_file before calculating pkgroot
      conf_file' = dropTrailingPathSeparator conf_file
      top_dir = unitConfigGHCDir cfg
      pkgroot = takeDirectory conf_file'
      pkg_configs1 = map (mungeUnitInfo top_dir pkgroot . mapUnitInfo (\(UnitKey x) -> UnitId x) unitIdFS . mkUnitKeyInfo)
                         proto_pkg_configs
  --
  return $ UnitDatabase conf_file' pkg_configs1
  where
    readDirStyleUnitInfo conf_dir = do
      let filename = conf_dir </> "package.cache"
      cache_exists <- doesFileExist filename
      if cache_exists
        then do
          printer 2 $ text "Using binary package database:" <+> text filename
          readPackageDbForGhc filename
        else do
          -- If there is no package.cache file, we check if the database is not
          -- empty by inspecting if the directory contains any .conf file. If it
          -- does, something is wrong and we fail. Otherwise we assume that the
          -- database is empty.
          printer 2 $ text "There is no package.cache in"
                      <+> text conf_dir
                       <> text ", checking if the database is empty"
          db_empty <- all (not . isSuffixOf ".conf")
                   <$> getDirectoryContents conf_dir
          if db_empty
            then do
              printer 3 $ text "There are no .conf files in"
                          <+> text conf_dir <> text ", treating"
                          <+> text "package database as empty"
              return []
            else do
              throwGhcExceptionIO $ InstallationError $
                "there is no package.cache in " ++ conf_dir ++
                " even though package database is not empty"


    -- Single-file style package dbs have been deprecated for some time, but
    -- it turns out that Cabal was using them in one place. So this is a
    -- workaround to allow older Cabal versions to use this newer ghc.
    -- We check if the file db contains just "[]" and if so, we look for a new
    -- dir-style db in conf_file.d/, ie in a dir next to the given file.
    -- We cannot just replace the file with a new dir style since Cabal still
    -- assumes it's a file and tries to overwrite with 'writeFile'.
    -- ghc-pkg also cooperates with this workaround.
    tryReadOldFileStyleUnitInfo = do
      content <- readFile conf_file `catchIO` \_ -> return ""
      if take 2 content == "[]"
        then do
          let conf_dir = conf_file <.> "d"
          direxists <- doesDirectoryExist conf_dir
          if direxists
             then do printer 2 (text "Ignoring old file-style db and trying:" <+> text conf_dir)
                     liftM Just (readDirStyleUnitInfo conf_dir)
             else return (Just []) -- ghc-pkg will create it when it's updated
        else return Nothing

distrustAllUnits :: [UnitInfo] -> [UnitInfo]
distrustAllUnits pkgs = map distrust pkgs
  where
    distrust pkg = pkg{ unitIsTrusted = False }

mungeUnitInfo :: FilePath -> FilePath
                   -> UnitInfo -> UnitInfo
mungeUnitInfo top_dir pkgroot =
    mungeDynLibFields
  . mungeUnitInfoPaths top_dir pkgroot

mungeDynLibFields :: UnitInfo -> UnitInfo
mungeDynLibFields pkg =
    pkg {
      unitLibraryDynDirs = case unitLibraryDynDirs pkg of
         [] -> unitLibraryDirs pkg
         ds -> ds
    }

-- -----------------------------------------------------------------------------
-- Modify our copy of the unit database based on trust flags,
-- -trust and -distrust.

applyTrustFlag
   :: SDocContext
   -> UnitPrecedenceMap
   -> UnusableUnits
   -> [UnitInfo]
   -> TrustFlag
   -> IO [UnitInfo]
applyTrustFlag ctx prec_map unusable pkgs flag =
  case flag of
    -- we trust all matching packages. Maybe should only trust first one?
    -- and leave others the same or set them untrusted
    TrustPackage str ->
       case selectPackages prec_map (PackageArg str) pkgs unusable of
         Left ps       -> trustFlagErr ctx flag ps
         Right (ps,qs) -> return (map trust ps ++ qs)
          where trust p = p {unitIsTrusted=True}

    DistrustPackage str ->
       case selectPackages prec_map (PackageArg str) pkgs unusable of
         Left ps       -> trustFlagErr ctx flag ps
         Right (ps,qs) -> return (distrustAllUnits ps ++ qs)

-- | A little utility to tell if the home unit is indefinite
-- (if it is not, we should never use on-the-fly renaming.)
homeUnitIsIndefinite :: DynFlags -> Bool
homeUnitIsIndefinite dflags = not (homeUnitIsDefinite dflags)

-- | A little utility to tell if the home unit is definite
-- (if it is, we should never use on-the-fly renaming.)
homeUnitIsDefinite :: DynFlags -> Bool
homeUnitIsDefinite dflags = unitIsDefinite (homeUnit dflags)

applyPackageFlag
   :: SDocContext
   -> UnitPrecedenceMap
   -> UnitInfoMap
   -> PreloadUnitClosure
   -> UnusableUnits
   -> Bool -- if False, if you expose a package, it implicitly hides
           -- any previously exposed packages with the same name
   -> [UnitInfo]
   -> VisibilityMap           -- Initially exposed
   -> PackageFlag               -- flag to apply
   -> IO VisibilityMap        -- Now exposed

applyPackageFlag ctx prec_map pkg_map closure unusable no_hide_others pkgs vm flag =
  case flag of
    ExposePackage _ arg (ModRenaming b rns) ->
       case findPackages prec_map pkg_map closure arg pkgs unusable of
         Left ps         -> packageFlagErr ctx flag ps
         Right (p:_) -> return vm'
          where
           n = fsPackageName p

           -- If a user says @-unit-id p[A=<A>]@, this imposes
           -- a requirement on us: whatever our signature A is,
           -- it must fulfill all of p[A=<A>]:A's requirements.
           -- This method is responsible for computing what our
           -- inherited requirements are.
           reqs | UnitIdArg orig_uid <- arg = collectHoles orig_uid
                | otherwise                 = Map.empty

           collectHoles uid = case uid of
             HoleUnit       -> Map.empty
             RealUnit {}    -> Map.empty -- definite units don't have holes
             VirtUnit indef ->
                  let local = [ Map.singleton
                                  (moduleName mod)
                                  (Set.singleton $ Module indef mod_name)
                              | (mod_name, mod) <- instUnitInsts indef
                              , isHoleModule mod ]
                      recurse = [ collectHoles (moduleUnit mod)
                                | (_, mod) <- instUnitInsts indef ]
                  in Map.unionsWith Set.union $ local ++ recurse

           uv = UnitVisibility
                { uv_expose_all = b
                , uv_renamings = rns
                , uv_package_name = First (Just n)
                , uv_requirements = reqs
                , uv_explicit = True
                }
           vm' = Map.insertWith mappend (mkUnit p) uv vm_cleared
           -- In the old days, if you said `ghc -package p-0.1 -package p-0.2`
           -- (or if p-0.1 was registered in the pkgdb as exposed: True),
           -- the second package flag would override the first one and you
           -- would only see p-0.2 in exposed modules.  This is good for
           -- usability.
           --
           -- However, with thinning and renaming (or Backpack), there might be
           -- situations where you legitimately want to see two versions of a
           -- package at the same time, and this behavior would make it
           -- impossible to do so.  So we decided that if you pass
           -- -hide-all-packages, this should turn OFF the overriding behavior
           -- where an exposed package hides all other packages with the same
           -- name.  This should not affect Cabal at all, which only ever
           -- exposes one package at a time.
           --
           -- NB: Why a variable no_hide_others?  We have to apply this logic to
           -- -plugin-package too, and it's more consistent if the switch in
           -- behavior is based off of
           -- -hide-all-packages/-hide-all-plugin-packages depending on what
           -- flag is in question.
           vm_cleared | no_hide_others = vm
                      -- NB: renamings never clear
                      | (_:_) <- rns = vm
                      | otherwise = Map.filterWithKey
                            (\k uv -> k == mkUnit p
                                   || First (Just n) /= uv_package_name uv) vm
         _ -> panic "applyPackageFlag"

    HidePackage str ->
       case findPackages prec_map pkg_map closure (PackageArg str) pkgs unusable of
         Left ps  -> packageFlagErr ctx flag ps
         Right ps -> return vm'
          where vm' = foldl' (flip Map.delete) vm (map mkUnit ps)

-- | Like 'selectPackages', but doesn't return a list of unmatched
-- packages.  Furthermore, any packages it returns are *renamed*
-- if the 'UnitArg' has a renaming associated with it.
findPackages :: UnitPrecedenceMap
             -> UnitInfoMap
             -> PreloadUnitClosure
             -> PackageArg -> [UnitInfo]
             -> UnusableUnits
             -> Either [(UnitInfo, UnusableUnitReason)]
                [UnitInfo]
findPackages prec_map pkg_map closure arg pkgs unusable
  = let ps = mapMaybe (finder arg) pkgs
    in if null ps
        then Left (mapMaybe (\(x,y) -> finder arg x >>= \x' -> return (x',y))
                            (Map.elems unusable))
        else Right (sortByPreference prec_map ps)
  where
    finder (PackageArg str) p
      = if str == unitPackageIdString p || str == unitPackageNameString p
          then Just p
          else Nothing
    finder (UnitIdArg uid) p
      = case uid of
          RealUnit (Definite iuid)
            | iuid == unitId p
            -> Just p
          VirtUnit inst
            | indefUnit (instUnitInstanceOf inst) == unitId p
            -> Just (renameUnitInfo pkg_map closure (instUnitInsts inst) p)
          _ -> Nothing

selectPackages :: UnitPrecedenceMap -> PackageArg -> [UnitInfo]
               -> UnusableUnits
               -> Either [(UnitInfo, UnusableUnitReason)]
                  ([UnitInfo], [UnitInfo])
selectPackages prec_map arg pkgs unusable
  = let matches = matching arg
        (ps,rest) = partition matches pkgs
    in if null ps
        then Left (filter (matches.fst) (Map.elems unusable))
        else Right (sortByPreference prec_map ps, rest)

-- | Rename a 'UnitInfo' according to some module instantiation.
renameUnitInfo :: UnitInfoMap -> PreloadUnitClosure -> [(ModuleName, Module)] -> UnitInfo -> UnitInfo
renameUnitInfo pkg_map closure insts conf =
    let hsubst = listToUFM insts
        smod  = renameHoleModule' pkg_map closure hsubst
        new_insts = map (\(k,v) -> (k,smod v)) (unitInstantiations conf)
    in conf {
        unitInstantiations = new_insts,
        unitExposedModules = map (\(mod_name, mb_mod) -> (mod_name, fmap smod mb_mod))
                             (unitExposedModules conf)
    }


-- A package named on the command line can either include the
-- version, or just the name if it is unambiguous.
matchingStr :: String -> UnitInfo -> Bool
matchingStr str p
        =  str == unitPackageIdString p
        || str == unitPackageNameString p

matchingId :: UnitId -> UnitInfo -> Bool
matchingId uid p = uid == unitId p

matching :: PackageArg -> UnitInfo -> Bool
matching (PackageArg str) = matchingStr str
matching (UnitIdArg (RealUnit (Definite uid))) = matchingId uid
matching (UnitIdArg _)  = \_ -> False -- TODO: warn in this case

-- | This sorts a list of packages, putting "preferred" packages first.
-- See 'compareByPreference' for the semantics of "preference".
sortByPreference :: UnitPrecedenceMap -> [UnitInfo] -> [UnitInfo]
sortByPreference prec_map = sortBy (flip (compareByPreference prec_map))

-- | Returns 'GT' if @pkg@ should be preferred over @pkg'@ when picking
-- which should be "active".  Here is the order of preference:
--
--      1. First, prefer the latest version
--      2. If the versions are the same, prefer the package that
--      came in the latest package database.
--
-- Pursuant to #12518, we could change this policy to, for example, remove
-- the version preference, meaning that we would always prefer the units
-- in later unit database.
compareByPreference
    :: UnitPrecedenceMap
    -> UnitInfo
    -> UnitInfo
    -> Ordering
compareByPreference prec_map pkg pkg'
  = case comparing unitPackageVersion pkg pkg' of
        GT -> GT
        EQ | Just prec  <- Map.lookup (unitId pkg)  prec_map
           , Just prec' <- Map.lookup (unitId pkg') prec_map
           -- Prefer the unit from the later DB flag (i.e., higher
           -- precedence)
           -> compare prec prec'
           | otherwise
           -> EQ
        LT -> LT

comparing :: Ord a => (t -> a) -> t -> t -> Ordering
comparing f a b = f a `compare` f b

packageFlagErr :: SDocContext
               -> PackageFlag
               -> [(UnitInfo, UnusableUnitReason)]
               -> IO a
packageFlagErr ctx flag reasons
  = packageFlagErr' ctx (pprFlag flag) reasons

trustFlagErr :: SDocContext
             -> TrustFlag
             -> [(UnitInfo, UnusableUnitReason)]
             -> IO a
trustFlagErr ctx flag reasons
  = packageFlagErr' ctx (pprTrustFlag flag) reasons

packageFlagErr' :: SDocContext
               -> SDoc
               -> [(UnitInfo, UnusableUnitReason)]
               -> IO a
packageFlagErr' ctx flag_doc reasons
  = throwGhcExceptionIO (CmdLineError (renderWithStyle ctx $ err))
  where err = text "cannot satisfy " <> flag_doc <>
                (if null reasons then Outputable.empty else text ": ") $$
              nest 4 (ppr_reasons $$
                      text "(use -v for more information)")
        ppr_reasons = vcat (map ppr_reason reasons)
        ppr_reason (p, reason) =
            pprReason (ppr (unitId p) <+> text "is") reason

pprFlag :: PackageFlag -> SDoc
pprFlag flag = case flag of
    HidePackage p   -> text "-hide-package " <> text p
    ExposePackage doc _ _ -> text doc

pprTrustFlag :: TrustFlag -> SDoc
pprTrustFlag flag = case flag of
    TrustPackage p    -> text "-trust " <> text p
    DistrustPackage p -> text "-distrust " <> text p

-- -----------------------------------------------------------------------------
-- Wired-in units
--
-- See Note [Wired-in units] in GHC.Unit.Module

type WiringMap = Map UnitId UnitId

findWiredInUnits
   :: (SDoc -> IO ())      -- debug trace
   -> UnitPrecedenceMap
   -> [UnitInfo]           -- database
   -> VisibilityMap             -- info on what units are visible
                                -- for wired in selection
   -> IO ([UnitInfo],  -- unit database updated for wired in
          WiringMap)   -- map from unit id to wired identity

findWiredInUnits printer prec_map pkgs vis_map = do
  -- Now we must find our wired-in units, and rename them to
  -- their canonical names (eg. base-1.0 ==> base), as described
  -- in Note [Wired-in units] in GHC.Unit.Module
  let
        matches :: UnitInfo -> UnitId -> Bool
        pc `matches` pid = unitPackageName pc == PackageName (unitIdFS pid)

        -- find which package corresponds to each wired-in package
        -- delete any other packages with the same name
        -- update the package and any dependencies to point to the new
        -- one.
        --
        -- When choosing which package to map to a wired-in package
        -- name, we try to pick the latest version of exposed packages.
        -- However, if there are no exposed wired in packages available
        -- (e.g. -hide-all-packages was used), we can't bail: we *have*
        -- to assign a package for the wired-in package: so we try again
        -- with hidden packages included to (and pick the latest
        -- version).
        --
        -- You can also override the default choice by using -ignore-package:
        -- this works even when there is no exposed wired in package
        -- available.
        --
        findWiredInUnit :: [UnitInfo] -> UnitId -> IO (Maybe (UnitId, UnitInfo))
        findWiredInUnit pkgs wired_pkg =
           let all_ps = [ p | p <- pkgs, p `matches` wired_pkg ]
               all_exposed_ps =
                    [ p | p <- all_ps
                        , Map.member (mkUnit p) vis_map ] in
           case all_exposed_ps of
            [] -> case all_ps of
                       []   -> notfound
                       many -> pick (head (sortByPreference prec_map many))
            many -> pick (head (sortByPreference prec_map many))
          where
                notfound = do
                          printer $
                            text "wired-in package "
                                 <> ftext (unitIdFS wired_pkg)
                                 <> text " not found."
                          return Nothing
                pick :: UnitInfo -> IO (Maybe (UnitId, UnitInfo))
                pick pkg = do
                        printer $
                            text "wired-in package "
                                 <> ftext (unitIdFS wired_pkg)
                                 <> text " mapped to "
                                 <> ppr (unitId pkg)
                        return (Just (wired_pkg, pkg))


  mb_wired_in_pkgs <- mapM (findWiredInUnit pkgs) wiredInUnitIds
  let
        wired_in_pkgs = catMaybes mb_wired_in_pkgs

        wiredInMap :: Map UnitId UnitId
        wiredInMap = Map.fromList
          [ (unitId realUnitInfo, wiredInUnitId)
          | (wiredInUnitId, realUnitInfo) <- wired_in_pkgs
          , not (unitIsIndefinite realUnitInfo)
          ]

        updateWiredInDependencies pkgs = map (upd_deps . upd_pkg) pkgs
          where upd_pkg pkg
                  | Just wiredInUnitId <- Map.lookup (unitId pkg) wiredInMap
                  = pkg { unitId         = wiredInUnitId
                        , unitInstanceOf = fmap (const wiredInUnitId) (unitInstanceOf pkg)
                           -- every non instantiated unit is an instance of
                           -- itself (required by Backpack...)
                           --
                           -- See Note [About Units] in GHC.Unit
                        }
                  | otherwise
                  = pkg
                upd_deps pkg = pkg {
                      unitDepends = map (upd_wired_in wiredInMap) (unitDepends pkg),
                      unitExposedModules
                        = map (\(k,v) -> (k, fmap (upd_wired_in_mod wiredInMap) v))
                              (unitExposedModules pkg)
                    }


  return (updateWiredInDependencies pkgs, wiredInMap)

-- Helper functions for rewiring Module and Unit.  These
-- rewrite Units of modules in wired-in packages to the form known to the
-- compiler, as described in Note [Wired-in units] in GHC.Unit.Module.
--
-- For instance, base-4.9.0.0 will be rewritten to just base, to match
-- what appears in GHC.Builtin.Names.

-- | Some wired units can be used to instantiate the home unit. We need to
-- replace their unit keys with their wired unit ids.
upd_wired_in_home_instantiations :: DynFlags -> DynFlags
upd_wired_in_home_instantiations dflags = dflags { homeUnitInstantiations = wiredInsts }
   where
      state        = unitState dflags
      wiringMap    = wireMap state
      unwiredInsts = homeUnitInstantiations dflags
      wiredInsts   = map (fmap (upd_wired_in_mod wiringMap)) unwiredInsts


upd_wired_in_mod :: WiringMap -> Module -> Module
upd_wired_in_mod wiredInMap (Module uid m) = Module (upd_wired_in_uid wiredInMap uid) m

upd_wired_in_uid :: WiringMap -> Unit -> Unit
upd_wired_in_uid wiredInMap u = case u of
   HoleUnit                -> HoleUnit
   RealUnit (Definite uid) -> RealUnit (Definite (upd_wired_in wiredInMap uid))
   VirtUnit indef_uid ->
      VirtUnit $ mkInstantiatedUnit
        (instUnitInstanceOf indef_uid)
        (map (\(x,y) -> (x,upd_wired_in_mod wiredInMap y)) (instUnitInsts indef_uid))

upd_wired_in :: WiringMap -> UnitId -> UnitId
upd_wired_in wiredInMap key
    | Just key' <- Map.lookup key wiredInMap = key'
    | otherwise = key

updateVisibilityMap :: WiringMap -> VisibilityMap -> VisibilityMap
updateVisibilityMap wiredInMap vis_map = foldl' f vis_map (Map.toList wiredInMap)
  where f vm (from, to) = case Map.lookup (RealUnit (Definite from)) vis_map of
                    Nothing -> vm
                    Just r -> Map.insert (RealUnit (Definite to)) r
                                (Map.delete (RealUnit (Definite from)) vm)


-- ----------------------------------------------------------------------------

-- | The reason why a unit is unusable.
data UnusableUnitReason
  = -- | We ignored it explicitly using @-ignore-package@.
    IgnoredWithFlag
    -- | This unit transitively depends on a unit that was never present
    -- in any of the provided databases.
  | BrokenDependencies   [UnitId]
    -- | This unit transitively depends on a unit involved in a cycle.
    -- Note that the list of 'UnitId' reports the direct dependencies
    -- of this unit that (transitively) depended on the cycle, and not
    -- the actual cycle itself (which we report separately at high verbosity.)
  | CyclicDependencies   [UnitId]
    -- | This unit transitively depends on a unit which was ignored.
  | IgnoredDependencies  [UnitId]
    -- | This unit transitively depends on a unit which was
    -- shadowed by an ABI-incompatible unit.
  | ShadowedDependencies [UnitId]

instance Outputable UnusableUnitReason where
    ppr IgnoredWithFlag = text "[ignored with flag]"
    ppr (BrokenDependencies uids)   = brackets (text "broken" <+> ppr uids)
    ppr (CyclicDependencies uids)   = brackets (text "cyclic" <+> ppr uids)
    ppr (IgnoredDependencies uids)  = brackets (text "ignored" <+> ppr uids)
    ppr (ShadowedDependencies uids) = brackets (text "shadowed" <+> ppr uids)

type UnusableUnits = Map UnitId (UnitInfo, UnusableUnitReason)

pprReason :: SDoc -> UnusableUnitReason -> SDoc
pprReason pref reason = case reason of
  IgnoredWithFlag ->
      pref <+> text "ignored due to an -ignore-package flag"
  BrokenDependencies deps ->
      pref <+> text "unusable due to missing dependencies:" $$
        nest 2 (hsep (map ppr deps))
  CyclicDependencies deps ->
      pref <+> text "unusable due to cyclic dependencies:" $$
        nest 2 (hsep (map ppr deps))
  IgnoredDependencies deps ->
      pref <+> text ("unusable because the -ignore-package flag was used to " ++
                     "ignore at least one of its dependencies:") $$
        nest 2 (hsep (map ppr deps))
  ShadowedDependencies deps ->
      pref <+> text "unusable due to shadowed dependencies:" $$
        nest 2 (hsep (map ppr deps))

reportCycles :: (SDoc -> IO ()) -> [SCC UnitInfo] -> IO ()
reportCycles printer sccs = mapM_ report sccs
  where
    report (AcyclicSCC _) = return ()
    report (CyclicSCC vs) =
        printer $
          text "these packages are involved in a cycle:" $$
            nest 2 (hsep (map (ppr . unitId) vs))

reportUnusable :: (SDoc -> IO ()) -> UnusableUnits -> IO ()
reportUnusable printer pkgs = mapM_ report (Map.toList pkgs)
  where
    report (ipid, (_, reason)) =
       printer $
         pprReason
           (text "package" <+> ppr ipid <+> text "is") reason

-- ----------------------------------------------------------------------------
--
-- Utilities on the database
--

-- | A reverse dependency index, mapping an 'UnitId' to
-- the 'UnitId's which have a dependency on it.
type RevIndex = Map UnitId [UnitId]

-- | Compute the reverse dependency index of a unit database.
reverseDeps :: UnitInfoMap -> RevIndex
reverseDeps db = Map.foldl' go Map.empty db
  where
    go r pkg = foldl' (go' (unitId pkg)) r (unitDepends pkg)
    go' from r to = Map.insertWith (++) to [from] r

-- | Given a list of 'UnitId's to remove, a database,
-- and a reverse dependency index (as computed by 'reverseDeps'),
-- remove those units, plus any units which depend on them.
-- Returns the pruned database, as well as a list of 'UnitInfo's
-- that was removed.
removeUnits :: [UnitId] -> RevIndex
               -> UnitInfoMap
               -> (UnitInfoMap, [UnitInfo])
removeUnits uids index m = go uids (m,[])
  where
    go [] (m,pkgs) = (m,pkgs)
    go (uid:uids) (m,pkgs)
        | Just pkg <- Map.lookup uid m
        = case Map.lookup uid index of
            Nothing    -> go uids (Map.delete uid m, pkg:pkgs)
            Just rdeps -> go (rdeps ++ uids) (Map.delete uid m, pkg:pkgs)
        | otherwise
        = go uids (m,pkgs)

-- | Given a 'UnitInfo' from some 'UnitInfoMap', return all entries in 'depends'
-- which correspond to units that do not exist in the index.
depsNotAvailable :: UnitInfoMap
                 -> UnitInfo
                 -> [UnitId]
depsNotAvailable pkg_map pkg = filter (not . (`Map.member` pkg_map)) (unitDepends pkg)

-- | Given a 'UnitInfo' from some 'UnitInfoMap' return all entries in
-- 'unitAbiDepends' which correspond to units that do not exist, OR have
-- mismatching ABIs.
depsAbiMismatch :: UnitInfoMap
                -> UnitInfo
                -> [UnitId]
depsAbiMismatch pkg_map pkg = map fst . filter (not . abiMatch) $ unitAbiDepends pkg
  where
    abiMatch (dep_uid, abi)
        | Just dep_pkg <- Map.lookup dep_uid pkg_map
        = unitAbiHash dep_pkg == abi
        | otherwise
        = False

-- -----------------------------------------------------------------------------
-- Ignore units

ignoreUnits :: [IgnorePackageFlag] -> [UnitInfo] -> UnusableUnits
ignoreUnits flags pkgs = Map.fromList (concatMap doit flags)
  where
  doit (IgnorePackage str) =
     case partition (matchingStr str) pkgs of
         (ps, _) -> [ (unitId p, (p, IgnoredWithFlag))
                    | p <- ps ]
        -- missing unit is not an error for -ignore-package,
        -- because a common usage is to -ignore-package P as
        -- a preventative measure just in case P exists.

-- ----------------------------------------------------------------------------
--
-- Merging databases
--

-- | For each unit, a mapping from uid -> i indicates that this
-- unit was brought into GHC by the ith @-package-db@ flag on
-- the command line.  We use this mapping to make sure we prefer
-- units that were defined later on the command line, if there
-- is an ambiguity.
type UnitPrecedenceMap = Map UnitId Int

-- | Given a list of databases, merge them together, where
-- units with the same unit id in later databases override
-- earlier ones.  This does NOT check if the resulting database
-- makes sense (that's done by 'validateDatabase').
mergeDatabases :: (SDoc -> IO ()) -> [UnitDatabase UnitId]
               -> IO (UnitInfoMap, UnitPrecedenceMap)
mergeDatabases printer = foldM merge (Map.empty, Map.empty) . zip [1..]
  where
    merge (pkg_map, prec_map) (i, UnitDatabase db_path db) = do
      printer $
          text "loading package database" <+> text db_path
      forM_ (Set.toList override_set) $ \pkg ->
          printer $
              text "package" <+> ppr pkg <+>
              text "overrides a previously defined package"
      return (pkg_map', prec_map')
     where
      db_map = mk_pkg_map db
      mk_pkg_map = Map.fromList . map (\p -> (unitId p, p))

      -- The set of UnitIds which appear in both db and pkgs.  These are the
      -- ones that get overridden.  Compute this just to give some
      -- helpful debug messages at -v2
      override_set :: Set UnitId
      override_set = Set.intersection (Map.keysSet db_map)
                                      (Map.keysSet pkg_map)

      -- Now merge the sets together (NB: in case of duplicate,
      -- first argument preferred)
      pkg_map' :: UnitInfoMap
      pkg_map' = Map.union db_map pkg_map

      prec_map' :: UnitPrecedenceMap
      prec_map' = Map.union (Map.map (const i) db_map) prec_map

-- | Validates a database, removing unusable units from it
-- (this includes removing units that the user has explicitly
-- ignored.)  Our general strategy:
--
-- 1. Remove all broken units (dangling dependencies)
-- 2. Remove all units that are cyclic
-- 3. Apply ignore flags
-- 4. Remove all units which have deps with mismatching ABIs
--
validateDatabase :: UnitConfig -> UnitInfoMap
                 -> (UnitInfoMap, UnusableUnits, [SCC UnitInfo])
validateDatabase cfg pkg_map1 =
    (pkg_map5, unusable, sccs)
  where
    ignore_flags = reverse (unitConfigFlagsIgnored cfg)

    -- Compute the reverse dependency index
    index = reverseDeps pkg_map1

    -- Helper function
    mk_unusable mk_err dep_matcher m uids =
      Map.fromList [ (unitId pkg, (pkg, mk_err (dep_matcher m pkg)))
                   | pkg <- uids ]

    -- Find broken units
    directly_broken = filter (not . null . depsNotAvailable pkg_map1)
                             (Map.elems pkg_map1)
    (pkg_map2, broken) = removeUnits (map unitId directly_broken) index pkg_map1
    unusable_broken = mk_unusable BrokenDependencies depsNotAvailable pkg_map2 broken

    -- Find recursive units
    sccs = stronglyConnComp [ (pkg, unitId pkg, unitDepends pkg)
                            | pkg <- Map.elems pkg_map2 ]
    getCyclicSCC (CyclicSCC vs) = map unitId vs
    getCyclicSCC (AcyclicSCC _) = []
    (pkg_map3, cyclic) = removeUnits (concatMap getCyclicSCC sccs) index pkg_map2
    unusable_cyclic = mk_unusable CyclicDependencies depsNotAvailable pkg_map3 cyclic

    -- Apply ignore flags
    directly_ignored = ignoreUnits ignore_flags (Map.elems pkg_map3)
    (pkg_map4, ignored) = removeUnits (Map.keys directly_ignored) index pkg_map3
    unusable_ignored = mk_unusable IgnoredDependencies depsNotAvailable pkg_map4 ignored

    -- Knock out units whose dependencies don't agree with ABI
    -- (i.e., got invalidated due to shadowing)
    directly_shadowed = filter (not . null . depsAbiMismatch pkg_map4)
                               (Map.elems pkg_map4)
    (pkg_map5, shadowed) = removeUnits (map unitId directly_shadowed) index pkg_map4
    unusable_shadowed = mk_unusable ShadowedDependencies depsAbiMismatch pkg_map5 shadowed

    unusable = directly_ignored `Map.union` unusable_ignored
                                `Map.union` unusable_broken
                                `Map.union` unusable_cyclic
                                `Map.union` unusable_shadowed

-- -----------------------------------------------------------------------------
-- When all the command-line options are in, we can process our unit
-- settings and populate the unit state.

mkUnitState
    :: SDocContext            -- ^ SDocContext used to render exception messages
    -> (Int -> SDoc -> IO ()) -- ^ Trace printer
    -> UnitConfig
    -> IO (UnitState,[UnitDatabase UnitId])
mkUnitState ctx printer cfg = do
{-
   Plan.

   There are two main steps for making the package state:

    1. We want to build a single, unified package database based
       on all of the input databases, which upholds the invariant that
       there is only one package per any UnitId and there are no
       dangling dependencies.  We'll do this by merging, and
       then successively filtering out bad dependencies.

       a) Merge all the databases together.
          If an input database defines unit ID that is already in
          the unified database, that package SHADOWS the existing
          package in the current unified database.  Note that
          order is important: packages defined later in the list of
          command line arguments shadow those defined earlier.

       b) Remove all packages with missing dependencies, or
          mutually recursive dependencies.

       b) Remove packages selected by -ignore-package from input database

       c) Remove all packages which depended on packages that are now
          shadowed by an ABI-incompatible package

       d) report (with -v) any packages that were removed by steps 1-3

    2. We want to look at the flags controlling package visibility,
       and build a mapping of what module names are in scope and
       where they live.

       a) on the final, unified database, we apply -trust/-distrust
          flags directly, modifying the database so that the 'trusted'
          field has the correct value.

       b) we use the -package/-hide-package flags to compute a
          visibility map, stating what packages are "exposed" for
          the purposes of computing the module map.
          * if any flag refers to a package which was removed by 1-5, then
            we can give an error message explaining why
          * if -hide-all-packages was not specified, this step also
            hides packages which are superseded by later exposed packages
          * this step is done TWICE if -plugin-package/-hide-all-plugin-packages
            are used

       c) based on the visibility map, we pick wired packages and rewrite
          them to have the expected unitId.

       d) finally, using the visibility map and the package database,
          we build a mapping saying what every in scope module name points to.
-}

  -- if databases have not been provided, read the database flags
  raw_dbs <- case unitConfigDBCache cfg of
               Nothing  -> readUnitDatabases printer cfg
               Just dbs -> return dbs

  -- distrust all units if the flag is set
  let distrust_all db = db { unitDatabaseUnits = distrustAllUnits (unitDatabaseUnits db) }
      dbs | unitConfigDistrustAll cfg = map distrust_all raw_dbs
          | otherwise                 = raw_dbs


  -- This, and the other reverse's that you will see, are due to the fact that
  -- packageFlags, pluginPackageFlags, etc. are all specified in *reverse* order
  -- than they are on the command line.
  let other_flags = reverse (unitConfigFlagsExposed cfg)
  printer 2 $
      text "package flags" <+> ppr other_flags

  -- Merge databases together, without checking validity
  (pkg_map1, prec_map) <- mergeDatabases (printer 2) dbs

  -- Now that we've merged everything together, prune out unusable
  -- packages.
  let (pkg_map2, unusable, sccs) = validateDatabase cfg pkg_map1

  reportCycles   (printer 2) sccs
  reportUnusable (printer 2) unusable

  -- Apply trust flags (these flags apply regardless of whether
  -- or not packages are visible or not)
  pkgs1 <- foldM (applyTrustFlag ctx prec_map unusable)
                 (Map.elems pkg_map2) (reverse (unitConfigFlagsTrusted cfg))
  let prelim_pkg_db = mkUnitInfoMap pkgs1

  --
  -- Calculate the initial set of units from package databases, prior to any package flags.
  --
  -- Conceptually, we select the latest versions of all valid (not unusable) *packages*
  -- (not units). This is empty if we have -hide-all-packages.
  --
  -- Then we create an initial visibility map with default visibilities for all
  -- exposed, definite units which belong to the latest valid packages.
  --
  let preferLater unit unit' =
        case compareByPreference prec_map unit unit' of
            GT -> unit
            _  -> unit'
      addIfMorePreferable m unit = addToUDFM_C preferLater m (fsPackageName unit) unit
      -- This is the set of maximally preferable packages. In fact, it is a set of
      -- most preferable *units* keyed by package name, which act as stand-ins in
      -- for "a package in a database". We use units here because we don't have
      -- "a package in a database" as a type currently.
      mostPreferablePackageReps = if unitConfigHideAll cfg
                    then emptyUDFM
                    else foldl' addIfMorePreferable emptyUDFM pkgs1
      -- When exposing units, we want to consider all of those in the most preferable
      -- packages. We can implement that by looking for units that are equi-preferable
      -- with the most preferable unit for package. Being equi-preferable means that
      -- they must be in the same database, with the same version, and the same package name.
      --
      -- We must take care to consider all these units and not just the most
      -- preferable one, otherwise we can end up with problems like #16228.
      mostPreferable u =
        case lookupUDFM mostPreferablePackageReps (fsPackageName u) of
          Nothing -> False
          Just u' -> compareByPreference prec_map u u' == EQ
      vis_map1 = foldl' (\vm p ->
                            -- Note: we NEVER expose indefinite packages by
                            -- default, because it's almost assuredly not
                            -- what you want (no mix-in linking has occurred).
                            if unitIsExposed p && unitIsDefinite (mkUnit p) && mostPreferable p
                               then Map.insert (mkUnit p)
                                               UnitVisibility {
                                                 uv_expose_all = True,
                                                 uv_renamings = [],
                                                 uv_package_name = First (Just (fsPackageName p)),
                                                 uv_requirements = Map.empty,
                                                 uv_explicit = False
                                               }
                                               vm
                               else vm)
                         Map.empty pkgs1

  --
  -- Compute a visibility map according to the command-line flags (-package,
  -- -hide-package).  This needs to know about the unusable packages, since if a
  -- user tries to enable an unusable package, we should let them know.
  --
  vis_map2 <- foldM (applyPackageFlag ctx prec_map prelim_pkg_db emptyUniqSet unusable
                        (unitConfigHideAll cfg) pkgs1)
                            vis_map1 other_flags

  --
  -- Sort out which packages are wired in. This has to be done last, since
  -- it modifies the unit ids of wired in packages, but when we process
  -- package arguments we need to key against the old versions.
  --
  (pkgs2, wired_map) <- findWiredInUnits (printer 2) prec_map pkgs1 vis_map2
  let pkg_db = mkUnitInfoMap pkgs2

  -- Update the visibility map, so we treat wired packages as visible.
  let vis_map = updateVisibilityMap wired_map vis_map2

  let hide_plugin_pkgs = unitConfigHideAllPlugins cfg
  plugin_vis_map <-
    case unitConfigFlagsPlugins cfg of
        -- common case; try to share the old vis_map
        [] | not hide_plugin_pkgs -> return vis_map
           | otherwise -> return Map.empty
        _ -> do let plugin_vis_map1
                        | hide_plugin_pkgs = Map.empty
                        -- Use the vis_map PRIOR to wired in,
                        -- because otherwise applyPackageFlag
                        -- won't work.
                        | otherwise = vis_map2
                plugin_vis_map2
                    <- foldM (applyPackageFlag ctx prec_map prelim_pkg_db emptyUniqSet unusable
                                hide_plugin_pkgs pkgs1)
                             plugin_vis_map1
                             (reverse (unitConfigFlagsPlugins cfg))
                -- Updating based on wired in packages is mostly
                -- good hygiene, because it won't matter: no wired in
                -- package has a compiler plugin.
                -- TODO: If a wired in package had a compiler plugin,
                -- and you tried to pick different wired in packages
                -- with the plugin flags and the normal flags... what
                -- would happen?  I don't know!  But this doesn't seem
                -- likely to actually happen.
                return (updateVisibilityMap wired_map plugin_vis_map2)

  let pkgname_map = foldl' add Map.empty pkgs2
        where add pn_map p
                = Map.insert (unitPackageName p) (unitInstanceOf p) pn_map

  -- The explicitUnits accurately reflects the set of units we have turned
  -- on; as such, it also is the only way one can come up with requirements.
  -- The requirement context is directly based off of this: we simply
  -- look for nested unit IDs that are directly fed holes: the requirements
  -- of those units are precisely the ones we need to track
  let explicit_pkgs = Map.keys vis_map
      req_ctx = Map.map (Set.toList)
              $ Map.unionsWith Set.union (map uv_requirements (Map.elems vis_map))


  --
  -- Here we build up a set of the packages mentioned in -package
  -- flags on the command line; these are called the "preload"
  -- packages.  we link these packages in eagerly.  The preload set
  -- should contain at least rts & base, which is why we pretend that
  -- the command line contains -package rts & -package base.
  --
  -- NB: preload IS important even for type-checking, because we
  -- need the correct include path to be set.
  --
  let preload1 = Map.keys (Map.filter uv_explicit vis_map)

      -- add default preload units if they can be found in the db
      basicLinkedUnits = fmap (RealUnit . Definite)
                         $ filter (flip Map.member pkg_db)
                         $ unitConfigAutoLink cfg
      preload3 = ordNub $ (basicLinkedUnits ++ preload1)

  -- Close the preload packages with their dependencies
  let dep_preload_err = closeUnitDeps pkg_db (zip (map toUnitId preload3) (repeat Nothing))
  dep_preload <- throwErr ctx dep_preload_err

  let mod_map1 = mkModuleNameProvidersMap ctx cfg pkg_db emptyUniqSet vis_map
      mod_map2 = mkUnusableModuleNameProvidersMap unusable
      mod_map = Map.union mod_map1 mod_map2

  -- Force the result to avoid leaking input parameters
  let !state = UnitState
         { preloadUnits                 = dep_preload
         , explicitUnits                = explicit_pkgs
         , unitInfoMap                  = pkg_db
         , preloadClosure               = emptyUniqSet
         , moduleNameProvidersMap       = mod_map
         , pluginModuleNameProvidersMap = mkModuleNameProvidersMap ctx cfg pkg_db emptyUniqSet plugin_vis_map
         , packageNameMap               = pkgname_map
         , wireMap                      = wired_map
         , unwireMap                    = Map.fromList [ (v,k) | (k,v) <- Map.toList wired_map ]
         , requirementContext           = req_ctx
         , allowVirtualUnits            = unitConfigAllowVirtualUnits cfg
         }

  return (state, raw_dbs)

-- | Given a wired-in 'Unit', "unwire" it into the 'Unit'
-- that it was recorded as in the package database.
unwireUnit :: UnitState -> Unit-> Unit
unwireUnit state uid@(RealUnit (Definite def_uid)) =
    maybe uid (RealUnit . Definite) (Map.lookup def_uid (unwireMap state))
unwireUnit _ uid = uid

-- -----------------------------------------------------------------------------
-- | Makes the mapping from ModuleName to package info

-- Slight irritation: we proceed by leafing through everything
-- in the installed package database, which makes handling indefinite
-- packages a bit bothersome.

mkModuleNameProvidersMap
  :: SDocContext     -- ^ SDocContext used to render exception messages
  -> UnitConfig
  -> UnitInfoMap
  -> PreloadUnitClosure
  -> VisibilityMap
  -> ModuleNameProvidersMap
mkModuleNameProvidersMap ctx cfg pkg_map closure vis_map =
    -- What should we fold on?  Both situations are awkward:
    --
    --    * Folding on the visibility map means that we won't create
    --      entries for packages that aren't mentioned in vis_map
    --      (e.g., hidden packages, causing #14717)
    --
    --    * Folding on pkg_map is awkward because if we have an
    --      Backpack instantiation, we need to possibly add a
    --      package from pkg_map multiple times to the actual
    --      ModuleNameProvidersMap.  Also, we don't really want
    --      definite package instantiations to show up in the
    --      list of possibilities.
    --
    -- So what will we do instead?  We'll extend vis_map with
    -- entries for every definite (for non-Backpack) and
    -- indefinite (for Backpack) package, so that we get the
    -- hidden entries we need.
    Map.foldlWithKey extend_modmap emptyMap vis_map_extended
 where
  vis_map_extended = Map.union vis_map {- preferred -} default_vis

  default_vis = Map.fromList
                  [ (mkUnit pkg, mempty)
                  | pkg <- Map.elems pkg_map
                  -- Exclude specific instantiations of an indefinite
                  -- package
                  , unitIsIndefinite pkg || null (unitInstantiations pkg)
                  ]

  emptyMap = Map.empty
  setOrigins m os = fmap (const os) m
  extend_modmap modmap uid
    UnitVisibility { uv_expose_all = b, uv_renamings = rns }
    = addListTo modmap theBindings
   where
    pkg = unit_lookup uid

    theBindings :: [(ModuleName, Map Module ModuleOrigin)]
    theBindings = newBindings b rns

    newBindings :: Bool
                -> [(ModuleName, ModuleName)]
                -> [(ModuleName, Map Module ModuleOrigin)]
    newBindings e rns  = es e ++ hiddens ++ map rnBinding rns

    rnBinding :: (ModuleName, ModuleName)
              -> (ModuleName, Map Module ModuleOrigin)
    rnBinding (orig, new) = (new, setOrigins origEntry fromFlag)
     where origEntry = case lookupUFM esmap orig of
            Just r -> r
            Nothing -> throwGhcException (CmdLineError (renderWithStyle ctx
                        (text "package flag: could not find module name" <+>
                            ppr orig <+> text "in package" <+> ppr pk)))

    es :: Bool -> [(ModuleName, Map Module ModuleOrigin)]
    es e = do
     (m, exposedReexport) <- exposed_mods
     let (pk', m', origin') =
          case exposedReexport of
           Nothing -> (pk, m, fromExposedModules e)
           Just (Module pk' m') ->
            let pkg' = unit_lookup pk'
            in (pk', m', fromReexportedModules e pkg')
     return (m, mkModMap pk' m' origin')

    esmap :: UniqFM (Map Module ModuleOrigin)
    esmap = listToUFM (es False) -- parameter here doesn't matter, orig will
                                 -- be overwritten

    hiddens = [(m, mkModMap pk m ModHidden) | m <- hidden_mods]

    pk = mkUnit pkg
    unit_lookup uid = lookupUnit' (unitConfigAllowVirtualUnits cfg) pkg_map closure uid
                        `orElse` pprPanic "unit_lookup" (ppr uid)

    exposed_mods = unitExposedModules pkg
    hidden_mods  = unitHiddenModules pkg

-- | Make a 'ModuleNameProvidersMap' covering a set of unusable packages.
mkUnusableModuleNameProvidersMap :: UnusableUnits -> ModuleNameProvidersMap
mkUnusableModuleNameProvidersMap unusables =
    Map.foldl' extend_modmap Map.empty unusables
 where
    extend_modmap modmap (pkg, reason) = addListTo modmap bindings
      where bindings :: [(ModuleName, Map Module ModuleOrigin)]
            bindings = exposed ++ hidden

            origin = ModUnusable reason
            pkg_id = mkUnit pkg

            exposed = map get_exposed exposed_mods
            hidden = [(m, mkModMap pkg_id m origin) | m <- hidden_mods]

            get_exposed (mod, Just mod') = (mod, Map.singleton mod' origin)
            get_exposed (mod, _)         = (mod, mkModMap pkg_id mod origin)

            exposed_mods = unitExposedModules pkg
            hidden_mods  = unitHiddenModules pkg

-- | Add a list of key/value pairs to a nested map.
--
-- The outer map is processed with 'Data.Map.Strict' to prevent memory leaks
-- when reloading modules in GHCi (see #4029). This ensures that each
-- value is forced before installing into the map.
addListTo :: (Monoid a, Ord k1, Ord k2)
          => Map k1 (Map k2 a)
          -> [(k1, Map k2 a)]
          -> Map k1 (Map k2 a)
addListTo = foldl' merge
  where merge m (k, v) = MapStrict.insertWith (Map.unionWith mappend) k v m

-- | Create a singleton module mapping
mkModMap :: Unit -> ModuleName -> ModuleOrigin -> Map Module ModuleOrigin
mkModMap pkg mod = Map.singleton (mkModule pkg mod)

-- -----------------------------------------------------------------------------
-- Extracting information from the packages in scope

-- Many of these functions take a list of packages: in those cases,
-- the list is expected to contain the "dependent packages",
-- i.e. those packages that were found to be depended on by the
-- current module/program.  These can be auto or non-auto packages, it
-- doesn't really matter.  The list is always combined with the list
-- of preload (command-line) packages to determine which packages to
-- use.

-- | Find all the include directories in these and the preload packages
getUnitIncludePath :: DynFlags -> [UnitId] -> IO [String]
getUnitIncludePath dflags pkgs =
  collectIncludeDirs `fmap` getPreloadUnitsAnd dflags pkgs

collectIncludeDirs :: [UnitInfo] -> [FilePath]
collectIncludeDirs ps = ordNub (filter notNull (concatMap unitIncludeDirs ps))

-- | Find all the library paths in these and the preload packages
getUnitLibraryPath :: DynFlags -> [UnitId] -> IO [String]
getUnitLibraryPath dflags pkgs =
  collectLibraryPaths dflags `fmap` getPreloadUnitsAnd dflags pkgs

collectLibraryPaths :: DynFlags -> [UnitInfo] -> [FilePath]
collectLibraryPaths dflags = ordNub . filter notNull
                           . concatMap (libraryDirsForWay dflags)

-- | Find all the link options in these and the preload packages,
-- returning (package hs lib options, extra library options, other flags)
getUnitLinkOpts :: DynFlags -> [UnitId] -> IO ([String], [String], [String])
getUnitLinkOpts dflags pkgs =
  collectLinkOpts dflags `fmap` getPreloadUnitsAnd dflags pkgs

collectLinkOpts :: DynFlags -> [UnitInfo] -> ([String], [String], [String])
collectLinkOpts dflags ps =
    (
        concatMap (map ("-l" ++) . packageHsLibs dflags) ps,
        concatMap (map ("-l" ++) . unitExtDepLibsSys) ps,
        concatMap unitLinkerOptions ps
    )
collectArchives :: DynFlags -> UnitInfo -> IO [FilePath]
collectArchives dflags pc =
  filterM doesFileExist [ searchPath </> ("lib" ++ lib ++ ".a")
                        | searchPath <- searchPaths
                        , lib <- libs ]
  where searchPaths = ordNub . filter notNull . libraryDirsForWay dflags $ pc
        libs        = packageHsLibs dflags pc ++ unitExtDepLibsSys pc

getLibs :: DynFlags -> [UnitId] -> IO [(String,String)]
getLibs dflags pkgs = do
  ps <- getPreloadUnitsAnd dflags pkgs
  fmap concat . forM ps $ \p -> do
    let candidates = [ (l </> f, f) | l <- collectLibraryPaths dflags [p]
                                    , f <- (\n -> "lib" ++ n ++ ".a") <$> packageHsLibs dflags p ]
    filterM (doesFileExist . fst) candidates

packageHsLibs :: DynFlags -> UnitInfo -> [String]
packageHsLibs dflags p = map (mkDynName . addSuffix) (unitLibraries p)
  where
        ways0 = ways dflags

        ways1 = Set.filter (/= WayDyn) ways0
        -- the name of a shared library is libHSfoo-ghc<version>.so
        -- we leave out the _dyn, because it is superfluous

        -- debug and profiled RTSs include support for -eventlog
        ways2 | WayDebug `Set.member` ways1 || WayProf `Set.member` ways1
              = Set.filter (/= WayEventLog) ways1
              | otherwise
              = ways1

        tag     = waysTag (Set.filter (not . wayRTSOnly) ways2)
        rts_tag = waysTag ways2

        mkDynName x
         | WayDyn `Set.notMember` ways dflags = x
         | "HS" `isPrefixOf` x                =
              x ++ '-':programName dflags ++ projectVersion dflags
           -- For non-Haskell libraries, we use the name "Cfoo". The .a
           -- file is libCfoo.a, and the .so is libfoo.so. That way the
           -- linker knows what we mean for the vanilla (-lCfoo) and dyn
           -- (-lfoo) ways. We therefore need to strip the 'C' off here.
         | Just x' <- stripPrefix "C" x = x'
         | otherwise
            = panic ("Don't understand library name " ++ x)

        -- Add _thr and other rts suffixes to packages named
        -- `rts` or `rts-1.0`. Why both?  Traditionally the rts
        -- package is called `rts` only.  However the tooling
        -- usually expects a package name to have a version.
        -- As such we will gradually move towards the `rts-1.0`
        -- package name, at which point the `rts` package name
        -- will eventually be unused.
        --
        -- This change elevates the need to add custom hooks
        -- and handling specifically for the `rts` package for
        -- example in ghc-cabal.
        addSuffix rts@"HSrts"    = rts       ++ (expandTag rts_tag)
        addSuffix rts@"HSrts-1.0"= rts       ++ (expandTag rts_tag)
        addSuffix other_lib      = other_lib ++ (expandTag tag)

        expandTag t | null t = ""
                    | otherwise = '_':t

-- | Either the 'unitLibraryDirs' or 'unitLibraryDynDirs' as appropriate for the way.
libraryDirsForWay :: DynFlags -> UnitInfo -> [String]
libraryDirsForWay dflags
  | WayDyn `elem` ways dflags = unitLibraryDynDirs
  | otherwise                 = unitLibraryDirs

-- | Find all the C-compiler options in these and the preload packages
getUnitExtraCcOpts :: DynFlags -> [UnitId] -> IO [String]
getUnitExtraCcOpts dflags pkgs = do
  ps <- getPreloadUnitsAnd dflags pkgs
  return (concatMap unitCcOptions ps)

-- | Find all the package framework paths in these and the preload packages
getUnitFrameworkPath  :: DynFlags -> [UnitId] -> IO [String]
getUnitFrameworkPath dflags pkgs = do
  ps <- getPreloadUnitsAnd dflags pkgs
  return (ordNub (filter notNull (concatMap unitExtDepFrameworkDirs ps)))

-- | Find all the package frameworks in these and the preload packages
getUnitFrameworks  :: DynFlags -> [UnitId] -> IO [String]
getUnitFrameworks dflags pkgs = do
  ps <- getPreloadUnitsAnd dflags pkgs
  return (concatMap unitExtDepFrameworks ps)

-- -----------------------------------------------------------------------------
-- Package Utils

-- | Takes a 'ModuleName', and if the module is in any package returns
-- list of modules which take that name.
lookupModuleInAllUnits :: UnitState
                          -> ModuleName
                          -> [(Module, UnitInfo)]
lookupModuleInAllUnits pkgs m
  = case lookupModuleWithSuggestions pkgs m Nothing of
      LookupFound a b -> [(a,b)]
      LookupMultiple rs -> map f rs
        where f (m,_) = (m, expectJust "lookupModule" (lookupUnit pkgs
                                                         (moduleUnit m)))
      _ -> []

-- | The result of performing a lookup
data LookupResult =
    -- | Found the module uniquely, nothing else to do
    LookupFound Module UnitInfo
    -- | Multiple modules with the same name in scope
  | LookupMultiple [(Module, ModuleOrigin)]
    -- | No modules found, but there were some hidden ones with
    -- an exact name match.  First is due to package hidden, second
    -- is due to module being hidden
  | LookupHidden [(Module, ModuleOrigin)] [(Module, ModuleOrigin)]
    -- | No modules found, but there were some unusable ones with
    -- an exact name match
  | LookupUnusable [(Module, ModuleOrigin)]
    -- | Nothing found, here are some suggested different names
  | LookupNotFound [ModuleSuggestion] -- suggestions

data ModuleSuggestion = SuggestVisible ModuleName Module ModuleOrigin
                      | SuggestHidden ModuleName Module ModuleOrigin

lookupModuleWithSuggestions :: UnitState
                            -> ModuleName
                            -> Maybe FastString
                            -> LookupResult
lookupModuleWithSuggestions pkgs
  = lookupModuleWithSuggestions' pkgs (moduleNameProvidersMap pkgs)

lookupPluginModuleWithSuggestions :: UnitState
                                  -> ModuleName
                                  -> Maybe FastString
                                  -> LookupResult
lookupPluginModuleWithSuggestions pkgs
  = lookupModuleWithSuggestions' pkgs (pluginModuleNameProvidersMap pkgs)

lookupModuleWithSuggestions' :: UnitState
                            -> ModuleNameProvidersMap
                            -> ModuleName
                            -> Maybe FastString
                            -> LookupResult
lookupModuleWithSuggestions' pkgs mod_map m mb_pn
  = case Map.lookup m mod_map of
        Nothing -> LookupNotFound suggestions
        Just xs ->
          case foldl' classify ([],[],[], []) (Map.toList xs) of
            ([], [], [], []) -> LookupNotFound suggestions
            (_, _, _, [(m, _)])             -> LookupFound m (mod_unit m)
            (_, _, _, exposed@(_:_))        -> LookupMultiple exposed
            ([], [], unusable@(_:_), [])    -> LookupUnusable unusable
            (hidden_pkg, hidden_mod, _, []) ->
              LookupHidden hidden_pkg hidden_mod
  where
    classify (hidden_pkg, hidden_mod, unusable, exposed) (m, origin0) =
      let origin = filterOrigin mb_pn (mod_unit m) origin0
          x = (m, origin)
      in case origin of
          ModHidden
            -> (hidden_pkg, x:hidden_mod, unusable, exposed)
          ModUnusable _
            -> (hidden_pkg, hidden_mod, x:unusable, exposed)
          _ | originEmpty origin
            -> (hidden_pkg,   hidden_mod, unusable, exposed)
            | originVisible origin
            -> (hidden_pkg, hidden_mod, unusable, x:exposed)
            | otherwise
            -> (x:hidden_pkg, hidden_mod, unusable, exposed)

    unit_lookup p = lookupUnit pkgs p `orElse` pprPanic "lookupModuleWithSuggestions" (ppr p <+> ppr m)
    mod_unit = unit_lookup . moduleUnit

    -- Filters out origins which are not associated with the given package
    -- qualifier.  No-op if there is no package qualifier.  Test if this
    -- excluded all origins with 'originEmpty'.
    filterOrigin :: Maybe FastString
                 -> UnitInfo
                 -> ModuleOrigin
                 -> ModuleOrigin
    filterOrigin Nothing _ o = o
    filterOrigin (Just pn) pkg o =
      case o of
          ModHidden -> if go pkg then ModHidden else mempty
          (ModUnusable _) -> if go pkg then o else mempty
          ModOrigin { fromOrigUnit = e, fromExposedReexport = res,
                      fromHiddenReexport = rhs }
            -> ModOrigin {
                  fromOrigUnit = if go pkg then e else Nothing
                , fromExposedReexport = filter go res
                , fromHiddenReexport = filter go rhs
                , fromPackageFlag = False -- always excluded
                }
      where go pkg = pn == fsPackageName pkg

    suggestions = fuzzyLookup (moduleNameString m) all_mods

    all_mods :: [(String, ModuleSuggestion)]     -- All modules
    all_mods = sortBy (comparing fst) $
        [ (moduleNameString m, suggestion)
        | (m, e) <- Map.toList (moduleNameProvidersMap pkgs)
        , suggestion <- map (getSuggestion m) (Map.toList e)
        ]
    getSuggestion name (mod, origin) =
        (if originVisible origin then SuggestVisible else SuggestHidden)
            name mod origin

listVisibleModuleNames :: UnitState -> [ModuleName]
listVisibleModuleNames state =
    map fst (filter visible (Map.toList (moduleNameProvidersMap state)))
  where visible (_, ms) = any originVisible (Map.elems ms)

-- | Lookup 'UnitInfo' for every preload unit, for every unit used to
-- instantiate the current unit, and for every unit explicitly passed in the
-- given list of UnitId.
getPreloadUnitsAnd :: DynFlags -> [UnitId] -> IO [UnitInfo]
getPreloadUnitsAnd dflags ids0 =
  let
      ids  = ids0 ++
              -- An indefinite package will have insts to HOLE,
              -- which is not a real package. Don't look it up.
              -- Fixes #14525
              if homeUnitIsIndefinite dflags
                then []
                else map (toUnitId . moduleUnit . snd)
                         (homeUnitInstantiations dflags)
      state   = unitState dflags
      pkg_map = unitInfoMap state
      preload = preloadUnits state
      ctx     = initSDocContext dflags defaultUserStyle
  in do
  all_pkgs <- throwErr ctx (closeUnitDeps' pkg_map preload (ids `zip` repeat Nothing))
  return (map (unsafeLookupUnitId state) all_pkgs)

throwErr :: SDocContext -> MaybeErr MsgDoc a -> IO a
throwErr ctx m = case m of
   Failed e    -> throwGhcExceptionIO (CmdLineError (renderWithStyle ctx e))
   Succeeded r -> return r

-- | Takes a list of UnitIds (and their "parent" dependency, used for error
-- messages), and returns the list with dependencies included, in reverse
-- dependency order (a units appears before those it depends on).
closeUnitDeps :: UnitInfoMap -> [(UnitId,Maybe UnitId)] -> MaybeErr MsgDoc [UnitId]
closeUnitDeps pkg_map ps = closeUnitDeps' pkg_map [] ps

-- | Similar to closeUnitDeps but takes a list of already loaded units as an
-- additional argument.
closeUnitDeps' :: UnitInfoMap -> [UnitId] -> [(UnitId,Maybe UnitId)] -> MaybeErr MsgDoc [UnitId]
closeUnitDeps' pkg_map current_ids ps = foldM (add_unit pkg_map) current_ids ps

-- | Add a UnitId and those it depends on (recursively) to the given list of
-- UnitIds if they are not already in it. Return a list in reverse dependency
-- order (a unit appears before those it depends on).
--
-- The UnitId is looked up in the given UnitInfoMap (to find its dependencies).
-- It it's not found, the optional parent unit is used to return a more precise
-- error message ("dependency of <PARENT>").
add_unit :: UnitInfoMap
            -> [UnitId]
            -> (UnitId,Maybe UnitId)
            -> MaybeErr MsgDoc [UnitId]
add_unit pkg_map ps (p, mb_parent)
  | p `elem` ps = return ps     -- Check if we've already added this unit
  | otherwise   = case lookupUnitId' pkg_map p of
      Nothing -> Failed $
                   (ftext (fsLit "unknown package:") <+> ppr p)
                   <> case mb_parent of
                         Nothing     -> Outputable.empty
                         Just parent -> space <> parens (text "dependency of"
                                                  <+> ftext (unitIdFS parent))
      Just info -> do
         -- Add the unit's dependents also
         ps' <- foldM add_unit_key ps (unitDepends info)
         return (p : ps')
        where
          add_unit_key ps key
            = add_unit pkg_map ps (key, Just p)

-- -----------------------------------------------------------------------------

-- Cabal packages may contain several components (programs, libraries, etc.).
-- As far as GHC is concerned, installed package components ("units") are
-- identified by an opaque IndefUnitId string provided by Cabal. As the string
-- contains a hash, we don't want to display it to users so GHC queries the
-- database to retrieve some infos about the original source package (name,
-- version, component name).
--
-- Instead we want to display: packagename-version[:componentname]
--
-- Component name is only displayed if it isn't the default library
--
-- To do this we need to query the database (cached in DynFlags). We cache
-- these details in the IndefUnitId itself because we don't want to query
-- DynFlags each time we pretty-print the IndefUnitId
--
mkIndefUnitId :: UnitState -> FastString -> IndefUnitId
mkIndefUnitId pkgstate raw =
    let uid = UnitId raw
    in case lookupUnitId pkgstate uid of
         Nothing -> Indefinite uid Nothing -- we didn't find the unit at all
         Just c  -> Indefinite uid $ Just $ mkUnitPprInfo c

-- | Update component ID details from the database
updateIndefUnitId :: UnitState -> IndefUnitId -> IndefUnitId
updateIndefUnitId pkgstate uid = mkIndefUnitId pkgstate (unitIdFS (indefUnit uid))


displayUnitId :: UnitState -> UnitId -> Maybe String
displayUnitId pkgstate uid =
    fmap unitPackageIdString (lookupUnitId pkgstate uid)

-- -----------------------------------------------------------------------------
-- Displaying packages

-- | Show (very verbose) package info
pprUnits :: UnitState -> SDoc
pprUnits = pprUnitsWith pprUnitInfo

pprUnitsWith :: (UnitInfo -> SDoc) -> UnitState -> SDoc
pprUnitsWith pprIPI pkgstate =
    vcat (intersperse (text "---") (map pprIPI (listUnitInfo pkgstate)))

-- | Show simplified unit info.
--
-- The idea is to only print package id, and any information that might
-- be different from the package databases (exposure, trust)
pprUnitsSimple :: UnitState -> SDoc
pprUnitsSimple = pprUnitsWith pprIPI
    where pprIPI ipi = let i = unitIdFS (unitId ipi)
                           e = if unitIsExposed ipi then text "E" else text " "
                           t = if unitIsTrusted ipi then text "T" else text " "
                       in e <> t <> text "  " <> ftext i

-- | Show the mapping of modules to where they come from.
pprModuleMap :: ModuleNameProvidersMap -> SDoc
pprModuleMap mod_map =
  vcat (map pprLine (Map.toList mod_map))
    where
      pprLine (m,e) = ppr m $$ nest 50 (vcat (map (pprEntry m) (Map.toList e)))
      pprEntry :: Outputable a => ModuleName -> (Module, a) -> SDoc
      pprEntry m (m',o)
        | m == moduleName m' = ppr (moduleUnit m') <+> parens (ppr o)
        | otherwise = ppr m' <+> parens (ppr o)

fsPackageName :: UnitInfo -> FastString
fsPackageName info = fs
   where
      PackageName fs = unitPackageName info


-- | Given a fully instantiated 'InstantiatedUnit', improve it into a
-- 'RealUnit' if we can find it in the package database.
improveUnit :: UnitState -> Unit -> Unit
improveUnit state u = improveUnit' (unitInfoMap state) (preloadClosure state) u

-- | Given a fully instantiated 'InstantiatedUnit', improve it into a
-- 'RealUnit' if we can find it in the package database.
improveUnit' :: UnitInfoMap -> PreloadUnitClosure -> Unit -> Unit
improveUnit' _       _       uid@(RealUnit _) = uid -- short circuit
improveUnit' pkg_map closure uid =
    -- Do NOT lookup indefinite ones, they won't be useful!
    case lookupUnit' False pkg_map closure uid of
        Nothing  -> uid
        Just pkg ->
            -- Do NOT improve if the indefinite unit id is not
            -- part of the closure unique set.  See
            -- Note [VirtUnit to RealUnit improvement]
            if unitId pkg `elementOfUniqSet` closure
                then mkUnit pkg
                else uid

-- | Check the database to see if we already have an installed unit that
-- corresponds to the given 'InstantiatedUnit'.
--
-- Return a `UnitId` which either wraps the `InstantiatedUnit` unchanged or
-- references a matching installed unit.
--
-- See Note [VirtUnit to RealUnit improvement]
instUnitToUnit :: UnitState -> InstantiatedUnit -> Unit
instUnitToUnit state iuid =
    -- NB: suppose that we want to compare the instantiated
    -- unit p[H=impl:H] against p+abcd (where p+abcd
    -- happens to be the existing, installed version of
    -- p[H=impl:H].  If we *only* wrap in p[H=impl:H]
    -- VirtUnit, they won't compare equal; only
    -- after improvement will the equality hold.
    improveUnit state $ VirtUnit iuid


-- | Substitution on module variables, mapping module names to module
-- identifiers.
type ShHoleSubst = ModuleNameEnv Module

-- | Substitutes holes in a 'Module'.  NOT suitable for being called
-- directly on a 'nameModule', see Note [Representation of module/name variable].
-- @p[A=\<A>]:B@ maps to @p[A=q():A]:B@ with @A=q():A@;
-- similarly, @\<A>@ maps to @q():A@.
renameHoleModule :: UnitState -> ShHoleSubst -> Module -> Module
renameHoleModule state = renameHoleModule' (unitInfoMap state) (preloadClosure state)

-- | Substitutes holes in a 'Unit', suitable for renaming when
-- an include occurs; see Note [Representation of module/name variable].
--
-- @p[A=\<A>]@ maps to @p[A=\<B>]@ with @A=\<B>@.
renameHoleUnit :: UnitState -> ShHoleSubst -> Unit -> Unit
renameHoleUnit state = renameHoleUnit' (unitInfoMap state) (preloadClosure state)

-- | Like 'renameHoleModule', but requires only 'ClosureUnitInfoMap'
-- so it can be used by "GHC.Unit.State".
renameHoleModule' :: UnitInfoMap -> PreloadUnitClosure -> ShHoleSubst -> Module -> Module
renameHoleModule' pkg_map closure env m
  | not (isHoleModule m) =
        let uid = renameHoleUnit' pkg_map closure env (moduleUnit m)
        in mkModule uid (moduleName m)
  | Just m' <- lookupUFM env (moduleName m) = m'
  -- NB m = <Blah>, that's what's in scope.
  | otherwise = m

-- | Like 'renameHoleUnit, but requires only 'ClosureUnitInfoMap'
-- so it can be used by "GHC.Unit.State".
renameHoleUnit' :: UnitInfoMap -> PreloadUnitClosure -> ShHoleSubst -> Unit -> Unit
renameHoleUnit' pkg_map closure env uid =
    case uid of
      (VirtUnit
        InstantiatedUnit{ instUnitInstanceOf = cid
                        , instUnitInsts      = insts
                        , instUnitHoles      = fh })
          -> if isNullUFM (intersectUFM_C const (udfmToUfm (getUniqDSet fh)) env)
                then uid
                -- Functorially apply the substitution to the instantiation,
                -- then check the 'ClosureUnitInfoMap' to see if there is
                -- a compiled version of this 'InstantiatedUnit' we can improve to.
                -- See Note [VirtUnit to RealUnit improvement]
                else improveUnit' pkg_map closure $
                        mkVirtUnit cid
                            (map (\(k,v) -> (k, renameHoleModule' pkg_map closure env v)) insts)
      _ -> uid

-- | Injects an 'InstantiatedModule' to 'Module' (see also
-- 'instUnitToUnit'.
instModuleToModule :: UnitState -> InstantiatedModule -> Module
instModuleToModule pkgstate (Module iuid mod_name) =
    mkModule (instUnitToUnit pkgstate iuid) mod_name

