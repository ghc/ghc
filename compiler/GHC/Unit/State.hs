-- (c) The University of Glasgow, 2006


-- | Unit manipulation
module GHC.Unit.State (
        module GHC.Unit.Info,
        -- * Reading the package config, and processing cmdline args
        UnitState(..),
        UnitDatabase (..),
        UnitErr (..),
        emptyUnitState,
        initUnits,
        readUnitDatabases,
        readUnitDatabase,
        getUnitDbRefs,
        resolveUnitDatabase,
        listUnitInfo,
        -- * Overlays over the unit set
        TrustOverlay,
        IsTrusted(..),
        lookupTrustOverlay,
        distrustUnits,
        trustUnits,
        emptyTrustOverlay,
        -- * Querying the package config
        lookupUnit,
        lookupUnit',
        unsafeLookupUnit,
        lookupUnitId,
        lookupUnitId',
        unsafeLookupUnitId,
        isUnitTrusted,
        isUnitIdTrusted,
        isUnitInfoTrusted,

        lookupPackageName,
        resolvePackageImport,
        searchPackageId,
        listVisibleModuleNames,
        lookupModuleInAllUnits,
        lookupModuleWithSuggestions,
        lookupModulePackage,
        lookupPluginModuleWithSuggestions,
        requirementMerges,
        LookupResult(..),
        ModuleSuggestion(..),
        ModuleOrigin(..),
        UnusableUnit(..),
        UnusableUnitReason(..),
        pprReason,

        closeUnitDeps,
        closeUnitDeps',
        mayThrowUnitErr,

        -- * Module hole substitution
        ShHoleSubst,
        renameHoleUnit,
        renameHoleModule,
        renameHoleUnit',
        renameHoleModule',
        instUnitToUnit,
        instModuleToModule,

        -- * Pretty-printing
        pprFlag,
        pprUnits,
        pprUnitsSimple,
        pprUnitIdForUser,
        pprUnitInfoForUser,
        pprModuleMap,
        pprWithUnitState,
        pprRawUnitIds,

        -- * Utils
        unwireUnit,
        selectHptFlag,
        )
where

import GHC.Prelude

import GHC.Driver.DynFlags

import GHC.Platform
import GHC.Platform.Ways

import GHC.Unit.Database
import GHC.Unit.Home
import GHC.Unit.Info
import GHC.Unit.Module
import GHC.Unit.Ppr

import GHC.Unit.External.Database
import GHC.Unit.External.Index
import GHC.Unit.External.ModuleOrigin
import GHC.Unit.External.Providers
import GHC.Unit.External.Query
import GHC.Unit.External.Substitution
import GHC.Unit.External.Validate
import GHC.Unit.External.Visibility
import GHC.Unit.External.Wired

import GHC.Types.PkgQual
import GHC.Types.Unique.DFM
import GHC.Types.Unique.FM
import GHC.Types.Unique.Map

import GHC.Data.Maybe
import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import GHC.Data.FastString
import GHC.Data.OsPath qualified as OsPath
import GHC.Data.ShortText qualified as ST
import GHC.Utils.Error
import GHC.Utils.Logger

import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.Graph (SCC (..))
import Data.List (intersperse, partition, sort, sortOn)
import Data.Monoid (First (..))
import Data.Set (Set)
import Data.Set qualified as Set

-- ---------------------------------------------------------------------------
-- The Unit state

-- The unit state is computed by 'initUnits', and kept in HscEnv.
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

-- | Unit configuration
data UnitConfig = UnitConfig
   { unitConfigPlatformArchOS :: !ArchOS        -- ^ Platform arch and OS
   , unitConfigWays           :: !Ways          -- ^ Ways to use

   , unitConfigAllowVirtual   :: !Bool          -- ^ Allow virtual units
      -- ^ Do we allow the use of virtual units instantiated on-the-fly (see
      -- Note [About units] in GHC.Unit). This should only be true when we are
      -- type-checking an indefinite unit (not producing any code).

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

   -- command-line flags
   , unitConfigFlagsDB      :: [PackageDBFlag]     -- ^ Unit databases flags
   , unitConfigFlagsExposed :: [PackageFlag]       -- ^ Exposed units
   , unitConfigFlagsIgnored :: [IgnorePackageFlag] -- ^ Ignored units
   , unitConfigFlagsTrusted :: [TrustFlag]         -- ^ Trusted units
   , unitConfigFlagsPlugins :: [PackageFlag]       -- ^ Plugins exposed units
   , unitConfigHomeUnits    :: Set.Set UnitId
   }

initUnitConfig :: DynFlags -> Set.Set UnitId -> UnitConfig
initUnitConfig dflags home_units =
   let !hu_id             = homeUnitId_ dflags
       !hu_instanceof     = homeUnitInstanceOf_ dflags
       !hu_instantiations = homeUnitInstantiations_ dflags

       autoLink
         | not (gopt Opt_AutoLinkPackages dflags) = []
         -- By default we add base, ghc-internal and rts to the preload units (when they are
         -- found in the unit database) except when we are building them
         --
         -- Since "base" is not wired in, then the unit-id is discovered
         -- from the settings file by default, but can be overriden by power-users
         -- by specifying `-base-unit-id` flag.
         | otherwise = filter (hu_id /=) (baseUnitId dflags:wiredInUnitIds)

       -- if the home unit is indefinite, it means we are type-checking it only
       -- (not producing any code). Hence we can use virtual units instantiated
       -- on-the-fly. See Note [About units] in GHC.Unit
       allow_virtual_units = case (hu_instanceof, hu_instantiations) of
            (Just u, is) -> u == hu_id && any (isHoleModule . snd) is
            _            -> False

   in UnitConfig
      { unitConfigPlatformArchOS = platformArchOS (targetPlatform dflags)
      , unitConfigProgramName    = programName dflags
      , unitConfigWays           = ways dflags
      , unitConfigAllowVirtual   = allow_virtual_units

      , unitConfigGlobalDB       = globalPackageDatabasePath dflags
      , unitConfigGHCDir         = topDir dflags
      , unitConfigDBName         = "package.conf.d"

      , unitConfigAutoLink       = autoLink
      , unitConfigDistrustAll    = gopt Opt_DistrustAllPackages dflags
      , unitConfigHideAll        = gopt Opt_HideAllPackages dflags
      , unitConfigHideAllPlugins = gopt Opt_HideAllPluginPackages dflags

      , unitConfigFlagsDB      = map (offsetPackageDb (workingDirectory dflags)) $ packageDBFlags dflags
      , unitConfigFlagsExposed = packageFlags dflags
      , unitConfigFlagsIgnored = ignorePackageFlags dflags
      , unitConfigFlagsTrusted = trustFlags dflags
      , unitConfigFlagsPlugins = pluginPackageFlags dflags
      , unitConfigHomeUnits    = home_units

      }

  where
    offsetPackageDb :: Maybe FilePath -> PackageDBFlag -> PackageDBFlag
    offsetPackageDb (Just offset) (PackageDB (PkgDbPath p)) | OsPath.isRelative p = PackageDB (PkgDbPath (OsPath.unsafeEncodeUtf offset OsPath.</> p))
    offsetPackageDb _ p = p

data IsTrusted
  = Trusted
  | Distrusted
  deriving ( Eq, Ord )

-- | The 'TrustOverlay' stores user overwrites of the on-disk 'unitIsTrusted' status.
--
-- The user can overwrite this value via flags such as @-distrust-all-packages@.
-- We do not modify the 'UnitInfo' directory, but rather store this user selection
-- in the 'TrustOverlay'.
--
-- This allows us to share the 'UnitInfo' completely and saves us memory.
newtype TrustOverlay = TrustOverlay
  { trustOverlay :: UniqMap UnitId IsTrusted
  }

lookupTrustOverlay :: TrustOverlay -> UnitId -> Maybe IsTrusted
lookupTrustOverlay to = lookupUniqMap (trustOverlay to)

distrustUnits :: [UnitId] -> TrustOverlay -> TrustOverlay
distrustUnits elements (TrustOverlay to) = TrustOverlay $ foldl' (\ acc uid -> addToUniqMap acc uid Distrusted) to elements

trustUnits :: [UnitId] -> TrustOverlay -> TrustOverlay
trustUnits elements (TrustOverlay to) = TrustOverlay $ foldl' (\ acc uid -> addToUniqMap acc uid Trusted) to elements

emptyTrustOverlay :: TrustOverlay
emptyTrustOverlay = TrustOverlay emptyUniqMap

{-
Note [Sharing 'UnitInfo's across the 'UnitEnv']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The 'UnitState' and 'UnitIndex' are closely related.

The 'UnitState' stores all information about the external units referenced by
a single 'HomeUnitEnv'.
As a reminder, the 'HomeUnitEnv' stores all information specific to a single home unit,
such as the 'HomePackageTable', 'DynFlags' and the 'UnitState'.
The 'UnitState' retains the 'unitInfoMap', an in-memory representation of
the unit databases that a 'HomeUnitEnv' depends on.
Multiple home units can depend on the same unit database, and reference the same
'UnitInfo's across the GHC session.
We share all 'UnitInfo's across multiple 'HomeUnitEnv's, saving a lot of
duplication of the same 'UnitInfo'.
This what the 'UnitIndex' takes care of.

The 'UnitIndex' stores all fully-resolved 'UnitInfo's that can be referenced
by the 'UnitState'.'unitInfoMap'.
We consider a 'UnitInfo' as fully-resolved, if its dependencies were updated to reference the
wired-in units (e.g., 'wiringMap') and the wired-in units are updated as well.
See Note [Wired-in units] for more details on wired-in units.
Further, the 'UnitInfo' is based on the 'ExternalUnitDatabases' results, resolving
variables such as @${pkgroot}@ in paths.

As such, we can consider the 'UnitIndex' to be global data that is referenced by
the 'UnitState' for better sharing of 'UnitInfo's.

In fact, using the 'UnitIndex', we can impose a hard upper bound on the number
of live 'UnitInfo's in a GHC session:

> For each on-disk 'GenericUnitInfo', there are at most two objects alive.

One instance is stored in 'ExternalUnitDatabases' where variables are resolved,
but the wired-in units haven't been resolved.

The second instance is the fully-resolved 'UnitInfo' stored in the 'UnitIndex'.

See the module documentation for 'GHC.Unit.External.Index' for an overview
of how the types relate to each other.
-}

-- | The 'UnitState' contains a plethora of information local to a single 'HomeUnitEnv'.
--
-- It stores module visibilities, unit trust for @SafeHaskell@, available units for error messages,
-- explicit unit dependencies and knows how to instantiate backpack signature and holes
-- on demand.
--
-- A 'HomeUnitEnv' should rarely/never have to look into the 'UnitIndex', all external
-- unit related information is stored in the 'UnitState'.
--
data UnitState = UnitState {
  -- | A mapping of 'Unit' to 'UnitInfo'.  This list is adjusted
  -- so that only valid units are here.  'UnitInfo' reflects
  -- what was stored *on disk*, except for the 'trusted' flag, which
  -- is adjusted at runtime.  (In particular, some units in this map
  -- may have the 'exposed' flag be 'False'.)
  --
  -- All values are shared with 'UnitIndex'.'globalUnits'.
  -- See Note [Sharing 'UnitInfo's across the 'UnitEnv'] for details.
  unitInfoMap :: UnitInfoMap,

  -- | Set of units that we trust.
  --
  -- Local overlay of 'UnitInfo'.
  -- This avoids modifying the 'UnitInfo' directly, potentially saving
  -- a lot of duplication.
  --
  -- We keep this in WHNF as it is relatively cheap but could easily retain
  -- references to bigger structures.
  trustedUnits :: !TrustOverlay,

  -- | A mapping of 'PackageName' to 'UnitId'. If several units have the same
  -- package name (e.g. different instantiations), then we return one of them...
  -- This is used when users refer to packages in Backpack includes.
  -- And also to resolve package qualifiers with the PackageImports extension.
  packageNameMap            :: UniqFM PackageName UnitId,

  -- | The units we're going to link in eagerly.  This list
  -- should be in reverse dependency order; that is, a unit
  -- is always mentioned before the units it depends on.
  preloadUnits      :: [UnitId],

  -- | Units which we explicitly depend on (from a command line flag).
  -- We'll use this to generate version macros and the unused packages warning. The
  -- original flag which was used to bring the unit into scope is recorded for the
  -- -Wunused-packages warning.
  explicitUnits :: [(Unit, Maybe PackageArg)],

  homeUnitDepends    :: Set UnitId,

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
  requirementContext :: UniqMap ModuleName [InstantiatedModule],

  -- | Indicate if we can instantiate units on-the-fly.
  --
  -- This should only be true when we are type-checking an indefinite unit.
  -- See Note [About units] in GHC.Unit.
  allowVirtualUnits :: !Bool
  }

emptyUnitState :: UnitState
emptyUnitState = UnitState {
    unitInfoMap    = emptyUniqMap,
    trustedUnits   = emptyTrustOverlay,
    packageNameMap = emptyUFM,
    preloadUnits   = [],
    explicitUnits  = [],
    homeUnitDepends = Set.empty,
    moduleNameProvidersMap       = emptyUniqMap,
    pluginModuleNameProvidersMap = emptyUniqMap,
    requirementContext           = emptyUniqMap,
    allowVirtualUnits = False
    }

-- | Find the unit we know about with the given unit, if any
lookupUnit :: UnitState -> Unit -> Maybe UnitInfo
lookupUnit pkgs = lookupUnit' (allowVirtualUnits pkgs) (unitInfoMap pkgs)

-- | Find the unit we know about with the given unit id, if any
lookupUnitId :: UnitState -> UnitId -> Maybe UnitInfo
lookupUnitId state uid = lookupUnitId' (unitInfoMap state) uid

-- | Looks up the given unit in the unit state, panicking if it is not found
unsafeLookupUnit :: HasDebugCallStack => UnitState -> Unit -> UnitInfo
unsafeLookupUnit state u = case lookupUnit state u of
   Just info -> info
   Nothing   -> pprPanic "unsafeLookupUnit" (ppr u)

-- | Looks up the given unit id in the unit state, panicking if it is not found
unsafeLookupUnitId :: HasDebugCallStack => UnitState -> UnitId -> UnitInfo
unsafeLookupUnitId state uid = case lookupUnitId state uid of
   Just info -> info
   Nothing   -> pprPanic "unsafeLookupUnitId" (ppr uid)


-- | Find the unit we know about with the given package name (e.g. @foo@), if any
-- (NB: there might be a locally defined unit name which overrides this)
-- This function is unsafe to use in general because it doesn't respect package
-- visibility.
lookupPackageName :: UnitState -> PackageName -> Maybe UnitId
lookupPackageName pkgstate n = lookupUFM (packageNameMap pkgstate) n

-- | Search for units with a given package ID (e.g. \"foo-0.1\")
searchPackageId :: UnitState -> PackageId -> [UnitInfo]
searchPackageId pkgstate pid = filter ((pid ==) . unitPackageId)
                               (listUnitInfo pkgstate)

-- | Find the UnitId which an import qualified by a package import comes from.
-- Compared to 'lookupPackageName', this function correctly accounts for visibility,
-- renaming and thinning.
resolvePackageImport :: UnitState -> ModuleName -> PackageName -> Maybe UnitId
resolvePackageImport unit_st mn pn = do
  -- 1. Find all modules providing the ModuleName (this accounts for visibility/thinning etc)
  providers <- filterUniqMap originVisible <$> lookupUniqMap (moduleNameProvidersMap unit_st) mn
  -- 2. Get the UnitIds of the candidates
  let candidates_uid = concatMap to_uid $ sortOn fst $ nonDetUniqMapToList providers
  -- 3. Get the package names of the candidates
  let candidates_units = map (\ui -> ((unitPackageName ui), unitId ui))
                              $ mapMaybe (\uid -> lookupUniqMap (unitInfoMap unit_st) uid) candidates_uid
  -- 4. Check to see if the PackageName helps us disambiguate any candidates.
  lookup pn candidates_units

  where

    -- Get the UnitId from which a visible identifier is from
    to_uid :: (Module, ModuleOrigin) -> [UnitId]
    to_uid (mod, ModOrigin mo re_exps _ _) =
      case mo of
        -- Available directly, but also potentially from re-exports
        Just True ->  (toUnitId (moduleUnit mod)) : map unitId re_exps
        -- Just available from these re-exports
        _ -> map unitId re_exps
    to_uid _ = []

-- | Get a list of entries from the unit database.  NB: be careful with
-- this function, although all units in this map are "visible", this
-- does not imply that the exposed-modules of the unit are available
-- (they may have been thinned or renamed).
listUnitInfo :: UnitState -> [UnitInfo]
listUnitInfo state = nonDetEltsUniqMap (unitInfoMap state)

-- | Do we trust the 'UnitInfo' for the given 'UnitId'?
isUnitIdTrusted :: HasDebugCallStack => UnitState -> UnitId -> Bool
isUnitIdTrusted ue u =
    case lookupTrustOverlay (trustedUnits ue) u of
      Just Trusted -> True
      Just Distrusted -> False
      Nothing -> unitIsTrusted (unsafeLookupUnitId ue u) -- No overlay for this unit, check the on-disk value

-- | Do we trust the 'UnitInfo' for the given 'Unit'?
isUnitTrusted :: HasDebugCallStack => UnitState -> Unit -> Bool
isUnitTrusted ue u =
  isUnitIdTrusted ue (toUnitId u)

-- | Do we trust the given 'UnitInfo'?
isUnitInfoTrusted :: HasDebugCallStack => UnitState -> UnitInfo -> Bool
isUnitInfoTrusted ue u =
  isUnitIdTrusted ue (unitId u)

-- ----------------------------------------------------------------------------
-- Loading the unit db files and building up the unit state

-- | Read the unit database files, and sets up various internal tables of
-- unit information, according to the unit-related flags on the
-- command-line (@-package@, @-hide-package@ etc.)
--
-- 'initUnits' can be called again subsequently after updating the
-- 'packageFlags' and 'packageDBFlags' fields of the 'DynFlags', and it will
-- update the 'unitState' in 'DynFlags'.
--
-- Also, see Note [Sharing 'UnitInfo's across the 'UnitEnv'] for implementation details.
initUnits :: Logger -> DynFlags -> UnitIndexCache -> Set.Set UnitId -> IO (UnitState, HomeUnit, Maybe PlatformConstants)
initUnits logger dflags unit_index home_units = do

  let forceUnitInfoMap state = unitInfoMap state `seq` ()

  unit_state <- withTiming logger (text "initializing unit database")
                   forceUnitInfoMap
                 $ mkUnitState logger unit_index (initUnitConfig dflags  home_units)

  putDumpFileMaybe logger Opt_D_dump_mod_map "Module Map"
    FormatText (updSDocContext (\ctx -> ctx {sdocLineLength = 200})
                $ pprModuleMap (moduleNameProvidersMap unit_state))

  wireMap <- wiringMap <$> readUnitIndex unit_index

  let home_unit = mkHomeUnit wireMap
                             (homeUnitId_ dflags)
                             (homeUnitInstanceOf_ dflags)
                             (homeUnitInstantiations_ dflags)

  -- Try to find platform constants
  --
  -- See Note [Platform constants] in GHC.Platform
  mconstants <- if homeUnitId_ dflags == rtsUnitId
    then do
      -- we're building the RTS! Lookup DerivedConstants.h in the include paths
      lookupPlatformConstants (includePathsGlobal (includePaths dflags))
    else
      -- lookup the DerivedConstants.h header bundled with the RTS unit. We
      -- don't fail if we can't find the RTS unit as it can be a valid (but
      -- uncommon) case, e.g. building a C utility program (not depending on the
      -- RTS) before building the RTS. In any case, we will fail later on if we
      -- really need to use the platform constants but they have not been loaded.
      case lookupUnitId unit_state rtsUnitId of
        Nothing   -> return Nothing
        Just info -> lookupPlatformConstants (fmap ST.unpack (unitIncludeDirs info))

  return (unit_state,home_unit,mconstants)

mkHomeUnit
    :: WireMap
    -> UnitId                 -- ^ Home unit id
    -> Maybe UnitId           -- ^ Home unit instance of
    -> [(ModuleName, Module)] -- ^ Home unit instantiations
    -> HomeUnit
mkHomeUnit wmap hu_id hu_instanceof hu_instantiations_ =
    let
        -- Some wired units can be used to instantiate the home unit. We need to
        -- replace their unit keys with their wired unit ids.
        hu_instantiations = map (fmap (updateWiredInUnitIdInModule wmap)) hu_instantiations_
    in case (hu_instanceof, hu_instantiations) of
      (Nothing,[]) -> DefiniteHomeUnit hu_id Nothing
      (Nothing, _) -> throwGhcException $ CmdLineError ("Use of -instantiated-with requires -this-component-id")
      (Just _, []) -> throwGhcException $ CmdLineError ("Use of -this-component-id requires -instantiated-with")
      (Just u, is)
         -- detect fully indefinite units: all their instantiations are hole
         -- modules and the home unit id is the same as the instantiating unit
         -- id (see Note [About units] in GHC.Unit)
         | all (isHoleModule . snd) is && u == hu_id
         -> IndefiniteHomeUnit u is
         -- otherwise it must be that we (fully) instantiate an indefinite unit
         -- to make it definite.
         -- TODO: error when the unit is partially instantiated??
         | otherwise
         -> DefiniteHomeUnit hu_id (Just (u, is))

-- -----------------------------------------------------------------------------
-- Modify our copy of the unit database based on trust flags,
-- -trust and -distrust.

applyTrustFlag
   :: UnitPrecedenceMap
   -> UnusableUnits
   -> [UnitInfo]
   -> TrustOverlay
   -> TrustFlag
   -> MaybeErr UnitErr TrustOverlay
applyTrustFlag prec_map unusable pkgs overlay flag =
  case flag of
    -- we trust all matching packages. Maybe should only trust first one?
    -- and leave others the same or set them untrusted
    TrustPackage str ->
       case selectPackages prec_map (PackageArg str) pkgs unusable of
         Left ps       -> Failed (TrustFlagErr flag ps)
         Right (ps,_) -> Succeeded (trustUnits (map unitId ps) overlay)

    DistrustPackage str ->
       case selectPackages prec_map (PackageArg str) pkgs unusable of
         Left ps       -> Failed (TrustFlagErr flag ps)
         Right (ps,_) -> Succeeded (distrustUnits (map unitId ps) overlay)

applyPackageFlag
   :: UnitPrecedenceMap
   -> UnitInfoMap
   -> UnusableUnits
   -> Bool -- if False, if you expose a package, it implicitly hides
           -- any previously exposed packages with the same name
   -> [UnitInfo]
   -> VisibilityMap           -- Initially exposed
   -> PackageFlag             -- flag to apply
   -> MaybeErr UnitErr VisibilityMap -- Now exposed

applyPackageFlag prec_map pkg_map unusable no_hide_others pkgs vm flag =
  case flag of
    ExposePackage _ arg (ModRenaming b rns) ->
       case findPackages prec_map pkg_map arg pkgs unusable of
         Left ps     -> Failed (PackageFlagErr flag ps)
         Right (p:_) -> Succeeded vm'
          where
           n = fsPackageName p

           -- If a user says @-unit-id p[A=<A>]@, this imposes
           -- a requirement on us: whatever our signature A is,
           -- it must fulfill all of p[A=<A>]:A's requirements.
           -- This method is responsible for computing what our
           -- inherited requirements are.
           reqs | UnitIdArg orig_uid <- arg = collectHoles orig_uid
                | otherwise                 = emptyUniqMap

           collectHoles uid = case uid of
             HoleUnit       -> emptyUniqMap
             RealUnit {}    -> emptyUniqMap -- definite units don't have holes
             VirtUnit indef ->
                  let local = [ unitUniqMap
                                  (moduleName mod)
                                  (Set.singleton $ Module indef mod_name)
                              | (mod_name, mod) <- instUnitInsts indef
                              , isHoleModule mod ]
                      recurse = [ collectHoles (moduleUnit mod)
                                | (_, mod) <- instUnitInsts indef ]
                  in plusUniqMapListWith Set.union $ local ++ recurse

           uv = UnitVisibility
                { uv_expose_all = b
                , uv_renamings = rns
                , uv_package_name = First (Just n)
                , uv_requirements = reqs
                , uv_explicit = Just arg
                }
           vm' = addToUniqMap_C mappend vm_cleared (mkUnit p) uv
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
                      | otherwise = filterWithKeyUniqMap
                            (\k uv -> k == mkUnit p
                                   || First (Just n) /= uv_package_name uv) vm
         _ -> panic "applyPackageFlag"

    HidePackage str ->
       case findPackages prec_map pkg_map (PackageArg str) pkgs unusable of
         Left ps  -> Failed (PackageFlagErr flag ps)
         Right ps -> Succeeded $ foldl' delFromUniqMap vm (map mkUnit ps)

updateVisibilityMap :: WireMap -> VisibilityMap -> VisibilityMap
updateVisibilityMap wiredInMap vis_map = foldl' f vis_map (listWireMap wiredInMap)
  where f vm (from, to) = case lookupUniqMap vis_map (RealUnit (Definite from)) of
                    Nothing -> vm
                    Just r -> addToUniqMap (delFromUniqMap vm (RealUnit (Definite from)))
                              (RealUnit (Definite to)) r

  -- ----------------------------------------------------------------------------

reportCycles :: Logger -> [SCC UnitInfo] -> IO ()
reportCycles logger sccs = when (logVerbAtLeast logger 2) $ mapM_ report sccs
  where
    report (AcyclicSCC _) = return ()
    report (CyclicSCC vs) =
        debugTraceMsg logger 2 $
          text "these packages are involved in a cycle:" $$
            nest 2 (hsep (map (ppr . unitId) vs))


-- -----------------------------------------------------------------------------
-- When all the command-line options are in, we can process our unit
-- settings and populate the unit state.

mkUnitState
    :: Logger
    -> UnitIndexCache
    -> UnitConfig
    -> IO UnitState
mkUnitState logger unit_index cfg = do
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

  dbs <- readUnitDatabases logger unit_index (initUnitDbConfig cfg)

  -- distrust all units if the flag is set
  let distrustUnitsOfDb overlay db = foldl' (\ acc ui -> distrustUnits [unitId ui] acc) overlay (unitDatabaseUnits db)
      distrustAllUnits = foldl' distrustUnitsOfDb emptyTrustOverlay dbs

      distrustedUnitsOverlay
        | unitConfigDistrustAll cfg = distrustAllUnits
        | otherwise = emptyTrustOverlay

  -- This, and the other reverse's that you will see, are due to the fact that
  -- packageFlags, pluginPackageFlags, etc. are all specified in *reverse* order
  -- than they are on the command line.
  let raw_other_flags = reverse (unitConfigFlagsExposed cfg)
      (hpt_flags, other_flags) = partition (selectHptFlag (unitConfigHomeUnits cfg)) raw_other_flags
  debugTraceMsg logger 2 $
      text "package flags" <+> ppr other_flags

  let home_unit_deps = selectHomeUnits (unitConfigHomeUnits cfg) hpt_flags

  -- Merge databases together, without checking validity
  (pkg_map1, prec_map) <- mergeDatabases logger dbs

  -- Now that we've merged everything together, prune out unusable
  -- packages.
  let (pkg_map2, unusable, sccs) = validateDatabase (unitConfigFlagsIgnored cfg) pkg_map1

  reportCycles   logger sccs
  reportUnusable logger unusable

  -- Compute trust flags (these flags apply regardless of whether
  -- or not packages are visible or not)
  trustUnitsOverlay <- mayThrowUnitErr
            $ foldM (applyTrustFlag prec_map unusable (nonDetEltsUniqMap pkg_map2))
                 distrustedUnitsOverlay (reverse (unitConfigFlagsTrusted cfg))
  let pkgs1 = nonDetEltsUniqMap pkg_map2
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
                               then addToUniqMap vm (mkUnit p)
                                               UnitVisibility {
                                                 uv_expose_all = True,
                                                 uv_renamings = [],
                                                 uv_package_name = First (Just $ fsPackageName p),
                                                 uv_requirements = emptyUniqMap,
                                                 uv_explicit = Nothing
                                               }
                               else vm)
                         emptyUniqMap pkgs1

  --
  -- Compute a visibility map according to the command-line flags (-package,
  -- -hide-package).  This needs to know about the unusable packages, since if a
  -- user tries to enable an unusable package, we should let them know.
  --
  vis_map2 <- mayThrowUnitErr
                $ foldM (applyPackageFlag prec_map prelim_pkg_db unusable
                        (unitConfigHideAll cfg) pkgs1)
                            vis_map1 other_flags

  --
  -- Sort out which packages are wired in. This has to be done last, since
  -- it modifies the unit ids of wired in packages, but when we process
  -- package arguments we need to key against the old versions.
  --
  wired_map <- setupWiredInUnits logger prec_map pkgs1 vis_map2 unit_index
  pkgs2 <- updateWiredInUnitIndex wired_map pkgs1 unit_index

  let pkg_db = mkUnitInfoMap pkgs2

  -- Update the visibility map, so we treat wired packages as visible.
  let vis_map = updateVisibilityMap wired_map vis_map2

  let hide_plugin_pkgs = unitConfigHideAllPlugins cfg
  plugin_vis_map <-
    case unitConfigFlagsPlugins cfg of
        -- common case; try to share the old vis_map
        [] | not hide_plugin_pkgs -> return vis_map
           | otherwise -> return emptyUniqMap
        _ -> do let plugin_vis_map1
                        | hide_plugin_pkgs = emptyUniqMap
                        -- Use the vis_map PRIOR to wired in,
                        -- because otherwise applyPackageFlag
                        -- won't work.
                        | otherwise = vis_map2
                plugin_vis_map2
                    <- mayThrowUnitErr
                        $ foldM (applyPackageFlag prec_map prelim_pkg_db unusable
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

  let pkgname_map = listToUFM [ (unitPackageName p, unitInstanceOf p)
                              | p <- pkgs2
                              ]
  -- The explicitUnits accurately reflects the set of units we have turned
  -- on; as such, it also is the only way one can come up with requirements.
  -- The requirement context is directly based off of this: we simply
  -- look for nested unit IDs that are directly fed holes: the requirements
  -- of those units are precisely the ones we need to track
  let explicit_pkgs = [(k, uv_explicit v) | (k, v) <- nonDetUniqMapToList vis_map]
      req_ctx = mapUniqMap (Set.toList)
              $ plusUniqMapListWith Set.union (map uv_requirements (nonDetEltsUniqMap vis_map))


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
  -- NB: Sorting keys here to ensure a deterministic order for the linker.
  --
  let preload1 = sort $ nonDetKeysUniqMap (filterUniqMap (isJust . uv_explicit) vis_map)

      -- add default preload units if they can be found in the db
      basicLinkedUnits = fmap (RealUnit . Definite)
                         $ filter (flip elemUniqMap pkg_db)
                         $ unitConfigAutoLink cfg
      preload3 = nubOrd $ (basicLinkedUnits ++ preload1)

  -- Close the preload packages with their dependencies
  dep_preload <- mayThrowUnitErr
                    $ closeUnitDeps pkg_db
                    $ zip (map toUnitId preload3) (repeat Nothing)

  let mod_map1 = mkModuleNameProvidersMap logger (unitConfigAllowVirtual cfg) pkg_db vis_map
      mod_map2 = mkUnusableModuleNameProvidersMap unusable
      mod_map = mod_map2 `plusUniqMap` mod_map1

  -- Force the result to avoid leaking input parameters
  let !state = UnitState
         { preloadUnits                 = dep_preload
         , explicitUnits                = explicit_pkgs
         , homeUnitDepends              = home_unit_deps
         , unitInfoMap                  = pkg_db
         , trustedUnits                 = trustUnitsOverlay
         , moduleNameProvidersMap       = mod_map
         , pluginModuleNameProvidersMap = mkModuleNameProvidersMap logger (unitConfigAllowVirtual cfg) pkg_db plugin_vis_map
         , packageNameMap               = pkgname_map
         , requirementContext           = req_ctx
         , allowVirtualUnits            = unitConfigAllowVirtual cfg
         }
  return state

selectHptFlag :: Set.Set UnitId -> PackageFlag -> Bool
selectHptFlag home_units (ExposePackage _ (UnitIdArg uid) _) | toUnitId uid `Set.member` home_units = True
selectHptFlag _ _ = False

selectHomeUnits :: Set.Set UnitId -> [PackageFlag] -> Set.Set UnitId
selectHomeUnits home_units flags = foldl' go Set.empty flags
  where
    go :: Set.Set UnitId -> PackageFlag -> Set.Set UnitId
    go cur (ExposePackage _ (UnitIdArg uid) _) | toUnitId uid `Set.member` home_units = Set.insert (toUnitId uid) cur
    -- MP: This does not yet support thinning/renaming
    go cur _ = cur

initUnitDbConfig :: UnitConfig -> UnitDbConfig
initUnitDbConfig uc = UnitDbConfig
  { unitDbConfigFlagsDB = unitConfigFlagsDB uc
  , unitDbConfigProgramName = unitConfigProgramName uc
  , unitDbConfigDBName = unitConfigDBName uc
  , unitDbConfigPlatformArchOS = unitConfigPlatformArchOS uc
  , unitDbConfigGlobalDB = unitConfigGlobalDB uc
  , unitDbConfigGHCDir = unitConfigGHCDir uc
  }

-- -----------------------------------------------------------------------------
-- Package Utils

-- | Takes a 'ModuleName', and if the module is in any package returns
-- list of modules which take that name.
lookupModuleInAllUnits :: UnitState
                          -> ModuleName
                          -> [(Module, UnitInfo)]
lookupModuleInAllUnits pkgs m
  = case lookupModuleWithSuggestions pkgs m NoPkgQual of
      LookupFound a b -> [(a,fst b)]
      LookupMultiple rs -> map f rs
        where f (m,_) = (m, expectJust (lookupUnit pkgs (moduleUnit m)))
      _ -> []

-- | The result of performing a lookup
data LookupResult =
    -- | Found the module uniquely, nothing else to do
    LookupFound Module (UnitInfo, ModuleOrigin)
    -- | Multiple modules with the same name in scope
  | LookupMultiple [(Module, ModuleOrigin)]
    -- | No modules found, but there were some hidden ones with
    -- an exact name match.  First is due to package hidden, second
    -- is due to module being hidden
  | LookupHidden [UnitInfo] [(Module, ModuleOrigin)]
    -- | No modules found, but there were some unusable ones with
    -- an exact name match
  | LookupUnusable [(Module, ModuleOrigin)]
    -- | Nothing found, here are some suggested different names
  | LookupNotFound [ModuleSuggestion] -- suggestions

data ModuleSuggestion = SuggestVisible ModuleName Module ModuleOrigin
                      | SuggestHidden ModuleName Module ModuleOrigin

lookupModuleWithSuggestions :: UnitState
                            -> ModuleName
                            -> PkgQual
                            -> LookupResult
lookupModuleWithSuggestions pkgs
  = lookupModuleWithSuggestions' pkgs (moduleNameProvidersMap pkgs)

-- | The package which the module **appears** to come from, this could be
-- the one which reexports the module from it's original package. This function
-- is currently only used for -Wunused-packages
lookupModulePackage :: UnitState -> ModuleName -> PkgQual -> Maybe [UnitInfo]
lookupModulePackage pkgs mn mfs =
    case lookupModuleWithSuggestions' pkgs (moduleNameProvidersMap pkgs) mn mfs of
      LookupFound _ (orig_unit, origin) ->
        case origin of
          ModOrigin {fromOrigUnit, fromExposedReexport} ->
            case fromOrigUnit of
              -- Just True means, the import is available from its original location
              Just True ->
                pure [orig_unit]
              -- Otherwise, it must be available from a reexport
              _ -> pure fromExposedReexport

          _ -> Nothing

      _ -> Nothing

lookupPluginModuleWithSuggestions :: UnitState
                                  -> ModuleName
                                  -> PkgQual
                                  -> LookupResult
lookupPluginModuleWithSuggestions pkgs
  = lookupModuleWithSuggestions' pkgs (pluginModuleNameProvidersMap pkgs)

lookupModuleWithSuggestions' :: UnitState
                            -> ModuleNameProvidersMap
                            -> ModuleName
                            -> PkgQual
                            -> LookupResult
lookupModuleWithSuggestions' pkgs mod_map name mb_pn
  = case lookupUniqMap mod_map name of
        Nothing -> LookupNotFound suggestions
        Just xs ->
          case foldl' classify ([],[],[], []) (sortOn fst $ nonDetUniqMapToList xs) of
            ([], [], [], []) -> LookupNotFound suggestions
            (_, _, _, [(m, o)])             -> LookupFound m (mod_unit m, o)
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
          ModOrigin { fromOrigUnit = origAvailableUnderSameName, fromHiddenReexport }
            | originEmpty origin
            -> (hidden_pkg,   hidden_mod, unusable, exposed)
            | originVisible origin
            -> (hidden_pkg, hidden_mod, unusable, x:exposed)
            | otherwise
            -> (reexports ++ maybe id (:) origUnit hidden_pkg, hidden_mod, unusable, exposed)
            where
              reexports :: [UnitInfo]
              reexports = sortOn unitId fromHiddenReexport

              origUnit :: Maybe UnitInfo
              origUnit = origAvailableUnderSameName >> lookupUnit pkgs (moduleUnit m)

    unit_lookup p = lookupUnit pkgs p `orElse` pprPanic "lookupModuleWithSuggestions" (ppr p <+> ppr name)
    mod_unit = unit_lookup . moduleUnit

    -- Filters out origins which are not associated with the given package
    -- qualifier.  No-op if there is no package qualifier.  Test if this
    -- excluded all origins with 'originEmpty'.
    filterOrigin :: PkgQual
                 -> UnitInfo
                 -> ModuleOrigin
                 -> ModuleOrigin
    filterOrigin NoPkgQual _ o = o
    filterOrigin (ThisPkg _) _ o = o
    filterOrigin (OtherPkg u) pkg o =
      let match_pkg p = u == unitId p
      in case o of
          ModHidden
            | match_pkg pkg -> ModHidden
            | otherwise     -> mempty
          ModUnusable _
            | match_pkg pkg -> o
            | otherwise     -> mempty
          ModOrigin { fromOrigUnit = e, fromExposedReexport = res,
                      fromHiddenReexport = rhs }
            -> ModOrigin
                { fromOrigUnit        = if match_pkg pkg then e else Nothing
                , fromExposedReexport = filter match_pkg res
                , fromHiddenReexport  = filter match_pkg rhs
                , fromPackageFlag     = False -- always excluded
                }

    suggestions = fuzzyLookup (moduleNameString name) all_mods

    all_mods :: [(String, ModuleSuggestion)]     -- All modules
    all_mods = sortOn fst $
        [ (moduleNameString m, suggestion)
        | (m, e) <- nonDetUniqMapToList (moduleNameProvidersMap pkgs)
        , suggestion <- map (getSuggestion m) (nonDetUniqMapToList e)
        ]
    getSuggestion name (mod, origin) =
        (if originVisible origin then SuggestVisible else SuggestHidden)
            name mod origin

listVisibleModuleNames :: UnitState -> [ModuleName]
listVisibleModuleNames state =
    map fst (filter visible (nonDetUniqMapToList (moduleNameProvidersMap state)))
  where visible (_, ms) = anyUniqMap originVisible ms



-- | Return this list of requirement interfaces that need to be merged
-- to form @mod_name@, or @[]@ if this is not a requirement.
requirementMerges :: UnitState -> ModuleName -> [InstantiatedModule]
requirementMerges pkgstate mod_name =
  fromMaybe [] (lookupUniqMap (requirementContext pkgstate) mod_name)

-- -----------------------------------------------------------------------------

-- | Pretty-print a UnitId for the user.
--
-- Cabal packages may contain several components (programs, libraries, etc.).
-- As far as GHC is concerned, installed package components ("units") are
-- identified by an opaque UnitId string provided by Cabal. As the string
-- contains a hash, we don't want to display it to users so GHC queries the
-- database to retrieve some infos about the original source package (name,
-- version, component name).
--
-- Instead we want to display: packagename-version[:componentname]
--
-- Component name is only displayed if it isn't the default library
--
-- To do this we need to query a unit database.
pprUnitIdForUser :: UnitState -> UnitId -> SDoc
pprUnitIdForUser state uid@(UnitId fs) =
   case lookupUnitPprInfo state uid of
      Nothing -> ftext fs -- we didn't find the unit at all
      Just i  -> ppr i

pprUnitInfoForUser :: UnitInfo -> SDoc
pprUnitInfoForUser info = ppr (mkUnitPprInfo unitIdFS info)

lookupUnitPprInfo :: UnitState -> UnitId -> Maybe UnitPprInfo
lookupUnitPprInfo state uid = fmap (mkUnitPprInfo unitIdFS) (lookupUnitId state uid)

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
pprUnitsSimple ue = pprUnitsWith pprIPI ue
    where pprIPI ipi = let i = unitIdFS (unitId ipi)
                           e = if unitIsExposed ipi then text "E" else text " "
                           t = if isUnitInfoTrusted ue ipi then text "T" else text " "
                       in e <> t <> text "  " <> ftext i

-- | Print unit-ids with UnitInfo found in the given UnitState
pprWithUnitState :: UnitState -> SDoc -> SDoc
pprWithUnitState state = updSDocContext (\ctx -> ctx
   { sdocUnitIdForUser = \fs -> pprUnitIdForUser state (UnitId fs)
   })

-- | Print raw unit-ids, without removing the hash
pprRawUnitIds :: SDoc -> SDoc
pprRawUnitIds = updSDocContext (\ctx -> ctx { sdocUnitIdForUser = ftext })

fsPackageName :: UnitInfo -> FastString
fsPackageName info = fs
   where
      PackageName fs = unitPackageName info

-- -----------------------------------------------------------------------------
-- Module renaming

-- | Substitutes holes in a 'Module'.  NOT suitable for being called
-- directly on a 'nameModule', see Note [Representation of module/name variables].
-- @p[A=\<A>]:B@ maps to @p[A=q():A]:B@ with @A=q():A@;
-- similarly, @\<A>@ maps to @q():A@.
renameHoleModule :: UnitState -> ShHoleSubst -> Module -> Module
renameHoleModule state = renameHoleModule' (unitInfoMap state)

-- | Substitutes holes in a 'Unit', suitable for renaming when
-- an include occurs; see Note [Representation of module/name variables].
--
-- @p[A=\<A>]@ maps to @p[A=\<B>]@ with @A=\<B>@.
renameHoleUnit :: UnitState -> ShHoleSubst -> Unit -> Unit
renameHoleUnit state = renameHoleUnit' (unitInfoMap state)

-- | Injects an 'InstantiatedModule' to 'Module' (see also
-- 'instUnitToUnit'.
instModuleToModule :: InstantiatedModule -> Module
instModuleToModule (Module iuid mod_name) =
    mkModule (instUnitToUnit iuid) mod_name

-- | Return a `UnitId` which either wraps the `InstantiatedUnit` unchanged.
instUnitToUnit :: InstantiatedUnit -> Unit
instUnitToUnit iuid =
    -- NB: suppose that we want to compare the instantiated
    -- unit p[H=impl:H] against p+abcd (where p+abcd
    -- happens to be the existing, installed version of
    -- p[H=impl:H].  If we *only* wrap in p[H=impl:H]
    -- VirtUnit, they won't compare equal; only
    -- after improvement will the equality hold.
    VirtUnit iuid
