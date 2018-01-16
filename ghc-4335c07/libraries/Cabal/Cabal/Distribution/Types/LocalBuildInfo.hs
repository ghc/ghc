{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Types.LocalBuildInfo (
    -- * The type

    LocalBuildInfo(..),

    -- * Convenience accessors

    localComponentId,
    localUnitId,
    localCompatPackageKey,
    localPackage,

    -- * Build targets of the 'LocalBuildInfo'.

    componentNameCLBIs,

    -- NB: the primes mean that they take a 'PackageDescription'
    -- which may not match 'localPkgDescr' in 'LocalBuildInfo'.
    -- More logical types would drop this argument, but
    -- at the moment, this is the ONLY supported function, because
    -- 'localPkgDescr' is not guaranteed to match.  At some point
    -- we will fix it and then we can use the (free) unprimed
    -- namespace for the correct commands.
    --
    -- See https://github.com/haskell/cabal/issues/3606 for more
    -- details.

    componentNameTargets',
    unitIdTarget',
    allTargetsInBuildOrder',
    withAllTargetsInBuildOrder',
    neededTargetsInBuildOrder',
    withNeededTargetsInBuildOrder',
    testCoverage,

    -- * Functions you SHOULD NOT USE (yet), but are defined here to
    -- prevent someone from accidentally defining them

    componentNameTargets,
    unitIdTarget,
    allTargetsInBuildOrder,
    withAllTargetsInBuildOrder,
    neededTargetsInBuildOrder,
    withNeededTargetsInBuildOrder,

    -- * Backwards compatibility.

    componentsConfigs,
    externalPackageDeps,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.PackageDescription
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.ComponentId
import Distribution.Types.MungedPackageId
import Distribution.Types.PackageId
import Distribution.Types.UnitId
import Distribution.Types.TargetInfo

import Distribution.Simple.InstallDirs hiding (absoluteInstallDirs,
                                               prefixRelativeInstallDirs,
                                               substPathTemplate, )
import Distribution.Simple.Program
import Distribution.PackageDescription
import Distribution.Simple.Compiler
import Distribution.Simple.PackageIndex
import Distribution.Simple.Setup
import Distribution.Text
import Distribution.System

import Distribution.Compat.Graph (Graph)
import qualified Distribution.Compat.Graph as Graph
import qualified Data.Map as Map

-- | Data cached after configuration step.  See also
-- 'Distribution.Simple.Setup.ConfigFlags'.
data LocalBuildInfo = LocalBuildInfo {
        configFlags   :: ConfigFlags,
        -- ^ Options passed to the configuration step.
        -- Needed to re-run configuration when .cabal is out of date
        flagAssignment :: FlagAssignment,
        -- ^ The final set of flags which were picked for this package
        componentEnabledSpec :: ComponentRequestedSpec,
        -- ^ What components were enabled during configuration, and why.
        extraConfigArgs     :: [String],
        -- ^ Extra args on the command line for the configuration step.
        -- Needed to re-run configuration when .cabal is out of date
        installDirTemplates :: InstallDirTemplates,
                -- ^ The installation directories for the various different
                -- kinds of files
        --TODO: inplaceDirTemplates :: InstallDirs FilePath
        compiler      :: Compiler,
                -- ^ The compiler we're building with
        hostPlatform  :: Platform,
                -- ^ The platform we're building for
        buildDir      :: FilePath,
                -- ^ Where to build the package.
        componentGraph :: Graph ComponentLocalBuildInfo,
                -- ^ All the components to build, ordered by topological
                -- sort, and with their INTERNAL dependencies over the
                -- intrapackage dependency graph.
                -- TODO: this is assumed to be short; otherwise we want
                -- some sort of ordered map.
        componentNameMap :: Map ComponentName [ComponentLocalBuildInfo],
                -- ^ A map from component name to all matching
                -- components.  These coincide with 'componentGraph'
        installedPkgs :: InstalledPackageIndex,
                -- ^ All the info about the installed packages that the
                -- current package depends on (directly or indirectly).
                -- The copy saved on disk does NOT include internal
                -- dependencies (because we just don't have enough
                -- information at this point to have an
                -- 'InstalledPackageInfo' for an internal dep), but we
                -- will often update it with the internal dependencies;
                -- see for example 'Distribution.Simple.Build.build'.
                -- (This admonition doesn't apply for per-component builds.)
        pkgDescrFile  :: Maybe FilePath,
                -- ^ the filename containing the .cabal file, if available
        localPkgDescr :: PackageDescription,
                -- ^ WARNING WARNING WARNING Be VERY careful about using
                -- this function; we haven't deprecated it but using it
                -- could introduce subtle bugs related to
                -- 'HookedBuildInfo'.
                --
                -- In principle, this is supposed to contain the
                -- resolved package description, that does not contain
                -- any conditionals.  However, it MAY NOT contain
                -- the description wtih a 'HookedBuildInfo' applied
                -- to it; see 'HookedBuildInfo' for the whole sordid saga.
                -- As much as possible, Cabal library should avoid using
                -- this parameter.
        withPrograms  :: ProgramDb, -- ^Location and args for all programs
        withPackageDB :: PackageDBStack,  -- ^What package database to use, global\/user
        withVanillaLib:: Bool,  -- ^Whether to build normal libs.
        withProfLib   :: Bool,  -- ^Whether to build profiling versions of libs.
        withSharedLib :: Bool,  -- ^Whether to build shared versions of libs.
        withStaticLib :: Bool,  -- ^Whether to build static versions of libs (with all other libs rolled in)
        withDynExe    :: Bool,  -- ^Whether to link executables dynamically
        withProfExe   :: Bool,  -- ^Whether to build executables for profiling.
        withProfLibDetail :: ProfDetailLevel, -- ^Level of automatic profile detail.
        withProfExeDetail :: ProfDetailLevel, -- ^Level of automatic profile detail.
        withOptimization :: OptimisationLevel, -- ^Whether to build with optimization (if available).
        withDebugInfo :: DebugInfoLevel, -- ^Whether to emit debug info (if available).
        withGHCiLib   :: Bool,  -- ^Whether to build libs suitable for use with GHCi.
        splitObjs     :: Bool,  -- ^Use -split-objs with GHC, if available
        stripExes     :: Bool,  -- ^Whether to strip executables during install
        stripLibs     :: Bool,  -- ^Whether to strip libraries during install
        exeCoverage :: Bool,  -- ^Whether to enable executable program coverage
        libCoverage :: Bool,  -- ^Whether to enable library program coverage
        progPrefix    :: PathTemplate, -- ^Prefix to be prepended to installed executables
        progSuffix    :: PathTemplate, -- ^Suffix to be appended to installed executables
        relocatable   :: Bool --  ^Whether to build a relocatable package
  } deriving (Generic, Read, Show)

instance Binary LocalBuildInfo

-------------------------------------------------------------------------------
-- Accessor functions

-- TODO: Get rid of these functions, as much as possible.  They are
-- a bit useful in some cases, but you should be very careful!

-- | Extract the 'ComponentId' from the public library component of a
-- 'LocalBuildInfo' if it exists, or make a fake component ID based
-- on the package ID.
localComponentId :: LocalBuildInfo -> ComponentId
localComponentId lbi =
    case componentNameCLBIs lbi CLibName of
        [LibComponentLocalBuildInfo { componentComponentId = cid }]
          -> cid
        _ -> mkComponentId (display (localPackage lbi))

-- | Extract the 'PackageIdentifier' of a 'LocalBuildInfo'.
-- This is a "safe" use of 'localPkgDescr'
localPackage :: LocalBuildInfo -> PackageId
localPackage lbi = package (localPkgDescr lbi)

-- | Extract the 'UnitId' from the library component of a
-- 'LocalBuildInfo' if it exists, or make a fake unit ID based on
-- the package ID.
localUnitId :: LocalBuildInfo -> UnitId
localUnitId lbi =
    case componentNameCLBIs lbi CLibName of
        [LibComponentLocalBuildInfo { componentUnitId = uid }]
          -> uid
        _ -> mkLegacyUnitId $ localPackage lbi

-- | Extract the compatibility package key from the public library component of a
-- 'LocalBuildInfo' if it exists, or make a fake package key based
-- on the package ID.
localCompatPackageKey :: LocalBuildInfo -> String
localCompatPackageKey lbi =
    case componentNameCLBIs lbi CLibName of
        [LibComponentLocalBuildInfo { componentCompatPackageKey = pk }]
          -> pk
        _ -> display (localPackage lbi)

-- | Convenience function to generate a default 'TargetInfo' from a
-- 'ComponentLocalBuildInfo'.  The idea is to call this once, and then
-- use 'TargetInfo' everywhere else.  Private to this module.
mkTargetInfo :: PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> TargetInfo
mkTargetInfo pkg_descr _lbi clbi =
    TargetInfo {
        targetCLBI = clbi,
        -- NB: @pkg_descr@, not @localPkgDescr lbi@!
        targetComponent = getComponent pkg_descr
                                       (componentLocalName clbi)
     }

-- | Return all 'TargetInfo's associated with 'ComponentName'.
-- In the presence of Backpack there may be more than one!
-- Has a prime because it takes a 'PackageDescription' argument
-- which may disagree with 'localPkgDescr' in 'LocalBuildInfo'.
componentNameTargets' :: PackageDescription -> LocalBuildInfo -> ComponentName -> [TargetInfo]
componentNameTargets' pkg_descr lbi cname =
    case Map.lookup cname (componentNameMap lbi) of
        Just clbis -> map (mkTargetInfo pkg_descr lbi) clbis
        Nothing -> []

unitIdTarget' :: PackageDescription -> LocalBuildInfo -> UnitId -> Maybe TargetInfo
unitIdTarget' pkg_descr lbi uid =
    case Graph.lookup uid (componentGraph lbi) of
        Just clbi -> Just (mkTargetInfo pkg_descr lbi clbi)
        Nothing -> Nothing

-- | Return all 'ComponentLocalBuildInfo's associated with 'ComponentName'.
-- In the presence of Backpack there may be more than one!
componentNameCLBIs :: LocalBuildInfo -> ComponentName -> [ComponentLocalBuildInfo]
componentNameCLBIs lbi cname =
    case Map.lookup cname (componentNameMap lbi) of
        Just clbis -> clbis
        Nothing -> []

-- TODO: Maybe cache topsort (Graph can do this)

-- | Return the list of default 'TargetInfo's associated with a
-- configured package, in the order they need to be built.
-- Has a prime because it takes a 'PackageDescription' argument
-- which may disagree with 'localPkgDescr' in 'LocalBuildInfo'.
allTargetsInBuildOrder' :: PackageDescription -> LocalBuildInfo -> [TargetInfo]
allTargetsInBuildOrder' pkg_descr lbi
    = map (mkTargetInfo pkg_descr lbi) (Graph.revTopSort (componentGraph lbi))

-- | Execute @f@ for every 'TargetInfo' in the package, respecting the
-- build dependency order.  (TODO: We should use Shake!)
-- Has a prime because it takes a 'PackageDescription' argument
-- which may disagree with 'localPkgDescr' in 'LocalBuildInfo'.
withAllTargetsInBuildOrder' :: PackageDescription -> LocalBuildInfo -> (TargetInfo -> IO ()) -> IO ()
withAllTargetsInBuildOrder' pkg_descr lbi f
    = sequence_ [ f target | target <- allTargetsInBuildOrder' pkg_descr lbi ]

-- | Return the list of all targets needed to build the @uids@, in
-- the order they need to be built.
-- Has a prime because it takes a 'PackageDescription' argument
-- which may disagree with 'localPkgDescr' in 'LocalBuildInfo'.
neededTargetsInBuildOrder' :: PackageDescription -> LocalBuildInfo -> [UnitId] -> [TargetInfo]
neededTargetsInBuildOrder' pkg_descr lbi uids =
  case Graph.closure (componentGraph lbi) uids of
    Nothing -> error $ "localBuildPlan: missing uids " ++ intercalate ", " (map display uids)
    Just clos -> map (mkTargetInfo pkg_descr lbi) (Graph.revTopSort (Graph.fromDistinctList clos))

-- | Execute @f@ for every 'TargetInfo' needed to build @uid@s, respecting
-- the build dependency order.
-- Has a prime because it takes a 'PackageDescription' argument
-- which may disagree with 'localPkgDescr' in 'LocalBuildInfo'.
withNeededTargetsInBuildOrder' :: PackageDescription -> LocalBuildInfo -> [UnitId] -> (TargetInfo -> IO ()) -> IO ()
withNeededTargetsInBuildOrder' pkg_descr lbi uids f
    = sequence_ [ f target | target <- neededTargetsInBuildOrder' pkg_descr lbi uids ]

-- | Is coverage enabled for test suites? In practice, this requires library
-- and executable profiling to be enabled.
testCoverage :: LocalBuildInfo -> Bool
testCoverage lbi = exeCoverage lbi && libCoverage lbi

-------------------------------------------------------------------------------
-- Stub functions to prevent someone from accidentally defining them

{-# WARNING componentNameTargets, unitIdTarget, allTargetsInBuildOrder, withAllTargetsInBuildOrder, neededTargetsInBuildOrder, withNeededTargetsInBuildOrder "By using this function, you may be introducing a bug where you retrieve a 'Component' which does not have 'HookedBuildInfo' applied to it.  See the documentation for 'HookedBuildInfo' for an explanation of the issue.  If you have a 'PakcageDescription' handy (NOT from the 'LocalBuildInfo'), try using the primed version of the function, which takes it as an extra argument." #-}

componentNameTargets :: LocalBuildInfo -> ComponentName -> [TargetInfo]
componentNameTargets lbi = componentNameTargets' (localPkgDescr lbi) lbi

unitIdTarget :: LocalBuildInfo -> UnitId -> Maybe TargetInfo
unitIdTarget lbi = unitIdTarget' (localPkgDescr lbi) lbi

allTargetsInBuildOrder :: LocalBuildInfo -> [TargetInfo]
allTargetsInBuildOrder lbi = allTargetsInBuildOrder' (localPkgDescr lbi) lbi

withAllTargetsInBuildOrder :: LocalBuildInfo -> (TargetInfo -> IO ()) -> IO ()
withAllTargetsInBuildOrder lbi = withAllTargetsInBuildOrder' (localPkgDescr lbi) lbi

neededTargetsInBuildOrder :: LocalBuildInfo -> [UnitId] -> [TargetInfo]
neededTargetsInBuildOrder lbi = neededTargetsInBuildOrder' (localPkgDescr lbi) lbi

withNeededTargetsInBuildOrder :: LocalBuildInfo -> [UnitId] -> (TargetInfo -> IO ()) -> IO ()
withNeededTargetsInBuildOrder lbi = withNeededTargetsInBuildOrder' (localPkgDescr lbi) lbi

-------------------------------------------------------------------------------
-- Backwards compatibility

{-# DEPRECATED componentsConfigs "Use 'componentGraph' instead; you can get a list of 'ComponentLocalBuildInfo' with 'Distribution.Compat.Graph.toList'. There's not a good way to get the list of 'ComponentName's the 'ComponentLocalBuildInfo' depends on because this query doesn't make sense; the graph is indexed by 'UnitId' not 'ComponentName'.  Given a 'UnitId' you can lookup the 'ComponentLocalBuildInfo' ('getCLBI') and then get the 'ComponentName' ('componentLocalName]). To be removed in Cabal 2.2" #-}
componentsConfigs :: LocalBuildInfo -> [(ComponentName, ComponentLocalBuildInfo, [ComponentName])]
componentsConfigs lbi =
    [ (componentLocalName clbi,
       clbi,
       mapMaybe (fmap componentLocalName . flip Graph.lookup g)
                (componentInternalDeps clbi))
    | clbi <- Graph.toList g ]
  where
    g = componentGraph lbi

-- | External package dependencies for the package as a whole. This is the
-- union of the individual 'componentPackageDeps', less any internal deps.
{-# DEPRECATED externalPackageDeps "You almost certainly don't want this function, which agglomerates the dependencies of ALL enabled components.  If you're using this to write out information on your dependencies, read off the dependencies directly from the actual component in question.  To be removed in Cabal 2.2" #-}
externalPackageDeps :: LocalBuildInfo -> [(UnitId, MungedPackageId)]
externalPackageDeps lbi =
    -- TODO:  what about non-buildable components?
    nub [ (ipkgid, pkgid)
        | clbi            <- Graph.toList (componentGraph lbi)
        , (ipkgid, pkgid) <- componentPackageDeps clbi
        , not (internal ipkgid) ]
  where
    -- True if this dependency is an internal one (depends on the library
    -- defined in the same package).
    internal ipkgid = any ((==ipkgid) . componentUnitId) (Graph.toList (componentGraph lbi))
