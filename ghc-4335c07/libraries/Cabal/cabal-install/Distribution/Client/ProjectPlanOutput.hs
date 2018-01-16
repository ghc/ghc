{-# LANGUAGE BangPatterns, RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}

module Distribution.Client.ProjectPlanOutput (
    -- * Plan output
    writePlanExternalRepresentation,

    -- * Project status
    -- | Several outputs rely on having a general overview of
    PostBuildProjectStatus(..),
    updatePostBuildProjectStatus,
    createPackageEnvironment,
    writePlanGhcEnvironment,
    argsEquivalentOfGhcEnvironmentFile,
  ) where

import           Distribution.Client.ProjectPlanning.Types
import           Distribution.Client.ProjectBuilding.Types
import           Distribution.Client.DistDirLayout
import           Distribution.Client.Types (confInstId)
import           Distribution.Client.PackageHash (showHashValue)

import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.Utils.Json as J
import qualified Distribution.Simple.InstallDirs as InstallDirs

import qualified Distribution.Solver.Types.ComponentDeps as ComponentDeps

import           Distribution.Package
import           Distribution.System
import           Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.PackageDescription as PD
import           Distribution.Compiler (CompilerFlavor(GHC, GHCJS))
import           Distribution.Simple.Compiler
                   ( PackageDBStack, PackageDB(..)
                   , compilerVersion, compilerFlavor, showCompilerId
                   , compilerId, CompilerId(..), Compiler )
import           Distribution.Simple.GHC
                   ( getImplInfo, GhcImplInfo(supportsPkgEnvFiles)
                   , GhcEnvironmentFileEntry(..), simpleGhcEnvironmentFile
                   , writeGhcEnvironmentFile )
import           Distribution.Text
import qualified Distribution.Compat.Graph as Graph
import           Distribution.Compat.Graph (Graph, Node)
import qualified Distribution.Compat.Binary as Binary
import           Distribution.Simple.Utils
import           Distribution.Verbosity
import qualified Paths_cabal_install as Our (version)

import Prelude ()
import Distribution.Client.Compat.Prelude

import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BB

import           System.FilePath
import           System.IO

import Distribution.Simple.Program.GHC (packageDbArgsDb)

-----------------------------------------------------------------------------
-- Writing plan.json files
--

-- | Write out a representation of the elaborated install plan.
--
-- This is for the benefit of debugging and external tools like editors.
--
writePlanExternalRepresentation :: DistDirLayout
                                -> ElaboratedInstallPlan
                                -> ElaboratedSharedConfig
                                -> IO ()
writePlanExternalRepresentation distDirLayout elaboratedInstallPlan
                                elaboratedSharedConfig =
    writeFileAtomic (distProjectCacheFile distDirLayout "plan.json") $
        BB.toLazyByteString
      . J.encodeToBuilder
      $ encodePlanAsJson distDirLayout elaboratedInstallPlan elaboratedSharedConfig

-- | Renders a subset of the elaborated install plan in a semi-stable JSON
-- format.
--
encodePlanAsJson :: DistDirLayout -> ElaboratedInstallPlan -> ElaboratedSharedConfig -> J.Value
encodePlanAsJson distDirLayout elaboratedInstallPlan elaboratedSharedConfig =
    --TODO: [nice to have] include all of the sharedPackageConfig and all of
    --      the parts of the elaboratedInstallPlan
    J.object [ "cabal-version"     J..= jdisplay Our.version
             , "cabal-lib-version" J..= jdisplay cabalVersion
             , "compiler-id"       J..= (J.String . showCompilerId . pkgConfigCompiler)
                                        elaboratedSharedConfig
             , "os"                J..= jdisplay os
             , "arch"              J..= jdisplay arch
             , "install-plan"      J..= installPlanToJ elaboratedInstallPlan
             ]
  where
    Platform arch os = pkgConfigPlatform elaboratedSharedConfig

    installPlanToJ :: ElaboratedInstallPlan -> [J.Value]
    installPlanToJ = map planPackageToJ . InstallPlan.toList

    planPackageToJ :: ElaboratedPlanPackage -> J.Value
    planPackageToJ pkg =
      case pkg of
        InstallPlan.PreExisting ipi -> installedPackageInfoToJ ipi
        InstallPlan.Configured elab -> elaboratedPackageToJ False elab
        InstallPlan.Installed  elab -> elaboratedPackageToJ True  elab
        -- Note that the plan.json currently only uses the elaborated plan,
        -- not the improved plan. So we will not get the Installed state for
        -- that case, but the code supports it in case we want to use this
        -- later in some use case where we want the status of the build.

    installedPackageInfoToJ :: InstalledPackageInfo -> J.Value
    installedPackageInfoToJ ipi =
      -- Pre-existing packages lack configuration information such as their flag
      -- settings or non-lib components. We only get pre-existing packages for
      -- the global/core packages however, so this isn't generally a problem.
      -- So these packages are never local to the project.
      --
      J.object
        [ "type"       J..= J.String "pre-existing"
        , "id"         J..= (jdisplay . installedUnitId) ipi
        , "pkg-name"   J..= (jdisplay . pkgName . packageId) ipi
        , "pkg-version" J..= (jdisplay . pkgVersion . packageId) ipi
        , "depends"    J..= map jdisplay (installedDepends ipi)
        ]

    elaboratedPackageToJ :: Bool -> ElaboratedConfiguredPackage -> J.Value
    elaboratedPackageToJ isInstalled elab =
      J.object $
        [ "type"       J..= J.String (if isInstalled then "installed"
                                                     else "configured")
        , "id"         J..= (jdisplay . installedUnitId) elab
        , "pkg-name"   J..= (jdisplay . pkgName . packageId) elab
        , "pkg-version" J..= (jdisplay . pkgVersion . packageId) elab
        , "flags"      J..= J.object [ PD.unFlagName fn J..= v
                                     | (fn,v) <- PD.unFlagAssignment (elabFlagAssignment elab) ]
        , "style"      J..= J.String (style2str (elabLocalToProject elab) (elabBuildStyle elab))
        ] ++
        [ "pkg-src-sha256" J..= J.String (showHashValue hash)
        | Just hash <- [elabPkgSourceHash elab] ] ++
        (case elabBuildStyle elab of
            BuildInplaceOnly ->
                ["dist-dir"   J..= J.String dist_dir]
            BuildAndInstall ->
                -- TODO: install dirs?
                []
            ) ++
        case elabPkgOrComp elab of
          ElabPackage pkg ->
            let components = J.object $
                  [ comp2str c J..= (J.object $
                    [ "depends"     J..= map (jdisplay . confInstId) ldeps
                    , "exe-depends" J..= map (jdisplay . confInstId) edeps ] ++
                    bin_file c)
                  | (c,(ldeps,edeps))
                      <- ComponentDeps.toList $
                         ComponentDeps.zip (pkgLibDependencies pkg)
                                           (pkgExeDependencies pkg) ]
            in ["components" J..= components]
          ElabComponent comp ->
            ["depends"     J..= map (jdisplay . confInstId) (elabLibDependencies elab)
            ,"exe-depends" J..= map jdisplay (elabExeDependencies elab)
            ,"component-name" J..= J.String (comp2str (compSolverName comp))
            ] ++
            bin_file (compSolverName comp)
     where
      dist_dir = distBuildDirectory distDirLayout
                    (elabDistDirParams elaboratedSharedConfig elab)

      bin_file c = case c of
        ComponentDeps.ComponentExe s   -> bin_file' s
        ComponentDeps.ComponentTest s  -> bin_file' s
        ComponentDeps.ComponentBench s -> bin_file' s
        _ -> []
      bin_file' s =
        ["bin-file" J..= J.String bin]
       where
        bin = if elabBuildStyle elab == BuildInplaceOnly
               then dist_dir </> "build" </> display s </> display s
               else InstallDirs.bindir (elabInstallDirs elab) </> display s

    -- TODO: maybe move this helper to "ComponentDeps" module?
    --       Or maybe define a 'Text' instance?
    comp2str :: ComponentDeps.Component -> String
    comp2str c = case c of
        ComponentDeps.ComponentLib     -> "lib"
        ComponentDeps.ComponentSubLib s -> "lib:"   <> display s
        ComponentDeps.ComponentFLib s  -> "flib:"  <> display s
        ComponentDeps.ComponentExe s   -> "exe:"   <> display s
        ComponentDeps.ComponentTest s  -> "test:"  <> display s
        ComponentDeps.ComponentBench s -> "bench:" <> display s
        ComponentDeps.ComponentSetup   -> "setup"

    style2str :: Bool -> BuildStyle -> String
    style2str True  _                = "local"
    style2str False BuildInplaceOnly = "inplace"
    style2str False BuildAndInstall  = "global"

    jdisplay :: Text a => a -> J.Value
    jdisplay = J.String . display


-----------------------------------------------------------------------------
-- Project status
--

-- So, what is the status of a project after a build? That is, how do the
-- inputs (package source files etc) compare to the output artefacts (build
-- libs, exes etc)? Do the outputs reflect the current values of the inputs
-- or are outputs out of date or invalid?
--
-- First of all, what do we mean by out-of-date and what do we mean by
-- invalid? We think of the build system as a morally pure function that
-- computes the output artefacts given input values. We say an output artefact
-- is out of date when its value is not the value that would be computed by a
-- build given the current values of the inputs. An output artefact can be
-- out-of-date but still be perfectly usable; it simply correspond to a
-- previous state of the inputs.
--
-- On the other hand there are cases where output artefacts cannot safely be
-- used. For example libraries and dynamically linked executables cannot be
-- used when the libs they depend on change without them being recompiled
-- themselves. Whether an artefact is still usable depends on what it is, e.g.
-- dynamically linked vs statically linked and on how it gets updated (e.g.
-- only atomically on success or if failure can leave invalid states). We need
-- a definition (or two) that is independent of the kind of artefact and can
-- be computed just in terms of changes in package graphs, but are still
-- useful for determining when particular kinds of artefacts are invalid.
--
-- Note that when we talk about packages in this context we just mean nodes
-- in the elaborated install plan, which can be components or packages.
--
-- There's obviously a close connection between packages being out of date and
-- their output artefacts being unusable: most of the time if a package
-- remains out of date at the end of a build then some of its output artefacts
-- will be unusable. That is true most of the time because a build will have
-- attempted to build one of the out-of-date package's dependencies. If the
-- build of the dependency succeeded then it changed output artefacts (like
-- libs) and if it failed then it may have failed after already changing
-- things (think failure after updating some but not all .hi files).
--
-- There are a few reasons we may end up with still-usable output artefacts
-- for a package even when it remains out of date at the end of a build.
-- Firstly if executing a plan fails then packages can be skipped, and thus we
-- may have packages where all their dependencies were skipped. Secondly we
-- have artefacts like statically linked executables which are not affected by
-- libs they depend on being recompiled. Furthermore, packages can be out of
-- date due to changes in build tools or Setup.hs scripts they depend on, but
-- again libraries or executables in those out-of-date packages remain usable.
--
-- So we have two useful definitions of invalid. Both are useful, for
-- different purposes, so we will compute both. The first corresponds to the
-- invalid libraries and dynamic executables. We say a package is invalid by
-- changed deps if any of the packages it depends on (via library dep edges)
-- were rebuilt (successfully or unsuccessfully). The second definition
-- corresponds to invalid static executables. We say a package is invalid by
-- a failed build simply if the package was built but unsuccessfully.
--
-- So how do we find out what packages are out of date or invalid?
--
-- Obviously we know something for all the packages that were part of the plan
-- that was executed, but that is just a subset since we prune the plan down
-- to the targets and their dependencies.
--
-- Recall the steps we go though:
--
-- + starting with the initial improved plan (this is the full project);
--
-- + prune the plan to the user's build targets;
--
-- + rebuildTargetsDryRun on the pruned plan giving us a BuildStatusMap
--   covering the pruned subset of the original plan;
--
-- + execute the plan giving us BuildOutcomes which tell us success/failure
--   for each package.
--
-- So given that the BuildStatusMap and BuildOutcomes do not cover everything
-- in the original plan, what can they tell us about the original plan?
--
-- The BuildStatusMap tells us directly that some packages are up to date and
-- others out of date (but only for the pruned subset). But we know that
-- everything that is a reverse dependency of an out-of-date package is itself
-- out-of-date (whether or not it is in the pruned subset). Of course after
-- a build the BuildOutcomes may tell us that some of those out-of-date
-- packages are now up to date (ie a successful build outcome).
--
-- The difference is packages that are reverse dependencies of out-of-date
-- packages but are not brought up-to-date by the build (i.e. did not have
-- successful outcomes, either because they failed or were not in the pruned
-- subset to be built). We also know which packages were rebuilt, so we can
-- use this to find the now-invalid packages.
--
-- Note that there are still packages for which we cannot discover full status
-- information. There may be packages outside of the pruned plan that do not
-- depend on packages within the pruned plan that were discovered to be
-- out-of-date. For these packages we do not know if their build artefacts
-- are out-of-date or not. We do know however that they are not invalid, as
-- that's not possible given our definition of invalid. Intuitively it is
-- because we have not disturbed anything that these packages depend on, e.g.
-- we've not rebuilt any libs they depend on. Recall that our widest
-- definition of invalid was only concerned about dependencies on libraries
-- (to cover problems like shared libs or GHC seeing inconsistent .hi files).
--
-- So our algorithm for out-of-date packages is relatively simple: take the
-- reverse dependency closure in the original improved plan (pre-pruning) of
-- the out-of-date packages (as determined by the BuildStatusMap from the dry
-- run). That gives a set of packages that were definitely out of date after
-- the dry run. Now we remove from this set the packages that the
-- BuildOutcomes tells us are now up-to-date after the build. The remaining
-- set is the out-of-date packages.
--
-- As for packages that are invalid by changed deps, we start with the plan
-- dependency graph but keep only those edges that point to libraries (so
-- ignoring deps on exes and setup scripts). We take the packages for which a
-- build was attempted (successfully or unsuccessfully, but not counting
-- knock-on failures) and take the reverse dependency closure. We delete from
-- this set all the packages that were built successfully. Note that we do not
-- need to intersect with the out-of-date packages since this follows
-- automatically: all rev deps of packages we attempted to build must have
-- been out of date at the start of the build, and if they were not built
-- successfully then they're still out of date -- meeting our definition of
-- invalid.


type PackageIdSet     = Set UnitId
type PackagesUpToDate = PackageIdSet

data PostBuildProjectStatus = PostBuildProjectStatus {

       -- | Packages that are known to be up to date. These were found to be
       -- up to date before the build, or they have a successful build outcome
       -- afterwards.
       --
       -- This does not include any packages outside of the subset of the plan
       -- that was executed because we did not check those and so don't know
       -- for sure that they're still up to date.
       --
       packagesDefinitelyUpToDate :: PackageIdSet,

       -- | Packages that are probably still up to date (and at least not
       -- known to be out of date, and certainly not invalid). This includes
       -- 'packagesDefinitelyUpToDate' plus packages that were up to date
       -- previously and are outside of the subset of the plan that was
       -- executed. It excludes 'packagesOutOfDate'.
       --
       packagesProbablyUpToDate :: PackageIdSet,

       -- | Packages that are known to be out of date. These are packages
       -- that were determined to be out of date before the build, and they
       -- do not have a successful build outcome afterwards.
       --
       -- Note that this can sometimes include packages outside of the subset
       -- of the plan that was executed. For example suppose package A and B
       -- depend on C, and A is the target so only A and C are in the subset
       -- to be built. Now suppose C is found to have changed, then both A
       -- and B are out-of-date before the build and since B is outside the
       -- subset to be built then it will remain out of date.
       --
       -- Note also that this is /not/ the inverse of
       -- 'packagesDefinitelyUpToDate' or 'packagesProbablyUpToDate'.
       -- There are packages where we have no information (ones that were not
       -- in the subset of the plan that was executed).
       --
       packagesOutOfDate :: PackageIdSet,

       -- | Packages that depend on libraries that have changed during the
       -- build (either build success or failure).
       --
       -- This corresponds to the fact that libraries and dynamic executables
       -- are invalid once any of the libs they depend on change.
       --
       -- This does include packages that themselves failed (i.e. it is a
       -- superset of 'packagesInvalidByFailedBuild'). It does not include
       -- changes in dependencies on executables (i.e. build tools).
       --
       packagesInvalidByChangedLibDeps :: PackageIdSet,

       -- | Packages that themselves failed during the build (i.e. them
       -- directly not a dep).
       --
       -- This corresponds to the fact that static executables are invalid
       -- in unlucky circumstances such as linking failing half way though,
       -- or data file generation failing.
       --
       -- This is a subset of 'packagesInvalidByChangedLibDeps'.
       --
       packagesInvalidByFailedBuild :: PackageIdSet,

       -- | A subset of the plan graph, including only dependency-on-library
       -- edges. That is, dependencies /on/ libraries, not dependencies /of/
       -- libraries. This tells us all the libraries that packages link to.
       --
       -- This is here as a convenience, as strictly speaking it's not status
       -- as it's just a function of the original 'ElaboratedInstallPlan'.
       --
       packagesLibDepGraph :: Graph (Node UnitId ElaboratedPlanPackage),

       -- | As a convenience for 'Set.intersection' with any of the other
       -- 'PackageIdSet's to select only packages that are part of the
       -- project locally (i.e. with a local source dir).
       --
       packagesBuildLocal     :: PackageIdSet,

       -- | As a convenience for 'Set.intersection' with any of the other
       -- 'PackageIdSet's to select only packages that are being built
       -- in-place within the project (i.e. not destined for the store).
       --
       packagesBuildInplace   :: PackageIdSet,

       -- | As a convenience for 'Set.intersection' or 'Set.difference' with
       -- any of the other 'PackageIdSet's to select only packages that were
       -- pre-installed or already in the store prior to the build.
       --
       packagesAlreadyInStore :: PackageIdSet
     }

-- | Work out which packages are out of date or invalid after a build.
--
postBuildProjectStatus :: ElaboratedInstallPlan
                       -> PackagesUpToDate
                       -> BuildStatusMap
                       -> BuildOutcomes
                       -> PostBuildProjectStatus
postBuildProjectStatus plan previousPackagesUpToDate
                       pkgBuildStatus buildOutcomes =
    PostBuildProjectStatus {
      packagesDefinitelyUpToDate,
      packagesProbablyUpToDate,
      packagesOutOfDate,
      packagesInvalidByChangedLibDeps,
      packagesInvalidByFailedBuild,
      -- convenience stuff
      packagesLibDepGraph,
      packagesBuildLocal,
      packagesBuildInplace,
      packagesAlreadyInStore
    }
  where
    packagesDefinitelyUpToDate =
       packagesUpToDatePreBuild
        `Set.union`
       packagesSuccessfulPostBuild

    packagesProbablyUpToDate =
      packagesDefinitelyUpToDate
        `Set.union`
      (previousPackagesUpToDate' `Set.difference` packagesOutOfDatePreBuild)

    packagesOutOfDate =
      packagesOutOfDatePreBuild `Set.difference` packagesSuccessfulPostBuild

    packagesInvalidByChangedLibDeps =
      packagesDepOnChangedLib `Set.difference` packagesSuccessfulPostBuild

    packagesInvalidByFailedBuild =
      packagesFailurePostBuild

    -- Note: if any of the intermediate values below turn out to be useful in
    -- their own right then we can simply promote them to the result record

    -- The previous set of up-to-date packages will contain bogus package ids
    -- when the solver plan or config contributing to the hash changes.
    -- So keep only the ones where the package id (i.e. hash) is the same.
    previousPackagesUpToDate' =
      Set.intersection
        previousPackagesUpToDate
        (InstallPlan.keysSet plan)

    packagesUpToDatePreBuild =
      Set.filter
        (\ipkgid -> not (lookupBuildStatusRequiresBuild True ipkgid))
        -- For packages not in the plan subset we did the dry-run on we don't
        -- know anything about their status, so not known to be /up to date/.
        (InstallPlan.keysSet plan)

    packagesOutOfDatePreBuild =
      Set.fromList . map installedUnitId $
      InstallPlan.reverseDependencyClosure plan
        [ ipkgid
        | pkg <- InstallPlan.toList plan
        , let ipkgid = installedUnitId pkg
        , lookupBuildStatusRequiresBuild False ipkgid
        -- For packages not in the plan subset we did the dry-run on we don't
        -- know anything about their status, so not known to be /out of date/.
        ]

    packagesSuccessfulPostBuild =
      Set.fromList
        [ ikgid | (ikgid, Right _) <- Map.toList buildOutcomes ]

    -- direct failures, not failures due to deps
    packagesFailurePostBuild =
      Set.fromList
        [ ikgid
        | (ikgid, Left failure) <- Map.toList buildOutcomes
        , case buildFailureReason failure of
            DependentFailed _ -> False
            _                 -> True
        ]

    -- Packages that have a library dependency on a package for which a build
    -- was attempted
    packagesDepOnChangedLib =
      Set.fromList . map Graph.nodeKey $
      fromMaybe (error "packagesBuildStatusAfterBuild: broken dep closure") $
      Graph.revClosure packagesLibDepGraph
        ( Map.keys
        . Map.filter (uncurry buildAttempted)
        $ Map.intersectionWith (,) pkgBuildStatus buildOutcomes 
        )

    -- The plan graph but only counting dependency-on-library edges
    packagesLibDepGraph :: Graph (Node UnitId ElaboratedPlanPackage)
    packagesLibDepGraph =
      Graph.fromDistinctList
        [ Graph.N pkg (installedUnitId pkg) libdeps
        | pkg <- InstallPlan.toList plan
        , let libdeps = case pkg of
                InstallPlan.PreExisting ipkg  -> installedDepends ipkg
                InstallPlan.Configured srcpkg -> elabLibDeps srcpkg
                InstallPlan.Installed  srcpkg -> elabLibDeps srcpkg
        ]
    elabLibDeps = map (newSimpleUnitId . confInstId) . elabLibDependencies

    -- Was a build was attempted for this package?
    -- If it doesn't have both a build status and outcome then the answer is no.
    buildAttempted :: BuildStatus -> BuildOutcome -> Bool
    -- And not if it didn't need rebuilding in the first place.
    buildAttempted buildStatus _buildOutcome
      | not (buildStatusRequiresBuild buildStatus)
      = False

    -- And not if it was skipped due to a dep failing first.
    buildAttempted _ (Left BuildFailure {buildFailureReason})
      | DependentFailed _ <- buildFailureReason
      = False

    -- Otherwise, succeeded or failed, yes the build was tried.
    buildAttempted _ (Left BuildFailure {}) = True
    buildAttempted _ (Right _)              = True

    lookupBuildStatusRequiresBuild def ipkgid =
      case Map.lookup ipkgid pkgBuildStatus of
        Nothing          -> def -- Not in the plan subset we did the dry-run on
        Just buildStatus -> buildStatusRequiresBuild buildStatus

    packagesBuildLocal =
      selectPlanPackageIdSet $ \pkg ->
        case pkg of
          InstallPlan.PreExisting _     -> False
          InstallPlan.Installed   _     -> False
          InstallPlan.Configured srcpkg -> elabLocalToProject srcpkg

    packagesBuildInplace =
      selectPlanPackageIdSet $ \pkg ->
        case pkg of
          InstallPlan.PreExisting _     -> False
          InstallPlan.Installed   _     -> False
          InstallPlan.Configured srcpkg -> elabBuildStyle srcpkg
                                        == BuildInplaceOnly

    packagesAlreadyInStore =
      selectPlanPackageIdSet $ \pkg ->
        case pkg of
          InstallPlan.PreExisting _ -> True
          InstallPlan.Installed   _ -> True
          InstallPlan.Configured  _ -> False

    selectPlanPackageIdSet p = Map.keysSet
                             . Map.filter p
                             $ InstallPlan.toMap plan



updatePostBuildProjectStatus :: Verbosity
                             -> DistDirLayout
                             -> ElaboratedInstallPlan
                             -> BuildStatusMap
                             -> BuildOutcomes
                             -> IO PostBuildProjectStatus
updatePostBuildProjectStatus verbosity distDirLayout
                             elaboratedInstallPlan
                             pkgsBuildStatus buildOutcomes = do

    -- Read the previous up-to-date set, update it and write it back
    previousUpToDate   <- readPackagesUpToDateCacheFile distDirLayout
    let currentBuildStatus@PostBuildProjectStatus{..}
                        = postBuildProjectStatus
                            elaboratedInstallPlan
                            previousUpToDate
                            pkgsBuildStatus
                            buildOutcomes
    let currentUpToDate = packagesProbablyUpToDate
    writePackagesUpToDateCacheFile distDirLayout currentUpToDate

    -- Report various possibly interesting things
    -- We additionally intersect with the packagesBuildInplace so that
    -- we don't show huge numbers of boring packages from the store.
    debugNoWrap verbosity $
        "packages definitely up to date: "
     ++ displayPackageIdSet (packagesDefinitelyUpToDate
          `Set.intersection` packagesBuildInplace)

    debugNoWrap verbosity $
        "packages previously probably up to date: "
     ++ displayPackageIdSet (previousUpToDate
          `Set.intersection` packagesBuildInplace)

    debugNoWrap verbosity $
        "packages now probably up to date: "
     ++ displayPackageIdSet (packagesProbablyUpToDate
          `Set.intersection` packagesBuildInplace)

    debugNoWrap verbosity $
        "packages newly up to date: "
     ++ displayPackageIdSet (packagesDefinitelyUpToDate
            `Set.difference` previousUpToDate
          `Set.intersection` packagesBuildInplace)

    debugNoWrap verbosity $
        "packages out to date: "
     ++ displayPackageIdSet (packagesOutOfDate
          `Set.intersection` packagesBuildInplace)

    debugNoWrap verbosity $
        "packages invalid due to dep change: "
     ++ displayPackageIdSet packagesInvalidByChangedLibDeps

    debugNoWrap verbosity $
        "packages invalid due to build failure: "
     ++ displayPackageIdSet packagesInvalidByFailedBuild

    return currentBuildStatus
  where
    displayPackageIdSet = intercalate ", " . map display . Set.toList

-- | Helper for reading the cache file.
--
-- This determines the type and format of the binary cache file.
--
readPackagesUpToDateCacheFile :: DistDirLayout -> IO PackagesUpToDate
readPackagesUpToDateCacheFile DistDirLayout{distProjectCacheFile} =
    handleDoesNotExist Set.empty $
    handleDecodeFailure $
      withBinaryFile (distProjectCacheFile "up-to-date") ReadMode $ \hnd ->
        Binary.decodeOrFailIO =<< BS.hGetContents hnd
  where
    handleDecodeFailure = fmap (either (const Set.empty) id)

-- | Helper for writing the package up-to-date cache file.
--
-- This determines the type and format of the binary cache file.
--
writePackagesUpToDateCacheFile :: DistDirLayout -> PackagesUpToDate -> IO ()
writePackagesUpToDateCacheFile DistDirLayout{distProjectCacheFile} upToDate =
    writeFileAtomic (distProjectCacheFile "up-to-date") $
      Binary.encode upToDate

-- | Prepare a package environment that includes all the library dependencies
-- for a plan.
--
-- When running cabal new-exec, we want to set things up so that the compiler
-- can find all the right packages (and nothing else). This function is
-- intended to do that work. It takes a location where it can write files
-- temporarily, in case the compiler wants to learn this information via the
-- filesystem, and returns any environment variable overrides the compiler
-- needs.
createPackageEnvironment :: Verbosity
                         -> FilePath
                         -> ElaboratedInstallPlan
                         -> ElaboratedSharedConfig
                         -> PostBuildProjectStatus
                         -> IO [(String, Maybe String)]
createPackageEnvironment verbosity
                         path
                         elaboratedPlan
                         elaboratedShared
                         buildStatus
  | compilerFlavor (pkgConfigCompiler elaboratedShared) == GHC
  = do
    envFileM <- writePlanGhcEnvironment
      path
      elaboratedPlan
      elaboratedShared
      buildStatus
    case envFileM of
      Just envFile -> return [("GHC_ENVIRONMENT", Just envFile)]
      Nothing -> do
        warn verbosity "the configured version of GHC does not support reading package lists from the environment; commands that need the current project's package database are likely to fail"
        return []
  | otherwise
  = do
    warn verbosity "package environment configuration is not supported for the currently configured compiler; commands that need the current project's package database are likely to fail"
    return []

-- Writing .ghc.environment files
--

writePlanGhcEnvironment :: FilePath
                        -> ElaboratedInstallPlan
                        -> ElaboratedSharedConfig
                        -> PostBuildProjectStatus
                        -> IO (Maybe FilePath)
writePlanGhcEnvironment path
                        elaboratedInstallPlan
                        ElaboratedSharedConfig {
                          pkgConfigCompiler = compiler,
                          pkgConfigPlatform = platform
                        }
                        postBuildStatus
  | compilerFlavor compiler == GHC
  , supportsPkgEnvFiles (getImplInfo compiler)
  --TODO: check ghcjs compat
  = fmap Just $ writeGhcEnvironmentFile
      path
      platform (compilerVersion compiler)
      (renderGhcEnvironmentFile path
                                elaboratedInstallPlan
                                postBuildStatus)
    --TODO: [required eventually] support for writing user-wide package
    -- environments, e.g. like a global project, but we would not put the
    -- env file in the home dir, rather it lives under ~/.ghc/

writePlanGhcEnvironment _ _ _ _ = return Nothing

renderGhcEnvironmentFile :: FilePath
                         -> ElaboratedInstallPlan
                         -> PostBuildProjectStatus
                         -> [GhcEnvironmentFileEntry]
renderGhcEnvironmentFile projectRootDir elaboratedInstallPlan
                         postBuildStatus =
    headerComment
  : simpleGhcEnvironmentFile packageDBs unitIds
  where
    headerComment =
        GhcEnvFileComment
      $ "This is a GHC environment file written by cabal. This means you can\n"
     ++ "run ghc or ghci and get the environment of the project as a whole.\n"
     ++ "But you still need to use cabal repl $target to get the environment\n"
     ++ "of specific components (libs, exes, tests etc) because each one can\n"
     ++ "have its own source dirs, cpp flags etc.\n\n"
    unitIds    = selectGhcEnvironmentFileLibraries postBuildStatus
    packageDBs = relativePackageDBPaths projectRootDir $
                 selectGhcEnvironmentFilePackageDbs elaboratedInstallPlan


argsEquivalentOfGhcEnvironmentFile
  :: Compiler
  -> DistDirLayout
  -> ElaboratedInstallPlan
  -> PostBuildProjectStatus
  -> [String]
argsEquivalentOfGhcEnvironmentFile compiler =
  case compilerId compiler
  of CompilerId GHC   _ -> argsEquivalentOfGhcEnvironmentFileGhc
     CompilerId GHCJS _ -> argsEquivalentOfGhcEnvironmentFileGhc
     CompilerId _     _ -> error "Only GHC and GHCJS are supported"

-- TODO remove this when we drop support for non-.ghc.env ghc
argsEquivalentOfGhcEnvironmentFileGhc
  :: DistDirLayout
  -> ElaboratedInstallPlan
  -> PostBuildProjectStatus
  -> [String]
argsEquivalentOfGhcEnvironmentFileGhc
  distDirLayout
  elaboratedInstallPlan
  postBuildStatus =
    clearPackageDbStackFlag
 ++ packageDbArgsDb packageDBs
 ++ foldMap packageIdFlag packageIds
  where
    projectRootDir = distProjectRootDirectory distDirLayout
    packageIds = selectGhcEnvironmentFileLibraries postBuildStatus
    packageDBs = relativePackageDBPaths projectRootDir $
                 selectGhcEnvironmentFilePackageDbs elaboratedInstallPlan
    -- TODO use proper flags? but packageDbArgsDb is private
    clearPackageDbStackFlag = ["-clear-package-db", "-global-package-db"]
    packageIdFlag uid = ["-package-id", display uid]


-- We're producing an environment for users to use in ghci, so of course
-- that means libraries only (can't put exes into the ghc package env!).
-- The library environment should be /consistent/ with the environment
-- that each of the packages in the project use (ie same lib versions).
-- So that means all the normal library dependencies of all the things
-- in the project (including deps of exes that are local to the project).
-- We do not however want to include the dependencies of Setup.hs scripts,
-- since these are generally uninteresting but also they need not in
-- general be consistent with the library versions that packages local to
-- the project use (recall that Setup.hs script's deps can be picked
-- independently of other packages in the project).
--
-- So, our strategy is as follows:
--
-- produce a dependency graph of all the packages in the install plan,
-- but only consider normal library deps as edges in the graph. Thus we
-- exclude the dependencies on Setup.hs scripts (in the case of
-- per-component granularity) or of Setup.hs scripts (in the case of
-- per-package granularity). Then take a dependency closure, using as
-- roots all the packages/components local to the project. This will
-- exclude Setup scripts and their dependencies.
--
-- Note: this algorithm will have to be adapted if/when the install plan
-- is extended to cover multiple compilers at once, and may also have to
-- change if we start to treat unshared deps of test suites in a similar
-- way to how we treat Setup.hs script deps (ie being able to pick them
-- independently).
--
-- Since we had to use all the local packages, including exes, (as roots
-- to find the libs) then those exes still end up in our list so we have
-- to filter them out at the end.
--
selectGhcEnvironmentFileLibraries :: PostBuildProjectStatus -> [UnitId]
selectGhcEnvironmentFileLibraries PostBuildProjectStatus{..} =
    case Graph.closure packagesLibDepGraph (Set.toList packagesBuildLocal) of
      Nothing    -> error "renderGhcEnvironmentFile: broken dep closure"
      Just nodes -> [ pkgid | Graph.N pkg pkgid _ <- nodes
                            , hasUpToDateLib pkg ]
  where
    hasUpToDateLib planpkg = case planpkg of
      -- A pre-existing global lib
      InstallPlan.PreExisting  _ -> True

      -- A package in the store. Check it's a lib.
      InstallPlan.Installed  pkg -> elabRequiresRegistration pkg

      -- A package we were installing this time, either destined for the store
      -- or just locally. Check it's a lib and that it is probably up to date.
      InstallPlan.Configured pkg ->
          elabRequiresRegistration pkg
       && installedUnitId pkg `Set.member` packagesProbablyUpToDate


selectGhcEnvironmentFilePackageDbs :: ElaboratedInstallPlan -> PackageDBStack
selectGhcEnvironmentFilePackageDbs elaboratedInstallPlan =
    -- If we have any inplace packages then their package db stack is the
    -- one we should use since it'll include the store + the local db but
    -- it's certainly possible to have no local inplace packages
    -- e.g. just "extra" packages coming from the store.
    case (inplacePackages, sourcePackages) of
      ([], pkgs) -> checkSamePackageDBs pkgs
      (pkgs, _)  -> checkSamePackageDBs pkgs
  where
    checkSamePackageDBs pkgs =
      case ordNub (map elabBuildPackageDBStack pkgs) of
        [packageDbs] -> packageDbs
        []           -> []
        _            -> error $ "renderGhcEnvironmentFile: packages with "
                             ++ "different package db stacks"
        -- This should not happen at the moment but will happen as soon
        -- as we support projects where we build packages with different
        -- compilers, at which point we have to consider how to adapt
        -- this feature, e.g. write out multiple env files, one for each
        -- compiler / project profile.

    inplacePackages =
      [ srcpkg
      | srcpkg <- sourcePackages
      , elabBuildStyle srcpkg == BuildInplaceOnly ]
    sourcePackages =
      [ srcpkg
      | pkg <- InstallPlan.toList elaboratedInstallPlan
      , srcpkg <- maybeToList $ case pkg of
                    InstallPlan.Configured srcpkg -> Just srcpkg
                    InstallPlan.Installed  srcpkg -> Just srcpkg
                    InstallPlan.PreExisting _     -> Nothing
      ]

relativePackageDBPaths :: FilePath -> PackageDBStack -> PackageDBStack
relativePackageDBPaths relroot = map (relativePackageDBPath relroot)

relativePackageDBPath :: FilePath -> PackageDB -> PackageDB
relativePackageDBPath relroot pkgdb =
    case pkgdb of
      GlobalPackageDB        -> GlobalPackageDB
      UserPackageDB          -> UserPackageDB
      SpecificPackageDB path -> SpecificPackageDB relpath
        where relpath = makeRelative relroot path

