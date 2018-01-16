{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

-- | This module deals with building and incrementally rebuilding a collection
-- of packages. It is what backs the @cabal build@ and @configure@ commands,
-- as well as being a core part of @run@, @test@, @bench@ and others.
--
-- The primary thing is in fact rebuilding (and trying to make that quick by
-- not redoing unnecessary work), so building from scratch is just a special
-- case.
--
-- The build process and the code can be understood by breaking it down into
-- three major parts:
--
-- * The 'ElaboratedInstallPlan' type
--
-- * The \"what to do\" phase, where we look at the all input configuration
--   (project files, .cabal files, command line etc) and produce a detailed
--   plan of what to do -- the 'ElaboratedInstallPlan'.
--
-- * The \"do it\" phase, where we take the 'ElaboratedInstallPlan' and we
-- re-execute it.
--
-- As far as possible, the \"what to do\" phase embodies all the policy, leaving
-- the \"do it\" phase policy free. The first phase contains more of the
-- complicated logic, but it is contained in code that is either pure or just
-- has read effects (except cache updates). Then the second phase does all the
-- actions to build packages, but as far as possible it just follows the
-- instructions and avoids any logic for deciding what to do (apart from
-- recompilation avoidance in executing the plan).
--
-- This division helps us keep the code under control, making it easier to
-- understand, test and debug. So when you are extending these modules, please
-- think about which parts of your change belong in which part. It is
-- perfectly ok to extend the description of what to do (i.e. the
-- 'ElaboratedInstallPlan') if that helps keep the policy decisions in the
-- first phase. Also, the second phase does not have direct access to any of
-- the input configuration anyway; all the information has to flow via the
-- 'ElaboratedInstallPlan'.
--
module Distribution.Client.ProjectOrchestration (
    -- * Discovery phase: what is in the project?
    establishProjectBaseContext,
    ProjectBaseContext(..),
    BuildTimeSettings(..),
    commandLineFlagsToProjectConfig,

    -- * Pre-build phase: decide what to do.
    runProjectPreBuildPhase,
    ProjectBuildContext(..),

    -- ** Selecting what targets we mean
    readTargetSelectors,
    reportTargetSelectorProblems,
    resolveTargets,
    TargetsMap,
    TargetSelector(..),
    PackageId,
    AvailableTarget(..),
    AvailableTargetStatus(..),
    TargetRequested(..),
    ComponentName(..),
    ComponentKind(..),
    ComponentTarget(..),
    SubComponentTarget(..),
    TargetProblemCommon(..),
    selectComponentTargetBasic,
    distinctTargetComponents,
    -- ** Utils for selecting targets
    filterTargetsKind,
    filterTargetsKindWith,
    selectBuildableTargets,
    selectBuildableTargetsWith,
    selectBuildableTargets',
    selectBuildableTargetsWith',
    forgetTargetsDetail,

    -- ** Adjusting the plan
    pruneInstallPlanToTargets,
    TargetAction(..),
    pruneInstallPlanToDependencies,
    CannotPruneDependencies(..),
    printPlan,

    -- * Build phase: now do it.
    runProjectBuildPhase,

    -- * Post build actions
    runProjectPostBuildPhase,
    dieOnBuildFailures,

    -- * Shared CLI utils
    cmdCommonHelpTextNewBuildBeta,
  ) where

import           Distribution.Client.ProjectConfig
import           Distribution.Client.ProjectPlanning
                   hiding ( pruneInstallPlanToTargets )
import qualified Distribution.Client.ProjectPlanning as ProjectPlanning
                   ( pruneInstallPlanToTargets )
import           Distribution.Client.ProjectPlanning.Types
import           Distribution.Client.ProjectBuilding
import           Distribution.Client.ProjectPlanOutput

import           Distribution.Client.Types
                   ( GenericReadyPackage(..), UnresolvedSourcePackage
                   , PackageSpecifier(..) )
import qualified Distribution.Client.InstallPlan as InstallPlan
import           Distribution.Client.TargetSelector
                   ( TargetSelector(..)
                   , ComponentKind(..), componentKind
                   , readTargetSelectors, reportTargetSelectorProblems )
import           Distribution.Client.DistDirLayout
import           Distribution.Client.Config (defaultCabalDir)
import           Distribution.Client.Setup hiding (packageName)

import           Distribution.Solver.Types.OptionalStanza

import           Distribution.Package
                   hiding (InstalledPackageId, installedPackageId)
import           Distribution.PackageDescription
                   ( FlagAssignment, unFlagAssignment, showFlagValue
                   , diffFlagAssignment )
import           Distribution.Simple.LocalBuildInfo
                   ( ComponentName(..), pkgComponents )
import qualified Distribution.Simple.Setup as Setup
import           Distribution.Simple.Command (commandShowOptions)

import           Distribution.Simple.Utils
                   ( die'
                   , notice, noticeNoWrap, debugNoWrap )
import           Distribution.Verbosity
import           Distribution.Text
import           Distribution.Simple.Compiler
                   ( showCompilerId
                   , OptimisationLevel(..))

import qualified Data.Monoid as Mon
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.List
import           Data.Maybe
import           Data.Either
import           Control.Monad (void)
import           Control.Exception (Exception(..), throwIO, assert)
import           System.Exit (ExitCode(..), exitFailure)
#ifdef MIN_VERSION_unix
import           System.Posix.Signals (sigKILL, sigSEGV)
#endif


-- | This holds the context of a project prior to solving: the content of the
-- @cabal.project@ and all the local package @.cabal@ files.
--
data ProjectBaseContext = ProjectBaseContext {
       distDirLayout  :: DistDirLayout,
       cabalDirLayout :: CabalDirLayout,
       projectConfig  :: ProjectConfig,
       localPackages  :: [PackageSpecifier UnresolvedSourcePackage],
       buildSettings  :: BuildTimeSettings
     }

establishProjectBaseContext :: Verbosity
                            -> ProjectConfig
                            -> IO ProjectBaseContext
establishProjectBaseContext verbosity cliConfig = do

    cabalDir <- defaultCabalDir
    projectRoot <- either throwIO return =<<
                   findProjectRoot Nothing mprojectFile

    let distDirLayout  = defaultDistDirLayout projectRoot
                                              mdistDirectory

    (projectConfig, localPackages) <-
      rebuildProjectConfig verbosity
                           distDirLayout
                           cliConfig

    let ProjectConfigBuildOnly {
          projectConfigLogsDir,
          projectConfigStoreDir
        } = projectConfigBuildOnly projectConfig

        mlogsDir = Setup.flagToMaybe projectConfigLogsDir
        mstoreDir = Setup.flagToMaybe projectConfigStoreDir
        cabalDirLayout = mkCabalDirLayout cabalDir mstoreDir mlogsDir

        buildSettings = resolveBuildTimeSettings
                          verbosity cabalDirLayout
                          projectConfig

    return ProjectBaseContext {
      distDirLayout,
      cabalDirLayout,
      projectConfig,
      localPackages,
      buildSettings
    }
  where
    mdistDirectory = Setup.flagToMaybe projectConfigDistDir
    mprojectFile   = Setup.flagToMaybe projectConfigProjectFile
    ProjectConfigShared {
      projectConfigDistDir,
      projectConfigProjectFile
    } = projectConfigShared cliConfig


-- | This holds the context between the pre-build, build and post-build phases.
--
data ProjectBuildContext = ProjectBuildContext {
      -- | This is the improved plan, before we select a plan subset based on
      -- the build targets, and before we do the dry-run. So this contains
      -- all packages in the project.
      elaboratedPlanOriginal :: ElaboratedInstallPlan,

      -- | This is the 'elaboratedPlanOriginal' after we select a plan subset
      -- and do the dry-run phase to find out what is up-to or out-of date.
      -- This is the plan that will be executed during the build phase. So
      -- this contains only a subset of packages in the project.
      elaboratedPlanToExecute:: ElaboratedInstallPlan,

      -- | The part of the install plan that's shared between all packages in
      -- the plan. This does not change between the two plan variants above,
      -- so there is just the one copy.
      elaboratedShared       :: ElaboratedSharedConfig,

      -- | The result of the dry-run phase. This tells us about each member of
      -- the 'elaboratedPlanToExecute'.
      pkgsBuildStatus        :: BuildStatusMap,

      -- | The targets selected by @selectPlanSubset@. This is useful eg. in
      -- CmdRun, where we need a valid target to execute.
      targetsMap             :: TargetsMap
    }


-- | Pre-build phase: decide what to do.
--
runProjectPreBuildPhase
    :: Verbosity
    -> ProjectBaseContext
    -> (ElaboratedInstallPlan -> IO (ElaboratedInstallPlan, TargetsMap))
    -> IO ProjectBuildContext
runProjectPreBuildPhase
    verbosity
    ProjectBaseContext {
      distDirLayout,
      cabalDirLayout,
      projectConfig,
      localPackages
    }
    selectPlanSubset = do

    -- Take the project configuration and make a plan for how to build
    -- everything in the project. This is independent of any specific targets
    -- the user has asked for.
    --
    (elaboratedPlan, _, elaboratedShared) <-
      rebuildInstallPlan verbosity
                         distDirLayout cabalDirLayout
                         projectConfig
                         localPackages

    -- The plan for what to do is represented by an 'ElaboratedInstallPlan'

    -- Now given the specific targets the user has asked for, decide
    -- which bits of the plan we will want to execute.
    --
    (elaboratedPlan', targets) <- selectPlanSubset elaboratedPlan

    -- Check which packages need rebuilding.
    -- This also gives us more accurate reasons for the --dry-run output.
    --
    pkgsBuildStatus <- rebuildTargetsDryRun distDirLayout elaboratedShared
                                            elaboratedPlan'

    -- Improve the plan by marking up-to-date packages as installed.
    --
    let elaboratedPlan'' = improveInstallPlanWithUpToDatePackages
                             pkgsBuildStatus elaboratedPlan'
    debugNoWrap verbosity (InstallPlan.showInstallPlan elaboratedPlan'')

    return ProjectBuildContext {
      elaboratedPlanOriginal = elaboratedPlan,
      elaboratedPlanToExecute = elaboratedPlan'',
      elaboratedShared,
      pkgsBuildStatus,
      targetsMap = targets
    }


-- | Build phase: now do it.
--
-- Execute all or parts of the description of what to do to build or
-- rebuild the various packages needed.
--
runProjectBuildPhase :: Verbosity
                     -> ProjectBaseContext
                     -> ProjectBuildContext
                     -> IO BuildOutcomes
runProjectBuildPhase _ ProjectBaseContext{buildSettings} _
  | buildSettingDryRun buildSettings
  = return Map.empty

runProjectBuildPhase verbosity
                     ProjectBaseContext{..} ProjectBuildContext {..} =
    fmap (Map.union (previousBuildOutcomes pkgsBuildStatus)) $
    rebuildTargets verbosity
                   distDirLayout
                   (cabalStoreDirLayout cabalDirLayout)
                   elaboratedPlanToExecute
                   elaboratedShared
                   pkgsBuildStatus
                   buildSettings
  where
    previousBuildOutcomes :: BuildStatusMap -> BuildOutcomes
    previousBuildOutcomes =
      Map.mapMaybe $ \status -> case status of
        BuildStatusUpToDate buildSuccess -> Just (Right buildSuccess)
        --TODO: [nice to have] record build failures persistently
        _                                  -> Nothing

-- | Post-build phase: various administrative tasks
--
-- Update bits of state based on the build outcomes and report any failures.
--
runProjectPostBuildPhase :: Verbosity
                         -> ProjectBaseContext
                         -> ProjectBuildContext
                         -> BuildOutcomes
                         -> IO ()
runProjectPostBuildPhase _ ProjectBaseContext{buildSettings} _ _
  | buildSettingDryRun buildSettings
  = return ()

runProjectPostBuildPhase verbosity
                         ProjectBaseContext {..} ProjectBuildContext {..}
                         buildOutcomes = do
    -- Update other build artefacts
    -- TODO: currently none, but could include:
    --        - bin symlinks/wrappers
    --        - haddock/hoogle/ctags indexes
    --        - delete stale lib registrations
    --        - delete stale package dirs

    postBuildStatus <- updatePostBuildProjectStatus
                         verbosity
                         distDirLayout
                         elaboratedPlanOriginal
                         pkgsBuildStatus
                         buildOutcomes

    void $ writePlanGhcEnvironment (distProjectRootDirectory
                                      distDirLayout)
                                   elaboratedPlanOriginal
                                   elaboratedShared
                                   postBuildStatus

    -- Finally if there were any build failures then report them and throw
    -- an exception to terminate the program
    dieOnBuildFailures verbosity elaboratedPlanToExecute buildOutcomes

    -- Note that it is a deliberate design choice that the 'buildTargets' is
    -- not passed to phase 1, and the various bits of input config is not
    -- passed to phase 2.
    --
    -- We make the install plan without looking at the particular targets the
    -- user asks us to build. The set of available things we can build is
    -- discovered from the env and config and is used to make the install plan.
    -- The targets just tell us which parts of the install plan to execute.
    --
    -- Conversely, executing the plan does not directly depend on any of the
    -- input config. The bits that are needed (or better, the decisions based
    -- on it) all go into the install plan.

    -- Notionally, the 'BuildFlags' should be things that do not affect what
    -- we build, just how we do it. These ones of course do


------------------------------------------------------------------------------
-- Taking targets into account, selecting what to build
--

-- | The set of components to build, represented as a mapping from 'UnitId's
-- to the 'ComponentTarget's within the unit that will be selected
-- (e.g. selected to build, test or repl).
--
-- Associated with each 'ComponentTarget' is the set of 'TargetSelector's that
-- matched this target. Typically this is exactly one, but in general it is
-- possible to for different selectors to match the same target. This extra
-- information is primarily to help make helpful error messages.
--
type TargetsMap = Map UnitId [(ComponentTarget, [TargetSelector PackageId])]

-- | Given a set of 'TargetSelector's, resolve which 'UnitId's and
-- 'ComponentTarget's they ought to refer to.
--
-- The idea is that every user target identifies one or more roots in the
-- 'ElaboratedInstallPlan', which we will use to determine the closure
-- of what packages need to be built, dropping everything from the plan
-- that is unnecessary. This closure and pruning is done by
-- 'pruneInstallPlanToTargets' and this needs to be told the roots in terms
-- of 'UnitId's and the 'ComponentTarget's within those.
--
-- This means we first need to translate the 'TargetSelector's into the
-- 'UnitId's and 'ComponentTarget's. This translation has to be different for
-- the different command line commands, like @build@, @repl@ etc. For example
-- the command @build pkgfoo@ could select a different set of components in
-- pkgfoo than @repl pkgfoo@. The @build@ command would select any library and
-- all executables, whereas @repl@ would select the library or a single
-- executable. Furthermore, both of these examples could fail, and fail in
-- different ways and each needs to be able to produce helpful error messages.
--
-- So 'resolveTargets' takes two helpers: one to select the targets to be used
-- by user targets that refer to a whole package ('TargetPackage'), and
-- another to check user targets that refer to a component (or a module or
-- file within a component). These helpers can fail, and use their own error
-- type. Both helpers get given the 'AvailableTarget' info about the
-- component(s).
--
-- While commands vary quite a bit in their behaviour about which components to
-- select for a whole-package target, most commands have the same behaviour for
-- checking a user target that refers to a specific component. To help with
-- this commands can use 'selectComponentTargetBasic', either directly or as
-- a basis for their own @selectComponentTarget@ implementation.
--
resolveTargets :: forall err.
                  (forall k. TargetSelector PackageId
                          -> [AvailableTarget k]
                          -> Either err [k])
               -> (forall k. PackageId -> ComponentName -> SubComponentTarget
                          -> AvailableTarget k
                          -> Either err  k )
               -> (TargetProblemCommon -> err)
               -> ElaboratedInstallPlan
               -> [TargetSelector PackageId]
               -> Either [err] TargetsMap
resolveTargets selectPackageTargets selectComponentTarget liftProblem
               installPlan targetSelectors =
    --TODO: [required eventually]
    -- we cannot resolve names of packages other than those that are
    -- directly in the current plan. We ought to keep a set of the known
    -- hackage packages so we can resolve names to those. Though we don't
    -- really need that until we can do something sensible with packages
    -- outside of the project.

    case partitionEithers
           [ fmap ((,) targetSelector) (checkTarget targetSelector)
           | targetSelector <- targetSelectors ] of
      ([], targets) -> Right
                     . Map.map nubComponentTargets
                     $ Map.fromListWith (++)
                         [ (uid, [(ct, ts)])
                         | (ts, cts) <- targets
                         , (uid, ct) <- cts ]

      (problems, _) -> Left problems
  where
    -- TODO [required eventually] currently all build targets refer to packages
    -- inside the project. Ultimately this has to be generalised to allow
    -- referring to other packages and targets.
    checkTarget :: TargetSelector PackageId
                -> Either err [(UnitId, ComponentTarget)]

    -- We can ask to build any whole package, project-local or a dependency
    checkTarget bt@(TargetPackage _ pkgid mkfilter)
      | Just ats <- fmap (maybe id filterTargetsKind mkfilter)
                  $ Map.lookup pkgid availableTargetsByPackage
      = case selectPackageTargets bt ats of
          Left  e  -> Left e
          Right ts -> Right [ (unitid, ComponentTarget cname WholeComponent)
                            | (unitid, cname) <- ts ]

      | otherwise
      = Left (liftProblem (TargetProblemNoSuchPackage pkgid))

    checkTarget bt@(TargetAllPackages mkfilter) =
      let ats = maybe id filterTargetsKind mkfilter
              $ filter availableTargetLocalToProject
              $ concat (Map.elems availableTargetsByPackage)
       in case selectPackageTargets bt ats of
            Left  e  -> Left e
            Right ts -> Right [ (unitid, ComponentTarget cname WholeComponent)
                              | (unitid, cname) <- ts ]

    checkTarget (TargetComponent pkgid cname subtarget)
      | Just ats <- Map.lookup (pkgid, cname) availableTargetsByComponent
      = case partitionEithers
               (map (selectComponentTarget pkgid cname subtarget) ats) of
          (e:_,_) -> Left e
          ([],ts) -> Right [ (unitid, ctarget)
                           | let ctarget = ComponentTarget cname subtarget
                           , (unitid, _) <- ts ]

      | Map.member pkgid availableTargetsByPackage
      = Left (liftProblem (TargetProblemNoSuchComponent pkgid cname))

      | otherwise
      = Left (liftProblem (TargetProblemNoSuchPackage pkgid))

    checkTarget bt@(TargetPackageName pkgname)
      | Just ats <- Map.lookup pkgname availableTargetsByPackageName
      = case selectPackageTargets bt ats of
          Left  e  -> Left e
          Right ts -> Right [ (unitid, ComponentTarget cname WholeComponent)
                            | (unitid, cname) <- ts ]

      | otherwise
      = Left (liftProblem (TargetNotInProject pkgname))
    --TODO: check if the package is in the plan, even if it's not local
    --TODO: check if the package is in hackage and return different
    -- error cases here so the commands can handle things appropriately

    availableTargetsByPackage     :: Map PackageId                  [AvailableTarget (UnitId, ComponentName)]
    availableTargetsByPackageName :: Map PackageName                [AvailableTarget (UnitId, ComponentName)]
    availableTargetsByComponent   :: Map (PackageId, ComponentName) [AvailableTarget (UnitId, ComponentName)]

    availableTargetsByComponent   = availableTargets installPlan
    availableTargetsByPackage     = Map.mapKeysWith
                                      (++) (\(pkgid, _cname) -> pkgid)
                                      availableTargetsByComponent
                        `Map.union` availableTargetsEmptyPackages
    availableTargetsByPackageName = Map.mapKeysWith
                                    (++) packageName
                                    availableTargetsByPackage

    -- Add in all the empty packages. These do not appear in the
    -- availableTargetsByComponent map, since that only contains components
    -- so packages with no components are invisible from that perspective.
    -- The empty packages need to be there for proper error reporting, so users
    -- can select the empty package and then we can report that it is empty,
    -- otherwise we falsely report there is no such package at all.
    availableTargetsEmptyPackages =
      Map.fromList
        [ (packageId pkg, [])
        | InstallPlan.Configured pkg <- InstallPlan.toList installPlan
        , case elabPkgOrComp pkg of
            ElabComponent _ -> False
            ElabPackage   _ -> null (pkgComponents (elabPkgDescription pkg))
        ]

    --TODO: [research required] what if the solution has multiple versions of this package?
    --      e.g. due to setup deps or due to multiple independent sets of
    --      packages being built (e.g. ghc + ghcjs in a project)

filterTargetsKind :: ComponentKind -> [AvailableTarget k] -> [AvailableTarget k]
filterTargetsKind ckind = filterTargetsKindWith (== ckind)

filterTargetsKindWith :: (ComponentKind -> Bool)
                     -> [AvailableTarget k] -> [AvailableTarget k]
filterTargetsKindWith p ts =
    [ t | t@(AvailableTarget _ cname _ _) <- ts
        , p (componentKind cname) ]

selectBuildableTargets :: [AvailableTarget k] -> [k]
selectBuildableTargets ts =
    [ k | AvailableTarget _ _ (TargetBuildable k _) _ <- ts ]

selectBuildableTargetsWith :: (TargetRequested -> Bool)
                          -> [AvailableTarget k] -> [k]
selectBuildableTargetsWith p ts =
    [ k | AvailableTarget _ _ (TargetBuildable k req) _ <- ts, p req ]

selectBuildableTargets' :: [AvailableTarget k] -> ([k], [AvailableTarget ()])
selectBuildableTargets' ts =
    (,) [ k | AvailableTarget _ _ (TargetBuildable k _) _ <- ts ]
        [ forgetTargetDetail t
        | t@(AvailableTarget _ _ (TargetBuildable _ _) _) <- ts ]

selectBuildableTargetsWith' :: (TargetRequested -> Bool)
                           -> [AvailableTarget k] -> ([k], [AvailableTarget ()])
selectBuildableTargetsWith' p ts =
    (,) [ k | AvailableTarget _ _ (TargetBuildable k req) _ <- ts, p req ]
        [ forgetTargetDetail t
        | t@(AvailableTarget _ _ (TargetBuildable _ req) _) <- ts, p req ]


forgetTargetDetail :: AvailableTarget k -> AvailableTarget ()
forgetTargetDetail = fmap (const ())

forgetTargetsDetail :: [AvailableTarget k] -> [AvailableTarget ()]
forgetTargetsDetail = map forgetTargetDetail

-- | A basic @selectComponentTarget@ implementation to use or pass to
-- 'resolveTargets', that does the basic checks that the component is
-- buildable and isn't a test suite or benchmark that is disabled. This
-- can also be used to do these basic checks as part of a custom impl that
--
selectComponentTargetBasic :: PackageId
                           -> ComponentName
                           -> SubComponentTarget
                           -> AvailableTarget k
                           -> Either TargetProblemCommon k
selectComponentTargetBasic pkgid cname subtarget AvailableTarget {..} =
    case availableTargetStatus of
      TargetDisabledByUser ->
        Left (TargetOptionalStanzaDisabledByUser pkgid cname subtarget)

      TargetDisabledBySolver ->
        Left (TargetOptionalStanzaDisabledBySolver pkgid cname subtarget)

      TargetNotLocal ->
        Left (TargetComponentNotProjectLocal pkgid cname subtarget)

      TargetNotBuildable ->
        Left (TargetComponentNotBuildable pkgid cname subtarget)

      TargetBuildable targetKey _ ->
        Right targetKey

data TargetProblemCommon
   = TargetNotInProject                   PackageName
   | TargetComponentNotProjectLocal       PackageId ComponentName SubComponentTarget
   | TargetComponentNotBuildable          PackageId ComponentName SubComponentTarget
   | TargetOptionalStanzaDisabledByUser   PackageId ComponentName SubComponentTarget
   | TargetOptionalStanzaDisabledBySolver PackageId ComponentName SubComponentTarget

    -- The target matching stuff only returns packages local to the project,
    -- so these lookups should never fail, but if 'resolveTargets' is called
    -- directly then of course it can.
   | TargetProblemNoSuchPackage           PackageId
   | TargetProblemNoSuchComponent         PackageId ComponentName
  deriving (Eq, Show)

-- | Wrapper around 'ProjectPlanning.pruneInstallPlanToTargets' that adjusts
-- for the extra unneeded info in the 'TargetsMap'.
--
pruneInstallPlanToTargets :: TargetAction -> TargetsMap
                          -> ElaboratedInstallPlan -> ElaboratedInstallPlan
pruneInstallPlanToTargets targetActionType targetsMap elaboratedPlan =
    assert (Map.size targetsMap > 0) $
    ProjectPlanning.pruneInstallPlanToTargets
      targetActionType
      (Map.map (map fst) targetsMap)
      elaboratedPlan

-- | Utility used by repl and run to check if the targets spans multiple
-- components, since those commands do not support multiple components.
--
distinctTargetComponents :: TargetsMap -> Set.Set (UnitId, ComponentName)
distinctTargetComponents targetsMap =
    Set.fromList [ (uid, cname)
                 | (uid, cts) <- Map.toList targetsMap
                 , (ComponentTarget cname _, _) <- cts ]


------------------------------------------------------------------------------
-- Displaying what we plan to do
--

-- | Print a user-oriented presentation of the install plan, indicating what
-- will be built.
--
printPlan :: Verbosity
          -> ProjectBaseContext
          -> ProjectBuildContext
          -> IO ()
printPlan verbosity
          ProjectBaseContext {
            buildSettings = BuildTimeSettings{buildSettingDryRun},
            projectConfig = ProjectConfig {
              projectConfigLocalPackages = PackageConfig {packageConfigOptimization}
            }
          }
          ProjectBuildContext {
            elaboratedPlanToExecute = elaboratedPlan,
            elaboratedShared,
            pkgsBuildStatus
          }

  | null pkgs
  = notice verbosity "Up to date"

  | otherwise
  = noticeNoWrap verbosity $ unlines $
      (showBuildProfile ++ "In order, the following " ++ wouldWill ++ " be built" ++
      ifNormal " (use -v for more details)" ++ ":")
    : map showPkgAndReason pkgs

  where
    pkgs = InstallPlan.executionOrder elaboratedPlan

    ifVerbose s | verbosity >= verbose = s
                | otherwise            = ""

    ifNormal s | verbosity >= verbose = ""
               | otherwise            = s

    wouldWill | buildSettingDryRun = "would"
              | otherwise          = "will"

    showPkgAndReason :: ElaboratedReadyPackage -> String
    showPkgAndReason (ReadyPackage elab) =
      " - " ++
      (if verbosity >= deafening
        then display (installedUnitId elab)
        else display (packageId elab)
        ) ++
      (case elabPkgOrComp elab of
          ElabPackage pkg -> showTargets elab ++ ifVerbose (showStanzas pkg)
          ElabComponent comp ->
            " (" ++ showComp elab comp ++ ")"
            ) ++
      showFlagAssignment (nonDefaultFlags elab) ++
      showConfigureFlags elab ++
      let buildStatus = pkgsBuildStatus Map.! installedUnitId elab in
      " (" ++ showBuildStatus buildStatus ++ ")"

    showComp elab comp =
        maybe "custom" display (compComponentName comp) ++
        if Map.null (elabInstantiatedWith elab)
            then ""
            else " with " ++
                intercalate ", "
                    -- TODO: Abbreviate the UnitIds
                    [ display k ++ "=" ++ display v
                    | (k,v) <- Map.toList (elabInstantiatedWith elab) ]

    nonDefaultFlags :: ElaboratedConfiguredPackage -> FlagAssignment
    nonDefaultFlags elab = elabFlagAssignment elab `diffFlagAssignment` elabFlagDefaults elab

    showStanzas pkg = concat
                    $ [ " *test"
                      | TestStanzas  `Set.member` pkgStanzasEnabled pkg ]
                   ++ [ " *bench"
                      | BenchStanzas `Set.member` pkgStanzasEnabled pkg ]

    showTargets elab
      | null (elabBuildTargets elab) = ""
      | otherwise
      = " (" ++ intercalate ", " [ showComponentTarget (packageId elab) t | t <- elabBuildTargets elab ]
             ++ ")"

    showFlagAssignment :: FlagAssignment -> String
    showFlagAssignment = concatMap ((' ' :) . showFlagValue) . unFlagAssignment

    showConfigureFlags elab =
        let fullConfigureFlags
              = setupHsConfigureFlags
                    (ReadyPackage elab)
                    elaboratedShared
                    verbosity
                    "$builddir"
            -- | Given a default value @x@ for a flag, nub @Flag x@
            -- into @NoFlag@.  This gives us a tidier command line
            -- rendering.
            nubFlag :: Eq a => a -> Setup.Flag a -> Setup.Flag a
            nubFlag x (Setup.Flag x') | x == x' = Setup.NoFlag
            nubFlag _ f = f
            -- TODO: Closely logic from 'configureProfiling'.
            tryExeProfiling = Setup.fromFlagOrDefault False
                                (configProf fullConfigureFlags)
            tryLibProfiling = Setup.fromFlagOrDefault False
                                (Mon.mappend (configProf    fullConfigureFlags)
                                             (configProfExe fullConfigureFlags))
            partialConfigureFlags
              = Mon.mempty {
                configProf    =
                    nubFlag False (configProf fullConfigureFlags),
                configProfExe =
                    nubFlag tryExeProfiling (configProfExe fullConfigureFlags),
                configProfLib =
                    nubFlag tryLibProfiling (configProfLib fullConfigureFlags)
                -- Maybe there are more we can add
              }
        -- Not necessary to "escape" it, it's just for user output
        in unwords . ("":) $
            commandShowOptions
            (Setup.configureCommand (pkgConfigCompilerProgs elaboratedShared))
            partialConfigureFlags

    showBuildStatus status = case status of
      BuildStatusPreExisting -> "existing package"
      BuildStatusInstalled   -> "already installed"
      BuildStatusDownload {} -> "requires download & build"
      BuildStatusUnpack   {} -> "requires build"
      BuildStatusRebuild _ rebuild -> case rebuild of
        BuildStatusConfigure
          (MonitoredValueChanged _)   -> "configuration changed"
        BuildStatusConfigure mreason  -> showMonitorChangedReason mreason
        BuildStatusBuild _ buildreason -> case buildreason of
          BuildReasonDepsRebuilt      -> "dependency rebuilt"
          BuildReasonFilesChanged
            mreason                   -> showMonitorChangedReason mreason
          BuildReasonExtraTargets _   -> "additional components to build"
          BuildReasonEphemeralTargets -> "ephemeral targets"
      BuildStatusUpToDate {} -> "up to date" -- doesn't happen

    showMonitorChangedReason (MonitoredFileChanged file) = "file " ++ file ++ " changed"
    showMonitorChangedReason (MonitoredValueChanged _)   = "value changed"
    showMonitorChangedReason  MonitorFirstRun     = "first run"
    showMonitorChangedReason  MonitorCorruptCache = "cannot read state cache"

    showBuildProfile = "Build profile: " ++ unwords [
      "-w " ++ (showCompilerId . pkgConfigCompiler) elaboratedShared,
      "-O" ++  (case packageConfigOptimization of
                Setup.Flag NoOptimisation      -> "0"
                Setup.Flag NormalOptimisation  -> "1"
                Setup.Flag MaximumOptimisation -> "2"
                Setup.NoFlag                   -> "1")]
      ++ "\n"

-- | If there are build failures then report them and throw an exception.
--
dieOnBuildFailures :: Verbosity
                   -> ElaboratedInstallPlan -> BuildOutcomes -> IO ()
dieOnBuildFailures verbosity plan buildOutcomes
  | null failures = return ()

  | isSimpleCase  = exitFailure

  | otherwise = do
      -- For failures where we have a build log, print the log plus a header
       sequence_
         [ do notice verbosity $
                '\n' : renderFailureDetail False pkg reason
                    ++ "\nBuild log ( " ++ logfile ++ " ):"
              readFile logfile >>= noticeNoWrap verbosity
         | (pkg, ShowBuildSummaryAndLog reason logfile)
             <- failuresClassification
         ]

       -- For all failures, print either a short summary (if we showed the
       -- build log) or all details
       die' verbosity $ unlines
         [ case failureClassification of
             ShowBuildSummaryAndLog reason _
               | verbosity > normal
              -> renderFailureDetail mentionDepOf pkg reason

               | otherwise
              -> renderFailureSummary mentionDepOf pkg reason
              ++ ". See the build log above for details."

             ShowBuildSummaryOnly reason ->
               renderFailureDetail mentionDepOf pkg reason

         | let mentionDepOf = verbosity <= normal
         , (pkg, failureClassification) <- failuresClassification ]
  where
    failures =  [ (pkgid, failure)
                | (pkgid, Left failure) <- Map.toList buildOutcomes ]

    failuresClassification =
      [ (pkg, classifyBuildFailure failure)
      | (pkgid, failure) <- failures
      , case buildFailureReason failure of
          DependentFailed {} -> verbosity > normal
          _                  -> True
      , InstallPlan.Configured pkg <-
           maybeToList (InstallPlan.lookup plan pkgid)
      ]

    classifyBuildFailure :: BuildFailure -> BuildFailurePresentation
    classifyBuildFailure BuildFailure {
                           buildFailureReason  = reason,
                           buildFailureLogFile = mlogfile
                         } =
      maybe (ShowBuildSummaryOnly   reason)
            (ShowBuildSummaryAndLog reason) $ do
        logfile <- mlogfile
        e       <- buildFailureException reason
        ExitFailure 1 <- fromException e
        return logfile

    -- Special case: we don't want to report anything complicated in the case
    -- of just doing build on the current package, since it's clear from
    -- context which package failed.
    --
    -- We generalise this rule as follows:
    --  - if only one failure occurs, and it is in a single root package (ie a
    --    package with nothing else depending on it)
    --  - and that failure is of a kind that always reports enough detail
    --    itself (e.g. ghc reporting errors on stdout)
    --  - then we do not report additional error detail or context.
    --
    isSimpleCase
      | [(pkgid, failure)] <- failures
      , [pkg]              <- rootpkgs
      , installedUnitId pkg == pkgid
      , isFailureSelfExplanatory (buildFailureReason failure)
      = True
      | otherwise
      = False

    -- NB: if the Setup script segfaulted or was interrupted,
    -- we should give more detailed information.  So only
    -- assume that exit code 1 is "pedestrian failure."
    isFailureSelfExplanatory (BuildFailed e)
      | Just (ExitFailure 1) <- fromException e = True

    isFailureSelfExplanatory (ConfigureFailed e)
      | Just (ExitFailure 1) <- fromException e = True

    isFailureSelfExplanatory _                  = False

    rootpkgs =
      [ pkg
      | InstallPlan.Configured pkg <- InstallPlan.toList plan
      , hasNoDependents pkg ]

    ultimateDeps pkgid =
        filter (\pkg -> hasNoDependents pkg && installedUnitId pkg /= pkgid)
               (InstallPlan.reverseDependencyClosure plan [pkgid])

    hasNoDependents :: HasUnitId pkg => pkg -> Bool
    hasNoDependents = null . InstallPlan.revDirectDeps plan . installedUnitId

    renderFailureDetail mentionDepOf pkg reason =
        renderFailureSummary mentionDepOf pkg reason ++ "."
     ++ renderFailureExtraDetail reason
     ++ maybe "" showException (buildFailureException reason)

    renderFailureSummary mentionDepOf pkg reason =
        case reason of
          DownloadFailed  _ -> "Failed to download " ++ pkgstr
          UnpackFailed    _ -> "Failed to unpack "   ++ pkgstr
          ConfigureFailed _ -> "Failed to build "    ++ pkgstr
          BuildFailed     _ -> "Failed to build "    ++ pkgstr
          ReplFailed      _ -> "repl failed for "    ++ pkgstr
          HaddocksFailed  _ -> "Failed to build documentation for " ++ pkgstr
          TestsFailed     _ -> "Tests failed for " ++ pkgstr
          BenchFailed     _ -> "Benchmarks failed for " ++ pkgstr
          InstallFailed   _ -> "Failed to build "  ++ pkgstr
          DependentFailed depid
                            -> "Failed to build " ++ display (packageId pkg)
                            ++ " because it depends on " ++ display depid
                            ++ " which itself failed to build"
      where
        pkgstr = elabConfiguredName verbosity pkg
              ++ if mentionDepOf
                   then renderDependencyOf (installedUnitId pkg)
                   else ""

    renderFailureExtraDetail reason =
      case reason of
        ConfigureFailed _ -> " The failure occurred during the configure step."
        InstallFailed   _ -> " The failure occurred during the final install step."
        _                 -> ""

    renderDependencyOf pkgid =
      case ultimateDeps pkgid of
        []         -> ""
        (p1:[])    -> " (which is required by " ++ elabPlanPackageName verbosity p1 ++ ")"
        (p1:p2:[]) -> " (which is required by " ++ elabPlanPackageName verbosity p1
                                     ++ " and " ++ elabPlanPackageName verbosity p2 ++ ")"
        (p1:p2:_)  -> " (which is required by " ++ elabPlanPackageName verbosity p1
                                        ++ ", " ++ elabPlanPackageName verbosity p2
                                        ++ " and others)"

    showException e = case fromException e of
      Just (ExitFailure 1) -> ""

#ifdef MIN_VERSION_unix
      -- Note [Positive "signal" exit code]
      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      -- What's the business with the test for negative and positive
      -- signal values?  The API for process specifies that if the
      -- process died due to a signal, it returns a *negative* exit
      -- code.  So that's the negative test.
      --
      -- What about the positive test?  Well, when we find out that
      -- a process died due to a signal, we ourselves exit with that
      -- exit code.  However, we don't "kill ourselves" with the
      -- signal; we just exit with the same code as the signal: thus
      -- the caller sees a *positive* exit code.  So that's what
      -- happens when we get a positive exit code.
      Just (ExitFailure n)
        | -n == fromIntegral sigSEGV ->
            " The build process segfaulted (i.e. SIGSEGV)."

        |  n == fromIntegral sigSEGV ->
            " The build process terminated with exit code " ++ show n
         ++ " which may be because some part of it segfaulted. (i.e. SIGSEGV)."

        | -n == fromIntegral sigKILL ->
            " The build process was killed (i.e. SIGKILL). " ++ explanation

        |  n == fromIntegral sigKILL ->
            " The build process terminated with exit code " ++ show n
         ++ " which may be because some part of it was killed "
         ++ "(i.e. SIGKILL). " ++ explanation
        where
          explanation = "The typical reason for this is that there is not "
                     ++ "enough memory available (e.g. the OS killed a process "
                     ++ "using lots of memory)."
#endif
      Just (ExitFailure n) ->
        " The build process terminated with exit code " ++ show n

      _ -> " The exception was:\n  "
#if MIN_VERSION_base(4,8,0)
             ++ displayException e
#else
             ++ show e
#endif

    buildFailureException reason =
      case reason of
        DownloadFailed  e -> Just e
        UnpackFailed    e -> Just e
        ConfigureFailed e -> Just e
        BuildFailed     e -> Just e
        ReplFailed      e -> Just e
        HaddocksFailed  e -> Just e
        TestsFailed     e -> Just e
        BenchFailed     e -> Just e
        InstallFailed   e -> Just e
        DependentFailed _ -> Nothing

data BuildFailurePresentation =
       ShowBuildSummaryOnly   BuildFailureReason
     | ShowBuildSummaryAndLog BuildFailureReason FilePath


cmdCommonHelpTextNewBuildBeta :: String
cmdCommonHelpTextNewBuildBeta =
    "Note: this command is part of the new project-based system (aka "
 ++ "nix-style\nlocal builds). These features are currently in beta. "
 ++ "Please see\n"
 ++ "http://cabal.readthedocs.io/en/latest/nix-local-build-overview.html "
 ++ "for\ndetails and advice on what you can expect to work. If you "
 ++ "encounter problems\nplease file issues at "
 ++ "https://github.com/haskell/cabal/issues and if you\nhave any time "
 ++ "to get involved and help with testing, fixing bugs etc then\nthat "
 ++ "is very much appreciated.\n"
