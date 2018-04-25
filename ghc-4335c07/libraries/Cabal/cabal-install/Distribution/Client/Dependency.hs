-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Dependency
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Top level interface to dependency resolution.
-----------------------------------------------------------------------------
module Distribution.Client.Dependency (
    -- * The main package dependency resolver
    chooseSolver,
    resolveDependencies,
    Progress(..),
    foldProgress,

    -- * Alternate, simple resolver that does not do dependencies recursively
    resolveWithoutDependencies,

    -- * Constructing resolver policies
    PackageProperty(..),
    PackageConstraint(..),
    scopeToplevel,
    PackagesPreferenceDefault(..),
    PackagePreference(..),

    -- ** Standard policy
    basicInstallPolicy,
    standardInstallPolicy,
    PackageSpecifier(..),

    -- ** Sandbox policy
    applySandboxInstallPolicy,

    -- ** Extra policy options
    upgradeDependencies,
    reinstallTargets,

    -- ** Policy utils
    addConstraints,
    addPreferences,
    setPreferenceDefault,
    setReorderGoals,
    setCountConflicts,
    setIndependentGoals,
    setAvoidReinstalls,
    setShadowPkgs,
    setStrongFlags,
    setAllowBootLibInstalls,
    setMaxBackjumps,
    setEnableBackjumping,
    setSolveExecutables,
    setGoalOrder,
    setSolverVerbosity,
    removeLowerBounds,
    removeUpperBounds,
    addDefaultSetupDependencies,
    addSetupCabalMinVersionConstraint,
  ) where

import Distribution.Solver.Modular
         ( modularResolver, SolverConfig(..) )
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as InstalledPackageIndex
import Distribution.Client.SolverInstallPlan (SolverInstallPlan)
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.Types
         ( SourcePackageDb(SourcePackageDb)
         , PackageSpecifier(..), pkgSpecifierTarget, pkgSpecifierConstraints
         , UnresolvedPkgLoc, UnresolvedSourcePackage
         , AllowNewer(..), AllowOlder(..), RelaxDeps(..), RelaxedDep(..)
         , RelaxDepScope(..), RelaxDepMod(..), RelaxDepSubject(..), isRelaxDeps
         )
import Distribution.Client.Dependency.Types
         ( PreSolver(..), Solver(..)
         , PackagesPreferenceDefault(..) )
import Distribution.Client.Sandbox.Types
         ( SandboxPackageInfo(..) )
import Distribution.Package
         ( PackageName, mkPackageName, PackageIdentifier(PackageIdentifier), PackageId
         , Package(..), packageName, packageVersion )
import Distribution.Types.Dependency
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Configuration as PD
import Distribution.PackageDescription.Configuration
         ( finalizePD )
import Distribution.Client.PackageUtils
         ( externalBuildDepends )
import Distribution.Compiler
         ( CompilerInfo(..) )
import Distribution.System
         ( Platform )
import Distribution.Client.Utils
         ( duplicates, duplicatesBy, mergeBy, MergeResult(..) )
import Distribution.Simple.Utils
         ( comparing )
import Distribution.Simple.Setup
         ( asBool )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( normal, Verbosity )
import Distribution.Version
import qualified Distribution.Compat.Graph as Graph

import           Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.ConstraintSource
import           Distribution.Solver.Types.DependencyResolver
import           Distribution.Solver.Types.InstalledPreference
import           Distribution.Solver.Types.LabeledPackageConstraint
import           Distribution.Solver.Types.OptionalStanza
import           Distribution.Solver.Types.PackageConstraint
import           Distribution.Solver.Types.PackagePath
import           Distribution.Solver.Types.PackagePreferences
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import           Distribution.Solver.Types.PkgConfigDb (PkgConfigDb)
import           Distribution.Solver.Types.Progress
import           Distribution.Solver.Types.ResolverPackage
import           Distribution.Solver.Types.Settings
import           Distribution.Solver.Types.SolverId
import           Distribution.Solver.Types.SolverPackage
import           Distribution.Solver.Types.SourcePackage
import           Distribution.Solver.Types.Variable

import Data.List
         ( foldl', sort, sortBy, nubBy, maximumBy, intercalate, nub )
import Data.Function (on)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Exception
         ( assert )


-- ------------------------------------------------------------
-- * High level planner policy
-- ------------------------------------------------------------

-- | The set of parameters to the dependency resolver. These parameters are
-- relatively low level but many kinds of high level policies can be
-- implemented in terms of adjustments to the parameters.
--
data DepResolverParams = DepResolverParams {
       depResolverTargets           :: Set PackageName,
       depResolverConstraints       :: [LabeledPackageConstraint],
       depResolverPreferences       :: [PackagePreference],
       depResolverPreferenceDefault :: PackagesPreferenceDefault,
       depResolverInstalledPkgIndex :: InstalledPackageIndex,
       depResolverSourcePkgIndex    :: PackageIndex.PackageIndex UnresolvedSourcePackage,
       depResolverReorderGoals      :: ReorderGoals,
       depResolverCountConflicts    :: CountConflicts,
       depResolverIndependentGoals  :: IndependentGoals,
       depResolverAvoidReinstalls   :: AvoidReinstalls,
       depResolverShadowPkgs        :: ShadowPkgs,
       depResolverStrongFlags       :: StrongFlags,

       -- | Whether to allow base and its dependencies to be installed.
       depResolverAllowBootLibInstalls :: AllowBootLibInstalls,

       depResolverMaxBackjumps      :: Maybe Int,
       depResolverEnableBackjumping :: EnableBackjumping,
       -- | Whether or not to solve for dependencies on executables.
       -- This should be true, except in the legacy code path where
       -- we can't tell if an executable has been installed or not,
       -- so we shouldn't solve for them.  See #3875.
       depResolverSolveExecutables  :: SolveExecutables,

       -- | Function to override the solver's goal-ordering heuristics.
       depResolverGoalOrder         :: Maybe (Variable QPN -> Variable QPN -> Ordering),
       depResolverVerbosity         :: Verbosity
     }

showDepResolverParams :: DepResolverParams -> String
showDepResolverParams p =
     "targets: " ++ intercalate ", " (map display $ Set.toList (depResolverTargets p))
  ++ "\nconstraints: "
  ++   concatMap (("\n  " ++) . showLabeledConstraint)
       (depResolverConstraints p)
  ++ "\npreferences: "
  ++   concatMap (("\n  " ++) . showPackagePreference)
       (depResolverPreferences p)
  ++ "\nstrategy: "          ++ show (depResolverPreferenceDefault        p)
  ++ "\nreorder goals: "     ++ show (asBool (depResolverReorderGoals     p))
  ++ "\ncount conflicts: "   ++ show (asBool (depResolverCountConflicts   p))
  ++ "\nindependent goals: " ++ show (asBool (depResolverIndependentGoals p))
  ++ "\navoid reinstalls: "  ++ show (asBool (depResolverAvoidReinstalls  p))
  ++ "\nshadow packages: "   ++ show (asBool (depResolverShadowPkgs       p))
  ++ "\nstrong flags: "      ++ show (asBool (depResolverStrongFlags      p))
  ++ "\nallow boot library installs: " ++ show (asBool (depResolverAllowBootLibInstalls p))
  ++ "\nmax backjumps: "     ++ maybe "infinite" show
                                     (depResolverMaxBackjumps             p)
  where
    showLabeledConstraint :: LabeledPackageConstraint -> String
    showLabeledConstraint (LabeledPackageConstraint pc src) =
        showPackageConstraint pc ++ " (" ++ showConstraintSource src ++ ")"

-- | A package selection preference for a particular package.
--
-- Preferences are soft constraints that the dependency resolver should try to
-- respect where possible. It is not specified if preferences on some packages
-- are more important than others.
--
data PackagePreference =

     -- | A suggested constraint on the version number.
     PackageVersionPreference   PackageName VersionRange

     -- | If we prefer versions of packages that are already installed.
   | PackageInstalledPreference PackageName InstalledPreference

     -- | If we would prefer to enable these optional stanzas
     -- (i.e. test suites and/or benchmarks)
   | PackageStanzasPreference   PackageName [OptionalStanza]


-- | Provide a textual representation of a package preference
-- for debugging purposes.
--
showPackagePreference :: PackagePreference -> String
showPackagePreference (PackageVersionPreference   pn vr) =
  display pn ++ " " ++ display (simplifyVersionRange vr)
showPackagePreference (PackageInstalledPreference pn ip) =
  display pn ++ " " ++ show ip
showPackagePreference (PackageStanzasPreference pn st) =
  display pn ++ " " ++ show st

basicDepResolverParams :: InstalledPackageIndex
                       -> PackageIndex.PackageIndex UnresolvedSourcePackage
                       -> DepResolverParams
basicDepResolverParams installedPkgIndex sourcePkgIndex =
    DepResolverParams {
       depResolverTargets           = Set.empty,
       depResolverConstraints       = [],
       depResolverPreferences       = [],
       depResolverPreferenceDefault = PreferLatestForSelected,
       depResolverInstalledPkgIndex = installedPkgIndex,
       depResolverSourcePkgIndex    = sourcePkgIndex,
       depResolverReorderGoals      = ReorderGoals False,
       depResolverCountConflicts    = CountConflicts True,
       depResolverIndependentGoals  = IndependentGoals False,
       depResolverAvoidReinstalls   = AvoidReinstalls False,
       depResolverShadowPkgs        = ShadowPkgs False,
       depResolverStrongFlags       = StrongFlags False,
       depResolverAllowBootLibInstalls = AllowBootLibInstalls False,
       depResolverMaxBackjumps      = Nothing,
       depResolverEnableBackjumping = EnableBackjumping True,
       depResolverSolveExecutables  = SolveExecutables True,
       depResolverGoalOrder         = Nothing,
       depResolverVerbosity         = normal
     }

addTargets :: [PackageName]
           -> DepResolverParams -> DepResolverParams
addTargets extraTargets params =
    params {
      depResolverTargets = Set.fromList extraTargets `Set.union` depResolverTargets params
    }

addConstraints :: [LabeledPackageConstraint]
               -> DepResolverParams -> DepResolverParams
addConstraints extraConstraints params =
    params {
      depResolverConstraints = extraConstraints
                            ++ depResolverConstraints params
    }

addPreferences :: [PackagePreference]
               -> DepResolverParams -> DepResolverParams
addPreferences extraPreferences params =
    params {
      depResolverPreferences = extraPreferences
                            ++ depResolverPreferences params
    }

setPreferenceDefault :: PackagesPreferenceDefault
                     -> DepResolverParams -> DepResolverParams
setPreferenceDefault preferenceDefault params =
    params {
      depResolverPreferenceDefault = preferenceDefault
    }

setReorderGoals :: ReorderGoals -> DepResolverParams -> DepResolverParams
setReorderGoals reorder params =
    params {
      depResolverReorderGoals = reorder
    }

setCountConflicts :: CountConflicts -> DepResolverParams -> DepResolverParams
setCountConflicts count params =
    params {
      depResolverCountConflicts = count
    }

setIndependentGoals :: IndependentGoals -> DepResolverParams -> DepResolverParams
setIndependentGoals indep params =
    params {
      depResolverIndependentGoals = indep
    }

setAvoidReinstalls :: AvoidReinstalls -> DepResolverParams -> DepResolverParams
setAvoidReinstalls avoid params =
    params {
      depResolverAvoidReinstalls = avoid
    }

setShadowPkgs :: ShadowPkgs -> DepResolverParams -> DepResolverParams
setShadowPkgs shadow params =
    params {
      depResolverShadowPkgs = shadow
    }

setStrongFlags :: StrongFlags -> DepResolverParams -> DepResolverParams
setStrongFlags sf params =
    params {
      depResolverStrongFlags = sf
    }

setAllowBootLibInstalls :: AllowBootLibInstalls -> DepResolverParams -> DepResolverParams
setAllowBootLibInstalls i params =
    params {
      depResolverAllowBootLibInstalls = i
    }

setMaxBackjumps :: Maybe Int -> DepResolverParams -> DepResolverParams
setMaxBackjumps n params =
    params {
      depResolverMaxBackjumps = n
    }

setEnableBackjumping :: EnableBackjumping -> DepResolverParams -> DepResolverParams
setEnableBackjumping b params =
    params {
      depResolverEnableBackjumping = b
    }

setSolveExecutables :: SolveExecutables -> DepResolverParams -> DepResolverParams
setSolveExecutables b params =
    params {
      depResolverSolveExecutables = b
    }

setGoalOrder :: Maybe (Variable QPN -> Variable QPN -> Ordering)
             -> DepResolverParams
             -> DepResolverParams
setGoalOrder order params =
    params {
      depResolverGoalOrder = order
    }

setSolverVerbosity :: Verbosity -> DepResolverParams -> DepResolverParams
setSolverVerbosity verbosity params =
    params {
      depResolverVerbosity = verbosity
    }

-- | Some packages are specific to a given compiler version and should never be
-- upgraded.
dontUpgradeNonUpgradeablePackages :: DepResolverParams -> DepResolverParams
dontUpgradeNonUpgradeablePackages params =
    addConstraints extraConstraints params
  where
    extraConstraints =
      [ LabeledPackageConstraint
        (PackageConstraint (ScopeAnyQualifier pkgname) PackagePropertyInstalled)
        ConstraintSourceNonUpgradeablePackage
      | Set.notMember (mkPackageName "base") (depResolverTargets params)
      -- If you change this enumeration, make sure to update the list in
      -- "Distribution.Solver.Modular.Solver" as well
      , pkgname <- [ mkPackageName "base"
                   , mkPackageName "ghc-prim"
                   , mkPackageName "integer-gmp"
                   , mkPackageName "integer-simple"
                   , mkPackageName "template-haskell"
                   ]
      , isInstalled pkgname ]

    isInstalled = not . null
                . InstalledPackageIndex.lookupPackageName
                                 (depResolverInstalledPkgIndex params)

addSourcePackages :: [UnresolvedSourcePackage]
                  -> DepResolverParams -> DepResolverParams
addSourcePackages pkgs params =
    params {
      depResolverSourcePkgIndex =
        foldl (flip PackageIndex.insert)
              (depResolverSourcePkgIndex params) pkgs
    }

hideInstalledPackagesSpecificBySourcePackageId :: [PackageId]
                                                  -> DepResolverParams
                                                  -> DepResolverParams
hideInstalledPackagesSpecificBySourcePackageId pkgids params =
    --TODO: this should work using exclude constraints instead
    params {
      depResolverInstalledPkgIndex =
        foldl' (flip InstalledPackageIndex.deleteSourcePackageId)
               (depResolverInstalledPkgIndex params) pkgids
    }

hideInstalledPackagesAllVersions :: [PackageName]
                                 -> DepResolverParams -> DepResolverParams
hideInstalledPackagesAllVersions pkgnames params =
    --TODO: this should work using exclude constraints instead
    params {
      depResolverInstalledPkgIndex =
        foldl' (flip InstalledPackageIndex.deletePackageName)
               (depResolverInstalledPkgIndex params) pkgnames
    }


-- | Remove upper bounds in dependencies using the policy specified by the
-- 'AllowNewer' argument (all/some/none).
--
-- Note: It's important to apply 'removeUpperBounds' after
-- 'addSourcePackages'. Otherwise, the packages inserted by
-- 'addSourcePackages' won't have upper bounds in dependencies relaxed.
--
removeUpperBounds :: AllowNewer -> DepResolverParams -> DepResolverParams
removeUpperBounds (AllowNewer relDeps) = removeBounds RelaxUpper relDeps

-- | Dual of 'removeUpperBounds'
removeLowerBounds :: AllowOlder -> DepResolverParams -> DepResolverParams
removeLowerBounds (AllowOlder relDeps) = removeBounds RelaxLower relDeps

data RelaxKind = RelaxLower | RelaxUpper

-- | Common internal implementation of 'removeLowerBounds'/'removeUpperBounds'
removeBounds :: RelaxKind -> RelaxDeps -> DepResolverParams -> DepResolverParams
removeBounds _ rd params | not (isRelaxDeps rd) = params -- no-op optimisation
removeBounds  relKind relDeps            params =
    params {
      depResolverSourcePkgIndex = sourcePkgIndex'
    }
  where
    sourcePkgIndex' = fmap relaxDeps $ depResolverSourcePkgIndex params

    relaxDeps :: UnresolvedSourcePackage -> UnresolvedSourcePackage
    relaxDeps srcPkg = srcPkg {
      packageDescription = relaxPackageDeps relKind relDeps
                           (packageDescription srcPkg)
      }

-- | Relax the dependencies of this package if needed.
--
-- Helper function used by 'removeBounds'
relaxPackageDeps :: RelaxKind
                 -> RelaxDeps
                 -> PD.GenericPackageDescription -> PD.GenericPackageDescription
relaxPackageDeps _ rd gpd | not (isRelaxDeps rd) = gpd -- subsumed by no-op case in 'removeBounds'
relaxPackageDeps relKind RelaxDepsAll  gpd = PD.transformAllBuildDepends relaxAll gpd
  where
    relaxAll :: Dependency -> Dependency
    relaxAll (Dependency pkgName verRange) =
        Dependency pkgName (removeBound relKind RelaxDepModNone verRange)

relaxPackageDeps relKind (RelaxDepsSome depsToRelax0) gpd =
  PD.transformAllBuildDepends relaxSome gpd
  where
    thisPkgName    = packageName gpd
    thisPkgId      = packageId   gpd
    depsToRelax    = Map.fromList $ mapMaybe f depsToRelax0

    f :: RelaxedDep -> Maybe (RelaxDepSubject,RelaxDepMod)
    f (RelaxedDep scope rdm p) = case scope of
      RelaxDepScopeAll        -> Just (p,rdm)
      RelaxDepScopePackage p0
          | p0 == thisPkgName -> Just (p,rdm)
          | otherwise         -> Nothing
      RelaxDepScopePackageId p0
          | p0 == thisPkgId   -> Just (p,rdm)
          | otherwise         -> Nothing

    relaxSome :: Dependency -> Dependency
    relaxSome d@(Dependency depName verRange)
        | Just relMod <- Map.lookup RelaxDepSubjectAll depsToRelax =
            -- a '*'-subject acts absorbing, for consistency with
            -- the 'Semigroup RelaxDeps' instance
            Dependency depName (removeBound relKind relMod verRange)
        | Just relMod <- Map.lookup (RelaxDepSubjectPkg depName) depsToRelax =
            Dependency depName (removeBound relKind relMod verRange)
        | otherwise = d -- no-op

-- | Internal helper for 'relaxPackageDeps'
removeBound :: RelaxKind -> RelaxDepMod -> VersionRange -> VersionRange
removeBound RelaxLower RelaxDepModNone = removeLowerBound
removeBound RelaxUpper RelaxDepModNone = removeUpperBound
removeBound relKind RelaxDepModCaret = hyloVersionRange embed projectVersionRange
  where
    embed (MajorBoundVersionF v) = caretTransformation v (majorUpperBound v)
    embed vr                     = embedVersionRange vr

    -- This function is the interesting part as it defines the meaning
    -- of 'RelaxDepModCaret', i.e. to transform only @^>=@ constraints;
    caretTransformation l u = case relKind of
      RelaxUpper -> orLaterVersion l -- rewrite @^>= x.y.z@ into @>= x.y.z@
      RelaxLower -> earlierVersion u -- rewrite @^>= x.y.z@ into @< x.(y+1)@

-- | Supply defaults for packages without explicit Setup dependencies
--
-- Note: It's important to apply 'addDefaultSetupDepends' after
-- 'addSourcePackages'. Otherwise, the packages inserted by
-- 'addSourcePackages' won't have upper bounds in dependencies relaxed.
--
addDefaultSetupDependencies :: (UnresolvedSourcePackage -> Maybe [Dependency])
                            -> DepResolverParams -> DepResolverParams
addDefaultSetupDependencies defaultSetupDeps params =
    params {
      depResolverSourcePkgIndex =
        fmap applyDefaultSetupDeps (depResolverSourcePkgIndex params)
    }
  where
    applyDefaultSetupDeps :: UnresolvedSourcePackage -> UnresolvedSourcePackage
    applyDefaultSetupDeps srcpkg =
        srcpkg {
          packageDescription = gpkgdesc {
            PD.packageDescription = pkgdesc {
              PD.setupBuildInfo =
                case PD.setupBuildInfo pkgdesc of
                  Just sbi -> Just sbi
                  Nothing  -> case defaultSetupDeps srcpkg of
                    Nothing -> Nothing
                    Just deps -> Just PD.SetupBuildInfo {
                      PD.defaultSetupDepends = True,
                      PD.setupDepends        = deps
                    }
            }
          }
        }
      where
        gpkgdesc = packageDescription srcpkg
        pkgdesc  = PD.packageDescription gpkgdesc

-- | If a package has a custom setup then we need to add a setup-depends
-- on Cabal.
--
addSetupCabalMinVersionConstraint :: Version
                                  -> DepResolverParams -> DepResolverParams
addSetupCabalMinVersionConstraint minVersion =
    addConstraints
      [ LabeledPackageConstraint
          (PackageConstraint (ScopeAnySetupQualifier cabalPkgname)
                             (PackagePropertyVersion $ orLaterVersion minVersion))
          ConstraintSetupCabalMinVersion
      ]
  where
    cabalPkgname = mkPackageName "Cabal"


upgradeDependencies :: DepResolverParams -> DepResolverParams
upgradeDependencies = setPreferenceDefault PreferAllLatest


reinstallTargets :: DepResolverParams -> DepResolverParams
reinstallTargets params =
    hideInstalledPackagesAllVersions (Set.toList $ depResolverTargets params) params


-- | A basic solver policy on which all others are built.
--
basicInstallPolicy :: InstalledPackageIndex
                   -> SourcePackageDb
                   -> [PackageSpecifier UnresolvedSourcePackage]
                   -> DepResolverParams
basicInstallPolicy
    installedPkgIndex (SourcePackageDb sourcePkgIndex sourcePkgPrefs)
    pkgSpecifiers

  = addPreferences
      [ PackageVersionPreference name ver
      | (name, ver) <- Map.toList sourcePkgPrefs ]

  . addConstraints
      (concatMap pkgSpecifierConstraints pkgSpecifiers)

  . addTargets
      (map pkgSpecifierTarget pkgSpecifiers)

  . hideInstalledPackagesSpecificBySourcePackageId
      [ packageId pkg | SpecificSourcePackage pkg <- pkgSpecifiers ]

  . addSourcePackages
      [ pkg  | SpecificSourcePackage pkg <- pkgSpecifiers ]

  $ basicDepResolverParams
      installedPkgIndex sourcePkgIndex


-- | The policy used by all the standard commands, install, fetch, freeze etc
-- (but not the new-build and related commands).
--
-- It extends the 'basicInstallPolicy' with a policy on setup deps.
--
standardInstallPolicy :: InstalledPackageIndex
                      -> SourcePackageDb
                      -> [PackageSpecifier UnresolvedSourcePackage]
                      -> DepResolverParams
standardInstallPolicy installedPkgIndex sourcePkgDb pkgSpecifiers

  = addDefaultSetupDependencies mkDefaultSetupDeps

  $ basicInstallPolicy
      installedPkgIndex sourcePkgDb pkgSpecifiers

    where
      -- Force Cabal >= 1.24 dep when the package is affected by #3199.
      mkDefaultSetupDeps :: UnresolvedSourcePackage -> Maybe [Dependency]
      mkDefaultSetupDeps srcpkg | affected        =
        Just [Dependency (mkPackageName "Cabal")
              (orLaterVersion $ mkVersion [1,24])]
                                | otherwise       = Nothing
        where
          gpkgdesc = packageDescription srcpkg
          pkgdesc  = PD.packageDescription gpkgdesc
          bt       = fromMaybe PD.Custom (PD.buildType pkgdesc)
          affected = bt == PD.Custom && hasBuildableFalse gpkgdesc

      -- Does this package contain any components with non-empty 'build-depends'
      -- and a 'buildable' field that could potentially be set to 'False'? False
      -- positives are possible.
      hasBuildableFalse :: PD.GenericPackageDescription -> Bool
      hasBuildableFalse gpkg =
        not (all alwaysTrue (zipWith PD.cOr buildableConditions noDepConditions))
        where
          buildableConditions      = PD.extractConditions PD.buildable gpkg
          noDepConditions          = PD.extractConditions
                                     (null . PD.targetBuildDepends)    gpkg
          alwaysTrue (PD.Lit True) = True
          alwaysTrue _             = False


applySandboxInstallPolicy :: SandboxPackageInfo
                             -> DepResolverParams
                             -> DepResolverParams
applySandboxInstallPolicy
  (SandboxPackageInfo modifiedDeps otherDeps allSandboxPkgs _allDeps)
  params

  = addPreferences [ PackageInstalledPreference n PreferInstalled
                   | n <- installedNotModified ]

  . addTargets installedNotModified

  . addPreferences
      [ PackageVersionPreference (packageName pkg)
        (thisVersion (packageVersion pkg)) | pkg <- otherDeps ]

  . addConstraints
      [ let pc = PackageConstraint
                 (scopeToplevel $ packageName pkg)
                 (PackagePropertyVersion $ thisVersion (packageVersion pkg))
        in LabeledPackageConstraint pc ConstraintSourceModifiedAddSourceDep
      | pkg <- modifiedDeps ]

  . addTargets [ packageName pkg | pkg <- modifiedDeps ]

  . hideInstalledPackagesSpecificBySourcePackageId
      [ packageId pkg | pkg <- modifiedDeps ]

  -- We don't need to add source packages for add-source deps to the
  -- 'installedPkgIndex' since 'getSourcePackages' did that for us.

  $ params

  where
    installedPkgIds =
      map fst . InstalledPackageIndex.allPackagesBySourcePackageId
      $ allSandboxPkgs
    modifiedPkgIds       = map packageId modifiedDeps
    installedNotModified = [ packageName pkg | pkg <- installedPkgIds,
                             pkg `notElem` modifiedPkgIds ]

-- ------------------------------------------------------------
-- * Interface to the standard resolver
-- ------------------------------------------------------------

chooseSolver :: Verbosity -> PreSolver -> CompilerInfo -> IO Solver
chooseSolver _verbosity preSolver _cinfo =
    case preSolver of
      AlwaysModular -> do
        return Modular

runSolver :: Solver -> SolverConfig -> DependencyResolver UnresolvedPkgLoc
runSolver Modular = modularResolver

-- | Run the dependency solver.
--
-- Since this is potentially an expensive operation, the result is wrapped in a
-- a 'Progress' structure that can be unfolded to provide progress information,
-- logging messages and the final result or an error.
--
resolveDependencies :: Platform
                    -> CompilerInfo
                    -> PkgConfigDb
                    -> Solver
                    -> DepResolverParams
                    -> Progress String String SolverInstallPlan

    --TODO: is this needed here? see dontUpgradeNonUpgradeablePackages
resolveDependencies platform comp _pkgConfigDB _solver params
  | Set.null (depResolverTargets params)
  = return (validateSolverResult platform comp indGoals [])
  where
    indGoals = depResolverIndependentGoals params

resolveDependencies platform comp pkgConfigDB solver params =

    Step (showDepResolverParams finalparams)
  $ fmap (validateSolverResult platform comp indGoals)
  $ runSolver solver (SolverConfig reordGoals cntConflicts
                      indGoals noReinstalls
                      shadowing strFlags allowBootLibs maxBkjumps enableBj
                      solveExes order verbosity)
                     platform comp installedPkgIndex sourcePkgIndex
                     pkgConfigDB preferences constraints targets
  where

    finalparams @ (DepResolverParams
      targets constraints
      prefs defpref
      installedPkgIndex
      sourcePkgIndex
      reordGoals
      cntConflicts
      indGoals
      noReinstalls
      shadowing
      strFlags
      allowBootLibs
      maxBkjumps
      enableBj
      solveExes
      order
      verbosity) =
        if asBool (depResolverAllowBootLibInstalls params)
        then params
        else dontUpgradeNonUpgradeablePackages params

    preferences = interpretPackagesPreference targets defpref prefs


-- | Give an interpretation to the global 'PackagesPreference' as
--  specific per-package 'PackageVersionPreference'.
--
interpretPackagesPreference :: Set PackageName
                            -> PackagesPreferenceDefault
                            -> [PackagePreference]
                            -> (PackageName -> PackagePreferences)
interpretPackagesPreference selected defaultPref prefs =
  \pkgname -> PackagePreferences (versionPref pkgname)
                                 (installPref pkgname)
                                 (stanzasPref pkgname)
  where
    versionPref pkgname =
      fromMaybe [anyVersion] (Map.lookup pkgname versionPrefs)
    versionPrefs = Map.fromListWith (++)
                   [(pkgname, [pref])
                   | PackageVersionPreference pkgname pref <- prefs]

    installPref pkgname =
      fromMaybe (installPrefDefault pkgname) (Map.lookup pkgname installPrefs)
    installPrefs = Map.fromList
      [ (pkgname, pref)
      | PackageInstalledPreference pkgname pref <- prefs ]
    installPrefDefault = case defaultPref of
      PreferAllLatest         -> const PreferLatest
      PreferAllInstalled      -> const PreferInstalled
      PreferLatestForSelected -> \pkgname ->
        -- When you say cabal install foo, what you really mean is, prefer the
        -- latest version of foo, but the installed version of everything else
        if pkgname `Set.member` selected then PreferLatest
                                         else PreferInstalled

    stanzasPref pkgname =
      fromMaybe [] (Map.lookup pkgname stanzasPrefs)
    stanzasPrefs = Map.fromListWith (\a b -> nub (a ++ b))
      [ (pkgname, pref)
      | PackageStanzasPreference pkgname pref <- prefs ]


-- ------------------------------------------------------------
-- * Checking the result of the solver
-- ------------------------------------------------------------

-- | Make an install plan from the output of the dep resolver.
-- It checks that the plan is valid, or it's an error in the dep resolver.
--
validateSolverResult :: Platform
                     -> CompilerInfo
                     -> IndependentGoals
                     -> [ResolverPackage UnresolvedPkgLoc]
                     -> SolverInstallPlan
validateSolverResult platform comp indepGoals pkgs =
    case planPackagesProblems platform comp pkgs of
      [] -> case SolverInstallPlan.new indepGoals graph of
              Right plan     -> plan
              Left  problems -> error (formatPlanProblems problems)
      problems               -> error (formatPkgProblems problems)

  where
    graph = Graph.fromDistinctList pkgs

    formatPkgProblems  = formatProblemMessage . map showPlanPackageProblem
    formatPlanProblems = formatProblemMessage . map SolverInstallPlan.showPlanProblem

    formatProblemMessage problems =
      unlines $
        "internal error: could not construct a valid install plan."
      : "The proposed (invalid) plan contained the following problems:"
      : problems
      ++ "Proposed plan:"
      : [SolverInstallPlan.showPlanIndex pkgs]


data PlanPackageProblem =
       InvalidConfiguredPackage (SolverPackage UnresolvedPkgLoc)
                                [PackageProblem]
     | DuplicatePackageSolverId SolverId [ResolverPackage UnresolvedPkgLoc]

showPlanPackageProblem :: PlanPackageProblem -> String
showPlanPackageProblem (InvalidConfiguredPackage pkg packageProblems) =
     "Package " ++ display (packageId pkg)
  ++ " has an invalid configuration, in particular:\n"
  ++ unlines [ "  " ++ showPackageProblem problem
             | problem <- packageProblems ]
showPlanPackageProblem (DuplicatePackageSolverId pid dups) =
     "Package " ++ display (packageId pid) ++ " has "
  ++ show (length dups) ++ " duplicate instances."

planPackagesProblems :: Platform -> CompilerInfo
                     -> [ResolverPackage UnresolvedPkgLoc]
                     -> [PlanPackageProblem]
planPackagesProblems platform cinfo pkgs =
     [ InvalidConfiguredPackage pkg packageProblems
     | Configured pkg <- pkgs
     , let packageProblems = configuredPackageProblems platform cinfo pkg
     , not (null packageProblems) ]
  ++ [ DuplicatePackageSolverId (Graph.nodeKey (head dups)) dups
     | dups <- duplicatesBy (comparing Graph.nodeKey) pkgs ]

data PackageProblem = DuplicateFlag PD.FlagName
                    | MissingFlag   PD.FlagName
                    | ExtraFlag     PD.FlagName
                    | DuplicateDeps [PackageId]
                    | MissingDep    Dependency
                    | ExtraDep      PackageId
                    | InvalidDep    Dependency PackageId

showPackageProblem :: PackageProblem -> String
showPackageProblem (DuplicateFlag flag) =
  "duplicate flag in the flag assignment: " ++ PD.unFlagName flag

showPackageProblem (MissingFlag flag) =
  "missing an assignment for the flag: " ++ PD.unFlagName flag

showPackageProblem (ExtraFlag flag) =
  "extra flag given that is not used by the package: " ++ PD.unFlagName flag

showPackageProblem (DuplicateDeps pkgids) =
     "duplicate packages specified as selected dependencies: "
  ++ intercalate ", " (map display pkgids)

showPackageProblem (MissingDep dep) =
     "the package has a dependency " ++ display dep
  ++ " but no package has been selected to satisfy it."

showPackageProblem (ExtraDep pkgid) =
     "the package configuration specifies " ++ display pkgid
  ++ " but (with the given flag assignment) the package does not actually"
  ++ " depend on any version of that package."

showPackageProblem (InvalidDep dep pkgid) =
     "the package depends on " ++ display dep
  ++ " but the configuration specifies " ++ display pkgid
  ++ " which does not satisfy the dependency."

-- | A 'ConfiguredPackage' is valid if the flag assignment is total and if
-- in the configuration given by the flag assignment, all the package
-- dependencies are satisfied by the specified packages.
--
configuredPackageProblems :: Platform -> CompilerInfo
                          -> SolverPackage UnresolvedPkgLoc -> [PackageProblem]
configuredPackageProblems platform cinfo
  (SolverPackage pkg specifiedFlags stanzas specifiedDeps' _specifiedExeDeps') =
     -- FIXME/TODO: FlagAssignment ought to be duplicate-free as internal invariant
     [ DuplicateFlag flag | ((flag,_):_) <- duplicates (PD.unFlagAssignment specifiedFlags) ]
  ++ [ MissingFlag flag | OnlyInLeft  flag <- mergedFlags ]
  ++ [ ExtraFlag   flag | OnlyInRight flag <- mergedFlags ]
  ++ [ DuplicateDeps pkgs
     | pkgs <- CD.nonSetupDeps (fmap (duplicatesBy (comparing packageName))
                                specifiedDeps) ]
  ++ [ MissingDep dep       | OnlyInLeft  dep       <- mergedDeps ]
  ++ [ ExtraDep       pkgid | OnlyInRight     pkgid <- mergedDeps ]
  ++ [ InvalidDep dep pkgid | InBoth      dep pkgid <- mergedDeps
                            , not (packageSatisfiesDependency pkgid dep) ]
  -- TODO: sanity tests on executable deps
  where
    specifiedDeps :: ComponentDeps [PackageId]
    specifiedDeps = fmap (map solverSrcId) specifiedDeps'

    mergedFlags = mergeBy compare
      (sort $ map PD.flagName (PD.genPackageFlags (packageDescription pkg)))
      (sort $ map fst (PD.unFlagAssignment specifiedFlags)) -- TODO

    packageSatisfiesDependency
      (PackageIdentifier name  version)
      (Dependency        name' versionRange) = assert (name == name') $
        version `withinRange` versionRange

    dependencyName (Dependency name _) = name

    mergedDeps :: [MergeResult Dependency PackageId]
    mergedDeps = mergeDeps requiredDeps (CD.flatDeps specifiedDeps)

    mergeDeps :: [Dependency] -> [PackageId]
              -> [MergeResult Dependency PackageId]
    mergeDeps required specified =
      let sortNubOn f = nubBy ((==) `on` f) . sortBy (compare `on` f) in
      mergeBy
        (\dep pkgid -> dependencyName dep `compare` packageName pkgid)
        (sortNubOn dependencyName required)
        (sortNubOn packageName    specified)

    -- TODO: It would be nicer to use ComponentDeps here so we can be more
    -- precise in our checks. That's a bit tricky though, as this currently
    -- relies on the 'buildDepends' field of 'PackageDescription'. (OTOH, that
    -- field is deprecated and should be removed anyway.)  As long as we _do_
    -- use a flat list here, we have to allow for duplicates when we fold
    -- specifiedDeps; once we have proper ComponentDeps here we should get rid
    -- of the `nubOn` in `mergeDeps`.
    requiredDeps :: [Dependency]
    requiredDeps =
      --TODO: use something lower level than finalizePD
      case finalizePD specifiedFlags
         (enableStanzas stanzas)
         (const True)
         platform cinfo
         []
         (packageDescription pkg) of
        Right (resolvedPkg, _) ->
             externalBuildDepends resolvedPkg
          ++ maybe [] PD.setupDepends (PD.setupBuildInfo resolvedPkg)
        Left  _ ->
          error "configuredPackageInvalidDeps internal error"


-- ------------------------------------------------------------
-- * Simple resolver that ignores dependencies
-- ------------------------------------------------------------

-- | A simplistic method of resolving a list of target package names to
-- available packages.
--
-- Specifically, it does not consider package dependencies at all. Unlike
-- 'resolveDependencies', no attempt is made to ensure that the selected
-- packages have dependencies that are satisfiable or consistent with
-- each other.
--
-- It is suitable for tasks such as selecting packages to download for user
-- inspection. It is not suitable for selecting packages to install.
--
-- Note: if no installed package index is available, it is OK to pass 'mempty'.
-- It simply means preferences for installed packages will be ignored.
--
resolveWithoutDependencies :: DepResolverParams
                           -> Either [ResolveNoDepsError] [UnresolvedSourcePackage]
resolveWithoutDependencies (DepResolverParams targets constraints
                              prefs defpref installedPkgIndex sourcePkgIndex
                              _reorderGoals _countConflicts _indGoals _avoidReinstalls
                              _shadowing _strFlags _maxBjumps _enableBj
                              _solveExes _allowBootLibInstalls _order _verbosity) =
    collectEithers $ map selectPackage (Set.toList targets)
  where
    selectPackage :: PackageName -> Either ResolveNoDepsError UnresolvedSourcePackage
    selectPackage pkgname
      | null choices = Left  $! ResolveUnsatisfiable pkgname requiredVersions
      | otherwise    = Right $! maximumBy bestByPrefs choices

      where
        -- Constraints
        requiredVersions = packageConstraints pkgname
        pkgDependency    = Dependency pkgname requiredVersions
        choices          = PackageIndex.lookupDependency sourcePkgIndex
                                                         pkgDependency

        -- Preferences
        PackagePreferences preferredVersions preferInstalled _
          = packagePreferences pkgname

        bestByPrefs   = comparing $ \pkg ->
                          (installPref pkg, versionPref pkg, packageVersion pkg)
        installPref   = case preferInstalled of
          PreferLatest    -> const False
          PreferInstalled -> not . null
                           . InstalledPackageIndex.lookupSourcePackageId
                                                     installedPkgIndex
                           . packageId
        versionPref pkg = length . filter (packageVersion pkg `withinRange`) $
                          preferredVersions

    packageConstraints :: PackageName -> VersionRange
    packageConstraints pkgname =
      Map.findWithDefault anyVersion pkgname packageVersionConstraintMap
    packageVersionConstraintMap =
      let pcs = map unlabelPackageConstraint constraints
      in Map.fromList [ (scopeToPackageName scope, range)
                      | PackageConstraint
                          scope (PackagePropertyVersion range) <- pcs ]

    packagePreferences :: PackageName -> PackagePreferences
    packagePreferences = interpretPackagesPreference targets defpref prefs


collectEithers :: [Either a b] -> Either [a] [b]
collectEithers = collect . partitionEithers
  where
    collect ([], xs) = Right xs
    collect (errs,_) = Left errs
    partitionEithers :: [Either a b] -> ([a],[b])
    partitionEithers = foldr (either left right) ([],[])
     where
       left  a (l, r) = (a:l, r)
       right a (l, r) = (l, a:r)

-- | Errors for 'resolveWithoutDependencies'.
--
data ResolveNoDepsError =

     -- | A package name which cannot be resolved to a specific package.
     -- Also gives the constraint on the version and whether there was
     -- a constraint on the package being installed.
     ResolveUnsatisfiable PackageName VersionRange

instance Show ResolveNoDepsError where
  show (ResolveUnsatisfiable name ver) =
       "There is no available version of " ++ display name
    ++ " that satisfies " ++ display (simplifyVersionRange ver)
