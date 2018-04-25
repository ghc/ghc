{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | DSL for testing the modular solver
module UnitTests.Distribution.Solver.Modular.DSL (
    ExampleDependency(..)
  , Dependencies(..)
  , ExTest(..)
  , ExExe(..)
  , ExConstraint(..)
  , ExPreference(..)
  , ExampleDb
  , ExampleVersionRange
  , ExamplePkgVersion
  , ExamplePkgName
  , ExampleFlagName
  , ExFlag(..)
  , ExampleAvailable(..)
  , ExampleInstalled(..)
  , ExampleQualifier(..)
  , ExampleVar(..)
  , EnableAllTests(..)
  , exAv
  , exInst
  , exFlagged
  , exResolve
  , extractInstallPlan
  , declareFlags
  , withSetupDeps
  , withTest
  , withTests
  , withExe
  , withExes
  , runProgress
  , mkSimpleVersion
  , mkVersionRange
  ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude

-- base
import Data.Either (partitionEithers)
import qualified Data.Map as Map

-- Cabal
import qualified Distribution.Compiler                  as C
import qualified Distribution.InstalledPackageInfo      as IPI
import           Distribution.License (License(..))
import qualified Distribution.ModuleName                as Module
import qualified Distribution.Package                   as C
  hiding (HasUnitId(..))
import qualified Distribution.Types.LegacyExeDependency as C
import qualified Distribution.Types.PkgconfigDependency as C
import qualified Distribution.Types.UnqualComponentName as C
import qualified Distribution.Types.CondTree            as C
import qualified Distribution.PackageDescription        as C
import qualified Distribution.PackageDescription.Check  as C
import qualified Distribution.Simple.PackageIndex       as C.PackageIndex
import           Distribution.Simple.Setup (BooleanFlag(..))
import qualified Distribution.System                    as C
import           Distribution.Text (display)
import qualified Distribution.Version                   as C
import Language.Haskell.Extension (Extension(..), Language(..))

-- cabal-install
import Distribution.Client.Dependency
import Distribution.Client.Dependency.Types
import Distribution.Client.Types
import qualified Distribution.Client.SolverInstallPlan as CI.SolverInstallPlan

import           Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.ConstraintSource
import           Distribution.Solver.Types.Flag
import           Distribution.Solver.Types.LabeledPackageConstraint
import           Distribution.Solver.Types.OptionalStanza
import qualified Distribution.Solver.Types.PackageIndex      as CI.PackageIndex
import           Distribution.Solver.Types.PackageConstraint
import qualified Distribution.Solver.Types.PackagePath as P
import qualified Distribution.Solver.Types.PkgConfigDb as PC
import           Distribution.Solver.Types.Settings
import           Distribution.Solver.Types.SolverPackage
import           Distribution.Solver.Types.SourcePackage
import           Distribution.Solver.Types.Variable

{-------------------------------------------------------------------------------
  Example package database DSL

  In order to be able to set simple examples up quickly, we define a very
  simple version of the package database here explicitly designed for use in
  tests.

  The design of `ExampleDb` takes the perspective of the solver, not the
  perspective of the package DB. This makes it easier to set up tests for
  various parts of the solver, but makes the mapping somewhat awkward,  because
  it means we first map from "solver perspective" `ExampleDb` to the package
  database format, and then the modular solver internally in `IndexConversion`
  maps this back to the solver specific data structures.

  IMPLEMENTATION NOTES
  --------------------

  TODO: Perhaps these should be made comments of the corresponding data type
  definitions. For now these are just my own conclusions and may be wrong.

  * The difference between `GenericPackageDescription` and `PackageDescription`
    is that `PackageDescription` describes a particular _configuration_ of a
    package (for instance, see documentation for `checkPackage`). A
    `GenericPackageDescription` can be turned into a `PackageDescription` in
    two ways:

      a. `finalizePD` does the proper translation, by taking
         into account the platform, available dependencies, etc. and picks a
         flag assignment (or gives an error if no flag assignment can be found)
      b. `flattenPackageDescription` ignores flag assignment and just joins all
         components together.

    The slightly odd thing is that a `GenericPackageDescription` contains a
    `PackageDescription` as a field; both of the above functions do the same
    thing: they take the embedded `PackageDescription` as a basis for the result
    value, but override `library`, `executables`, `testSuites`, `benchmarks`
    and `buildDepends`.
  * The `condTreeComponents` fields of a `CondTree` is a list of triples
    `(condition, then-branch, else-branch)`, where the `else-branch` is
    optional.
-------------------------------------------------------------------------------}

type ExamplePkgName    = String
type ExamplePkgVersion = Int
type ExamplePkgHash    = String  -- for example "installed" packages
type ExampleFlagName   = String
type ExampleTestName   = String
type ExampleExeName    = String
type ExampleVersionRange = C.VersionRange

data Dependencies = NotBuildable | Buildable [ExampleDependency]
  deriving Show

data ExampleDependency =
    -- | Simple dependency on any version
    ExAny ExamplePkgName

    -- | Simple dependency on a fixed version
  | ExFix ExamplePkgName ExamplePkgVersion

    -- | Simple dependency on a range of versions, with an inclusive lower bound
    -- and an exclusive upper bound.
  | ExRange ExamplePkgName ExamplePkgVersion ExamplePkgVersion

    -- | Build-tools dependency
  | ExBuildToolAny ExamplePkgName

    -- | Build-tools dependency on a fixed version
  | ExBuildToolFix ExamplePkgName ExamplePkgVersion

    -- | Dependencies indexed by a flag
  | ExFlagged ExampleFlagName Dependencies Dependencies

    -- | Dependency on a language extension
  | ExExt Extension

    -- | Dependency on a language version
  | ExLang Language

    -- | Dependency on a pkg-config package
  | ExPkg (ExamplePkgName, ExamplePkgVersion)
  deriving Show

-- | Simplified version of D.Types.GenericPackageDescription.Flag for use in
-- example source packages.
data ExFlag = ExFlag {
    exFlagName    :: ExampleFlagName
  , exFlagDefault :: Bool
  , exFlagType    :: FlagType
  } deriving Show

data ExTest = ExTest ExampleTestName [ExampleDependency]

data ExExe = ExExe ExampleExeName [ExampleDependency]

exFlagged :: ExampleFlagName -> [ExampleDependency] -> [ExampleDependency]
          -> ExampleDependency
exFlagged n t e = ExFlagged n (Buildable t) (Buildable e)

data ExConstraint =
    ExVersionConstraint ConstraintScope ExampleVersionRange
  | ExFlagConstraint ConstraintScope ExampleFlagName Bool
  | ExStanzaConstraint ConstraintScope [OptionalStanza]
  deriving Show

data ExPreference =
    ExPkgPref ExamplePkgName ExampleVersionRange
  | ExStanzaPref ExamplePkgName [OptionalStanza]
  deriving Show

data ExampleAvailable = ExAv {
    exAvName    :: ExamplePkgName
  , exAvVersion :: ExamplePkgVersion
  , exAvDeps    :: ComponentDeps [ExampleDependency]

  -- Setting flags here is only necessary to override the default values of
  -- the fields in C.Flag.
  , exAvFlags   :: [ExFlag]
  } deriving Show

data ExampleVar =
    P ExampleQualifier ExamplePkgName
  | F ExampleQualifier ExamplePkgName ExampleFlagName
  | S ExampleQualifier ExamplePkgName OptionalStanza

data ExampleQualifier =
    None
  | Indep ExamplePkgName
  | Setup ExamplePkgName

    -- The two package names are the build target and the package containing the
    -- setup script.
  | IndepSetup ExamplePkgName ExamplePkgName

-- | Whether to enable tests in all packages in a test case.
newtype EnableAllTests = EnableAllTests Bool
  deriving BooleanFlag

-- | Constructs an 'ExampleAvailable' package for the 'ExampleDb',
-- given:
--
--      1. The name 'ExamplePkgName' of the available package,
--      2. The version 'ExamplePkgVersion' available
--      3. The list of dependency constraints 'ExampleDependency'
--         that this package has.  'ExampleDependency' provides
--         a number of pre-canned dependency types to look at.
--
exAv :: ExamplePkgName -> ExamplePkgVersion -> [ExampleDependency]
     -> ExampleAvailable
exAv n v ds = ExAv { exAvName = n, exAvVersion = v
                   , exAvDeps = CD.fromLibraryDeps ds, exAvFlags = [] }

-- | Override the default settings (e.g., manual vs. automatic) for a subset of
-- a package's flags.
declareFlags :: [ExFlag] -> ExampleAvailable -> ExampleAvailable
declareFlags flags ex = ex {
      exAvFlags = flags
    }

withSetupDeps :: ExampleAvailable -> [ExampleDependency] -> ExampleAvailable
withSetupDeps ex setupDeps = ex {
      exAvDeps = exAvDeps ex <> CD.fromSetupDeps setupDeps
    }

withTest :: ExampleAvailable -> ExTest -> ExampleAvailable
withTest ex test = withTests ex [test]

withTests :: ExampleAvailable -> [ExTest] -> ExampleAvailable
withTests ex tests =
  let testCDs = CD.fromList [(CD.ComponentTest $ C.mkUnqualComponentName name, deps)
                            | ExTest name deps <- tests]
  in ex { exAvDeps = exAvDeps ex <> testCDs }

withExe :: ExampleAvailable -> ExExe -> ExampleAvailable
withExe ex exe = withExes ex [exe]

withExes :: ExampleAvailable -> [ExExe] -> ExampleAvailable
withExes ex exes =
  let exeCDs = CD.fromList [(CD.ComponentExe $ C.mkUnqualComponentName name, deps)
                           | ExExe name deps <- exes]
  in ex { exAvDeps = exAvDeps ex <> exeCDs }

-- | An installed package in 'ExampleDb'; construct me with 'exInst'.
data ExampleInstalled = ExInst {
    exInstName         :: ExamplePkgName
  , exInstVersion      :: ExamplePkgVersion
  , exInstHash         :: ExamplePkgHash
  , exInstBuildAgainst :: [ExamplePkgHash]
  } deriving Show

-- | Constructs an example installed package given:
--
--      1. The name of the package 'ExamplePkgName', i.e., 'String'
--      2. The version of the package 'ExamplePkgVersion', i.e., 'Int'
--      3. The IPID for the package 'ExamplePkgHash', i.e., 'String'
--         (just some unique identifier for the package.)
--      4. The 'ExampleInstalled' packages which this package was
--         compiled against.)
--
exInst :: ExamplePkgName -> ExamplePkgVersion -> ExamplePkgHash
       -> [ExampleInstalled] -> ExampleInstalled
exInst pn v hash deps = ExInst pn v hash (map exInstHash deps)

-- | An example package database is a list of installed packages
-- 'ExampleInstalled' and available packages 'ExampleAvailable'.
-- Generally, you want to use 'exInst' and 'exAv' to construct
-- these packages.
type ExampleDb = [Either ExampleInstalled ExampleAvailable]

type DependencyTree a = C.CondTree C.ConfVar [C.Dependency] a

type DependencyComponent a = C.CondBranch C.ConfVar [C.Dependency] a

exDbPkgs :: ExampleDb -> [ExamplePkgName]
exDbPkgs = map (either exInstName exAvName)

exAvSrcPkg :: ExampleAvailable -> UnresolvedSourcePackage
exAvSrcPkg ex =
    let pkgId = exAvPkgId ex

        flags :: [C.Flag]
        flags =
          let declaredFlags :: Map ExampleFlagName C.Flag
              declaredFlags =
                  Map.fromListWith
                      (\f1 f2 -> error $ "duplicate flag declarations: " ++ show [f1, f2])
                      [(exFlagName flag, mkFlag flag) | flag <- exAvFlags ex]

              usedFlags :: Map ExampleFlagName C.Flag
              usedFlags = Map.fromList [(fn, mkDefaultFlag fn) | fn <- names]
                where
                  names = concatMap extractFlags $
                          CD.libraryDeps (exAvDeps ex)
                           ++ concatMap snd testSuites
                           ++ concatMap snd executables
          in -- 'declaredFlags' overrides 'usedFlags' to give flags non-default settings:
             Map.elems $ declaredFlags `Map.union` usedFlags

        testSuites = [(name, deps) | (CD.ComponentTest name, deps) <- CD.toList (exAvDeps ex)]
        executables = [(name, deps) | (CD.ComponentExe name, deps) <- CD.toList (exAvDeps ex)]
        setup = case CD.setupDeps (exAvDeps ex) of
                  []   -> Nothing
                  deps -> Just C.SetupBuildInfo {
                            C.setupDepends = mkSetupDeps deps,
                            C.defaultSetupDepends = False
                          }
        package = SourcePackage {
            packageInfoId        = pkgId
          , packageSource        = LocalTarballPackage "<<path>>"
          , packageDescrOverride = Nothing
          , packageDescription   = C.GenericPackageDescription {
                C.packageDescription = C.emptyPackageDescription {
                    C.package        = pkgId
                  , C.setupBuildInfo = setup
                  , C.license = BSD3
                  , C.buildType = if isNothing setup
                                  then Just C.Simple
                                  else Just C.Custom
                  , C.category = "category"
                  , C.maintainer = "maintainer"
                  , C.description = "description"
                  , C.synopsis = "synopsis"
                  , C.licenseFiles = ["LICENSE"]
                  , C.specVersionRaw = Left $ C.mkVersion [1,12]
                  }
              , C.genPackageFlags = flags
              , C.condLibrary =
                  let mkLib bi = mempty { C.libBuildInfo = bi }
                  in Just $ mkCondTree defaultLib mkLib $ mkBuildInfoTree $
                     Buildable (CD.libraryDeps (exAvDeps ex))
              , C.condSubLibraries = []
              , C.condForeignLibs = []
              , C.condExecutables =
                  let mkTree = mkCondTree defaultExe mkExe . mkBuildInfoTree . Buildable
                      mkExe bi = mempty { C.buildInfo = bi }
                  in map (\(t, deps) -> (t, mkTree deps)) executables
              , C.condTestSuites =
                  let mkTree = mkCondTree defaultTest mkTest . mkBuildInfoTree . Buildable
                      mkTest bi = mempty { C.testBuildInfo = bi }
                  in map (\(t, deps) -> (t, mkTree deps)) testSuites
              , C.condBenchmarks  = []
              }
            }
        pkgCheckErrors =
          -- We ignore these warnings because some unit tests test that the
          -- solver allows unknown extensions/languages when the compiler
          -- supports them.
          let ignore = ["Unknown extensions:", "Unknown languages:"]
          in [ err | err <- C.checkPackage (packageDescription package) Nothing
             , not $ any (`isPrefixOf` C.explanation err) ignore ]
    in if null pkgCheckErrors
       then package
       else error $ "invalid GenericPackageDescription for package "
                 ++ display pkgId ++ ": " ++ show pkgCheckErrors
  where
    defaultTopLevelBuildInfo :: C.BuildInfo
    defaultTopLevelBuildInfo = mempty { C.defaultLanguage = Just Haskell98 }

    defaultLib :: C.Library
    defaultLib = mempty { C.exposedModules = [Module.fromString "Module"] }

    defaultExe :: C.Executable
    defaultExe = mempty { C.modulePath = "Main.hs" }

    defaultTest :: C.TestSuite
    defaultTest = mempty {
        C.testInterface = C.TestSuiteExeV10 (C.mkVersion [1,0]) "Test.hs"
      }

    -- Split the set of dependencies into the set of dependencies of the library,
    -- the dependencies of the test suites and extensions.
    splitTopLevel :: [ExampleDependency]
                  -> ( [ExampleDependency]
                     , [Extension]
                     , Maybe Language
                     , [(ExamplePkgName, ExamplePkgVersion)] -- pkg-config
                     , [(ExamplePkgName, C.VersionRange)] -- build tools
                     )
    splitTopLevel [] =
        ([], [], Nothing, [], [])
    splitTopLevel (ExBuildToolAny p:deps) =
      let (other, exts, lang, pcpkgs, exes) = splitTopLevel deps
      in (other, exts, lang, pcpkgs, (p, C.anyVersion):exes)
    splitTopLevel (ExBuildToolFix p v:deps) =
      let (other, exts, lang, pcpkgs, exes) = splitTopLevel deps
      in (other, exts, lang, pcpkgs, (p, C.thisVersion (mkSimpleVersion v)):exes)
    splitTopLevel (ExExt ext:deps) =
      let (other, exts, lang, pcpkgs, exes) = splitTopLevel deps
      in (other, ext:exts, lang, pcpkgs, exes)
    splitTopLevel (ExLang lang:deps) =
        case splitTopLevel deps of
            (other, exts, Nothing, pcpkgs, exes) -> (other, exts, Just lang, pcpkgs, exes)
            _ -> error "Only 1 Language dependency is supported"
    splitTopLevel (ExPkg pkg:deps) =
      let (other, exts, lang, pcpkgs, exes) = splitTopLevel deps
      in (other, exts, lang, pkg:pcpkgs, exes)
    splitTopLevel (dep:deps) =
      let (other, exts, lang, pcpkgs, exes) = splitTopLevel deps
      in (dep:other, exts, lang, pcpkgs, exes)

    -- Extract the total set of flags used
    extractFlags :: ExampleDependency -> [ExampleFlagName]
    extractFlags (ExAny _)            = []
    extractFlags (ExFix _ _)          = []
    extractFlags (ExRange _ _ _)      = []
    extractFlags (ExBuildToolAny _)   = []
    extractFlags (ExBuildToolFix _ _) = []
    extractFlags (ExFlagged f a b)    =
        f : concatMap extractFlags (deps a ++ deps b)
      where
        deps :: Dependencies -> [ExampleDependency]
        deps NotBuildable = []
        deps (Buildable ds) = ds
    extractFlags (ExExt _)      = []
    extractFlags (ExLang _)     = []
    extractFlags (ExPkg _)      = []

    -- Convert a tree of BuildInfos into a tree of a specific component type.
    -- 'defaultTopLevel' contains the default values for the component, and
    -- 'mkComponent' creates a component from a 'BuildInfo'.
    mkCondTree :: forall a. Semigroup a =>
                  a -> (C.BuildInfo -> a)
               -> DependencyTree C.BuildInfo
               -> DependencyTree a
    mkCondTree defaultTopLevel mkComponent (C.CondNode topData topConstraints topComps) =
        C.CondNode {
            C.condTreeData =
                defaultTopLevel <> mkComponent (defaultTopLevelBuildInfo <> topData)
          , C.condTreeConstraints = topConstraints
          , C.condTreeComponents = goComponents topComps
          }
      where
        go :: DependencyTree C.BuildInfo -> DependencyTree a
        go (C.CondNode ctData constraints comps) =
            C.CondNode (mkComponent ctData) constraints (goComponents comps)

        goComponents :: [DependencyComponent C.BuildInfo]
                     -> [DependencyComponent a]
        goComponents comps = [C.CondBranch cond (go t) (go <$> me) | C.CondBranch cond t me <- comps]

    mkBuildInfoTree :: Dependencies -> DependencyTree C.BuildInfo
    mkBuildInfoTree NotBuildable =
      C.CondNode {
             C.condTreeData        = mempty { C.buildable = False }
           , C.condTreeConstraints = []
           , C.condTreeComponents  = []
           }
    mkBuildInfoTree (Buildable deps) =
      let (libraryDeps, exts, mlang, pcpkgs, buildTools) = splitTopLevel deps
          (directDeps, flaggedDeps) = splitDeps libraryDeps
          bi = mempty {
                  C.otherExtensions = exts
                , C.defaultLanguage = mlang
                , C.buildTools = [ C.LegacyExeDependency n vr
                                 | (n,vr) <- buildTools ]
                , C.pkgconfigDepends = [ C.PkgconfigDependency n' v'
                                       | (n,v) <- pcpkgs
                                       , let n' = C.mkPkgconfigName n
                                       , let v' = C.thisVersion (mkSimpleVersion v) ]
              }
      in C.CondNode {
             C.condTreeData        = bi -- Necessary for language extensions
           -- TODO: Arguably, build-tools dependencies should also
           -- effect constraints on conditional tree. But no way to
           -- distinguish between them
           , C.condTreeConstraints = map mkDirect directDeps
           , C.condTreeComponents  = map mkFlagged flaggedDeps
           }

    mkDirect :: (ExamplePkgName, C.VersionRange) -> C.Dependency
    mkDirect (dep, vr) = C.Dependency (C.mkPackageName dep) vr

    mkFlagged :: (ExampleFlagName, Dependencies, Dependencies)
              -> DependencyComponent C.BuildInfo
    mkFlagged (f, a, b) =
        C.CondBranch (C.Var (C.Flag (C.mkFlagName f)))
                     (mkBuildInfoTree a)
                     (Just (mkBuildInfoTree b))

    -- Split a set of dependencies into direct dependencies and flagged
    -- dependencies. A direct dependency is a tuple of the name of package and
    -- its version range meant to be converted to a 'C.Dependency' with
    -- 'mkDirect' for example. A flagged dependency is the set of dependencies
    -- guarded by a flag.
    splitDeps :: [ExampleDependency]
              -> ( [(ExamplePkgName, C.VersionRange)]
                 , [(ExampleFlagName, Dependencies, Dependencies)]
                 )
    splitDeps [] =
      ([], [])
    splitDeps (ExAny p:deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
      in ((p, C.anyVersion):directDeps, flaggedDeps)
    splitDeps (ExFix p v:deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
      in ((p, C.thisVersion $ mkSimpleVersion v):directDeps, flaggedDeps)
    splitDeps (ExRange p v1 v2:deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
      in ((p, mkVersionRange v1 v2):directDeps, flaggedDeps)
    splitDeps (ExFlagged f a b:deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
      in (directDeps, (f, a, b):flaggedDeps)
    splitDeps (dep:_) = error $ "Unexpected dependency: " ++ show dep

    -- custom-setup only supports simple dependencies
    mkSetupDeps :: [ExampleDependency] -> [C.Dependency]
    mkSetupDeps deps =
      let (directDeps, []) = splitDeps deps in map mkDirect directDeps

mkSimpleVersion :: ExamplePkgVersion -> C.Version
mkSimpleVersion n = C.mkVersion [n, 0, 0]

mkVersionRange :: ExamplePkgVersion -> ExamplePkgVersion -> C.VersionRange
mkVersionRange v1 v2 =
    C.intersectVersionRanges (C.orLaterVersion $ mkSimpleVersion v1)
                             (C.earlierVersion $ mkSimpleVersion v2)

mkFlag :: ExFlag -> C.Flag
mkFlag flag = C.MkFlag {
    C.flagName        = C.mkFlagName $ exFlagName flag
  , C.flagDescription = ""
  , C.flagDefault     = exFlagDefault flag
  , C.flagManual      =
      case exFlagType flag of
        Manual    -> True
        Automatic -> False
  }

mkDefaultFlag :: ExampleFlagName -> C.Flag
mkDefaultFlag flag = C.MkFlag {
    C.flagName        = C.mkFlagName flag
  , C.flagDescription = ""
  , C.flagDefault     = True
  , C.flagManual      = False
  }

exAvPkgId :: ExampleAvailable -> C.PackageIdentifier
exAvPkgId ex = C.PackageIdentifier {
      pkgName    = C.mkPackageName (exAvName ex)
    , pkgVersion = C.mkVersion [exAvVersion ex, 0, 0]
    }

exInstInfo :: ExampleInstalled -> IPI.InstalledPackageInfo
exInstInfo ex = IPI.emptyInstalledPackageInfo {
      IPI.installedUnitId    = C.mkUnitId (exInstHash ex)
    , IPI.sourcePackageId    = exInstPkgId ex
    , IPI.depends            = map C.mkUnitId (exInstBuildAgainst ex)
    }

exInstPkgId :: ExampleInstalled -> C.PackageIdentifier
exInstPkgId ex = C.PackageIdentifier {
      pkgName    = C.mkPackageName (exInstName ex)
    , pkgVersion = C.mkVersion [exInstVersion ex, 0, 0]
    }

exAvIdx :: [ExampleAvailable] -> CI.PackageIndex.PackageIndex UnresolvedSourcePackage
exAvIdx = CI.PackageIndex.fromList . map exAvSrcPkg

exInstIdx :: [ExampleInstalled] -> C.PackageIndex.InstalledPackageIndex
exInstIdx = C.PackageIndex.fromList . map exInstInfo

exResolve :: ExampleDb
          -- List of extensions supported by the compiler, or Nothing if unknown.
          -> Maybe [Extension]
          -- List of languages supported by the compiler, or Nothing if unknown.
          -> Maybe [Language]
          -> PC.PkgConfigDb
          -> [ExamplePkgName]
          -> Maybe Int
          -> CountConflicts
          -> IndependentGoals
          -> ReorderGoals
          -> AllowBootLibInstalls
          -> EnableBackjumping
          -> Maybe (Variable P.QPN -> Variable P.QPN -> Ordering)
          -> [ExConstraint]
          -> [ExPreference]
          -> EnableAllTests
          -> Progress String String CI.SolverInstallPlan.SolverInstallPlan
exResolve db exts langs pkgConfigDb targets mbj countConflicts indepGoals
          reorder allowBootLibInstalls enableBj goalOrder constraints prefs
          enableAllTests
    = resolveDependencies C.buildPlatform compiler pkgConfigDb Modular params
  where
    defaultCompiler = C.unknownCompilerInfo C.buildCompilerId C.NoAbiTag
    compiler = defaultCompiler { C.compilerInfoExtensions = exts
                               , C.compilerInfoLanguages  = langs
                               }
    (inst, avai) = partitionEithers db
    instIdx      = exInstIdx inst
    avaiIdx      = SourcePackageDb {
                       packageIndex       = exAvIdx avai
                     , packagePreferences = Map.empty
                     }
    enableTests
        | asBool enableAllTests = fmap (\p -> PackageConstraint
                                              (scopeToplevel (C.mkPackageName p))
                                              (PackagePropertyStanzas [TestStanzas]))
                                       (exDbPkgs db)
        | otherwise             = []
    targets'     = fmap (\p -> NamedPackage (C.mkPackageName p) []) targets
    params       =   addConstraints (fmap toConstraint constraints)
                   $ addConstraints (fmap toLpc enableTests)
                   $ addPreferences (fmap toPref prefs)
                   $ setCountConflicts countConflicts
                   $ setIndependentGoals indepGoals
                   $ setReorderGoals reorder
                   $ setMaxBackjumps mbj
                   $ setAllowBootLibInstalls allowBootLibInstalls
                   $ setEnableBackjumping enableBj
                   $ setGoalOrder goalOrder
                   $ standardInstallPolicy instIdx avaiIdx targets'
    toLpc     pc = LabeledPackageConstraint pc ConstraintSourceUnknown

    toConstraint (ExVersionConstraint scope v) =
        toLpc $ PackageConstraint scope (PackagePropertyVersion v)
    toConstraint (ExFlagConstraint scope fn b) =
        toLpc $ PackageConstraint scope (PackagePropertyFlags (C.mkFlagAssignment [(C.mkFlagName fn, b)]))
    toConstraint (ExStanzaConstraint scope stanzas) =
        toLpc $ PackageConstraint scope (PackagePropertyStanzas stanzas)

    toPref (ExPkgPref n v)          = PackageVersionPreference (C.mkPackageName n) v
    toPref (ExStanzaPref n stanzas) = PackageStanzasPreference (C.mkPackageName n) stanzas

extractInstallPlan :: CI.SolverInstallPlan.SolverInstallPlan
                   -> [(ExamplePkgName, ExamplePkgVersion)]
extractInstallPlan = catMaybes . map confPkg . CI.SolverInstallPlan.toList
  where
    confPkg :: CI.SolverInstallPlan.SolverPlanPackage -> Maybe (String, Int)
    confPkg (CI.SolverInstallPlan.Configured pkg) = Just $ srcPkg pkg
    confPkg _                               = Nothing

    srcPkg :: SolverPackage UnresolvedPkgLoc -> (String, Int)
    srcPkg cpkg =
      let C.PackageIdentifier pn ver = packageInfoId (solverPkgSource cpkg)
      in (C.unPackageName pn, head (C.versionNumbers ver))

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Run Progress computation
runProgress :: Progress step e a -> ([step], Either e a)
runProgress = go
  where
    go (Step s p) = let (ss, result) = go p in (s:ss, result)
    go (Fail e)   = ([], Left e)
    go (Done a)   = ([], Right a)
