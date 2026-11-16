{-# LANGUAGE DeriveGeneric #-}
module Macro.Types (module Macro.Types, Version(..)) where

import GHC.Generics

newtype InstalledPackageId = InstalledPackageId String
    deriving (Eq, Ord, Generic)

newtype PackageName = PackageName String
    deriving (Eq, Ord, Generic)

data Version = Version [Int] [String]
    deriving (Eq, Ord, Generic)

data PackageId = PackageId {
      pkgName    :: PackageName, -- ^The name of this package, eg. foo
      pkgVersion :: Version -- ^the version of this package, eg 1.2
    }
    deriving (Eq, Ord, Generic)

newtype ModuleName = ModuleName [String]
    deriving (Eq, Ord, Generic)

data License =
    GPL  (Maybe Version)
  | AGPL (Maybe Version)
  | LGPL (Maybe Version)
  | BSD3
  | BSD4
  | MIT
  | Apache (Maybe Version)
  | PublicDomain
  | AllRightsReserved
  | OtherLicense
  | UnknownLicense String
  deriving (Eq, Generic)
{-
data InstalledPackageInfo = InstalledPackageInfo {
      -- these parts are exactly the same as PackageDescription
      installedPackageId :: InstalledPackageId,
      sourcePackageId    :: PackageId,
      license           :: License,
      copyright         :: String,
      maintainer        :: String,
      author            :: String,
      stability         :: String,
      homepage          :: String,
      pkgUrl            :: String,
      synopsis          :: String,
      description       :: String,
      category          :: String,
      -- these parts are required by an installed package only:
      exposed           :: Bool,
      exposedModules    :: [ModuleName],
      hiddenModules     :: [ModuleName],
      trusted           :: Bool,
      importDirs        :: [FilePath],  -- contain sources in case of Hugs
      libraryDirs       :: [FilePath],
      hsLibraries       :: [String],
      extraLibraries    :: [String],
      extraGHCiLibraries:: [String],    -- overrides extraLibraries for GHCi
      includeDirs       :: [FilePath],
      includes          :: [String],
      depends           :: [InstalledPackageId],
      hugsOptions       :: [String],
      ccOptions         :: [String],
      ldOptions         :: [String],
      frameworkDirs     :: [FilePath],
      frameworks        :: [String],
      haddockInterfaces :: [FilePath],
      haddockHTMLs      :: [FilePath]
    }
    deriving (Eq, Generic)
-}

data VersionRange
    = AnyVersion
    | ThisVersion            Version -- = version
    | LaterVersion           Version -- > version  (NB. not >=)
    | EarlierVersion         Version -- < version
    | WildcardVersion        Version -- == ver.*   (same as >= ver && < ver+1)
    | UnionVersionRanges     VersionRange VersionRange
    | IntersectVersionRanges VersionRange VersionRange
    | VersionRangeParens     VersionRange -- just '(exp)' parentheses syntax
    deriving (Eq, Generic)

data Dependency = Dependency PackageName VersionRange
                  deriving (Eq, Generic)

data CompilerFlavor = GHC | NHC | YHC | Hugs | HBC | Helium | JHC | LHC | UHC
                    | HaskellSuite String -- string is the id of the actual compiler
                    | OtherCompiler String
    deriving (Eq, Ord, Generic)

data SourceRepo = SourceRepo {
      repoKind     :: RepoKind,
      repoType     :: Maybe RepoType,
      repoLocation :: Maybe String,
      repoModule   :: Maybe String,
      repoBranch   :: Maybe String,
      repoTag      :: Maybe String,
      repoSubdir   :: Maybe FilePath
    }
    deriving (Eq, Generic)

data RepoKind =
      RepoHead
    | RepoThis
    | RepoKindUnknown String
    deriving (Eq, Ord, Generic)

data RepoType = Darcs | Git | SVN | CVS
              | Mercurial | GnuArch | Bazaar | Monotone
              | OtherRepoType String
    deriving (Eq, Ord, Generic)

data BuildType
    = Simple
    | Configure
    | Make
    | Custom
    | UnknownBuildType String
    deriving (Eq, Generic)

data Library = Library {
        exposedModules    :: [ModuleName],
        libExposed        :: Bool, -- ^ Is the lib to be exposed by default?
        libBuildInfo      :: BuildInfo
    }
    deriving (Eq, Generic)

data Executable = Executable {
        exeName    :: String,
        modulePath :: FilePath,
        buildInfo  :: BuildInfo
    }
    deriving (Eq, Generic)

data TestSuite = TestSuite {
        testName      :: String,
        testInterface :: TestSuiteInterface,
        testBuildInfo :: BuildInfo,
        testEnabled   :: Bool
    }
    deriving (Eq, Generic)

data TestType = TestTypeExe Version
              | TestTypeLib Version
              | TestTypeUnknown String Version
    deriving (Eq, Generic)

data TestSuiteInterface =
     TestSuiteExeV10 Version FilePath
   | TestSuiteLibV09 Version ModuleName
   | TestSuiteUnsupported TestType
   deriving (Eq, Generic)

data Benchmark = Benchmark {
        benchmarkName      :: String,
        benchmarkInterface :: BenchmarkInterface,
        benchmarkBuildInfo :: BuildInfo,
        benchmarkEnabled   :: Bool
    }
    deriving (Eq, Generic)

data BenchmarkType = BenchmarkTypeExe Version
                   | BenchmarkTypeUnknown String Version
    deriving (Eq, Generic)

data BenchmarkInterface =
      BenchmarkExeV10 Version FilePath
    | BenchmarkUnsupported BenchmarkType
    deriving (Eq, Generic)

data Language =
    Haskell98
  | Haskell2010
  | UnknownLanguage String
  deriving (Eq, Generic)

data Extension =
    EnableExtension KnownExtension
  | DisableExtension KnownExtension
  | UnknownExtension String
  deriving (Eq, Generic)

data KnownExtension =
    OverlappingInstances
  | UndecidableInstances
  | IncoherentInstances
  | DoRec
  | RecursiveDo
  | ParallelListComp
  | MultiParamTypeClasses
  | MonomorphismRestriction
  | FunctionalDependencies
  | Rank2Types
  | RankNTypes
  | PolymorphicComponents
  | ExistentialQuantification
  | ScopedTypeVariables
  | PatternSignatures
  | ImplicitParams
  | FlexibleContexts
  | FlexibleInstances
  | EmptyDataDecls
  | CPP
  | KindSignatures
  | BangPatterns
  | TypeSynonymInstances
  | TemplateHaskell
  | ForeignFunctionInterface
  | Arrows
  | Generics
  | ImplicitPrelude
  | NamedFieldPuns
  | PatternGuards
  | GeneralizedNewtypeDeriving
  | ExtensibleRecords
  | RestrictedTypeSynonyms
  | HereDocuments
  | MagicHash
  | TypeFamilies
  | StandaloneDeriving
  | UnicodeSyntax
  | UnliftedFFITypes
  | InterruptibleFFI
  | CApiFFI
  | LiberalTypeSynonyms
  | TypeOperators
  | RecordWildCards
  | RecordPuns
  | DisambiguateRecordFields
  | TraditionalRecordSyntax
  | OverloadedStrings
  | GADTs
  | GADTSyntax
  | MonoPatBinds
  | RelaxedPolyRec
  | ExtendedDefaultRules
  | UnboxedTuples
  | DeriveDataTypeable
  | DeriveGeneric
  | DefaultSignatures
  | InstanceSigs
  | ConstrainedClassMethods
  | PackageImports
  | ImpredicativeTypes
  | NewQualifiedOperators
  | PostfixOperators
  | QuasiQuotes
  | TransformListComp
  | MonadComprehensions
  | ViewPatterns
  | XmlSyntax
  | RegularPatterns
  | TupleSections
  | GHCForeignImportPrim
  | NPlusKPatterns
  | DoAndIfThenElse
  | MultiWayIf
  | LambdaCase
  | RebindableSyntax
  | ExplicitForAll
  | DatatypeContexts
  | MonoLocalBinds
  | DeriveFunctor
  | DeriveTraversable
  | DeriveFoldable
  | NondecreasingIndentation
  | SafeImports
  | Safe
  | Trustworthy
  | Unsafe
  | ConstraintKinds
  | PolyKinds
  | DataKinds
  | ParallelArrays
  | RoleAnnotations
  | OverloadedLists
  | EmptyCase
  | AutoDeriveTypeable
  | NegativeLiterals
  | NumDecimals
  | NullaryTypeClasses
  | ExplicitNamespaces
  | AllowAmbiguousTypes
  deriving (Eq, Enum, Bounded, Generic)

data BuildInfo = BuildInfo {
        buildable         :: Bool,      -- ^ component is buildable here
        buildTools        :: [Dependency], -- ^ tools needed to build this bit
        cppOptions        :: [String],  -- ^ options for pre-processing Haskell code
        ccOptions         :: [String],  -- ^ options for C compiler
        ldOptions         :: [String],  -- ^ options for linker
        pkgconfigDepends  :: [Dependency], -- ^ pkg-config packages that are used
        frameworks        :: [String], -- ^support frameworks for Mac OS X
        cSources          :: [FilePath],
        hsSourceDirs      :: [FilePath], -- ^ where to look for the haskell module Macro.hierarchy
        otherModules      :: [ModuleName], -- ^ non-exposed or non-main modules

        defaultLanguage   :: Maybe Language,-- ^ language used when not explicitly specified
        otherLanguages    :: [Language],    -- ^ other languages used within the package
        defaultExtensions :: [Extension],   -- ^ language extensions used by all modules
        otherExtensions   :: [Extension],   -- ^ other language extensions used within the package
        oldExtensions     :: [Extension],   -- ^ the old extensions field, treated same as 'defaultExtensions'

        extraLibs         :: [String], -- ^ what libraries to link with when compiling a program that uses your package
        extraLibDirs      :: [String],
        includeDirs       :: [FilePath], -- ^directories to find .h files
        includes          :: [FilePath], -- ^ The .h files to be found in includeDirs
        installIncludes   :: [FilePath], -- ^ .h files to install with the package
        options           :: [(CompilerFlavor,[String])],
        ghcProfOptions    :: [String],
        ghcSharedOptions  :: [String],
        customFieldsBI    :: [(String,String)], -- ^Custom fields starting
                                                -- with x-, stored in a
                                                -- simple assoc-list.
        targetBuildDepends :: [Dependency] -- ^ Dependencies specific to a library or executable target
    }
    deriving (Eq, Generic)

data PackageDescription =  PackageDescription {
      -- the following are required by all packages:
      package        :: PackageId,
      license        :: License,
      licenseFile    :: FilePath,
      copyright      :: String,
      maintainer     :: String,
      author         :: String,
      stability      :: String,
      testedWith     :: [(CompilerFlavor,VersionRange)],
      homepage       :: String,
      pkgUrl         :: String,
      bugReports     :: String,
      sourceRepos    :: [SourceRepo],
      synopsis       :: String, -- ^A one-line summary of this package
      description    :: String, -- ^A more verbose description of this package
      category       :: String,
      customFieldsPD :: [(String,String)], -- ^Custom fields starting
                                           -- with x-, stored in a
                                           -- simple assoc-list.
      buildDepends   :: [Dependency],
      -- | The version of the Cabal spec that this package description uses.
      -- For historical reasons this is specified with a version range but
      -- only ranges of the form @>= v@ make sense. We are in the process of
      -- transitioning to specifying just a single version, not a range.
      specVersionRaw :: Either Version VersionRange,
      buildType      :: Maybe BuildType,
      -- components
      library        :: Maybe Library,
      executables    :: [Executable],
      testSuites     :: [TestSuite],
      benchmarks     :: [Benchmark],
      dataFiles      :: [FilePath],
      dataDir        :: FilePath,
      extraSrcFiles  :: [FilePath],
      extraTmpFiles  :: [FilePath],
      extraDocFiles  :: [FilePath]
    }
    deriving (Eq, Generic)

data GenericPackageDescription =
    GenericPackageDescription {
        packageDescription :: PackageDescription,
        genPackageFlags       :: [Flag],
        condLibrary        :: Maybe (CondTree ConfVar [Dependency] Library),
        condExecutables    :: [(String, CondTree ConfVar [Dependency] Executable)],
        condTestSuites     :: [(String, CondTree ConfVar [Dependency] TestSuite)],
        condBenchmarks     :: [(String, CondTree ConfVar [Dependency] Benchmark)]
      }
    deriving (Eq, Generic)

data OS = Linux | Windows | OSX        -- tier 1 desktop OSs
        | FreeBSD | OpenBSD | NetBSD   -- other free unix OSs
        | Solaris | AIX | HPUX | IRIX  -- ageing Unix OSs
        | HaLVM                        -- bare metal / VMs / hypervisors
        | IOS                          -- iOS
        | OtherOS String
  deriving (Eq, Ord, Generic)

data Arch = I386  | X86_64 | PPC | PPC64 | Sparc
          | Arm   | Mips   | SH
          | IA64  | S390
          | Alpha | Hppa   | Rs6000
          | M68k  | Vax
          | OtherArch String
  deriving (Eq, Ord, Generic)

data Flag = MkFlag
    { flagName        :: FlagName
    , flagDescription :: String
    , flagDefault     :: Bool
    , flagManual      :: Bool
    }
    deriving (Eq, Generic)

newtype FlagName = FlagName String
    deriving (Eq, Ord, Generic)

type FlagAssignment = [(FlagName, Bool)]

data ConfVar = OS OS
             | Arch Arch
             | Flag FlagName
             | Impl CompilerFlavor VersionRange
    deriving (Eq, Generic)

data Condition c = Var c
                 | Lit Bool
                 | CNot (Condition c)
                 | COr (Condition c) (Condition c)
                 | CAnd (Condition c) (Condition c)
    deriving (Eq, Generic)

data CondTree v c a = CondNode
    { condTreeData        :: a
    , condTreeConstraints :: c
    , condTreeComponents  :: [( Condition v
                              , CondTree v c a
                              , Maybe (CondTree v c a))]
    }
    deriving (Eq, Generic)

