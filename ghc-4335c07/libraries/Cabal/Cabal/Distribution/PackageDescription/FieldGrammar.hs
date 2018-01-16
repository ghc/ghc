{-# LANGUAGE OverloadedStrings #-}
-- | 'GenericPackageDescription' Field descriptions
module Distribution.PackageDescription.FieldGrammar (
    -- * Package description
    packageDescriptionFieldGrammar,
    -- * Library
    libraryFieldGrammar,
    -- * Foreign library
    foreignLibFieldGrammar,
    -- * Executable
    executableFieldGrammar,
    -- * Test suite
    TestSuiteStanza (..),
    testSuiteFieldGrammar,
    validateTestSuite,
    unvalidateTestSuite,
    -- ** Lenses
    testStanzaTestType,
    testStanzaMainIs,
    testStanzaTestModule,
    testStanzaBuildInfo,
    -- * Benchmark
    BenchmarkStanza (..),
    benchmarkFieldGrammar,
    validateBenchmark,
    unvalidateBenchmark,
    -- ** Lenses
    benchmarkStanzaBenchmarkType,
    benchmarkStanzaMainIs,
    benchmarkStanzaBenchmarkModule,
    benchmarkStanzaBuildInfo,
    -- * Flag
    flagFieldGrammar,
    -- * Source repository
    sourceRepoFieldGrammar,
    -- * Setup build info
    setupBInfoFieldGrammar,
    -- * Component build info
    buildInfoFieldGrammar,
    ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compiler                  (CompilerFlavor (..))
import Distribution.FieldGrammar
import Distribution.License                   (License (..))
import Distribution.ModuleName                (ModuleName)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Parsec.Common
import Distribution.Parsec.Newtypes
import Distribution.Parsec.ParseResult
import Distribution.Text                      (display)
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibType
import Distribution.Types.UnqualComponentName
import Distribution.Version                   (anyVersion)

import qualified Distribution.Types.Lens as L

-------------------------------------------------------------------------------
-- PackageDescription
-------------------------------------------------------------------------------

packageDescriptionFieldGrammar
    :: (FieldGrammar g, Applicative (g PackageDescription), Applicative (g PackageIdentifier))
    => g PackageDescription PackageDescription
packageDescriptionFieldGrammar = PackageDescription
    <$> blurFieldGrammar L.package packageIdentifierGrammar
    <*> optionalFieldDef    "license"                                  L.license UnspecifiedLicense
    <*> licenseFilesGrammar
    <*> optionalFieldDefAla "copyright"     FreeText                   L.copyright ""
    <*> optionalFieldDefAla "maintainer"    FreeText                   L.maintainer ""
    <*> optionalFieldDefAla "author"        FreeText                   L.author ""
    <*> optionalFieldDefAla "stability"     FreeText                   L.stability ""
    <*> monoidalFieldAla    "tested-with"   (alaList' FSep TestedWith) L.testedWith
    <*> optionalFieldDefAla "homepage"      FreeText                   L.homepage ""
    <*> optionalFieldDefAla "package-url"   FreeText                   L.pkgUrl ""
    <*> optionalFieldDefAla "bug-reports"   FreeText                   L.bugReports ""
    <*> pure [] -- source-repos are stanza
    <*> optionalFieldDefAla "synopsis"      FreeText                   L.synopsis ""
    <*> optionalFieldDefAla "description"   FreeText                   L.description ""
    <*> optionalFieldDefAla "category"      FreeText                   L.category ""
    <*> prefixedFields      "x-"                                       L.customFieldsPD
    <*> pure [] -- build-depends
    <*> optionalFieldDefAla "cabal-version" SpecVersion                L.specVersionRaw (Right anyVersion)
    <*> optionalField       "build-type"                               L.buildType
    <*> pure Nothing -- custom-setup
    -- components
    <*> pure Nothing  -- lib
    <*> pure []       -- sub libs
    <*> pure []       -- executables
    <*> pure []       -- foreign libs
    <*> pure []       -- test suites
    <*> pure []       -- benchmarks
    --  * Files
    <*> monoidalFieldAla    "data-files"         (alaList' VCat FilePathNT) L.dataFiles
    <*> optionalFieldDefAla "data-dir"           FilePathNT                 L.dataDir ""
    <*> monoidalFieldAla    "extra-source-files" (alaList' VCat FilePathNT) L.extraSrcFiles
    <*> monoidalFieldAla    "extra-tmp-files"    (alaList' VCat FilePathNT) L.extraTmpFiles
    <*> monoidalFieldAla    "extra-doc-files"    (alaList' VCat FilePathNT) L.extraDocFiles
  where
    packageIdentifierGrammar = PackageIdentifier
        <$> uniqueField "name"    L.pkgName
        <*> uniqueField "version" L.pkgVersion

    licenseFilesGrammar = (++)
        -- TODO: neither field is deprecated
        -- should we pretty print license-file if there's single license file
        -- and license-files when more
        <$> monoidalFieldAla    "license-file"  (alaList' FSep FilePathNT)  L.licenseFiles
        <*> monoidalFieldAla    "license-files"  (alaList' FSep FilePathNT) L.licenseFiles
            ^^^ hiddenField

-------------------------------------------------------------------------------
-- Library
-------------------------------------------------------------------------------

libraryFieldGrammar
    :: (FieldGrammar g, Applicative (g Library), Applicative (g BuildInfo))
    => Maybe UnqualComponentName -> g Library Library
libraryFieldGrammar n = Library n
    <$> monoidalFieldAla  "exposed-modules"    (alaList' VCat MQuoted) L.exposedModules
    <*> monoidalFieldAla  "reexported-modules" (alaList  CommaVCat)    L.reexportedModules
    <*> monoidalFieldAla  "signatures"         (alaList' VCat MQuoted) L.signatures
    <*> booleanFieldDef   "exposed"                                    L.libExposed True
    <*> blurFieldGrammar L.libBuildInfo buildInfoFieldGrammar
{-# SPECIALIZE libraryFieldGrammar :: Maybe UnqualComponentName -> ParsecFieldGrammar' Library #-}
{-# SPECIALIZE libraryFieldGrammar :: Maybe UnqualComponentName -> PrettyFieldGrammar' Library #-}

-------------------------------------------------------------------------------
-- Foreign library
-------------------------------------------------------------------------------

foreignLibFieldGrammar
    :: (FieldGrammar g, Applicative (g ForeignLib), Applicative (g BuildInfo))
    => UnqualComponentName -> g ForeignLib ForeignLib
foreignLibFieldGrammar n = ForeignLib n
    <$> optionalFieldDef "type"                                         L.foreignLibType ForeignLibTypeUnknown
    <*> monoidalFieldAla "options"           (alaList FSep)             L.foreignLibOptions
    <*> blurFieldGrammar L.foreignLibBuildInfo buildInfoFieldGrammar
    <*> optionalField    "lib-version-info"                             L.foreignLibVersionInfo
    <*> optionalField    "lib-version-linux"                            L.foreignLibVersionLinux
    <*> monoidalFieldAla "mod-def-file"      (alaList' FSep FilePathNT) L.foreignLibModDefFile
{-# SPECIALIZE foreignLibFieldGrammar :: UnqualComponentName -> ParsecFieldGrammar' ForeignLib #-}
{-# SPECIALIZE foreignLibFieldGrammar :: UnqualComponentName -> PrettyFieldGrammar' ForeignLib #-}

-------------------------------------------------------------------------------
-- Executable
-------------------------------------------------------------------------------

executableFieldGrammar
    :: (FieldGrammar g, Applicative (g Executable), Applicative (g BuildInfo))
    => UnqualComponentName -> g Executable Executable
executableFieldGrammar n = Executable n
    -- main-is is optional as conditional blocks don't have it
    <$> optionalFieldDefAla "main-is" FilePathNT L.modulePath ""
    <*> monoidalField       "scope"              L.exeScope
    <*> blurFieldGrammar L.buildInfo buildInfoFieldGrammar
{-# SPECIALIZE executableFieldGrammar :: UnqualComponentName -> ParsecFieldGrammar' Executable #-}
{-# SPECIALIZE executableFieldGrammar :: UnqualComponentName -> PrettyFieldGrammar' Executable #-}

-------------------------------------------------------------------------------
-- TestSuite
-------------------------------------------------------------------------------

-- | An intermediate type just used for parsing the test-suite stanza.
-- After validation it is converted into the proper 'TestSuite' type.
data TestSuiteStanza = TestSuiteStanza
    { _testStanzaTestType   :: Maybe TestType
    , _testStanzaMainIs     :: Maybe FilePath
    , _testStanzaTestModule :: Maybe ModuleName
    , _testStanzaBuildInfo  :: BuildInfo
    }

testStanzaTestType :: Lens' TestSuiteStanza (Maybe TestType)
testStanzaTestType f s = fmap (\x -> s { _testStanzaTestType = x }) (f (_testStanzaTestType s))
{-# INLINE testStanzaTestType #-}

testStanzaMainIs :: Lens' TestSuiteStanza (Maybe FilePath)
testStanzaMainIs f s = fmap (\x -> s { _testStanzaMainIs = x }) (f (_testStanzaMainIs s))
{-# INLINE testStanzaMainIs #-}

testStanzaTestModule :: Lens' TestSuiteStanza (Maybe ModuleName)
testStanzaTestModule f s = fmap (\x -> s { _testStanzaTestModule = x }) (f (_testStanzaTestModule s))
{-# INLINE testStanzaTestModule #-}

testStanzaBuildInfo :: Lens' TestSuiteStanza BuildInfo
testStanzaBuildInfo f s = fmap (\x -> s { _testStanzaBuildInfo = x }) (f (_testStanzaBuildInfo s))
{-# INLINE testStanzaBuildInfo #-}

testSuiteFieldGrammar
    :: (FieldGrammar g, Applicative (g TestSuiteStanza), Applicative (g BuildInfo))
    => g TestSuiteStanza TestSuiteStanza
testSuiteFieldGrammar = TestSuiteStanza
    <$> optionalField    "type"                   testStanzaTestType
    <*> optionalFieldAla "main-is"     FilePathNT testStanzaMainIs
    <*> optionalField    "test-module"            testStanzaTestModule
    <*> blurFieldGrammar testStanzaBuildInfo buildInfoFieldGrammar

validateTestSuite :: Position -> TestSuiteStanza -> ParseResult TestSuite
validateTestSuite pos stanza = case _testStanzaTestType stanza of
    Nothing -> return $
        emptyTestSuite { testBuildInfo = _testStanzaBuildInfo stanza }

    Just tt@(TestTypeUnknown _ _) ->
        pure emptyTestSuite
            { testInterface = TestSuiteUnsupported tt
            , testBuildInfo = _testStanzaBuildInfo stanza
            }

    Just tt | tt `notElem` knownTestTypes ->
        pure emptyTestSuite
            { testInterface = TestSuiteUnsupported tt
            , testBuildInfo = _testStanzaBuildInfo stanza
            }

    Just tt@(TestTypeExe ver) -> case _testStanzaMainIs stanza of
        Nothing   -> do
            parseFailure pos (missingField "main-is" tt)
            pure emptyTestSuite
        Just file -> do
            when (isJust (_testStanzaTestModule stanza)) $
                parseWarning pos PWTExtraBenchmarkModule (extraField "test-module" tt)
            pure emptyTestSuite
                { testInterface = TestSuiteExeV10 ver file
                , testBuildInfo = _testStanzaBuildInfo stanza
                }

    Just tt@(TestTypeLib ver) -> case _testStanzaTestModule stanza of
         Nothing      -> do
             parseFailure pos (missingField "test-module" tt)
             pure emptyTestSuite
         Just module_ -> do
            when (isJust (_testStanzaMainIs stanza)) $
                parseWarning pos PWTExtraMainIs (extraField "main-is" tt)
            pure emptyTestSuite
                { testInterface = TestSuiteLibV09 ver module_
                , testBuildInfo = _testStanzaBuildInfo stanza
                }

  where
    missingField name tt = "The '" ++ name ++ "' field is required for the "
                        ++ display tt ++ " test suite type."

    extraField   name tt = "The '" ++ name ++ "' field is not used for the '"
                        ++ display tt ++ "' test suite type."

unvalidateTestSuite :: TestSuite -> TestSuiteStanza
unvalidateTestSuite t = TestSuiteStanza
    { _testStanzaTestType   = ty
    , _testStanzaMainIs     = ma
    , _testStanzaTestModule = mo
    , _testStanzaBuildInfo  = testBuildInfo t
    }
  where
    (ty, ma, mo) = case testInterface t of
        TestSuiteExeV10 ver file -> (Just $ TestTypeExe ver, Just file, Nothing)
        TestSuiteLibV09 ver modu -> (Just $ TestTypeLib ver, Nothing, Just modu)
        _                        -> (Nothing, Nothing, Nothing)

-------------------------------------------------------------------------------
-- Benchmark
-------------------------------------------------------------------------------

-- | An intermediate type just used for parsing the benchmark stanza.
-- After validation it is converted into the proper 'Benchmark' type.
data BenchmarkStanza = BenchmarkStanza
    { _benchmarkStanzaBenchmarkType   :: Maybe BenchmarkType
    , _benchmarkStanzaMainIs          :: Maybe FilePath
    , _benchmarkStanzaBenchmarkModule :: Maybe ModuleName
    , _benchmarkStanzaBuildInfo       :: BuildInfo
    }

benchmarkStanzaBenchmarkType :: Lens' BenchmarkStanza (Maybe BenchmarkType)
benchmarkStanzaBenchmarkType f s = fmap (\x -> s { _benchmarkStanzaBenchmarkType = x }) (f (_benchmarkStanzaBenchmarkType s))
{-# INLINE benchmarkStanzaBenchmarkType #-}

benchmarkStanzaMainIs :: Lens' BenchmarkStanza (Maybe FilePath)
benchmarkStanzaMainIs f s = fmap (\x -> s { _benchmarkStanzaMainIs = x }) (f (_benchmarkStanzaMainIs s))
{-# INLINE benchmarkStanzaMainIs #-}

benchmarkStanzaBenchmarkModule :: Lens' BenchmarkStanza (Maybe ModuleName)
benchmarkStanzaBenchmarkModule f s = fmap (\x -> s { _benchmarkStanzaBenchmarkModule = x }) (f (_benchmarkStanzaBenchmarkModule s))
{-# INLINE benchmarkStanzaBenchmarkModule #-}

benchmarkStanzaBuildInfo :: Lens' BenchmarkStanza BuildInfo
benchmarkStanzaBuildInfo f s = fmap (\x -> s { _benchmarkStanzaBuildInfo = x }) (f (_benchmarkStanzaBuildInfo s))
{-# INLINE benchmarkStanzaBuildInfo #-}

benchmarkFieldGrammar
    :: (FieldGrammar g, Applicative (g BenchmarkStanza), Applicative (g BuildInfo))
    => g BenchmarkStanza BenchmarkStanza
benchmarkFieldGrammar = BenchmarkStanza
    <$> optionalField    "type"                        benchmarkStanzaBenchmarkType
    <*> optionalFieldAla "main-is"          FilePathNT benchmarkStanzaMainIs
    <*> optionalField    "benchmark-module"            benchmarkStanzaBenchmarkModule
    <*> blurFieldGrammar benchmarkStanzaBuildInfo buildInfoFieldGrammar

validateBenchmark :: Position -> BenchmarkStanza -> ParseResult Benchmark
validateBenchmark pos stanza = case _benchmarkStanzaBenchmarkType stanza of
    Nothing -> pure emptyBenchmark
        { benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza }

    Just tt@(BenchmarkTypeUnknown _ _) -> pure emptyBenchmark
        { benchmarkInterface = BenchmarkUnsupported tt
        , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
        }

    Just tt | tt `notElem` knownBenchmarkTypes -> pure emptyBenchmark
        { benchmarkInterface = BenchmarkUnsupported tt
        , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
        }

    Just tt@(BenchmarkTypeExe ver) -> case _benchmarkStanzaMainIs stanza of
        Nothing   -> do
            parseFailure pos (missingField "main-is" tt)
            pure emptyBenchmark
        Just file -> do
            when (isJust (_benchmarkStanzaBenchmarkModule stanza)) $
                parseWarning pos PWTExtraBenchmarkModule (extraField "benchmark-module" tt)
            pure emptyBenchmark
                { benchmarkInterface = BenchmarkExeV10 ver file
                , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
                }

  where
    missingField name tt = "The '" ++ name ++ "' field is required for the "
                        ++ display tt ++ " benchmark type."

    extraField   name tt = "The '" ++ name ++ "' field is not used for the '"
                        ++ display tt ++ "' benchmark type."

unvalidateBenchmark :: Benchmark -> BenchmarkStanza
unvalidateBenchmark b = BenchmarkStanza
    { _benchmarkStanzaBenchmarkType   = ty
    , _benchmarkStanzaMainIs          = ma
    , _benchmarkStanzaBenchmarkModule = mo
    , _benchmarkStanzaBuildInfo       = benchmarkBuildInfo b
    }
  where
    (ty, ma, mo) = case benchmarkInterface b of
        BenchmarkExeV10 ver ""  -> (Just $ BenchmarkTypeExe ver, Nothing,  Nothing)
        BenchmarkExeV10 ver ma' -> (Just $ BenchmarkTypeExe ver, Just ma', Nothing)
        _                       -> (Nothing, Nothing,  Nothing)

-------------------------------------------------------------------------------
-- Build info
-------------------------------------------------------------------------------

buildInfoFieldGrammar
    :: (FieldGrammar g, Applicative (g BuildInfo))
    => g BuildInfo BuildInfo
buildInfoFieldGrammar = BuildInfo
    <$> booleanFieldDef  "buildable"                                          L.buildable True
    <*> monoidalFieldAla "build-tools"          (alaList  CommaFSep)          L.buildTools
        ^^^ deprecatedSince [2,0] "Please use 'build-tool-depends' field"
    <*> monoidalFieldAla "build-tool-depends"   (alaList  CommaFSep)          L.buildToolDepends
        ^^^ availableSince [2,0]
    <*> monoidalFieldAla "cpp-options"          (alaList' NoCommaFSep Token') L.cppOptions
    <*> monoidalFieldAla "asm-options"          (alaList' NoCommaFSep Token') L.asmOptions
    <*> monoidalFieldAla "cmm-options"          (alaList' NoCommaFSep Token') L.cmmOptions
    <*> monoidalFieldAla "cc-options"           (alaList' NoCommaFSep Token') L.ccOptions
    <*> monoidalFieldAla "cxx-options"          (alaList' NoCommaFSep Token') L.cxxOptions
    <*> monoidalFieldAla "ld-options"           (alaList' NoCommaFSep Token') L.ldOptions
    <*> monoidalFieldAla "pkgconfig-depends"    (alaList  CommaFSep)          L.pkgconfigDepends
    <*> monoidalFieldAla "frameworks"           (alaList' FSep Token)         L.frameworks
    <*> monoidalFieldAla "extra-framework-dirs" (alaList' FSep FilePathNT)    L.extraFrameworkDirs
    <*> monoidalFieldAla "asm-sources"          (alaList' VCat FilePathNT)    L.asmSources
    <*> monoidalFieldAla "cmm-sources"          (alaList' VCat FilePathNT)    L.cmmSources
    <*> monoidalFieldAla "c-sources"            (alaList' VCat FilePathNT)    L.cSources
    <*> monoidalFieldAla "cxx-sources"          (alaList' VCat FilePathNT)    L.cxxSources
    <*> monoidalFieldAla "js-sources"           (alaList' VCat FilePathNT)    L.jsSources
    <*> hsSourceDirsGrammar
    <*> monoidalFieldAla "other-modules"        (alaList' VCat MQuoted)       L.otherModules
    <*> monoidalFieldAla "virtual-modules"      (alaList' VCat MQuoted)       L.virtualModules
    <*> monoidalFieldAla "autogen-modules"      (alaList' VCat MQuoted)       L.autogenModules
    <*> optionalFieldAla "default-language"     MQuoted                       L.defaultLanguage
    <*> monoidalFieldAla "other-languages"      (alaList' FSep MQuoted)       L.otherLanguages
    <*> monoidalFieldAla "default-extensions"   (alaList' FSep MQuoted)       L.defaultExtensions
    <*> monoidalFieldAla "other-extensions"     (alaList' FSep MQuoted)       L.otherExtensions
    <*> monoidalFieldAla "extensions"           (alaList' FSep MQuoted)       L.oldExtensions
        ^^^ deprecatedSince [1,12] "Please use 'default-extensions' or 'other-extensions' fields."
    <*> monoidalFieldAla "extra-libraries"      (alaList' VCat Token)         L.extraLibs
    <*> monoidalFieldAla "extra-ghci-libraries" (alaList' VCat Token)         L.extraGHCiLibs
    <*> monoidalFieldAla "extra-bundled-libraries" (alaList' VCat Token)      L.extraBundledLibs
    <*> monoidalFieldAla "extra-library-flavours" (alaList' VCat Token)       L.extraLibFlavours
    <*> monoidalFieldAla "extra-lib-dirs"       (alaList' FSep FilePathNT)    L.extraLibDirs
    <*> monoidalFieldAla "include-dirs"         (alaList' FSep FilePathNT)    L.includeDirs
    <*> monoidalFieldAla "includes"             (alaList' FSep FilePathNT)    L.includes
    <*> monoidalFieldAla "install-includes"     (alaList' FSep FilePathNT)    L.installIncludes
    <*> optionsFieldGrammar
    <*> profOptionsFieldGrammar
    <*> sharedOptionsFieldGrammar
    <*> pure [] -- static-options ???
    <*> prefixedFields   "x-"                                                 L.customFieldsBI
    <*> monoidalFieldAla "build-depends"        (alaList  CommaVCat)          L.targetBuildDepends
    <*> monoidalFieldAla "mixins"               (alaList  CommaVCat)          L.mixins
{-# SPECIALIZE buildInfoFieldGrammar :: ParsecFieldGrammar' BuildInfo #-}
{-# SPECIALIZE buildInfoFieldGrammar :: PrettyFieldGrammar' BuildInfo #-}

hsSourceDirsGrammar
    :: (FieldGrammar g, Applicative (g BuildInfo))
    => g BuildInfo [FilePath]
hsSourceDirsGrammar = (++)
    <$> monoidalFieldAla "hs-source-dirs" (alaList' FSep FilePathNT) L.hsSourceDirs
    <*> monoidalFieldAla "hs-source-dir"  (alaList' FSep FilePathNT) L.hsSourceDirs
        ^^^ deprecatedField' "Please use 'hs-source-dirs'"

optionsFieldGrammar
    :: (FieldGrammar g, Applicative (g BuildInfo))
    => g BuildInfo [(CompilerFlavor, [String])]
optionsFieldGrammar = combine
    <$> monoidalFieldAla "ghc-options"   (alaList' NoCommaFSep Token') (extract GHC)
    <*> monoidalFieldAla "ghcjs-options" (alaList' NoCommaFSep Token') (extract GHCJS)
    <*> monoidalFieldAla "jhc-options"   (alaList' NoCommaFSep Token') (extract JHC)
    -- NOTE: Hugs and NHC are not supported anymore, but these fields are kept
    -- around for backwards compatibility.
    <*  knownField "hugs-options"
    <*  knownField "nhc98-options"
  where
    extract :: CompilerFlavor -> ALens' BuildInfo [String]
    extract flavor = L.options . lookupLens flavor

    combine ghc ghcjs jhs =
        f GHC ghc ++ f GHCJS ghcjs ++ f JHC jhs
      where
        f _flavor []   = []
        f  flavor opts = [(flavor, opts)]

profOptionsFieldGrammar
    :: (FieldGrammar g, Applicative (g BuildInfo))
    => g BuildInfo [(CompilerFlavor, [String])]
profOptionsFieldGrammar = combine
    <$> monoidalFieldAla "ghc-prof-options"   (alaList' NoCommaFSep Token') (extract GHC)
    <*> monoidalFieldAla "ghcjs-prof-options" (alaList' NoCommaFSep Token') (extract GHCJS)
  where
    extract :: CompilerFlavor -> ALens' BuildInfo [String]
    extract flavor = L.profOptions . lookupLens flavor

    combine ghc ghcjs = f GHC ghc ++ f GHCJS ghcjs
      where
        f _flavor []   = []
        f  flavor opts = [(flavor, opts)]

sharedOptionsFieldGrammar
    :: (FieldGrammar g, Applicative (g BuildInfo))
    => g BuildInfo [(CompilerFlavor, [String])]
sharedOptionsFieldGrammar = combine
    <$> monoidalFieldAla "ghc-shared-options"   (alaList' NoCommaFSep Token') (extract GHC)
    <*> monoidalFieldAla "ghcjs-shared-options" (alaList' NoCommaFSep Token') (extract GHCJS)
  where
    extract :: CompilerFlavor -> ALens' BuildInfo [String]
    extract flavor = L.sharedOptions . lookupLens flavor

    combine ghc ghcjs = f GHC ghc ++ f GHCJS ghcjs
      where
        f _flavor []   = []
        f  flavor opts = [(flavor, opts)]

lookupLens :: (Functor f, Ord k) => k -> LensLike' f [(k, [v])] [v]
lookupLens k f kvs = str kvs <$> f (gtr kvs)
  where
    gtr = fromMaybe [] . lookup k

    str []            v = [(k, v)]
    str (x@(k',_):xs) v
        | k == k'       = (k, v) : xs
        | otherwise     = x : str xs v

-------------------------------------------------------------------------------
-- Flag
-------------------------------------------------------------------------------

flagFieldGrammar
    :: (FieldGrammar g, Applicative (g Flag))
    =>  FlagName -> g Flag Flag
flagFieldGrammar name = MkFlag name
    <$> optionalFieldDefAla "description" FreeText L.flagDescription ""
    <*> booleanFieldDef     "default"              L.flagDefault     True
    <*> booleanFieldDef     "manual"               L.flagManual      False
{-# SPECIALIZE flagFieldGrammar :: FlagName -> ParsecFieldGrammar' Flag #-}
{-# SPECIALIZE flagFieldGrammar :: FlagName -> PrettyFieldGrammar' Flag #-}

-------------------------------------------------------------------------------
-- SourceRepo
-------------------------------------------------------------------------------

sourceRepoFieldGrammar
    :: (FieldGrammar g, Applicative (g SourceRepo))
    => RepoKind -> g SourceRepo SourceRepo
sourceRepoFieldGrammar kind = SourceRepo kind
    <$> optionalField    "type"                L.repoType
    <*> optionalFieldAla "location" FreeText   L.repoLocation
    <*> optionalFieldAla "module"   Token      L.repoModule
    <*> optionalFieldAla "branch"   Token      L.repoBranch
    <*> optionalFieldAla "tag"      Token      L.repoTag
    <*> optionalFieldAla "subdir"   FilePathNT L.repoSubdir
{-# SPECIALIZE sourceRepoFieldGrammar :: RepoKind -> ParsecFieldGrammar' SourceRepo #-}
{-# SPECIALIZE sourceRepoFieldGrammar :: RepoKind ->PrettyFieldGrammar' SourceRepo #-}

-------------------------------------------------------------------------------
-- SetupBuildInfo
-------------------------------------------------------------------------------

setupBInfoFieldGrammar
    :: (FieldGrammar g, Functor (g SetupBuildInfo))
    => Bool -> g SetupBuildInfo SetupBuildInfo
setupBInfoFieldGrammar def = flip SetupBuildInfo def
    <$> monoidalFieldAla "setup-depends" (alaList CommaVCat) L.setupDepends
{-# SPECIALIZE setupBInfoFieldGrammar :: Bool -> ParsecFieldGrammar' SetupBuildInfo #-}
{-# SPECIALIZE setupBInfoFieldGrammar :: Bool ->PrettyFieldGrammar' SetupBuildInfo #-}
