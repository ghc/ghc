{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription.Parse
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defined parsers and partial pretty printers for the @.cabal@ format.
-- Some of the complexity in this module is due to the fact that we have to be
-- backwards compatible with old @.cabal@ files, so there's code to translate
-- into the newer structure.

module Distribution.PackageDescription.Parse (
        -- * Package descriptions
        readGenericPackageDescription,
        parseGenericPackageDescription,

        -- ** Deprecated names
        readPackageDescription,
        parsePackageDescription,

        -- ** Parsing
        ParseResult(..),
        FieldDescr(..),
        LineNo,

        -- ** Private, but needed for pretty-printer
        TestSuiteStanza(..),
        BenchmarkStanza(..),

        -- ** Supplementary build information
        readHookedBuildInfo,
        parseHookedBuildInfo,

        pkgDescrFieldDescrs,
        libFieldDescrs,
        foreignLibFieldDescrs,
        executableFieldDescrs,
        binfoFieldDescrs,
        sourceRepoFieldDescrs,
        testSuiteFieldDescrs,
        benchmarkFieldDescrs,
        flagFieldDescrs
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Dependency
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibType
import Distribution.Types.UnqualComponentName
import Distribution.Types.CondTree
import Distribution.Types.PackageId
import Distribution.ParseUtils hiding (parseFields)
import Distribution.PackageDescription
import Distribution.PackageDescription.Utils
import Distribution.Package
import Distribution.ModuleName
import Distribution.Version
import Distribution.Verbosity
import Distribution.Compiler
import Distribution.PackageDescription.Configuration
import Distribution.Simple.Utils
import Distribution.Text
import Distribution.Compat.ReadP hiding (get)

import Data.List        (partition, (\\))
import System.Directory (doesFileExist)
import Control.Monad    (mapM)

import Text.PrettyPrint
       (vcat, ($$), (<+>), text, render,
        comma, fsep, nest, ($+$), punctuate)


-- -----------------------------------------------------------------------------
-- The PackageDescription type

pkgDescrFieldDescrs :: [FieldDescr PackageDescription]
pkgDescrFieldDescrs =
    [ simpleField "name"
           disp                   parse
           packageName            (\name pkg -> pkg{package=(package pkg){pkgName=name}})
 , simpleField "version"
           disp                   parse
           packageVersion         (\ver pkg -> pkg{package=(package pkg){pkgVersion=ver}})
 , simpleField "cabal-version"
           (either disp disp)     (liftM Left parse +++ liftM Right parse)
           specVersionRaw         (\v pkg -> pkg{specVersionRaw=v})
 , simpleField "build-type"
           (maybe mempty disp)  (fmap Just parse)
           buildType              (\t pkg -> pkg{buildType=t})
 , simpleField "license"
           disp                   parseLicenseQ
           license                (\l pkg -> pkg{license=l})
   -- We have both 'license-file' and 'license-files' fields.
   -- Rather than declaring license-file to be deprecated, we will continue
   -- to allow both. The 'license-file' will continue to only allow single
   -- tokens, while 'license-files' allows multiple. On pretty-printing, we
   -- will use 'license-file' if there's just one, and use 'license-files'
   -- otherwise.
 , simpleField "license-file"
           showFilePath           parseFilePathQ
           (\pkg -> case licenseFiles pkg of
                      [x] -> x
                      _   -> "")
           (\l pkg -> pkg{licenseFiles=licenseFiles pkg ++ [l]})
 , listField "license-files"
           showFilePath           parseFilePathQ
           (\pkg -> case licenseFiles pkg of
                      [_] -> []
                      xs  -> xs)
           (\ls pkg -> pkg{licenseFiles=licenseFiles pkg ++ ls})
 , simpleField "copyright"
           showFreeText           parseFreeText
           copyright              (\val pkg -> pkg{copyright=val})
 , simpleField "maintainer"
           showFreeText           parseFreeText
           maintainer             (\val pkg -> pkg{maintainer=val})
 , simpleField "stability"
           showFreeText           parseFreeText
           stability              (\val pkg -> pkg{stability=val})
 , simpleField "homepage"
           showFreeText           parseFreeText
           homepage               (\val pkg -> pkg{homepage=val})
 , simpleField "package-url"
           showFreeText           parseFreeText
           pkgUrl                 (\val pkg -> pkg{pkgUrl=val})
 , simpleField "bug-reports"
           showFreeText           parseFreeText
           bugReports             (\val pkg -> pkg{bugReports=val})
 , simpleField "synopsis"
           showFreeText           parseFreeText
           synopsis               (\val pkg -> pkg{synopsis=val})
 , simpleField "description"
           showFreeText           parseFreeText
           description            (\val pkg -> pkg{description=val})
 , simpleField "category"
           showFreeText           parseFreeText
           category               (\val pkg -> pkg{category=val})
 , simpleField "author"
           showFreeText           parseFreeText
           author                 (\val pkg -> pkg{author=val})
 , listField "tested-with"
           showTestedWith         parseTestedWithQ
           testedWith             (\val pkg -> pkg{testedWith=val})
 , listFieldWithSep vcat "data-files"
           showFilePath           parseFilePathQ
           dataFiles              (\val pkg -> pkg{dataFiles=val})
 , simpleField "data-dir"
           showFilePath           parseFilePathQ
           dataDir                (\val pkg -> pkg{dataDir=val})
 , listFieldWithSep vcat "extra-source-files"
           showFilePath    parseFilePathQ
           extraSrcFiles          (\val pkg -> pkg{extraSrcFiles=val})
 , listFieldWithSep vcat "extra-tmp-files"
           showFilePath       parseFilePathQ
           extraTmpFiles          (\val pkg -> pkg{extraTmpFiles=val})
 , listFieldWithSep vcat "extra-doc-files"
           showFilePath    parseFilePathQ
           extraDocFiles          (\val pkg -> pkg{extraDocFiles=val})
 ]

-- | Store any fields beginning with "x-" in the customFields field of
--   a PackageDescription.  All other fields will generate a warning.
storeXFieldsPD :: UnrecFieldParser PackageDescription
storeXFieldsPD (f@('x':'-':_),val) pkg =
  Just pkg{ customFieldsPD =
               customFieldsPD pkg ++ [(f,val)]}
storeXFieldsPD _ _ = Nothing

-- ---------------------------------------------------------------------------
-- The Library type

libFieldDescrs :: [FieldDescr Library]
libFieldDescrs =
  [ listFieldWithSep vcat "exposed-modules" disp parseModuleNameQ
      exposedModules (\mods lib -> lib{exposedModules=mods})

  , commaListFieldWithSep vcat "reexported-modules" disp parse
      reexportedModules (\mods lib -> lib{reexportedModules=mods})

  , listFieldWithSep vcat "signatures" disp parseModuleNameQ
      signatures (\mods lib -> lib{signatures=mods})

  , boolField "exposed"
      libExposed     (\val lib -> lib{libExposed=val})
  ] ++ map biToLib binfoFieldDescrs
  where biToLib = liftField libBuildInfo (\bi lib -> lib{libBuildInfo=bi})

storeXFieldsLib :: UnrecFieldParser Library
storeXFieldsLib (f@('x':'-':_), val) l@(Library { libBuildInfo = bi }) =
    Just $ l {libBuildInfo =
                 bi{ customFieldsBI = customFieldsBI bi ++ [(f,val)]}}
storeXFieldsLib _ _ = Nothing

-- ---------------------------------------------------------------------------
-- Foreign libraries

foreignLibFieldDescrs :: [FieldDescr ForeignLib]
foreignLibFieldDescrs =
  [ simpleField "type"
      disp                   parse
      foreignLibType         (\x flib -> flib { foreignLibType = x })
  , listField "options"
      disp                   parse
      foreignLibOptions      (\x flib -> flib { foreignLibOptions = x })
  , simpleField "lib-version-info"
      (maybe mempty disp)    (fmap Just parse)
      foreignLibVersionInfo  (\x flib -> flib { foreignLibVersionInfo = x })
  , simpleField "lib-version-linux"
      (maybe mempty disp)    (fmap Just parse)
      foreignLibVersionLinux (\x flib -> flib { foreignLibVersionLinux = x })
  , listField "mod-def-file"
      showFilePath           parseFilePathQ
      foreignLibModDefFile   (\x flib -> flib { foreignLibModDefFile = x })
  ]
  ++ map biToFLib binfoFieldDescrs
  where biToFLib = liftField foreignLibBuildInfo $ \bi flib ->
          flib { foreignLibBuildInfo = bi }

storeXFieldsForeignLib :: UnrecFieldParser ForeignLib
storeXFieldsForeignLib (f@('x':'-':_), val)
                        l@(ForeignLib { foreignLibBuildInfo = bi }) =
    Just $ l { foreignLibBuildInfo = bi {
                   customFieldsBI = (f,val):customFieldsBI bi
                 }
             }
storeXFieldsForeignLib _ _ = Nothing

-- ---------------------------------------------------------------------------
-- The Executable type


executableFieldDescrs :: [FieldDescr Executable]
executableFieldDescrs =
  [ simpleField "main-is"
                           showFilePath       parseFilePathQ
                           modulePath         (\xs    exe -> exe{modulePath=xs})
  , simpleField "scope"
                           disp               parse
                           exeScope           (\sc    exe -> exe{exeScope=sc})
  ]
  ++ map biToExe binfoFieldDescrs
  where biToExe = liftField buildInfo (\bi exe -> exe{buildInfo=bi})

storeXFieldsExe :: UnrecFieldParser Executable
storeXFieldsExe (f@('x':'-':_), val) e@(Executable { buildInfo = bi }) =
    Just $ e {buildInfo = bi{ customFieldsBI = (f,val):customFieldsBI bi}}
storeXFieldsExe _ _ = Nothing

-- ---------------------------------------------------------------------------
-- The TestSuite type

-- | An intermediate type just used for parsing the test-suite stanza.
-- After validation it is converted into the proper 'TestSuite' type.
data TestSuiteStanza = TestSuiteStanza {
       testStanzaTestType   :: Maybe TestType,
       testStanzaMainIs     :: Maybe FilePath,
       testStanzaTestModule :: Maybe ModuleName,
       testStanzaBuildInfo  :: BuildInfo
     }

emptyTestStanza :: TestSuiteStanza
emptyTestStanza = TestSuiteStanza Nothing Nothing Nothing mempty

testSuiteFieldDescrs :: [FieldDescr TestSuiteStanza]
testSuiteFieldDescrs =
    [ simpleField "type"
        (maybe mempty disp) (fmap Just parse)
        testStanzaTestType    (\x suite -> suite { testStanzaTestType = x })
    , simpleField "main-is"
        (maybe mempty showFilePath)  (fmap Just parseFilePathQ)
        testStanzaMainIs      (\x suite -> suite { testStanzaMainIs = x })
    , simpleField "test-module"
        (maybe mempty disp) (fmap Just parseModuleNameQ)
        testStanzaTestModule  (\x suite -> suite { testStanzaTestModule = x })
    ]
    ++ map biToTest binfoFieldDescrs
  where
    biToTest = liftField testStanzaBuildInfo
                         (\bi suite -> suite { testStanzaBuildInfo = bi })

storeXFieldsTest :: UnrecFieldParser TestSuiteStanza
storeXFieldsTest (f@('x':'-':_), val) t@(TestSuiteStanza { testStanzaBuildInfo = bi }) =
    Just $ t {testStanzaBuildInfo = bi{ customFieldsBI = (f,val):customFieldsBI bi}}
storeXFieldsTest _ _ = Nothing

validateTestSuite :: LineNo -> TestSuiteStanza -> ParseResult TestSuite
validateTestSuite line stanza =
    case testStanzaTestType stanza of
      Nothing -> return $
        emptyTestSuite { testBuildInfo = testStanzaBuildInfo stanza }

      Just tt@(TestTypeUnknown _ _) ->
        return emptyTestSuite {
          testInterface = TestSuiteUnsupported tt,
          testBuildInfo = testStanzaBuildInfo stanza
        }

      Just tt | tt `notElem` knownTestTypes ->
        return emptyTestSuite {
          testInterface = TestSuiteUnsupported tt,
          testBuildInfo = testStanzaBuildInfo stanza
        }

      Just tt@(TestTypeExe ver) ->
        case testStanzaMainIs stanza of
          Nothing   -> syntaxError line (missingField "main-is" tt)
          Just file -> do
            when (isJust (testStanzaTestModule stanza)) $
              warning (extraField "test-module" tt)
            return emptyTestSuite {
              testInterface = TestSuiteExeV10 ver file,
              testBuildInfo = testStanzaBuildInfo stanza
            }

      Just tt@(TestTypeLib ver) ->
        case testStanzaTestModule stanza of
          Nothing      -> syntaxError line (missingField "test-module" tt)
          Just module_ -> do
            when (isJust (testStanzaMainIs stanza)) $
              warning (extraField "main-is" tt)
            return emptyTestSuite {
              testInterface = TestSuiteLibV09 ver module_,
              testBuildInfo = testStanzaBuildInfo stanza
            }

  where
    missingField name tt = "The '" ++ name ++ "' field is required for the "
                        ++ display tt ++ " test suite type."

    extraField   name tt = "The '" ++ name ++ "' field is not used for the '"
                        ++ display tt ++ "' test suite type."


-- ---------------------------------------------------------------------------
-- The Benchmark type

-- | An intermediate type just used for parsing the benchmark stanza.
-- After validation it is converted into the proper 'Benchmark' type.
data BenchmarkStanza = BenchmarkStanza {
       benchmarkStanzaBenchmarkType   :: Maybe BenchmarkType,
       benchmarkStanzaMainIs          :: Maybe FilePath,
       benchmarkStanzaBenchmarkModule :: Maybe ModuleName,
       benchmarkStanzaBuildInfo       :: BuildInfo
     }

emptyBenchmarkStanza :: BenchmarkStanza
emptyBenchmarkStanza = BenchmarkStanza Nothing Nothing Nothing mempty

benchmarkFieldDescrs :: [FieldDescr BenchmarkStanza]
benchmarkFieldDescrs =
    [ simpleField "type"
        (maybe mempty disp)    (fmap Just parse)
        benchmarkStanzaBenchmarkType
        (\x suite -> suite { benchmarkStanzaBenchmarkType = x })
    , simpleField "main-is"
        (maybe mempty showFilePath)  (fmap Just parseFilePathQ)
        benchmarkStanzaMainIs
        (\x suite -> suite { benchmarkStanzaMainIs = x })
    ]
    ++ map biToBenchmark binfoFieldDescrs
  where
    biToBenchmark = liftField benchmarkStanzaBuildInfo
                    (\bi suite -> suite { benchmarkStanzaBuildInfo = bi })

storeXFieldsBenchmark :: UnrecFieldParser BenchmarkStanza
storeXFieldsBenchmark (f@('x':'-':_), val)
    t@(BenchmarkStanza { benchmarkStanzaBuildInfo = bi }) =
        Just $ t {benchmarkStanzaBuildInfo =
                       bi{ customFieldsBI = (f,val):customFieldsBI bi}}
storeXFieldsBenchmark _ _ = Nothing

validateBenchmark :: LineNo -> BenchmarkStanza -> ParseResult Benchmark
validateBenchmark line stanza =
    case benchmarkStanzaBenchmarkType stanza of
      Nothing -> return $
        emptyBenchmark { benchmarkBuildInfo = benchmarkStanzaBuildInfo stanza }

      Just tt@(BenchmarkTypeUnknown _ _) ->
        return emptyBenchmark {
          benchmarkInterface = BenchmarkUnsupported tt,
          benchmarkBuildInfo = benchmarkStanzaBuildInfo stanza
        }

      Just tt | tt `notElem` knownBenchmarkTypes ->
        return emptyBenchmark {
          benchmarkInterface = BenchmarkUnsupported tt,
          benchmarkBuildInfo = benchmarkStanzaBuildInfo stanza
        }

      Just tt@(BenchmarkTypeExe ver) ->
        case benchmarkStanzaMainIs stanza of
          Nothing   -> syntaxError line (missingField "main-is" tt)
          Just file -> do
            when (isJust (benchmarkStanzaBenchmarkModule stanza)) $
              warning (extraField "benchmark-module" tt)
            return emptyBenchmark {
              benchmarkInterface = BenchmarkExeV10 ver file,
              benchmarkBuildInfo = benchmarkStanzaBuildInfo stanza
            }

  where
    missingField name tt = "The '" ++ name ++ "' field is required for the "
                        ++ display tt ++ " benchmark type."

    extraField   name tt = "The '" ++ name ++ "' field is not used for the '"
                        ++ display tt ++ "' benchmark type."

-- ---------------------------------------------------------------------------
-- The BuildInfo type

binfoFieldDescrs :: [FieldDescr BuildInfo]
binfoFieldDescrs =
 [ boolField "buildable"
           buildable          (\val binfo -> binfo{buildable=val})
 , commaListField  "build-tools"
           disp               parse
           buildTools         (\xs  binfo -> binfo{buildTools=xs})
 , commaListField  "build-tool-depends"
           disp               parse
           buildToolDepends   (\xs  binfo -> binfo{buildToolDepends=xs})
 , commaListFieldWithSep vcat "build-depends"
           disp               parse
           targetBuildDepends (\xs binfo -> binfo{targetBuildDepends=xs})
 , commaListFieldWithSep vcat "mixins"
           disp               parse
           mixins             (\xs binfo -> binfo{mixins=xs})
 , spaceListField "cpp-options"
           showToken          parseTokenQ'
           cppOptions         (\val binfo -> binfo{cppOptions=val})
 , spaceListField "asm-options"
           showToken          parseTokenQ'
           asmOptions         (\val binfo -> binfo{asmOptions=val})
 , spaceListField "cmm-options"
           showToken          parseTokenQ'
           cmmOptions         (\val binfo -> binfo{cmmOptions=val})
 , spaceListField "cc-options"
           showToken          parseTokenQ'
           ccOptions          (\val binfo -> binfo{ccOptions=val})
 , spaceListField "cxx-options"
           showToken          parseTokenQ'
           cxxOptions         (\val binfo -> binfo{cxxOptions=val})
 , spaceListField "ld-options"
           showToken          parseTokenQ'
           ldOptions          (\val binfo -> binfo{ldOptions=val})
 , commaListField  "pkgconfig-depends"
           disp               parse
           pkgconfigDepends   (\xs  binfo -> binfo{pkgconfigDepends=xs})
 , listField "frameworks"
           showToken          parseTokenQ
           frameworks         (\val binfo -> binfo{frameworks=val})
 , listField "extra-framework-dirs"
           showToken          parseFilePathQ
           extraFrameworkDirs (\val binfo -> binfo{extraFrameworkDirs=val})
 , listFieldWithSep vcat "asm-sources"
           showFilePath       parseFilePathQ
           asmSources         (\paths binfo -> binfo{asmSources=paths})
 , listFieldWithSep vcat "cmm-sources"
           showFilePath       parseFilePathQ
           cmmSources         (\paths binfo -> binfo{cmmSources=paths})
 , listFieldWithSep vcat "c-sources"
           showFilePath       parseFilePathQ
           cSources           (\paths binfo -> binfo{cSources=paths})
 , listFieldWithSep vcat "cxx-sources"
           showFilePath       parseFilePathQ
           cxxSources         (\paths binfo -> binfo{cxxSources=paths})
 , listFieldWithSep vcat "js-sources"
           showFilePath       parseFilePathQ
           jsSources          (\paths binfo -> binfo{jsSources=paths})
 , simpleField "default-language"
           (maybe mempty disp) (option Nothing (fmap Just parseLanguageQ))
           defaultLanguage    (\lang  binfo -> binfo{defaultLanguage=lang})
 , listField   "other-languages"
           disp               parseLanguageQ
           otherLanguages     (\langs binfo -> binfo{otherLanguages=langs})
 , listField   "default-extensions"
           disp               parseExtensionQ
           defaultExtensions  (\exts  binfo -> binfo{defaultExtensions=exts})
 , listField   "other-extensions"
           disp               parseExtensionQ
           otherExtensions    (\exts  binfo -> binfo{otherExtensions=exts})
 , listField   "extensions"
           disp               parseExtensionQ
           oldExtensions      (\exts  binfo -> binfo{oldExtensions=exts})

 , listFieldWithSep vcat "extra-libraries"
           showToken          parseTokenQ
           extraLibs          (\xs    binfo -> binfo{extraLibs=xs})
 , listFieldWithSep vcat "extra-ghci-libraries"
           showToken          parseTokenQ
           extraGHCiLibs      (\xs    binfo -> binfo{extraGHCiLibs=xs})
 , listFieldWithSep vcat "extra-bundled-libraries"
           showToken          parseTokenQ
           extraBundledLibs   (\xs binfo -> binfo{extraBundledLibs=xs})
 , listFieldWithSep vcat "extra-library-flavours"
           showToken          parseTokenQ
           extraLibFlavours   (\xs binfo -> binfo{extraLibFlavours=xs})
 , listField   "extra-lib-dirs"
           showFilePath       parseFilePathQ
           extraLibDirs       (\xs    binfo -> binfo{extraLibDirs=xs})
 , listFieldWithSep vcat "includes"
           showFilePath       parseFilePathQ
           includes           (\paths binfo -> binfo{includes=paths})
 , listFieldWithSep vcat "install-includes"
           showFilePath       parseFilePathQ
           installIncludes    (\paths binfo -> binfo{installIncludes=paths})
 , listField   "include-dirs"
           showFilePath       parseFilePathQ
           includeDirs        (\paths binfo -> binfo{includeDirs=paths})
 , listField   "hs-source-dirs"
           showFilePath       parseFilePathQ
           hsSourceDirs       (\paths binfo -> binfo{hsSourceDirs=paths})
 , listFieldWithSep vcat "other-modules"
           disp               parseModuleNameQ
           otherModules       (\val binfo -> binfo{otherModules=val})
 , listFieldWithSep vcat "virtual-modules"
           disp               parseModuleNameQ
           virtualModules       (\val binfo -> binfo{virtualModules=val})
 , listFieldWithSep vcat "autogen-modules"
           disp               parseModuleNameQ
           autogenModules       (\val binfo -> binfo{autogenModules=val})
 , optsField   "ghc-prof-options" GHC
           profOptions        (\val binfo -> binfo{profOptions=val})
 , optsField   "ghcjs-prof-options" GHCJS
           profOptions        (\val binfo -> binfo{profOptions=val})
 , optsField   "ghc-shared-options" GHC
           sharedOptions      (\val binfo -> binfo{sharedOptions=val})
 , optsField   "ghcjs-shared-options" GHCJS
           sharedOptions      (\val binfo -> binfo{sharedOptions=val})
 , optsField   "ghc-options"  GHC
           options            (\path  binfo -> binfo{options=path})
 , optsField   "ghcjs-options" GHCJS
           options            (\path  binfo -> binfo{options=path})
 , optsField   "jhc-options"  JHC
           options            (\path  binfo -> binfo{options=path})

 -- NOTE: Hugs and NHC are not supported anymore, but these fields are kept
 -- around for backwards compatibility.
 , optsField   "hugs-options" Hugs
           options            (const id)
 , optsField   "nhc98-options" NHC
           options            (const id)
 ]

storeXFieldsBI :: UnrecFieldParser BuildInfo
storeXFieldsBI (f@('x':'-':_),val) bi = Just bi{ customFieldsBI = (f,val):customFieldsBI bi }
storeXFieldsBI _ _ = Nothing

------------------------------------------------------------------------------

flagFieldDescrs :: [FieldDescr Flag]
flagFieldDescrs =
    [ simpleField "description"
        showFreeText     parseFreeText
        flagDescription  (\val fl -> fl{ flagDescription = val })
    , boolField "default"
        flagDefault      (\val fl -> fl{ flagDefault = val })
    , boolField "manual"
        flagManual       (\val fl -> fl{ flagManual = val })
    ]

------------------------------------------------------------------------------

sourceRepoFieldDescrs :: [FieldDescr SourceRepo]
sourceRepoFieldDescrs =
    [ simpleField "type"
        (maybe mempty disp)         (fmap Just parse)
        repoType                   (\val repo -> repo { repoType = val })
    , simpleField "location"
        (maybe mempty showFreeText) (fmap Just parseFreeText)
        repoLocation               (\val repo -> repo { repoLocation = val })
    , simpleField "module"
        (maybe mempty showToken)    (fmap Just parseTokenQ)
        repoModule                 (\val repo -> repo { repoModule = val })
    , simpleField "branch"
        (maybe mempty showToken)    (fmap Just parseTokenQ)
        repoBranch                 (\val repo -> repo { repoBranch = val })
    , simpleField "tag"
        (maybe mempty showToken)    (fmap Just parseTokenQ)
        repoTag                    (\val repo -> repo { repoTag = val })
    , simpleField "subdir"
        (maybe mempty showFilePath) (fmap Just parseFilePathQ)
        repoSubdir                 (\val repo -> repo { repoSubdir = val })
    ]

------------------------------------------------------------------------------

setupBInfoFieldDescrs :: [FieldDescr SetupBuildInfo]
setupBInfoFieldDescrs =
    [ commaListFieldWithSep vcat "setup-depends"
        disp         parse
        setupDepends (\xs binfo -> binfo{setupDepends=xs})
    ]

-- ---------------------------------------------------------------
-- Parsing

-- | Given a parser and a filename, return the parse of the file,
-- after checking if the file exists.
readAndParseFile :: (FilePath -> (String -> IO a) -> IO a)
                 -> (String -> ParseResult a)
                 -> Verbosity
                 -> FilePath -> IO a
readAndParseFile withFileContents' parser verbosity fpath = do
  exists <- doesFileExist fpath
  unless exists $
    die' verbosity $
      "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue."
  withFileContents' fpath $ \str -> case parser str of
    ParseFailed e -> do
        let (line, message) = locatedErrorMsg e
        dieWithLocation' verbosity fpath line message
    ParseOk warnings x -> do
        traverse_ (warn verbosity . showPWarning fpath) $ reverse warnings
        return x

readHookedBuildInfo :: Verbosity -> FilePath -> IO HookedBuildInfo
readHookedBuildInfo =
    readAndParseFile withFileContents parseHookedBuildInfo

readPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
readPackageDescription = readGenericPackageDescription
{-# DEPRECATED readPackageDescription "Use readGenericPackageDescription, old name is misleading." #-}

-- | Parse the given package file.
readGenericPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
readGenericPackageDescription =
    readAndParseFile withUTF8FileContents parseGenericPackageDescription

stanzas :: [Field] -> [[Field]]
stanzas [] = []
stanzas (f:fields) = (f:this) : stanzas rest
  where
    (this, rest) = break isStanzaHeader fields

isStanzaHeader :: Field -> Bool
isStanzaHeader (F _ f _) = f == "executable"
isStanzaHeader _ = False

------------------------------------------------------------------------------


mapSimpleFields :: (Field -> ParseResult Field) -> [Field]
                -> ParseResult [Field]
mapSimpleFields f = traverse walk
  where
    walk fld@F{} = f fld
    walk (IfBlock l c fs1 fs2) = do
      fs1' <- traverse walk fs1
      fs2' <- traverse walk fs2
      return (IfBlock l c fs1' fs2')
    walk (Section ln n l fs1) = do
      fs1' <-  traverse walk fs1
      return (Section ln n l fs1')

-- prop_isMapM fs = mapSimpleFields return fs == return fs


-- names of fields that represents dependencies
-- TODO: maybe build-tools should go here too?
constraintFieldNames :: [String]
constraintFieldNames = ["build-depends"]

-- Possible refactoring would be to have modifiers be explicit about what
-- they add and define an accessor that specifies what the dependencies
-- are.  This way we would completely reuse the parsing knowledge from the
-- field descriptor.
parseConstraint :: Field -> ParseResult [Dependency]
parseConstraint (F l n v)
    | n `elem` constraintFieldNames = runP l n (parseCommaList parse) v
parseConstraint f = userBug $ "Constraint was expected (got: " ++ show f ++ ")"

{-
headerFieldNames :: [String]
headerFieldNames = filter (\n -> not (n `elem` constraintFieldNames))
                 . map fieldName $ pkgDescrFieldDescrs
-}

libFieldNames :: [String]
libFieldNames = map fieldName libFieldDescrs
                ++ buildInfoNames ++ constraintFieldNames

-- exeFieldNames :: [String]
-- exeFieldNames = map fieldName executableFieldDescrs
--                 ++ buildInfoNames

buildInfoNames :: [String]
buildInfoNames = map fieldName binfoFieldDescrs
                ++ map fst deprecatedFieldsBuildInfo

-- A minimal implementation of the StateT monad transformer to avoid depending
-- on the 'mtl' package.
newtype StT s m a = StT { runStT :: s -> m (a,s) }

instance Functor f => Functor (StT s f) where
    fmap g (StT f) = StT $ fmap (first g)  . f

#if __GLASGOW_HASKELL__ >= 710
instance (Monad m) => Applicative (StT s m) where
#else
instance (Monad m, Functor m) => Applicative (StT s m) where
#endif
    pure a = StT (\s -> return (a,s))
    (<*>) = ap


instance Monad m => Monad (StT s m) where
#if __GLASGOW_HASKELL__ < 710
    return a = StT (\s -> return (a,s))
#endif
    StT f >>= g = StT $ \s -> do
                        (a,s') <- f s
                        runStT (g a) s'

getSt :: Monad m => StT s m s
getSt = StT $ \s -> return (s, s)

modify :: Monad m => (s -> s) -> StT s m ()
modify f = StT $ \s -> return ((),f s)

lift :: Monad m => m a -> StT s m a
lift m = StT $ \s -> m >>= \a -> return (a,s)

evalStT :: Monad m => StT s m a -> s -> m a
evalStT st s = liftM fst $ runStT st s

-- Our monad for parsing a list/tree of fields.
--
-- The state represents the remaining fields to be processed.
type PM a = StT [Field] ParseResult a



-- return look-ahead field or nothing if we're at the end of the file
peekField :: PM (Maybe Field)
peekField = liftM listToMaybe getSt

-- Unconditionally discard the first field in our state.  Will error when it
-- reaches end of file.  (Yes, that's evil.)
skipField :: PM ()
skipField = modify tail

--FIXME: this should take a ByteString, not a String. We have to be able to
-- decode UTF8 and handle the BOM.

parsePackageDescription :: String -> ParseResult GenericPackageDescription
parsePackageDescription = parseGenericPackageDescription
{-# DEPRECATED parsePackageDescription "Use parseGenericPackageDescription, old name is misleading" #-}

-- | Parses the given file into a 'GenericPackageDescription'.
--
-- In Cabal 1.2 the syntax for package descriptions was changed to a format
-- with sections and possibly indented property descriptions.
parseGenericPackageDescription :: String -> ParseResult GenericPackageDescription
parseGenericPackageDescription file = do

    -- This function is quite complex because it needs to be able to parse
    -- both pre-Cabal-1.2 and post-Cabal-1.2 files.  Additionally, it contains
    -- a lot of parser-related noise since we do not want to depend on Parsec.
    --
    -- If we detect an pre-1.2 file we implicitly convert it to post-1.2
    -- style.  See 'sectionizeFields' below for details about the conversion.

    fields0 <- readFields file `catchParseError` \err ->
                 let tabs = findIndentTabs file in
                 case err of
                   -- In case of a TabsError report them all at once.
                   TabsError tabLineNo -> reportTabsError
                   -- but only report the ones including and following
                   -- the one that caused the actual error
                                            [ t | t@(lineNo',_) <- tabs
                                                , lineNo' >= tabLineNo ]
                   _ -> parseFail err

    let cabalVersionNeeded =
          head $ [ minVersionBound versionRange
                 | Just versionRange <- [ simpleParse v
                                        | F _ "cabal-version" v <- fields0 ] ]
              ++ [mkVersion [0]]
        minVersionBound versionRange =
          case asVersionIntervals versionRange of
            []                            -> mkVersion [0]
            ((LowerBound version _, _):_) -> version

    handleFutureVersionParseFailure cabalVersionNeeded $ do

      let sf = sectionizeFields fields0  -- ensure 1.2 format

        -- figure out and warn about deprecated stuff (warnings are collected
        -- inside our parsing monad)
      fields <- mapSimpleFields deprecField sf

        -- Our parsing monad takes the not-yet-parsed fields as its state.
        -- After each successful parse we remove the field from the state
        -- ('skipField') and move on to the next one.
        --
        -- Things are complicated a bit, because fields take a tree-like
        -- structure -- they can be sections or "if"/"else" conditionals.

      flip evalStT fields $ do

          -- The header consists of all simple fields up to the first section
          -- (flag, library, executable).
        header_fields <- getHeader []

          -- Parses just the header fields and stores them in a
          -- 'PackageDescription'.  Note that our final result is a
          -- 'GenericPackageDescription'; for pragmatic reasons we just store
          -- the partially filled-out 'PackageDescription' inside the
          -- 'GenericPackageDescription'.
        pkg <- lift $ parseFields pkgDescrFieldDescrs
                                  storeXFieldsPD
                                  emptyPackageDescription
                                  header_fields

          -- 'getBody' assumes that the remaining fields only consist of
          -- flags, lib and exe sections.
        (repos, flags, mcsetup, mlib, sub_libs, flibs, exes, tests, bms) <- getBody pkg
        warnIfRest  -- warn if getBody did not parse up to the last field.
          -- warn about using old/new syntax with wrong cabal-version:
        maybeWarnCabalVersion (not $ oldSyntax fields0) pkg
        checkForUndefinedFlags flags mlib sub_libs exes tests
        return $ GenericPackageDescription
                   pkg { sourceRepos = repos, setupBuildInfo = mcsetup }
                   flags mlib sub_libs flibs exes tests bms

  where
    oldSyntax = all isSimpleField
    reportTabsError tabs =
        syntaxError (fst (head tabs)) $
          "Do not use tabs for indentation (use spaces instead)\n"
          ++ "  Tabs were used at (line,column): " ++ show tabs

    maybeWarnCabalVersion newsyntax pkg
      | newsyntax && specVersion pkg < mkVersion [1,2]
      = lift $ warning $
             "A package using section syntax must specify at least\n"
          ++ "'cabal-version: >= 1.2'."

    maybeWarnCabalVersion newsyntax pkg
      | not newsyntax && specVersion pkg >= mkVersion [1,2]
      = lift $ warning $
             "A package using 'cabal-version: "
          ++ displaySpecVersion (specVersionRaw pkg)
          ++ "' must use section syntax. See the Cabal user guide for details."
      where
        displaySpecVersion (Left version)       = display version
        displaySpecVersion (Right versionRange) =
          case asVersionIntervals versionRange of
            [] {- impossible -}           -> display versionRange
            ((LowerBound version _, _):_) -> display (orLaterVersion version)

    maybeWarnCabalVersion _ _ = return ()


    handleFutureVersionParseFailure cabalVersionNeeded parseBody =
      (unless versionOk (warning message) >> parseBody)
        `catchParseError` \parseError -> case parseError of
        TabsError _   -> parseFail parseError
        _ | versionOk -> parseFail parseError
          | otherwise -> fail message
      where versionOk = cabalVersionNeeded <= cabalVersion
            message   = "This package requires at least Cabal version "
                     ++ display cabalVersionNeeded

    -- "Sectionize" an old-style Cabal file.  A sectionized file has:
    --
    --  * all global fields at the beginning, followed by
    --
    --  * all flag declarations, followed by
    --
    --  * an optional library section, and an arbitrary number of executable
    --    sections (in any order).
    --
    -- The current implementation just gathers all library-specific fields
    -- in a library section and wraps all executable stanzas in an executable
    -- section.
    sectionizeFields :: [Field] -> [Field]
    sectionizeFields fs
      | oldSyntax fs =
          let
            -- "build-depends" is a local field now.  To be backwards
            -- compatible, we still allow it as a global field in old-style
            -- package description files and translate it to a local field by
            -- adding it to every non-empty section
            (hdr0, exes0) = break ((=="executable") . fName) fs
            (hdr, libfs0) = partition (not . (`elem` libFieldNames) . fName) hdr0

            (deps, libfs) = partition ((== "build-depends") . fName)
                                       libfs0

            exes = unfoldr toExe exes0
            toExe [] = Nothing
            toExe (F l e n : r)
              | e == "executable" =
                  let (efs, r') = break ((=="executable") . fName) r
                  in Just (Section l "executable" n (deps ++ efs), r')
            toExe _ = cabalBug "unexpected input to 'toExe'"
          in
            hdr ++
           (if null libfs then []
            else [Section (lineNo (head libfs)) "library" "" (deps ++ libfs)])
            ++ exes
      | otherwise = fs

    isSimpleField F{} = True
    isSimpleField _ = False

    -- warn if there's something at the end of the file
    warnIfRest :: PM ()
    warnIfRest = do
      s <- getSt
      case s of
        [] -> return ()
        _ -> lift $ warning "Ignoring trailing declarations."  -- add line no.

    -- all simple fields at the beginning of the file are (considered) header
    -- fields
    getHeader :: [Field] -> PM [Field]
    getHeader acc = peekField >>= \mf -> case mf of
        Just f@F{} -> skipField >> getHeader (f:acc)
        _ -> return (reverse acc)

    --
    -- body ::= { repo | flag | library | sub library | foreign library
    --          | executable | test | bench }+
    --
    -- The body consists of an optional sequence of declarations of flags and
    -- an arbitrary number of components
    --
    -- TODO: This method is long due for a rewrite to use a accumulator
    -- data type, or perhaps some more general way of balling the
    -- components up.
    getBody :: PackageDescription
            -> PM ([SourceRepo], [Flag]
                  ,Maybe SetupBuildInfo
                  ,(Maybe (CondTree ConfVar [Dependency] Library))
                  ,[(UnqualComponentName, CondTree ConfVar [Dependency] Library)]
                  ,[(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)]
                  ,[(UnqualComponentName, CondTree ConfVar [Dependency] Executable)]
                  ,[(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)]
                  ,[(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)])
    getBody pkg = peekField >>= \mf -> case mf of
      Just (Section line_no sec_type sec_label sec_fields)
        | sec_type == "executable" -> do
            when (null sec_label) $ lift $ syntaxError line_no
              "'executable' needs one argument (the executable's name)"
            exename <- lift $ runP line_no "executable" parseTokenQ sec_label
            flds <- collectFields parseExeFields sec_fields
            skipField
            (repos, flags, csetup, mlib, sub_libs, flibs, exes, tests, bms) <- getBody pkg
            return (repos, flags, csetup, mlib, sub_libs, flibs, (mkUnqualComponentName exename, flds): exes, tests, bms)

        | sec_type == "foreign-library" -> do
            when (null sec_label) $ lift $ syntaxError line_no
              "'foreign-library' needs one argument (the library's name)"
            libname <- lift $ runP line_no "foreign-library" parseTokenQ sec_label
            flds <- collectFields parseForeignLibFields sec_fields

            -- Check that a valid foreign library type has been chosen. A type
            -- field may be given inside a conditional block, so we must check
            -- for that before complaining that a type field has not been given.
            -- The foreign library must always have a valid type, so we need to
            -- check both the 'then' and 'else' blocks, though the blocks need
            -- not have the same type.
            let hasType ts = foreignLibType ts /= foreignLibType mempty
            if onAllBranches hasType flds
                then do
                    skipField
                    (repos, flags, csetup, mlib, sub_libs, flibs, exes, tests, bms) <- getBody pkg
                    return (repos, flags, csetup, mlib, sub_libs, (mkUnqualComponentName libname, flds):flibs, exes, tests, bms)
                else lift $ syntaxError line_no $
                         "Foreign library \"" ++ libname
                      ++ "\" is missing required field \"type\" or the field "
                      ++ "is not present in all conditional branches. The "
                      ++ "available test types are: "
                      ++ intercalate ", " (map display knownForeignLibTypes)

        | sec_type == "test-suite" -> do
            when (null sec_label) $ lift $ syntaxError line_no
                "'test-suite' needs one argument (the test suite's name)"
            testname <- lift $ runP line_no "test" parseTokenQ sec_label
            flds <- collectFields (parseTestFields line_no) sec_fields

            -- Check that a valid test suite type has been chosen. A type field
            -- may be given inside a conditional block, so we must check for
            -- that before complaining that a type field has not been given. The
            -- test suite must always have a valid type, so we need to check
            -- both the 'then' and 'else' blocks, though the blocks need not
            -- have the same type.
            let hasType ts = testInterface ts /= testInterface mempty
            if onAllBranches hasType flds
                then do
                    skipField
                    (repos, flags, csetup, mlib, sub_libs, flibs, exes, tests, bms) <- getBody pkg
                    return (repos, flags, csetup, mlib, sub_libs, flibs, exes,
                            (mkUnqualComponentName testname, flds) : tests, bms)
                else lift $ syntaxError line_no $
                         "Test suite \"" ++ testname
                      ++ "\" is missing required field \"type\" or the field "
                      ++ "is not present in all conditional branches. The "
                      ++ "available test types are: "
                      ++ intercalate ", " (map display knownTestTypes)

        | sec_type == "benchmark" -> do
            when (null sec_label) $ lift $ syntaxError line_no
                "'benchmark' needs one argument (the benchmark's name)"
            benchname <- lift $ runP line_no "benchmark" parseTokenQ sec_label
            flds <- collectFields (parseBenchmarkFields line_no) sec_fields

            -- Check that a valid benchmark type has been chosen. A type field
            -- may be given inside a conditional block, so we must check for
            -- that before complaining that a type field has not been given. The
            -- benchmark must always have a valid type, so we need to check both
            -- the 'then' and 'else' blocks, though the blocks need not have the
            -- same type.
            let hasType ts = benchmarkInterface ts /= benchmarkInterface mempty
            if onAllBranches hasType flds
                then do
                    skipField
                    (repos, flags, csetup, mlib, sub_libs, flibs, exes, tests, bms) <- getBody pkg
                    return (repos, flags, csetup, mlib, sub_libs, flibs, exes,
                            tests, (mkUnqualComponentName benchname, flds) : bms)
                else lift $ syntaxError line_no $
                         "Benchmark \"" ++ benchname
                      ++ "\" is missing required field \"type\" or the field "
                      ++ "is not present in all conditional branches. The "
                      ++ "available benchmark types are: "
                      ++ intercalate ", " (map display knownBenchmarkTypes)

        | sec_type == "library" -> do
            mb_libname <- if null sec_label
                            then return Nothing
                            -- TODO: relax this parsing so that scoping is handled
                            -- correctly
                            else fmap Just . lift
                                  $ runP line_no "library" parseTokenQ sec_label
            flds <- collectFields parseLibFields sec_fields
            skipField
            (repos, flags, csetup, mlib, sub_libs, flibs, exes, tests, bms) <- getBody pkg
            case mb_libname of
                Just libname ->
                    return (repos, flags, csetup, mlib, (mkUnqualComponentName libname, flds) : sub_libs, flibs, exes, tests, bms)
                Nothing -> do
                    when (isJust mlib) $ lift $ syntaxError line_no
                      "There can only be one (public) library section in a package description."
                    return (repos, flags, csetup, Just flds, sub_libs, flibs, exes, tests, bms)

        | sec_type == "flag" -> do
            when (null sec_label) $ lift $
              syntaxError line_no "'flag' needs one argument (the flag's name)"
            flag <- lift $ parseFields
                    flagFieldDescrs
                    warnUnrec
                    (emptyFlag (mkFlagName (lowercase sec_label)))
                    sec_fields
            skipField
            (repos, flags, csetup, mlib, sub_libs, flibs, exes, tests, bms) <- getBody pkg
            return (repos, flag:flags, csetup, mlib, sub_libs, flibs, exes, tests, bms)

        | sec_type == "source-repository" -> do
            when (null sec_label) $ lift $ syntaxError line_no $
                 "'source-repository' needs one argument, "
              ++ "the repo kind which is usually 'head' or 'this'"
            kind <- case simpleParse sec_label of
              Just kind -> return kind
              Nothing   -> lift $ syntaxError line_no $
                             "could not parse repo kind: " ++ sec_label
            repo <- lift $ parseFields
                    sourceRepoFieldDescrs
                    warnUnrec
                    (emptySourceRepo kind)
                    sec_fields
            skipField
            (repos, flags, csetup, mlib, sub_libs, flibs, exes, tests, bms) <- getBody pkg
            return (repo:repos, flags, csetup, mlib, sub_libs, flibs, exes, tests, bms)

        | sec_type == "custom-setup" -> do
            unless (null sec_label) $ lift $
              syntaxError line_no "'setup' expects no argument"
            flds <- lift $ parseFields
                             setupBInfoFieldDescrs
                             warnUnrec
                             mempty
                             sec_fields
            skipField
            (repos, flags, csetup0, mlib, sub_libs, flibs, exes, tests, bms) <- getBody pkg
            when (isJust csetup0) $ lift $ syntaxError line_no
              "There can only be one 'custom-setup' section in a package description."
            return (repos, flags, Just flds, mlib, sub_libs, flibs, exes, tests, bms)

        | otherwise -> do
            lift $ warning $ "Ignoring unknown section type: " ++ sec_type
            skipField
            getBody pkg
      Just f@(F {}) -> do
            _ <- lift $ syntaxError (lineNo f) $
              "Plain fields are not allowed in between stanzas: " ++ show f
            skipField
            getBody pkg
      Just f@(IfBlock {}) -> do
            _ <- lift $ syntaxError (lineNo f) $
              "If-blocks are not allowed in between stanzas: " ++ show f
            skipField
            getBody pkg
      Nothing -> return ([], [], Nothing, Nothing, [], [], [], [], [])

    -- Extracts all fields in a block and returns a 'CondTree'.
    --
    -- We have to recurse down into conditionals and we treat fields that
    -- describe dependencies specially.
    collectFields :: ([Field] -> PM a) -> [Field]
                  -> PM (CondTree ConfVar [Dependency] a)
    collectFields parser allflds = do

        let simplFlds = [ F l n v | F l n v <- allflds ]
            condFlds = [ f | f@IfBlock{} <- allflds ]
            sections = [ s | s@Section{} <- allflds ]

        traverse_
            (\(Section l n _ _) -> lift . warning $
                "Unexpected section '" ++ n ++ "' on line " ++ show l)
            sections

        a <- parser simplFlds

        -- Dependencies must be treated specially: when we
        -- parse into a CondTree, not only do we parse them into
        -- the targetBuildDepends/etc field inside the
        -- PackageDescription, but we also have to put the
        -- combined dependencies into CondTree.
        --
        -- This information is, in principle, redundant, but
        -- putting it here makes it easier for the constraint
        -- solver to pick a flag assignment which supports
        -- all of the dependencies (because it only has
        -- to check the CondTree, rather than grovel everywhere
        -- inside the conditional bits).
        deps <- liftM concat
              . traverse (lift . parseConstraint)
              . filter isConstraint
              $ simplFlds

        ifs <- traverse processIfs condFlds

        return (CondNode a deps ifs)
      where
        isConstraint (F _ n _) = n `elem` constraintFieldNames
        isConstraint _ = False

        processIfs (IfBlock l c t e) = do
            cnd <- lift $ runP l "if" parseCondition c
            t' <- collectFields parser t
            e' <- case e of
                   [] -> return Nothing
                   es -> do fs <- collectFields parser es
                            return (Just fs)
            return (CondBranch cnd t' e')
        processIfs _ = cabalBug "processIfs called with wrong field type"

    parseLibFields :: [Field] -> PM Library
    parseLibFields = lift . parseFields libFieldDescrs storeXFieldsLib emptyLibrary

    -- Note: we don't parse the "executable" field here, hence the tail hack.
    parseExeFields :: [Field] -> PM Executable
    parseExeFields = lift . parseFields executableFieldDescrs
                                        storeXFieldsExe emptyExecutable

    parseForeignLibFields :: [Field] -> PM ForeignLib
    parseForeignLibFields =
        lift . parseFields foreignLibFieldDescrs
                           storeXFieldsForeignLib
                           emptyForeignLib

    parseTestFields :: LineNo -> [Field] -> PM TestSuite
    parseTestFields line fields = do
        x <- lift $ parseFields testSuiteFieldDescrs storeXFieldsTest
                                emptyTestStanza fields
        lift $ validateTestSuite line x

    parseBenchmarkFields :: LineNo -> [Field] -> PM Benchmark
    parseBenchmarkFields line fields = do
        x <- lift $ parseFields benchmarkFieldDescrs storeXFieldsBenchmark
                                emptyBenchmarkStanza fields
        lift $ validateBenchmark line x

    checkForUndefinedFlags ::
        [Flag] ->
        Maybe (CondTree ConfVar [Dependency] Library) ->
        [(UnqualComponentName, CondTree ConfVar [Dependency] Library)] ->
        [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)] ->
        [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)] ->
        PM ()
    checkForUndefinedFlags flags mlib sub_libs exes tests = do
        let definedFlags = map flagName flags
        traverse_ (checkCondTreeFlags definedFlags) (maybeToList mlib)
        traverse_ (checkCondTreeFlags definedFlags . snd) sub_libs
        traverse_ (checkCondTreeFlags definedFlags . snd) exes
        traverse_ (checkCondTreeFlags definedFlags . snd) tests

    checkCondTreeFlags :: [FlagName] -> CondTree ConfVar c a -> PM ()
    checkCondTreeFlags definedFlags ct = do
        let fv = nub $ freeVars ct
        unless (all (`elem` definedFlags) fv) $
            fail $ "These flags are used without having been defined: "
                ++ intercalate ", " [ unFlagName fn | fn <- fv \\ definedFlags ]

-- Check that a property holds on all branches of a condition tree
onAllBranches :: forall v c a. Monoid a => (a -> Bool) -> CondTree v c a -> Bool
onAllBranches p = go mempty
  where
    -- If the current level of the tree satisfies the property, then we are
    -- done. If not, then one of the conditional branches below the current node
    -- must satisfy it. Each node may have multiple immediate children; we only
    -- one need one to satisfy the property because the configure step uses
    -- 'mappend' to join together the results of flag resolution.
    go :: a -> CondTree v c a -> Bool
    go acc ct = let acc' = acc `mappend` condTreeData ct
                in p acc' || any (goBranch acc') (condTreeComponents ct)

    -- Both the 'true' and the 'false' block must satisfy the property.
    goBranch :: a -> CondBranch v c a -> Bool
    goBranch _   (CondBranch _ _ Nothing) = False
    goBranch acc (CondBranch _ t (Just e))  = go acc t && go acc e

-- | Parse a list of fields, given a list of field descriptions,
--   a structure to accumulate the parsed fields, and a function
--   that can decide what to do with fields which don't match any
--   of the field descriptions.
parseFields :: [FieldDescr a]      -- ^ descriptions of fields we know how to
                                   --   parse
            -> UnrecFieldParser a  -- ^ possibly do something with
                                   --   unrecognized fields
            -> a                   -- ^ accumulator
            -> [Field]             -- ^ fields to be parsed
            -> ParseResult a
parseFields descrs unrec ini fields =
    do (a, unknowns) <- foldM (parseField descrs unrec) (ini, []) fields
       unless (null unknowns) $ warning $ render $
         text "Unknown fields:" <+>
              commaSep (map (\(l,u) -> u ++ " (line " ++ show l ++ ")")
                            (reverse unknowns))
         $+$
         text "Fields allowed in this section:" $$
           nest 4 (commaSep $ map fieldName descrs)
       return a
  where
    commaSep = fsep . punctuate comma . map text

parseField :: [FieldDescr a]     -- ^ list of parseable fields
           -> UnrecFieldParser a -- ^ possibly do something with
                                 --   unrecognized fields
           -> (a,[(Int,String)]) -- ^ accumulated result and warnings
           -> Field              -- ^ the field to be parsed
           -> ParseResult (a, [(Int,String)])
parseField (FieldDescr name _ parser : fields) unrec (a, us) (F line f val)
  | name == f = parser line val a >>= \a' -> return (a',us)
  | otherwise = parseField fields unrec (a,us) (F line f val)
parseField [] unrec (a,us) (F l f val) = return $
  case unrec (f,val) a of        -- no fields matched, see if the 'unrec'
    Just a' -> (a',us)           -- function wants to do anything with it
    Nothing -> (a, (l,f):us)
parseField _ _ _ _ = cabalBug "'parseField' called on a non-field"

deprecatedFields :: [(String,String)]
deprecatedFields =
    deprecatedFieldsPkgDescr ++ deprecatedFieldsBuildInfo

deprecatedFieldsPkgDescr :: [(String,String)]
deprecatedFieldsPkgDescr = [ ("other-files", "extra-source-files") ]

deprecatedFieldsBuildInfo :: [(String,String)]
deprecatedFieldsBuildInfo = [ ("hs-source-dir","hs-source-dirs") ]

-- Handle deprecated fields
deprecField :: Field -> ParseResult Field
deprecField (F line fld val) = do
  fld' <- case lookup fld deprecatedFields of
            Nothing -> return fld
            Just newName -> do
              warning $ "The field \"" ++ fld
                      ++ "\" is deprecated, please use \"" ++ newName ++ "\""
              return newName
  return (F line fld' val)
deprecField _ = cabalBug "'deprecField' called on a non-field"


parseHookedBuildInfo :: String -> ParseResult HookedBuildInfo
parseHookedBuildInfo inp = do
  fields <- readFields inp
  let ss@(mLibFields:exes) = stanzas fields
  mLib <- parseLib mLibFields
  biExes <- mapM parseExe (maybe ss (const exes) mLib)
  return (mLib, biExes)
  where
    parseLib :: [Field] -> ParseResult (Maybe BuildInfo)
    parseLib (bi@(F _ inFieldName _:_))
        | lowercase inFieldName /= "executable" = liftM Just (parseBI bi)
    parseLib _ = return Nothing

    parseExe :: [Field] -> ParseResult (UnqualComponentName, BuildInfo)
    parseExe (F line inFieldName mName:bi)
        | lowercase inFieldName == "executable"
            = do bis <- parseBI bi
                 return (mkUnqualComponentName mName, bis)
        | otherwise = syntaxError line "expecting 'executable' at top of stanza"
    parseExe (_:_) = cabalBug "`parseExe' called on a non-field"
    parseExe [] = syntaxError 0 "error in parsing buildinfo file. Expected executable stanza"

    parseBI st = parseFields binfoFieldDescrs storeXFieldsBI emptyBuildInfo st

-- replace all tabs used as indentation with whitespace, also return where
-- tabs were found
findIndentTabs :: String -> [(Int,Int)]
findIndentTabs = concatMap checkLine
               . zip [1..]
               . lines
    where
      checkLine (lineno, l) =
          let (indent, _content) = span isSpace l
              tabCols = map fst . filter ((== '\t') . snd) . zip [0..]
              addLineNo = map (\col -> (lineno,col))
          in addLineNo (tabCols indent)

--test_findIndentTabs = findIndentTabs $ unlines $
--    [ "foo", "  bar", " \t baz", "\t  biz\t", "\t\t \t mib" ]
