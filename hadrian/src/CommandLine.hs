module CommandLine (
    optDescrs, cmdLineArgsMap, cmdFlavour, lookupFreeze1, lookupFreeze2, lookupSkipDepends,
    cmdBignum, cmdBignumCheck, cmdProgressInfo, cmdCompleteSetting,
    cmdDocsArgs, cmdUnitIdHash, lookupBuildRoot, TestArgs(..), TestSpeed(..), defaultTestArgs,
    cmdPrefix, DocArgs(..), defaultDocArgs
    ) where

import Data.Either
import qualified Data.HashMap.Strict as Map
import Data.List.Extra
import Development.Shake hiding (Normal)
import Flavour (DocTargets, DocTarget(..))
import Hadrian.Utilities hiding (buildRoot)
import Settings.Parser
import System.Console.GetOpt
import System.Environment
import qualified System.Directory as Directory
import Debug.Trace ( trace )

import qualified Data.Set as Set

data TestSpeed = TestSlow | TestNormal | TestFast deriving (Show, Eq)

-- | All arguments that can be passed to Hadrian via the command line.
data CommandLineArgs = CommandLineArgs
    { configure      :: Bool
    , flavour        :: Maybe String
    , freeze1        :: Bool
    , freeze2        :: Bool
    , skipDepends    :: Bool
    , unitIdHash     :: Bool
    , bignum         :: Maybe String
    , bignumCheck    :: Bool
    , progressInfo   :: ProgressInfo
    , buildRoot      :: BuildRoot
    , testArgs       :: TestArgs
    , docsArgs       :: DocArgs
    , docTargets     :: DocTargets
    , prefix         :: Maybe FilePath
    , completeStg    :: Maybe String }
    deriving (Eq, Show)

-- | Default values for 'CommandLineArgs'.
defaultCommandLineArgs :: CommandLineArgs
defaultCommandLineArgs = CommandLineArgs
    { configure      = False
    , flavour        = Nothing
    , freeze1        = False
    , freeze2        = False
    , skipDepends    = False
    , unitIdHash     = False
    , bignum         = Nothing
    , bignumCheck    = False
    , progressInfo   = Brief
    , buildRoot      = BuildRoot "_build"
    , testArgs       = defaultTestArgs
    , docsArgs       = defaultDocArgs
    , docTargets     = Set.fromList [minBound..maxBound]
    , prefix         = Nothing
    , completeStg    = Nothing }

-- | These arguments are used by the `test` target.
data TestArgs = TestArgs
    { testKeepFiles  :: Bool
    , testCompiler   :: String
    , testConfigFile :: String
    , testConfigs    :: [String]
    , testJUnit      :: Maybe FilePath
    , testMetricsFile:: Maybe FilePath
    , testOnly       :: [String]
    , testOnlyPerf   :: Bool
    , testSkipPerf   :: Bool
    , testRootDirs   :: [FilePath]
    , testSpeed      :: TestSpeed
    , testSummary    :: Maybe FilePath
    , testVerbosity  :: Maybe String
    , testWays       :: [String]
    , brokenTests    :: [String]
    , testAccept     :: Bool
    , testHasInTreeFiles :: Bool
      -- ^ This is used to signal that we have access to in-tree files like
      -- the rts sources and the haddock stats directory even if the test
      -- compiler is not in-tree
      -- If this flag is set, then those tests will also be run.
      -- This is useful when we want to test the bindist, but we still
      -- have access to the build directory
    }
    deriving (Eq, Show)

-- | Default value for `TestArgs`.
defaultTestArgs :: TestArgs
defaultTestArgs = TestArgs
    { testKeepFiles  = False
    , testCompiler   = "stage2"
    , testConfigFile = "testsuite/config/ghc"
    , testConfigs    = []
    , testJUnit      = Nothing
    , testMetricsFile= Nothing
    , testOnly       = []
    , testOnlyPerf   = False
    , testSkipPerf   = False
    , testRootDirs   = []
    , testSpeed      = TestNormal
    , testSummary    = Nothing
    , testVerbosity  = Nothing
    , testWays       = []
    , brokenTests    = []
    , testAccept     = False
    , testHasInTreeFiles = False
    }

data DocArgs = DocArgs
  { docsBaseUrl :: String
  } deriving (Eq, Show)

defaultDocArgs :: DocArgs
defaultDocArgs = DocArgs { docsBaseUrl = "../%pkgid%" }

readConfigure :: Either String (CommandLineArgs -> CommandLineArgs)
readConfigure = Left "hadrian --configure has been deprecated (see #20167). Please run ./boot; ./configure manually"

readFlavour :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readFlavour ms = Right $ \flags -> flags { flavour = lower <$> ms }

readBignum :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readBignum Nothing   = Right id
readBignum (Just ms) = Right $ \flags -> case break (== '-') (lower ms) of
   (backend,"")          -> flags { bignum = Just backend }
   ("check",'-':backend) -> flags { bignum = Just backend, bignumCheck = True }
   _                     -> flags { bignum = Just (lower ms) }

readBuildRoot :: FilePath -> Either String (CommandLineArgs -> CommandLineArgs)
readBuildRoot ms =
    Right $ \flags -> flags { buildRoot = BuildRoot ms }

readFreeze1, readFreeze2, readSkipDepends :: Either String (CommandLineArgs -> CommandLineArgs)
readFreeze1 = Right $ \flags -> flags { freeze1 = True }
readFreeze2 = Right $ \flags -> flags { freeze1 = True, freeze2 = True }
readSkipDepends = Right $ \flags -> flags { skipDepends = True }

readUnitIdHash :: Either String (CommandLineArgs -> CommandLineArgs)
readUnitIdHash = Right $ \flags ->
  trace "--hash-unit-ids is deprecated. It is enabled by release flavour or +hash_unit_ids flavour transformer" $
  flags { unitIdHash = True }

readProgressInfo :: String -> Either String (CommandLineArgs -> CommandLineArgs)
readProgressInfo ms =
  case lower ms of
    "none"    -> set None
    "brief"   -> set Brief
    "normal"  -> set Normal
    "unicorn" -> set Unicorn
    _         -> Left "Cannot parse progress-info"
  where
    set :: ProgressInfo -> Either String (CommandLineArgs -> CommandLineArgs)
    set flag = Right $ \flags -> flags { progressInfo = flag }

readTestKeepFiles :: Either String (CommandLineArgs -> CommandLineArgs)
readTestKeepFiles = Right $ \flags -> flags { testArgs = (testArgs flags) { testKeepFiles = True } }

readTestAccept :: Either String (CommandLineArgs -> CommandLineArgs)
readTestAccept = Right $ \flags -> flags { testArgs = (testArgs flags) { testAccept = True } }

readTestHasInTreeFiles :: Either String (CommandLineArgs -> CommandLineArgs)
readTestHasInTreeFiles = Right $ \flags -> flags { testArgs = (testArgs flags) { testHasInTreeFiles = True } }

readTestCompiler :: String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestCompiler compiler = Right $ \flags -> flags { testArgs = (testArgs flags) { testCompiler = compiler } }

readTestConfig :: String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestConfig conf = Right $ \flags ->
                        let configs = conf : testConfigs (testArgs flags)
                        in flags { testArgs = (testArgs flags) { testConfigs = configs } }

readTestConfigFile :: String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestConfigFile filepath = Right $ \flags ->  flags { testArgs = (testArgs flags) { testConfigFile = filepath } }

readTestJUnit :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestJUnit filepath = Right $ \flags -> flags { testArgs = (testArgs flags) { testJUnit = filepath } }

readTestMetrics :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestMetrics filepath = Right $ \flags -> flags { testArgs = (testArgs flags) { testMetricsFile = filepath } }

readTestOnly :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestOnly tests = Right $ \flags ->
  flags { testArgs = (testArgs flags) { testOnly = tests'' flags } }

  where tests' = maybe [] words tests
        tests'' flags = testOnly (testArgs flags) ++ tests'

readTestOnlyPerf :: Either String (CommandLineArgs -> CommandLineArgs)
readTestOnlyPerf = Right $ \flags -> flags { testArgs = (testArgs flags) { testOnlyPerf = True } }

readTestSkipPerf :: Either String (CommandLineArgs -> CommandLineArgs)
readTestSkipPerf = Right $ \flags -> flags { testArgs = (testArgs flags) { testSkipPerf = True } }

readHaddockBaseUrl :: Either String (CommandLineArgs -> CommandLineArgs)
readHaddockBaseUrl = Right $ \flags ->
  flags { docsArgs = (docsArgs flags) { docsBaseUrl = base_url } }

  where base_url = "/package/%pkg%/docs"


readTestRootDirs :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestRootDirs rootdirs = Right $ \flags ->
  flags { testArgs = (testArgs flags) { testRootDirs = rootdirs'' flags } }

  where rootdirs' = maybe [] (splitOn ":") rootdirs
        rootdirs'' flags = testRootDirs (testArgs flags) ++ rootdirs'

readTestSpeed :: String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestSpeed ms =
  case lower ms of
    "fast"    -> set TestFast
    "slow"    -> set TestSlow
    "normal"  -> set TestNormal
    _         -> Left "Cannot parse test-speed"
  where
    set :: TestSpeed -> Either String (CommandLineArgs -> CommandLineArgs)
    set flag = Right $ \flags -> flags { testArgs = (testArgs flags) {testSpeed = flag} }

readTestSummary :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestSummary filepath = Right $ \flags -> flags { testArgs = (testArgs flags) { testSummary = filepath } }

readTestVerbose :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestVerbose verbose = Right $ \flags -> flags { testArgs = (testArgs flags) { testVerbosity = verbose } }

readTestWay :: String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestWay way =
    Right $ \flags ->
            let newWays = way : testWays (testArgs flags)
            in flags { testArgs = (testArgs flags) {testWays = newWays} }

readBrokenTests :: String -> Either String (CommandLineArgs -> CommandLineArgs)
readBrokenTests tests =
    Right $ \flags ->
            let newTests = words tests ++ brokenTests (testArgs flags)
            in flags { testArgs = (testArgs flags) {brokenTests = newTests} }

readPrefix :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readPrefix ms = Right $ \flags -> flags { prefix = ms }

readCompleteStg :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readCompleteStg ms = Right $ \flags -> flags { completeStg = ms }

readDocsArg :: String -> Either String (CommandLineArgs -> CommandLineArgs)
readDocsArg ms =
  case ms of
    "none"           -> set (const Set.empty)
    "no-haddocks"    -> set (Set.delete Haddocks)
    "no-sphinx-html" -> set (Set.delete SphinxHTML)
    "no-sphinx-pdfs" -> set (Set.delete SphinxPDFs)
    "no-sphinx-man"  -> set (Set.delete SphinxMan)
    "no-sphinx-info" -> set (Set.delete SphinxInfo)
    "no-sphinx"      -> set (Set.delete SphinxHTML
                           . Set.delete SphinxPDFs
                           . Set.delete SphinxMan
                           . Set.delete SphinxInfo)
    _                -> Left "Cannot parse docs argument"

  where
    set :: (DocTargets -> DocTargets) -> Either String (CommandLineArgs -> CommandLineArgs)
    set tweakTargets = Right $ \flags ->
      flags { docTargets = tweakTargets (docTargets flags) }

-- | Standard 'OptDescr' descriptions of Hadrian's command line arguments.
optDescrs :: [OptDescr (Either String (CommandLineArgs -> CommandLineArgs))]
optDescrs =
    [ Option ['c'] ["configure"] (NoArg readConfigure)
      "Deprecated: Run the boot and configure scripts."
    , Option ['o'] ["build-root"] (ReqArg readBuildRoot "BUILD_ROOT")
      "Where to store build artifacts. (Default _build)."
    , Option [] ["flavour"] (OptArg readFlavour "FLAVOUR")
      "Build flavour (Default, Devel1, Devel2, Perf, Prof, Quick or Quickest)."
    , Option [] ["freeze1"] (NoArg readFreeze1)
      "Freeze Stage1 GHC."
    , Option [] ["freeze2"] (NoArg readFreeze2)
      "Freeze Stage2 GHC."
    , Option [] ["hash-unit-ids"] (NoArg readUnitIdHash)
      "Include package hashes in unit ids."
    , Option [] ["skip-depends"] (NoArg readSkipDepends)
      "Skip rebuilding dependency information."
    , Option [] ["bignum"] (OptArg readBignum "BACKEND")
      "Select bignum backend: native, gmp (default), check-gmp (gmp compared to native), ffi."
    , Option [] ["progress-info"] (ReqArg readProgressInfo "STYLE")
      "Progress info style (None, Brief, Normal or Unicorn)."
    , Option [] ["docs"] (ReqArg readDocsArg "TARGET")
      "Strip down docs targets (none, no-haddocks, no-sphinx[-{html, pdfs, man}]."
    , Option ['k'] ["keep-test-files"] (NoArg readTestKeepFiles)
      "Keep all the files generated when running the testsuite."
    , Option [] ["test-compiler"] (ReqArg readTestCompiler "TEST_COMPILER")
      "Use given compiler [Default=stage2]."
    , Option [] ["test-config-file"] (ReqArg readTestConfigFile "CONFIG_FILE")
      "configuration file for testsuite. Default=testsuite/config/ghc"
    , Option [] ["config"] (ReqArg readTestConfig "EXTRA_TEST_CONFIG")
      "Configurations to run test, in key=value format."
    , Option [] ["summary-junit"] (OptArg readTestJUnit "TEST_SUMMARY_JUNIT")
      "Output testsuite summary in JUnit format."
    , Option [] ["summary-metrics"] (OptArg readTestMetrics "METRICS_FILE")
      "Output testsuite performance metrics summary."
    , Option [] ["only"] (OptArg readTestOnly "TESTS")
      "Test cases to run."
    , Option [] ["only-perf"] (NoArg readTestOnlyPerf)
      "Only run performance tests."
    , Option [] ["skip-perf"] (NoArg readTestSkipPerf)
      "Skip performance tests."
    , Option [] ["test-root-dirs"] (OptArg readTestRootDirs "DIR1:[DIR2:...:DIRn]")
      "Test root directories to look at (all by default)."
    , Option [] ["test-speed"] (ReqArg readTestSpeed "SPEED")
      "fast, slow or normal. Normal by default"
    , Option [] ["summary"] (OptArg readTestSummary "TEST_SUMMARY")
      "Where to output the test summary file."
    , Option [] ["test-verbose"] (OptArg readTestVerbose "TEST_VERBOSE")
      "A verbosity value between 0 and 5. 0 is silent, 4 and higher activates extra output."
    , Option [] ["test-way"] (ReqArg readTestWay "TEST_WAY")
      "only run these ways"
    , Option [] ["broken-test"] (ReqArg readBrokenTests "TEST_NAME")
      "consider these tests to be broken"
    , Option ['a'] ["test-accept"] (NoArg readTestAccept) "Accept new output of tests"
    , Option [] ["test-have-intree-files"] (NoArg readTestHasInTreeFiles) "Run the in-tree tests even with an out of tree compiler"
    , Option [] ["prefix"] (OptArg readPrefix "PATH")
        "Destination path for the bindist 'install' rule"
    , Option [] ["complete-setting"] (OptArg readCompleteStg "SETTING")
        "Setting key to autocomplete, for the 'autocomplete' target."
    , Option [] ["haddock-for-hackage"] (NoArg readHaddockBaseUrl)
        "Generate documentation suitable for upload to a hackage server."
    ]

-- | A type-indexed map containing Hadrian command line arguments to be passed
-- to Shake via 'shakeExtra'.
cmdLineArgsMap :: IO (Map.HashMap TypeRep Dynamic)
cmdLineArgsMap = do
    xs <- getArgs
    let -- We split the arguments between the ones that look like
        -- "k = v" or "k += v", in cliSettings, and the rest in
        -- optArgs.
        (optsArgs, cliSettings) = partitionKVs xs

        -- We only use the arguments that don't look like setting
        -- updates for parsing Hadrian and Shake flags/options.
        (opts, _, _) = getOpt Permute optDescrs optsArgs
        args = foldl (flip id) defaultCommandLineArgs (rights opts)

        BuildRoot root = buildRoot args
        settingsFile = root -/- "hadrian.settings"

    -- We try to look at <root>/hadrian.settings, and if it exists
    -- we read as many settings as we can from it, combining
    -- them with the ones we got on the command line, in allSettings.
    -- We then insert all those settings in the dynamic map, so that
    -- the 'Settings.flavour' action can look them up and apply
    -- all the relevant updates to the flavour that Hadrian is set
    -- to run with.
    settingsFileExists <- Directory.doesFileExist settingsFile
    fileSettings <-
      if settingsFileExists
        then parseJustKVs . lines <$> readFile settingsFile
        else return []
    let allSettings = cliSettings ++ fileSettings

    return $ insertExtra (progressInfo   args) -- Accessed by Hadrian.Utilities
           $ insertExtra (buildRoot      args) -- Accessed by Hadrian.Utilities
           $ insertExtra (testArgs       args) -- Accessed by Settings.Builders.RunTest
           $ insertExtra (docsArgs       args) -- Accessed by Rules.Documentation
           $ insertExtra allSettings           -- Accessed by Settings
           $ insertExtra args Map.empty

cmdLineArgs :: Action CommandLineArgs
cmdLineArgs = userSetting defaultCommandLineArgs

cmdFlavour :: Action (Maybe String)
cmdFlavour = flavour <$> cmdLineArgs

cmdPrefix :: Action (Maybe String)
cmdPrefix = prefix <$> cmdLineArgs

cmdCompleteSetting :: Action (Maybe String)
cmdCompleteSetting = completeStg <$> cmdLineArgs

lookupBuildRoot :: Map.HashMap TypeRep Dynamic -> BuildRoot
lookupBuildRoot = buildRoot . lookupExtra defaultCommandLineArgs

lookupFreeze1 :: Map.HashMap TypeRep Dynamic -> Bool
lookupFreeze1 = freeze1 . lookupExtra defaultCommandLineArgs

lookupFreeze2 :: Map.HashMap TypeRep Dynamic -> Bool
lookupFreeze2 = freeze2 . lookupExtra defaultCommandLineArgs

lookupSkipDepends :: Map.HashMap TypeRep Dynamic -> Bool
lookupSkipDepends = skipDepends . lookupExtra defaultCommandLineArgs

cmdUnitIdHash :: Action Bool
cmdUnitIdHash = unitIdHash <$> cmdLineArgs

cmdBignum :: Action (Maybe String)
cmdBignum = bignum <$> cmdLineArgs

cmdBignumCheck :: Action Bool
cmdBignumCheck = bignumCheck <$> cmdLineArgs

cmdProgressInfo :: Action ProgressInfo
cmdProgressInfo = progressInfo <$> cmdLineArgs

cmdDocsArgs :: Action DocTargets
cmdDocsArgs = docTargets <$> cmdLineArgs
