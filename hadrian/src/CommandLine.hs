module CommandLine (
    optDescrs, cmdLineArgsMap, cmdFlavour, lookupFreeze1, lookupFreeze2,
    cmdIntegerSimple, cmdProgressInfo, cmdConfigure, cmdCompleteSetting,
    cmdDocsArgs, lookupBuildRoot, TestArgs(..), TestSpeed(..), defaultTestArgs,
    cmdPrefix
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

import qualified Data.Set as Set

data TestSpeed = TestSlow | TestNormal | TestFast deriving (Show, Eq)

-- | All arguments that can be passed to Hadrian via the command line.
data CommandLineArgs = CommandLineArgs
    { configure      :: Bool
    , flavour        :: Maybe String
    , freeze1        :: Bool
    , freeze2        :: Bool
    , integerSimple  :: Bool
    , progressInfo   :: ProgressInfo
    , buildRoot      :: BuildRoot
    , testArgs       :: TestArgs
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
    , integerSimple  = False
    , progressInfo   = Brief
    , buildRoot      = BuildRoot "_build"
    , testArgs       = defaultTestArgs
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
    , testAccept     :: Bool}
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
    , testAccept     = False }

readConfigure :: Either String (CommandLineArgs -> CommandLineArgs)
readConfigure = Right $ \flags -> flags { configure = True }

readFlavour :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readFlavour ms = Right $ \flags -> flags { flavour = lower <$> ms }

readBuildRoot :: Maybe FilePath -> Either String (CommandLineArgs -> CommandLineArgs)
readBuildRoot ms =
    maybe (Left "Cannot parse build-root") (Right . set) (go =<< ms)
  where
    go :: String -> Maybe BuildRoot
    go = Just . BuildRoot
    set :: BuildRoot -> CommandLineArgs -> CommandLineArgs
    set flag flags = flags { buildRoot = flag }

readFreeze1, readFreeze2 :: Either String (CommandLineArgs -> CommandLineArgs)
readFreeze1 = Right $ \flags -> flags { freeze1 = True }
readFreeze2 = Right $ \flags -> flags { freeze1 = True, freeze2 = True }

readIntegerSimple :: Either String (CommandLineArgs -> CommandLineArgs)
readIntegerSimple = Right $ \flags -> flags { integerSimple = True }

readProgressInfo :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readProgressInfo ms =
    maybe (Left "Cannot parse progress-info") (Right . set) (go =<< lower <$> ms)
  where
    go :: String -> Maybe ProgressInfo
    go "none"    = Just None
    go "brief"   = Just Brief
    go "normal"  = Just Normal
    go "unicorn" = Just Unicorn
    go _         = Nothing
    set :: ProgressInfo -> CommandLineArgs -> CommandLineArgs
    set flag flags = flags { progressInfo = flag }

readTestKeepFiles :: Either String (CommandLineArgs -> CommandLineArgs)
readTestKeepFiles = Right $ \flags -> flags { testArgs = (testArgs flags) { testKeepFiles = True } }

readTestAccept :: Either String (CommandLineArgs -> CommandLineArgs)
readTestAccept = Right $ \flags -> flags { testArgs = (testArgs flags) { testAccept = True } }

readTestCompiler :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestCompiler compiler = maybe (Left "Cannot parse compiler") (Right . set) compiler
  where
     set compiler  = \flags -> flags { testArgs = (testArgs flags) { testCompiler = compiler } }

readTestConfig :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestConfig config =
    case config of
         Nothing -> Right id
         Just conf -> Right $ \flags ->
                        let configs = conf : testConfigs (testArgs flags)
                        in flags { testArgs = (testArgs flags) { testConfigs = configs } }

readTestConfigFile :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestConfigFile filepath =
    maybe (Left "Cannot parse test-config-file") (Right . set) filepath
  where
    set filepath flags =  flags { testArgs = (testArgs flags) { testConfigFile = filepath } }

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

readTestRootDirs :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestRootDirs rootdirs = Right $ \flags ->
  flags { testArgs = (testArgs flags) { testRootDirs = rootdirs'' flags } }

  where rootdirs' = maybe [] (splitOn ":") rootdirs
        rootdirs'' flags = testRootDirs (testArgs flags) ++ rootdirs'

readTestSpeed :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestSpeed ms =
    maybe (Left "Cannot parse test-speed") (Right . set) (go =<< lower <$> ms)
  where
    go :: String -> Maybe TestSpeed
    go "fast"    = Just TestFast
    go "slow"    = Just TestSlow
    go "normal"  = Just TestNormal
    go _         = Nothing
    set :: TestSpeed -> CommandLineArgs -> CommandLineArgs
    set flag flags = flags { testArgs = (testArgs flags) {testSpeed = flag} }

readTestSummary :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestSummary filepath = Right $ \flags -> flags { testArgs = (testArgs flags) { testSummary = filepath } }

readTestVerbose :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestVerbose verbose = Right $ \flags -> flags { testArgs = (testArgs flags) { testVerbosity = verbose } }

readTestWay :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestWay way =
    case way of
        Nothing -> Right id
        Just way -> Right $ \flags ->
            let newWays = way : testWays (testArgs flags)
            in flags { testArgs = (testArgs flags) {testWays = newWays} }

readBrokenTests :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readBrokenTests way =
    case way of
        Nothing -> Left "--broken-tests expects argument"
        Just tests -> Right $ \flags ->
            let newTests = words tests ++ brokenTests (testArgs flags)
            in flags { testArgs = (testArgs flags) {brokenTests = newTests} }

readPrefix :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readPrefix ms = Right $ \flags -> flags { prefix = ms }

readCompleteStg :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readCompleteStg ms = Right $ \flags -> flags { completeStg = ms }

readDocsArg :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readDocsArg ms = maybe (Left "Cannot parse docs argument") (Right . set) (go =<< ms)

  where
    go :: String -> Maybe (DocTargets -> DocTargets)
    go "none"           = Just (const Set.empty)
    go "no-haddocks"    = Just (Set.delete Haddocks)
    go "no-sphinx-html" = Just (Set.delete SphinxHTML)
    go "no-sphinx-pdfs" = Just (Set.delete SphinxPDFs)
    go "no-sphinx-man"  = Just (Set.delete SphinxMan)
    go "no-sphinx-info" = Just (Set.delete SphinxInfo)
    go "no-sphinx"      = Just (Set.delete SphinxHTML
                              . Set.delete SphinxPDFs
                              . Set.delete SphinxMan
                              . Set.delete SphinxInfo)
    go _                = Nothing

    set :: (DocTargets -> DocTargets) -> CommandLineArgs -> CommandLineArgs
    set tweakTargets flags = flags
      { docTargets = tweakTargets (docTargets flags) }

-- | Standard 'OptDescr' descriptions of Hadrian's command line arguments.
optDescrs :: [OptDescr (Either String (CommandLineArgs -> CommandLineArgs))]
optDescrs =
    [ Option ['c'] ["configure"] (NoArg readConfigure)
      "Run the boot and configure scripts (if you do not want to run them manually)."
    , Option ['o'] ["build-root"] (OptArg readBuildRoot "BUILD_ROOT")
      "Where to store build artifacts. (Default _build)."
    , Option [] ["flavour"] (OptArg readFlavour "FLAVOUR")
      "Build flavour (Default, Devel1, Devel2, Perf, Prof, Quick or Quickest)."
    , Option [] ["freeze1"] (NoArg readFreeze1)
      "Freeze Stage1 GHC."
    , Option [] ["freeze2"] (NoArg readFreeze2)
      "Freeze Stage2 GHC."
    , Option [] ["integer-simple"] (NoArg readIntegerSimple)
      "Build GHC with integer-simple library."
    , Option [] ["progress-info"] (OptArg readProgressInfo "STYLE")
      "Progress info style (None, Brief, Normal or Unicorn)."
    , Option [] ["docs"] (OptArg readDocsArg "TARGET")
      "Strip down docs targets (none, no-haddocks, no-sphinx[-{html, pdfs, man}]."
    , Option ['k'] ["keep-test-files"] (NoArg readTestKeepFiles)
      "Keep all the files generated when running the testsuite."
    , Option [] ["test-compiler"] (OptArg readTestCompiler "TEST_COMPILER")
      "Use given compiler [Default=stage2]."
    , Option [] ["test-config-file"] (OptArg readTestConfigFile "CONFIG_FILE")
      "configuration file for testsuite. Default=testsuite/config/ghc"
    , Option [] ["config"] (OptArg readTestConfig "EXTRA_TEST_CONFIG")
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
    , Option [] ["test-speed"] (OptArg readTestSpeed "SPEED")
      "fast, slow or normal. Normal by default"
    , Option [] ["summary"] (OptArg readTestSummary "TEST_SUMMARY")
      "Where to output the test summary file."
    , Option [] ["test-verbose"] (OptArg readTestVerbose "TEST_VERBOSE")
      "A verbosity value between 0 and 5. 0 is silent, 4 and higher activates extra output."
    , Option [] ["test-way"] (OptArg readTestWay "TEST_WAY")
      "only run these ways"
    , Option [] ["broken-test"] (OptArg readBrokenTests "TEST_NAME")
      "consider these tests to be broken"
    , Option ['a'] ["test-accept"] (NoArg readTestAccept) "Accept new output of tests"
    , Option [] ["prefix"] (OptArg readPrefix "PATH")
        "Destination path for the bindist 'install' rule"
    , Option [] ["complete-setting"] (OptArg readCompleteStg "SETTING")
        "Setting key to autocomplete, for the 'autocomplete' target."
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
           $ insertExtra allSettings           -- Accessed by Settings
           $ insertExtra args Map.empty

cmdLineArgs :: Action CommandLineArgs
cmdLineArgs = userSetting defaultCommandLineArgs

cmdConfigure :: Action Bool
cmdConfigure = configure <$> cmdLineArgs

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

cmdIntegerSimple :: Action Bool
cmdIntegerSimple = integerSimple <$> cmdLineArgs

cmdProgressInfo :: Action ProgressInfo
cmdProgressInfo = progressInfo <$> cmdLineArgs

cmdDocsArgs :: Action DocTargets
cmdDocsArgs = docTargets <$> cmdLineArgs
