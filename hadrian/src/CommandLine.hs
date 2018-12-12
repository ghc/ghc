module CommandLine (
    optDescrs, cmdLineArgsMap, cmdFlavour, lookupFreeze1, cmdIntegerSimple,
    cmdProgressColour, cmdProgressInfo, cmdConfigure, cmdSplitObjects,
    lookupBuildRoot, TestArgs(..), TestSpeed(..), defaultTestArgs
    ) where

import Data.Either
import qualified Data.HashMap.Strict as Map
import Data.List.Extra
import Development.Shake hiding (Normal)
import Hadrian.Utilities hiding (buildRoot)
import System.Console.GetOpt
import System.Environment

data TestSpeed = Slow | Average | Fast deriving (Show, Eq)

-- | All arguments that can be passed to Hadrian via the command line.
data CommandLineArgs = CommandLineArgs
    { configure      :: Bool
    , flavour        :: Maybe String
    , freeze1        :: Bool
    , integerSimple  :: Bool
    , progressColour :: UseColour
    , progressInfo   :: ProgressInfo
    , splitObjects   :: Bool
    , buildRoot      :: BuildRoot
    , testArgs       :: TestArgs }
    deriving (Eq, Show)

-- | Default values for 'CommandLineArgs'.
defaultCommandLineArgs :: CommandLineArgs
defaultCommandLineArgs = CommandLineArgs
    { configure      = False
    , flavour        = Nothing
    , freeze1        = False
    , integerSimple  = False
    , progressColour = Auto
    , progressInfo   = Brief
    , splitObjects   = False
    , buildRoot      = BuildRoot "_build"
    , testArgs       = defaultTestArgs }

-- | These arguments are used by the `test` target.
data TestArgs = TestArgs
    { testCompiler   :: String
    , testConfigFile :: String
    , testConfigs    :: [String]
    , testJUnit      :: Maybe FilePath
    , testOnly       :: [String]
    , testOnlyPerf   :: Bool
    , testSkipPerf   :: Bool
    , testSpeed      :: TestSpeed
    , testSummary    :: Maybe FilePath
    , testVerbosity  :: Maybe String
    , testWays       :: [String] }
    deriving (Eq, Show)

-- | Default value for `TestArgs`.
defaultTestArgs :: TestArgs
defaultTestArgs = TestArgs
    { testCompiler   = "stage2"
    , testConfigFile = "testsuite/config/ghc"
    , testConfigs    = []
    , testJUnit      = Nothing
    , testOnly       = []
    , testOnlyPerf   = False
    , testSkipPerf   = False
    , testSpeed      = Fast
    , testSummary    = Nothing
    , testVerbosity  = Nothing
    , testWays       = [] }

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

readFreeze1 :: Either String (CommandLineArgs -> CommandLineArgs)
readFreeze1 = Right $ \flags -> flags { freeze1 = True }

readIntegerSimple :: Either String (CommandLineArgs -> CommandLineArgs)
readIntegerSimple = Right $ \flags -> flags { integerSimple = True }

readProgressColour :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readProgressColour ms =
    maybe (Left "Cannot parse progress-colour") (Right . set) (go =<< lower <$> ms)
  where
    go :: String -> Maybe UseColour
    go "never"   = Just Never
    go "auto"    = Just Auto
    go "always"  = Just Always
    go _         = Nothing
    set :: UseColour -> CommandLineArgs -> CommandLineArgs
    set flag flags = flags { progressColour = flag }

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

readSplitObjects :: Either String (CommandLineArgs -> CommandLineArgs)
readSplitObjects = Right $ \flags -> flags { splitObjects = True }

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
    maybe (Left "Cannot parse test-speed") (Right . set) filepath
  where
    set filepath flags =  flags { testArgs = (testArgs flags) { testConfigFile = filepath } }

readTestJUnit :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestJUnit filepath = Right $ \flags -> flags { testArgs = (testArgs flags) { testJUnit = filepath } }

readTestOnly :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestOnly tests = Right $ \flags ->
  flags { testArgs = (testArgs flags) { testOnly = tests' } }

  where tests' = maybe [] words tests

readTestOnlyPerf :: Either String (CommandLineArgs -> CommandLineArgs)
readTestOnlyPerf = Right $ \flags -> flags { testArgs = (testArgs flags) { testOnlyPerf = True } }

readTestSkipPerf :: Either String (CommandLineArgs -> CommandLineArgs)
readTestSkipPerf = Right $ \flags -> flags { testArgs = (testArgs flags) { testSkipPerf = True } }

readTestSpeed :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestSpeed ms =
    maybe (Left "Cannot parse test-speed") (Right . set) (go =<< lower <$> ms)
  where
    go :: String -> Maybe TestSpeed
    go "fast"    = Just Fast
    go "slow"    = Just Slow
    go "average" = Just Average
    go _         = Nothing
    set :: TestSpeed -> CommandLineArgs -> CommandLineArgs
    set flag flags = flags { testArgs = (testArgs flags) {testSpeed = flag} }

readTestSummary :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestSummary filepath = Right $ \flags -> flags { testArgs = (testArgs flags) { testJUnit = filepath } }

readTestVerbose :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestVerbose verbose = Right $ \flags -> flags { testArgs = (testArgs flags) { testVerbosity = verbose } }

readTestWay :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestWay way =
    case way of
        Nothing -> Right id
        Just way -> Right $ \flags ->
            let newWays = way : testWays (testArgs flags)
            in flags { testArgs = (testArgs flags) {testWays = newWays} }

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
    , Option [] ["integer-simple"] (NoArg readIntegerSimple)
      "Build GHC with integer-simple library."
    , Option [] ["progress-colour"] (OptArg readProgressColour "MODE")
      "Use colours in progress info (Never, Auto or Always)."
    , Option [] ["progress-info"] (OptArg readProgressInfo "STYLE")
      "Progress info style (None, Brief, Normal or Unicorn)."
    , Option [] ["split-objects"] (NoArg readSplitObjects)
      "Generate split objects (requires a full clean rebuild)."
    , Option [] ["test-compiler"] (OptArg readTestCompiler "TEST_COMPILER")
      "Use given compiler [Default=stage2]."
    , Option [] ["test-config-file"] (OptArg readTestConfigFile "CONFIG_FILE")
      "configuration file for testsuite. Default=testsuite/config/ghc"
    , Option [] ["config"] (OptArg readTestConfig "EXTRA_TEST_CONFIG")
      "Configurations to run test, in key=value format."
    , Option [] ["summary-junit"] (OptArg readTestJUnit "TEST_SUMMARY_JUNIT")
      "Output testsuite summary in JUnit format."
    , Option [] ["only"] (OptArg readTestOnly "TESTS")
      "Test cases to run."
    , Option [] ["only-perf"] (NoArg readTestOnlyPerf)
      "Only run performance tests."
    , Option [] ["skip-perf"] (NoArg readTestSkipPerf)
      "Skip performance tests."
    , Option [] ["test-speed"] (OptArg readTestSpeed "SPEED")
      "fast, slow or normal. Normal by default"
    , Option [] ["summary"] (OptArg readTestSummary "TEST_SUMMARY")
      "Where to output the test summary file."
    , Option [] ["test-verbose"] (OptArg readTestVerbose "TEST_VERBOSE")
      "A verbosity value between 0 and 5. 0 is silent, 4 and higher activates extra output."
    , Option [] ["test-way"] (OptArg readTestWay "TEST_WAY")
      "only run these ways" ]

-- | A type-indexed map containing Hadrian command line arguments to be passed
-- to Shake via 'shakeExtra'.
cmdLineArgsMap :: IO (Map.HashMap TypeRep Dynamic)
cmdLineArgsMap = do
    (opts, _, _) <- getOpt Permute optDescrs <$> getArgs
    let args = foldl (flip id) defaultCommandLineArgs (rights opts)
    return $ insertExtra (progressColour args) -- Accessed by Hadrian.Utilities
           $ insertExtra (progressInfo   args) -- Accessed by Hadrian.Utilities
           $ insertExtra (buildRoot      args) -- Accessed by Hadrian.Utilities
           $ insertExtra (testArgs       args) -- Accessed by Settings.Builders.RunTest
           $ insertExtra args Map.empty

cmdLineArgs :: Action CommandLineArgs
cmdLineArgs = userSetting defaultCommandLineArgs

cmdConfigure :: Action Bool
cmdConfigure = configure <$> cmdLineArgs

cmdFlavour :: Action (Maybe String)
cmdFlavour = flavour <$> cmdLineArgs

lookupBuildRoot :: Map.HashMap TypeRep Dynamic -> BuildRoot
lookupBuildRoot = buildRoot . lookupExtra defaultCommandLineArgs

lookupFreeze1 :: Map.HashMap TypeRep Dynamic -> Bool
lookupFreeze1 = freeze1 . lookupExtra defaultCommandLineArgs

cmdIntegerSimple :: Action Bool
cmdIntegerSimple = integerSimple <$> cmdLineArgs

cmdProgressColour :: Action UseColour
cmdProgressColour = progressColour <$> cmdLineArgs

cmdProgressInfo :: Action ProgressInfo
cmdProgressInfo = progressInfo <$> cmdLineArgs

cmdSplitObjects :: Action Bool
cmdSplitObjects = splitObjects <$> cmdLineArgs
