module CommandLine (
    optDescrs, cmdLineArgsMap, cmdFlavour, lookupFreeze1, cmdIntegerSimple,
    cmdProgressColour, cmdProgressInfo, cmdConfigure,
    cmdDocsArgs, lookupBuildRoot, TestArgs(..), TestSpeed(..), defaultTestArgs
    ) where

import Data.Either
import qualified Data.HashMap.Strict as Map
import Data.List.Extra
import Development.Shake hiding (Normal)
import Flavour (DocTargets, DocTarget(..))
import Hadrian.Utilities hiding (buildRoot)
import System.Console.GetOpt
import System.Environment

import qualified Data.Set as Set

data TestSpeed = TestSlow | TestNormal | TestFast deriving (Show, Eq)

-- | All arguments that can be passed to Hadrian via the command line.
data CommandLineArgs = CommandLineArgs
    { configure      :: Bool
    , flavour        :: Maybe String
    , freeze1        :: Bool
    , integerSimple  :: Bool
    , progressColour :: UseColour
    , progressInfo   :: ProgressInfo
    , buildRoot      :: BuildRoot
    , testArgs       :: TestArgs
    , docTargets     :: DocTargets }
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
    , buildRoot      = BuildRoot "_build"
    , testArgs       = defaultTestArgs
    , docTargets     = Set.fromList [minBound..maxBound] }

-- | These arguments are used by the `test` target.
data TestArgs = TestArgs
    { testKeepFiles  :: Bool
    , testCompiler   :: String
    , testConfigFile :: String
    , testConfigs    :: [String]
    , testJUnit      :: Maybe FilePath
    , testOnly       :: [String]
    , testOnlyPerf   :: Bool
    , testSkipPerf   :: Bool
    , testRootDirs   :: [FilePath]
    , testSpeed      :: TestSpeed
    , testSummary    :: Maybe FilePath
    , testVerbosity  :: Maybe String
    , testWays       :: [String]
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
    , testOnly       = []
    , testOnlyPerf   = False
    , testSkipPerf   = False
    , testRootDirs   = []
    , testSpeed      = TestNormal
    , testSummary    = Nothing
    , testVerbosity  = Nothing
    , testWays       = []
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

readDocsArg :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readDocsArg ms = maybe (Left "Cannot parse docs argument") (Right . set) (go =<< ms)

  where
    go :: String -> Maybe (DocTargets -> DocTargets)
    go "none"           = Just (const Set.empty)
    go "no-haddocks"    = Just (Set.delete Haddocks)
    go "no-sphinx-html" = Just (Set.delete SphinxHTML)
    go "no-sphinx-pdfs" = Just (Set.delete SphinxPDFs)
    go "no-sphinx-man"  = Just (Set.delete SphinxMan)
    go "no-sphinx"      = Just (Set.delete SphinxHTML
                              . Set.delete SphinxPDFs
                              . Set.delete SphinxMan)
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
    , Option [] ["integer-simple"] (NoArg readIntegerSimple)
      "Build GHC with integer-simple library."
    , Option [] ["progress-colour"] (OptArg readProgressColour "MODE")
      "Use colours in progress info (Never, Auto or Always)."
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
    , Option ['a'] ["test-accept"] (NoArg readTestAccept) "Accept new output of tests" ]

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

cmdDocsArgs :: Action DocTargets
cmdDocsArgs = docTargets <$> cmdLineArgs
