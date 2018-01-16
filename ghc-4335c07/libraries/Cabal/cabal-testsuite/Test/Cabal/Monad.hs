{-# LANGUAGE ScopedTypeVariables #-}

-- | The test monad
module Test.Cabal.Monad (
    -- * High-level runners
    setupAndCabalTest,
    setupTest,
    cabalTest,
    -- * The monad
    TestM,
    runTestM,
    -- * Helper functions
    programPathM,
    requireProgramM,
    isAvailableProgram,
    hackageRepoToolProgram,
    gitProgram,
    cabalProgram,
    diffProgram,
    -- * The test environment
    TestEnv(..),
    getTestEnv,
    -- * Recording mode
    RecordMode(..),
    testRecordMode,
    -- * Regex utility (move me somewhere else)
    resub,
    -- * Derived values from 'TestEnv'
    testCurrentDir,
    testWorkDir,
    testPrefixDir,
    testDistDir,
    testPackageDbDir,
    testHomeDir,
    testSandboxDir,
    testSandboxConfigFile,
    testRepoDir,
    testKeysDir,
    testSourceCopyDir,
    testUserCabalConfigFile,
    testActualFile,
    -- * Skipping tests
    skip,
    skipIf,
    skipUnless,
    skipExitCode,
    -- * Known broken tests
    expectedBroken,
    unexpectedSuccess,
    expectedBrokenExitCode,
    unexpectedSuccessExitCode,
    -- whenHasSharedLibraries,
    -- * Arguments (TODO: move me)
    CommonArgs(..),
    renderCommonArgs,
    commonArgParser,
) where

import Test.Cabal.Script
import Test.Cabal.Plan

import Distribution.Simple.Compiler
    ( PackageDBStack, PackageDB(..), compilerFlavor
    , Compiler, compilerVersion )
import Distribution.System
import Distribution.Simple.Program.Db
import Distribution.Simple.Program
import Distribution.Simple.Configure
    ( getPersistBuildConfig, configCompilerEx )
import Distribution.Types.LocalBuildInfo
import Distribution.Version
import Distribution.Text
import Distribution.Package

import Distribution.Verbosity

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Maybe
import Control.Applicative
import Data.Monoid
import qualified Data.Foldable as F
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Process hiding (env)
import Options.Applicative
import Text.Regex

data CommonArgs = CommonArgs {
        argCabalInstallPath    :: Maybe FilePath,
        argGhcPath             :: Maybe FilePath,
        argHackageRepoToolPath :: Maybe FilePath,
        argHaddockPath         :: Maybe FilePath,
        argAccept              :: Bool,
        argSkipSetupTests      :: Bool
    }

commonArgParser :: Parser CommonArgs
commonArgParser = CommonArgs
    <$> optional (option str
        ( help "Path to cabal-install executable to test"
       <> long "with-cabal"
       <> metavar "PATH"
        ))
    <*> optional (option str
        ( help "GHC to ask Cabal to use via --with-ghc flag"
       <> short 'w'
       <> long "with-ghc"
       <> metavar "PATH"
        ))
    <*> optional (option str
        ( help "Path to hackage-repo-tool to use for repository manipulation"
       <> long "with-hackage-repo-tool"
       <> metavar "PATH"
        ))
    <*> optional (option str
        ( help "Path to haddock to use for --with-haddock flag"
       <> long "with-haddock"
       <> metavar "PATH"
        ))
    <*> switch
        ( long "accept"
       <> help "Accept output"
        )
    <*> switch (long "skip-setup-tests" <> help "Skip setup tests")

renderCommonArgs :: CommonArgs -> [String]
renderCommonArgs args =
    maybe [] (\x -> ["--with-cabal",             x]) (argCabalInstallPath    args) ++
    maybe [] (\x -> ["--with-ghc",               x]) (argGhcPath             args) ++
    maybe [] (\x -> ["--with-haddock",           x]) (argHaddockPath         args) ++
    maybe [] (\x -> ["--with-hackage-repo-tool", x]) (argHackageRepoToolPath args) ++
    (if argAccept args then ["--accept"] else []) ++
    (if argSkipSetupTests args then ["--skip-setup-tests"] else [])

data TestArgs = TestArgs {
        testArgDistDir    :: FilePath,
        testArgScriptPath :: FilePath,
        testCommonArgs    :: CommonArgs
    }

testArgParser :: Parser TestArgs
testArgParser = TestArgs
    <$> option str
        ( help "Build directory of cabal-testsuite"
       <> long "builddir"
       <> metavar "DIR")
    <*> argument str ( metavar "FILE")
    <*> commonArgParser

skip :: TestM ()
skip = liftIO $ do
    putStrLn "SKIP"
    exitWith (ExitFailure skipExitCode)

skipIf :: Bool -> TestM ()
skipIf b = when b skip

skipUnless :: Bool -> TestM ()
skipUnless b = unless b skip

expectedBroken :: TestM ()
expectedBroken = liftIO $ do
    putStrLn "EXPECTED FAIL"
    exitWith (ExitFailure expectedBrokenExitCode)

unexpectedSuccess :: TestM ()
unexpectedSuccess = liftIO $ do
    putStrLn "UNEXPECTED OK"
    exitWith (ExitFailure unexpectedSuccessExitCode)

skipExitCode :: Int
skipExitCode = 64

expectedBrokenExitCode :: Int
expectedBrokenExitCode = 65

unexpectedSuccessExitCode :: Int
unexpectedSuccessExitCode = 66

catchSkip :: IO a -> IO a -> IO a
catchSkip m r = m `E.catch` \e ->
                case e of
                    ExitFailure c | c == skipExitCode
                        -> r
                    _ -> E.throwIO e

setupAndCabalTest :: TestM () -> IO ()
setupAndCabalTest m = do
    r1 <- (setupTest m >> return False) `catchSkip` return True
    r2 <- (cabalTest' "cabal" m >> return False) `catchSkip` return True
    when (r1 && r2) $ do
        putStrLn "SKIP"
        exitWith (ExitFailure skipExitCode)

setupTest :: TestM () -> IO ()
setupTest m = runTestM "" $ do
    env <- getTestEnv
    skipIf (testSkipSetupTests env)
    m

cabalTest :: TestM () -> IO ()
cabalTest = cabalTest' ""

cabalTest' :: String -> TestM () -> IO ()
cabalTest' mode m = runTestM mode $ do
    skipUnless =<< isAvailableProgram cabalProgram
    withReaderT (\nenv -> nenv { testCabalInstallAsSetup = True }) m

type TestM = ReaderT TestEnv IO

gitProgram :: Program
gitProgram = simpleProgram "git"

hackageRepoToolProgram :: Program
hackageRepoToolProgram = simpleProgram "hackage-repo-tool"

cabalProgram :: Program
cabalProgram = (simpleProgram "cabal") {
        -- Do NOT search for executable named cabal, it's probably
        -- not the one you were intending to test
        programFindLocation = \_ _ -> return Nothing
    }

diffProgram :: Program
diffProgram = simpleProgram "diff"

-- | Run a test in the test monad according to program's arguments.
runTestM :: String -> TestM a -> IO a
runTestM mode m = do
    args <- execParser (info testArgParser mempty)
    let dist_dir = testArgDistDir args
        (script_dir0, script_filename) = splitFileName (testArgScriptPath args)
        script_base = dropExtensions script_filename
    -- Canonicalize this so that it is stable across working directory changes
    script_dir <- canonicalizePath script_dir0
    lbi <- getPersistBuildConfig dist_dir
    let verbosity = normal -- TODO: configurable
    senv <- mkScriptEnv verbosity lbi
    -- Add test suite specific programs
    let program_db0 =
            addKnownPrograms
                ([gitProgram, hackageRepoToolProgram, cabalProgram, diffProgram] ++ builtinPrograms)
                (withPrograms lbi)
    -- Reconfigure according to user flags
    let cargs = testCommonArgs args

    -- Reconfigure GHC
    (comp, platform, program_db2) <- case argGhcPath cargs of
        Nothing -> return (compiler lbi, hostPlatform lbi, program_db0)
        Just ghc_path -> do
            -- All the things that get updated paths from
            -- configCompilerEx.  The point is to make sure
            -- we reconfigure these when we need them.
            let program_db1 = unconfigureProgram "ghc"
                            . unconfigureProgram "ghc-pkg"
                            . unconfigureProgram "hsc2hs"
                            . unconfigureProgram "haddock"
                            . unconfigureProgram "hpc"
                            . unconfigureProgram "runghc"
                            . unconfigureProgram "gcc"
                            . unconfigureProgram "ld"
                            . unconfigureProgram "ar"
                            . unconfigureProgram "strip"
                            $ program_db0
            -- TODO: this actually leaves a pile of things unconfigured.
            -- Optimal strategy for us is to lazily configure them, so
            -- we don't pay for things we don't need.  A bit difficult
            -- to do in the current design.
            configCompilerEx
                (Just (compilerFlavor (compiler lbi)))
                (Just ghc_path)
                Nothing
                program_db1
                verbosity

    program_db3 <-
        reconfigurePrograms verbosity
            ([("cabal", p)   | p <- maybeToList (argCabalInstallPath cargs)] ++
             [("hackage-repo-tool", p)
                             | p <- maybeToList (argHackageRepoToolPath cargs)] ++
             [("haddock", p) | p <- maybeToList (argHaddockPath cargs)])
            [] -- --prog-options not supported ATM
            program_db2
    -- configCompilerEx only marks some programs as known, so to pick
    -- them up we must configure them
    program_db <- configureAllKnownPrograms verbosity program_db3

    let db_stack =
            case argGhcPath (testCommonArgs args) of
                Nothing -> withPackageDB lbi
                -- Can't use the build package db stack since they
                -- are all for the wrong versions!  TODO: Make
                -- this configurable
                Just _  -> [GlobalPackageDB]
        env = TestEnv {
                    testSourceDir = script_dir,
                    testSubName = script_base,
                    testMode = mode,
                    testProgramDb = program_db,
                    testPlatform = platform,
                    testCompiler = comp,
                    testPackageDBStack = db_stack,
                    testVerbosity = verbosity,
                    testMtimeChangeDelay = Nothing,
                    testScriptEnv = senv,
                    testSetupPath = dist_dir </> "setup" </> "setup",
                    testSkipSetupTests =  argSkipSetupTests (testCommonArgs args),
                    testHaveCabalShared = withSharedLib lbi,
                    testEnvironment =
                        -- Try to avoid Unicode output
                        [ ("LC_ALL", Just "C")
                        -- Hermetic builds (knot-tied)
                        , ("HOME", Just (testHomeDir env))],
                    testShouldFail = False,
                    testRelativeCurrentDir = ".",
                    testHavePackageDb = False,
                    testHaveSandbox = False,
                    testHaveRepo = False,
                    testHaveSourceCopy = False,
                    testCabalInstallAsSetup = False,
                    testCabalProjectFile = "cabal.project",
                    testPlan = Nothing,
                    testRecordDefaultMode = DoNotRecord,
                    testRecordUserMode = Nothing,
                    testRecordNormalizer = id
                }
    let go = do cleanup
                r <- m
                check_expect (argAccept (testCommonArgs args))
                return r
    runReaderT go env
  where
    cleanup = do
        env <- getTestEnv
        onlyIfExists . removeDirectoryRecursive $ testWorkDir env
        -- NB: it's important to initialize this ourselves, as
        -- the default configuration hardcodes Hackage, which we do
        -- NOT want to assume for these tests (no test should
        -- hit Hackage.)
        liftIO $ createDirectoryIfMissing True (testHomeDir env </> ".cabal")
        ghc_path <- programPathM ghcProgram
        liftIO $ writeFile (testUserCabalConfigFile env)
               $ unlines [ "with-compiler: " ++ ghc_path ]

    check_expect accept = do
        env <- getTestEnv
        actual_raw <- liftIO $ readFileOrEmpty (testActualFile env)
        expect <- liftIO $ readFileOrEmpty (testExpectFile env)
        norm_env <- mkNormalizerEnv
        let actual = normalizeOutput norm_env actual_raw
        when (words actual /= words expect) $ do
            -- First try whitespace insensitive diff
            let actual_fp = testNormalizedActualFile env
                expect_fp = testNormalizedExpectFile env
            liftIO $ writeFile actual_fp actual
            liftIO $ writeFile expect_fp expect
            liftIO $ putStrLn "Actual output differs from expected:"
            b <- diff ["-uw"] expect_fp actual_fp
            unless b . void $ diff ["-u"] expect_fp actual_fp
            if accept
                then do liftIO $ putStrLn "Accepting new output."
                        liftIO $ writeFileNoCR (testExpectFile env) actual
                else liftIO $ exitWith (ExitFailure 1)

readFileOrEmpty :: FilePath -> IO String
readFileOrEmpty f = readFile f `E.catch` \e ->
                                    if isDoesNotExistError e
                                        then return ""
                                        else E.throwIO e

-- | Runs 'diff' with some arguments on two files, outputting the
-- diff to stderr, and returning true if the two files differ
diff :: [String] -> FilePath -> FilePath -> TestM Bool
diff args path1 path2 = do
    diff_path <- programPathM diffProgram
    (_,_,_,h) <- liftIO $
        createProcess (proc diff_path (args ++ [path1, path2])) {
                std_out = UseHandle stderr
            }
    r <- liftIO $ waitForProcess h
    return (r /= ExitSuccess)

-- | Write a file with no CRs, always.
writeFileNoCR :: FilePath -> String -> IO ()
writeFileNoCR f s =
    withFile f WriteMode $ \h -> do
        hSetNewlineMode h noNewlineTranslation
        hPutStr h s

normalizeOutput :: NormalizerEnv -> String -> String
normalizeOutput nenv =
    -- Munge away .exe suffix on filenames (Windows)
    resub "([A-Za-z0-9.-]+)\\.exe" "\\1"
    -- Normalize backslashes to forward slashes to normalize
    -- file paths
  . map (\c -> if c == '\\' then '/' else c)
    -- Install path frequently has architecture specific elements, so
    -- nub it out
  . resub "Installing (.+) in .+" "Installing \\1 in <PATH>"
    -- Things that look like libraries
  . resub "libHS[A-Za-z0-9.-]+\\.(so|dll|a|dynlib)" "<LIBRARY>"
    -- This is dumb but I don't feel like pulling in another dep for
    -- string search-replace.  Make sure we do this before backslash
    -- normalization!
  . resub (posixRegexEscape (normalizerRoot nenv)) "<ROOT>/"
  . appEndo (F.fold (map (Endo . packageIdRegex) (normalizerKnownPackages nenv)))
    -- Look for foo-0.1/installed-0d6...
    -- These installed packages will vary depending on GHC version
    -- Makes assumption that installed packages don't have numbers
    -- in package name segment.
    -- Apply this before packageIdRegex, otherwise this regex doesn't match.
  . resub "([a-zA-Z]+(-[a-zA-Z])*)-[0-9]+(\\.[0-9]+)*/installed-[A-Za-z0-9.]+"
          "\\1-<VERSION>/installed-<HASH>..."
  . -- Normalize architecture
    resub (posixRegexEscape (display (normalizerPlatform nenv))) "<ARCH>"
    -- Normalize the current GHC version.  Apply this BEFORE packageIdRegex,
    -- which will pick up the install ghc library (which doesn't have the
    -- date glob).
  . (if normalizerGhcVersion nenv /= nullVersion
        then resub (posixRegexEscape (display (normalizerGhcVersion nenv))
                        -- Also glob the date, for nightly GHC builds
                        ++ "(\\.[0-9]+)?")
                   "<GHCVER>"
        else id)
  where
    packageIdRegex pid =
        resub (posixRegexEscape (display pid) ++ "(-[A-Za-z0-9.-]+)?")
              ((display (packageName pid)) ++ "-<VERSION>")

data NormalizerEnv = NormalizerEnv {
        normalizerRoot :: FilePath,
        normalizerGhcVersion :: Version,
        normalizerKnownPackages :: [PackageId],
        normalizerPlatform :: Platform
    }

mkNormalizerEnv :: TestM NormalizerEnv
mkNormalizerEnv = do
    env <- getTestEnv
    ghc_pkg_program <- requireProgramM ghcPkgProgram
    -- Arguably we should use Cabal's APIs but I am too lazy
    -- to remember what it is
    list_out <- liftIO $ readProcess (programPath ghc_pkg_program)
                      ["list", "--global", "--simple-output"] ""
    return NormalizerEnv {
        normalizerRoot
            = addTrailingPathSeparator (testSourceDir env),
        normalizerGhcVersion
            = compilerVersion (testCompiler env),
        normalizerKnownPackages
            = mapMaybe simpleParse (words list_out),
        normalizerPlatform
            = testPlatform env
    }

posixSpecialChars :: [Char]
posixSpecialChars = ".^$*+?()[{\\|"

posixRegexEscape :: String -> String
posixRegexEscape = concatMap (\c -> if c `elem` posixSpecialChars then ['\\', c] else [c])

resub :: String {- search -} -> String {- replace -} -> String {- input -} -> String
resub search replace s =
    subRegex (mkRegex search) s replace

requireProgramM :: Program -> TestM ConfiguredProgram
requireProgramM program = do
    env <- getTestEnv
    (configured_program, _) <- liftIO $
        requireProgram (testVerbosity env) program (testProgramDb env)
    return configured_program

programPathM :: Program -> TestM FilePath
programPathM program = do
    fmap programPath (requireProgramM program)

isAvailableProgram :: Program -> TestM Bool
isAvailableProgram program = do
    env <- getTestEnv
    case lookupProgram program (testProgramDb env) of
        Just _ -> return True
        Nothing -> do
            -- It might not have been configured. Try to configure.
            progdb <- liftIO $ configureProgram (testVerbosity env) program (testProgramDb env)
            case lookupProgram program progdb of
                Just _  -> return True
                Nothing -> return False

-- | Run an IO action, and suppress a "does not exist" error.
onlyIfExists :: MonadIO m => IO () -> m ()
onlyIfExists m =
    liftIO $ E.catch m $ \(e :: IOError) ->
        unless (isDoesNotExistError e) $ E.throwIO e

data TestEnv = TestEnv
    -- UNCHANGING:

    {
    -- | Path to the test directory, as specified by path to test
    -- script.
      testSourceDir     :: FilePath
    -- | Test sub-name, used to qualify dist/database directory to avoid
    -- conflicts.
    , testSubName       :: String
    -- | Test mode, further qualifies multiple invocations of the
    -- same test source code.
    , testMode          :: String
    -- | Program database to use when we want ghc, ghc-pkg, etc.
    , testProgramDb     :: ProgramDb
    -- | Compiler we are running tests for
    , testCompiler      :: Compiler
    -- | Platform we are running tests on
    , testPlatform      :: Platform
    -- | Package database stack (actually this changes lol)
    , testPackageDBStack :: PackageDBStack
    -- | How verbose to be
    , testVerbosity     :: Verbosity
    -- | How long we should 'threadDelay' to make sure the file timestamp is
    -- updated correctly for recompilation tests.  Nothing if we haven't
    -- calibrated yet.
    , testMtimeChangeDelay :: Maybe Int
    -- | Script environment for runghc
    , testScriptEnv :: ScriptEnv
    -- | Setup script path
    , testSetupPath :: FilePath
    -- | Skip Setup tests?
    , testSkipSetupTests :: Bool
    -- | Do we have shared libraries for the Cabal-under-tests?
    -- This is used for example to determine whether we can build
    -- detailed-0.9 tests dynamically, since they link against Cabal-under-test.
    , testHaveCabalShared :: Bool

    -- CHANGING:

    -- | Environment override
    , testEnvironment   :: [(String, Maybe String)]
    -- | When true, we invert the meaning of command execution failure
    , testShouldFail    :: Bool
    -- | The current working directory, relative to 'testSourceDir'
    , testRelativeCurrentDir :: FilePath
    -- | Says if we've initialized the per-test package DB
    , testHavePackageDb  :: Bool
    -- | Says if we're working in a sandbox
    , testHaveSandbox :: Bool
    -- | Says if we've setup a repository
    , testHaveRepo :: Bool
    -- | Says if we've copied the source to a hermetic directory
    , testHaveSourceCopy :: Bool
    -- | Says if we're testing cabal-install as setup
    , testCabalInstallAsSetup :: Bool
    -- | Says what cabal.project file to use (probed)
    , testCabalProjectFile :: FilePath
    -- | Cached record of the plan metadata from a new-build
    -- invocation; controlled by 'withPlan'.
    , testPlan :: Maybe Plan
    -- | If user mode is not set, this is the record mode we default to.
    , testRecordDefaultMode :: RecordMode
    -- | User explicitly set record mode.  Not implemented ATM.
    , testRecordUserMode :: Maybe RecordMode
    -- | Function to normalize recorded output
    , testRecordNormalizer :: String -> String
    }

testRecordMode :: TestEnv -> RecordMode
testRecordMode env = fromMaybe (testRecordDefaultMode env) (testRecordUserMode env)

data RecordMode = DoNotRecord | RecordMarked | RecordAll
    deriving (Show, Eq, Ord)

getTestEnv :: TestM TestEnv
getTestEnv = ask

------------------------------------------------------------------------
-- * Directories

-- | The absolute path to the root of the package directory; it's
-- where the Cabal file lives.  This is what you want the CWD of cabal
-- calls to be.
testCurrentDir :: TestEnv -> FilePath
testCurrentDir env =
    (if testHaveSourceCopy env
        then testSourceCopyDir env
        else testSourceDir env) </> testRelativeCurrentDir env

testName :: TestEnv -> String
testName env = testSubName env <.> testMode env

-- | The absolute path to the directory containing all the
-- files for ALL tests associated with a test (respecting
-- subtests.)  To clean, you ONLY need to delete this directory.
testWorkDir :: TestEnv -> FilePath
testWorkDir env =
    testSourceDir env </> (testName env <.> "dist")

-- | The absolute prefix where installs go.
testPrefixDir :: TestEnv -> FilePath
testPrefixDir env = testWorkDir env </> "usr"

-- | The absolute path to the build directory that should be used
-- for the current package in a test.
testDistDir :: TestEnv -> FilePath
testDistDir env = testWorkDir env </> "work" </> testRelativeCurrentDir env </> "dist"

-- | The absolute path to the shared package database that should
-- be used by all packages in this test.
testPackageDbDir :: TestEnv -> FilePath
testPackageDbDir env = testWorkDir env </> "packagedb"

-- | The absolute prefix where our simulated HOME directory is.
testHomeDir :: TestEnv -> FilePath
testHomeDir env = testWorkDir env </> "home"

-- | The absolute prefix of our sandbox directory
testSandboxDir :: TestEnv -> FilePath
testSandboxDir env = testWorkDir env </> "sandbox"

-- | The sandbox configuration file
testSandboxConfigFile :: TestEnv -> FilePath
testSandboxConfigFile env = testWorkDir env </> "cabal.sandbox.config"

-- | The absolute prefix of our local secure repository, which we
-- use to simulate "external" packages
testRepoDir :: TestEnv -> FilePath
testRepoDir env = testWorkDir env </> "repo"

-- | The absolute prefix of keys for the test.
testKeysDir :: TestEnv -> FilePath
testKeysDir env = testWorkDir env </> "keys"

-- | If 'withSourceCopy' is used, where the source files go.
testSourceCopyDir :: TestEnv -> FilePath
testSourceCopyDir env = testWorkDir env </> "source"

-- | The user cabal config file
-- TODO: Not obviously working on Windows
testUserCabalConfigFile :: TestEnv -> FilePath
testUserCabalConfigFile env = testHomeDir env </> ".cabal" </> "config"

-- | The file where the expected output of the test lives
testExpectFile :: TestEnv -> FilePath
testExpectFile env = testSourceDir env </> testName env <.> "out"

-- | Where we store the actual output
testActualFile :: TestEnv -> FilePath
testActualFile env = testWorkDir env </> testName env <.> "comp.out"

-- | Where we will write the normalized actual file (for diffing)
testNormalizedActualFile :: TestEnv -> FilePath
testNormalizedActualFile env = testActualFile env <.> "normalized"

-- | Where we will write the normalized expected file (for diffing)
testNormalizedExpectFile :: TestEnv -> FilePath
testNormalizedExpectFile env = testWorkDir env </> testName env <.> "out.normalized"
