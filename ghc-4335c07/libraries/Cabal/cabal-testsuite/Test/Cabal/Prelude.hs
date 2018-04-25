{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

-- | Generally useful definitions that we expect most test scripts
-- to use.
module Test.Cabal.Prelude (
    module Test.Cabal.Prelude,
    module Test.Cabal.Monad,
    module Test.Cabal.Run,
    module System.FilePath,
    module Control.Monad,
    module Distribution.Version,
    module Distribution.Simple.Program,
) where

import Test.Cabal.Script
import Test.Cabal.Run
import Test.Cabal.Monad
import Test.Cabal.Plan

import Distribution.Compat.Time (calibrateMtimeChangeDelay)
import Distribution.Simple.Compiler (PackageDBStack, PackageDB(..))
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Db
import Distribution.Simple.Program
import Distribution.System (OS(Windows,Linux,OSX), buildOS)
import Distribution.Simple.Utils
    ( withFileContents, tryFindPackageDesc )
import Distribution.Simple.Configure
    ( getPersistBuildConfig )
import Distribution.Version
import Distribution.Package
import Distribution.Types.UnqualComponentName
import Distribution.Types.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec

import Distribution.Compat.Stack

import Text.Regex.TDFA

import Control.Concurrent.Async
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BSL
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Maybe
import System.Exit
import System.FilePath
import Control.Concurrent (threadDelay)
import qualified Data.Char as Char
import System.Directory

#ifndef mingw32_HOST_OS
import Control.Monad.Catch ( bracket_ )
import System.Posix.Files  ( createSymbolicLink )
#endif

------------------------------------------------------------------------
-- * Utilities

runM :: FilePath -> [String] -> TestM Result
runM path args = do
    env <- getTestEnv
    r <- liftIO $ run (testVerbosity env)
                 (Just (testCurrentDir env))
                 (testEnvironment env)
                 path
                 args
    recordLog r
    requireSuccess r

runProgramM :: Program -> [String] -> TestM Result
runProgramM prog args = do
    configured_prog <- requireProgramM prog
    -- TODO: Consider also using other information from
    -- ConfiguredProgram, e.g., env and args
    runM (programPath configured_prog) args

getLocalBuildInfoM :: TestM LocalBuildInfo
getLocalBuildInfoM = do
    env <- getTestEnv
    liftIO $ getPersistBuildConfig (testDistDir env)

------------------------------------------------------------------------
-- * Changing parameters

withDirectory :: FilePath -> TestM a -> TestM a
withDirectory f = withReaderT
    (\env -> env { testRelativeCurrentDir = testRelativeCurrentDir env </> f })

-- We append to the environment list, as per 'getEffectiveEnvironment'
-- which prefers the latest override.
withEnv :: [(String, Maybe String)] -> TestM a -> TestM a
withEnv e = withReaderT (\env -> env { testEnvironment = testEnvironment env ++ e })

-- HACK please don't use me
withEnvFilter :: (String -> Bool) -> TestM a -> TestM a
withEnvFilter p = withReaderT (\env -> env { testEnvironment = filter (p . fst) (testEnvironment env) })

------------------------------------------------------------------------
-- * Running Setup

marked_verbose :: String
marked_verbose = "-vverbose +markoutput +nowrap"

setup :: String -> [String] -> TestM ()
setup cmd args = void (setup' cmd args)

setup' :: String -> [String] -> TestM Result
setup' cmd args = do
    env <- getTestEnv
    when ((cmd == "register" || cmd == "copy") && not (testHavePackageDb env)) $
        error "Cannot register/copy without using 'withPackageDb'"
    ghc_path     <- programPathM ghcProgram
    haddock_path <- programPathM haddockProgram
    let args' = case cmd of
            "configure" ->
                -- If the package database is empty, setting --global
                -- here will make us error loudly if we try to install
                -- into a bad place.
                [ "--global"
                -- NB: technically unnecessary with Cabal, but
                -- definitely needed for Setup, which doesn't
                -- respect cabal.config
                , "--with-ghc", ghc_path
                , "--with-haddock", haddock_path
                -- This avoids generating hashes in our package IDs,
                -- which helps the test suite's expect tests.
                , "--enable-deterministic"
                -- These flags make the test suite run faster
                -- Can't do this unless we LD_LIBRARY_PATH correctly
                -- , "--enable-executable-dynamic"
                -- , "--disable-optimization"
                -- Specify where we want our installed packages to go
                , "--prefix=" ++ testPrefixDir env
                ] ++ packageDBParams (testPackageDBStack env)
                  ++ args
            _ -> args
    let rel_dist_dir = definitelyMakeRelative (testCurrentDir env) (testDistDir env)
        full_args = cmd : [marked_verbose, "--distdir", rel_dist_dir] ++ args'
    defaultRecordMode RecordMarked $ do
    recordHeader ["Setup", cmd]
    if testCabalInstallAsSetup env
        then runProgramM cabalProgram full_args
        else do
            pdfile <- liftIO $ tryFindPackageDesc (testCurrentDir env)
            pdesc <- liftIO $ readGenericPackageDescription (testVerbosity env) pdfile
            if buildType (packageDescription pdesc) == Just Simple
                then runM (testSetupPath env) full_args
                -- Run the Custom script!
                else do
                  r <- liftIO $ runghc (testScriptEnv env)
                                       (Just (testCurrentDir env))
                                       (testEnvironment env)
                                       (testCurrentDir env </> "Setup.hs")
                                       full_args
                  recordLog r
                  requireSuccess r
    -- This code is very tempting (and in principle should be quick:
    -- after all we are loading the built version of Cabal), but
    -- actually it costs quite a bit in wallclock time (e.g. 54sec to
    -- 68sec on AllowNewer, working with un-optimized Cabal.)
    {-
    r <- liftIO $ runghc (testScriptEnv env)
                         (Just (testCurrentDir env))
                         (testEnvironment env)
                         "Setup.hs"
                         (cmd : ["-v", "--distdir", testDistDir env] ++ args')
    -- don't forget to check results...
    -}

definitelyMakeRelative :: FilePath -> FilePath -> FilePath
definitelyMakeRelative base0 path0 =
    let go [] path = joinPath path
        go base [] = joinPath (replicate (length base) "..")
        go (x:xs) (y:ys)
            | x == y    = go xs ys
            | otherwise = go (x:xs) [] </> go [] (y:ys)
    -- NB: It's important to normalize, as otherwise if
    -- we see "foo/./bar" we'll incorrectly conclude that we need
    -- to go "../../.." to get out of it.
    in go (splitPath (normalise base0)) (splitPath (normalise path0))

-- | This abstracts the common pattern of configuring and then building.
setup_build :: [String] -> TestM ()
setup_build args = do
    setup "configure" args
    setup "build" []
    return ()

-- | This abstracts the common pattern of "installing" a package.
setup_install :: [String] -> TestM ()
setup_install args = do
    setup "configure" args
    setup "build" []
    setup "copy" []
    setup "register" []
    return ()

-- | This abstracts the common pattern of "installing" a package,
-- with haddock documentation.
setup_install_with_docs :: [String] -> TestM ()
setup_install_with_docs args = do
    setup "configure" args
    setup "build" []
    setup "haddock" []
    setup "copy" []
    setup "register" []
    return ()

packageDBParams :: PackageDBStack -> [String]
packageDBParams dbs = "--package-db=clear"
                    : map (("--package-db=" ++) . convert) dbs
  where
    convert :: PackageDB -> String
    convert  GlobalPackageDB         = "global"
    convert  UserPackageDB           = "user"
    convert (SpecificPackageDB path) = path

------------------------------------------------------------------------
-- * Running cabal

cabal :: String -> [String] -> TestM ()
cabal "sandbox" _ =
    error "Use cabal_sandbox instead"
cabal cmd args = void (cabal' cmd args)

cabal' :: String -> [String] -> TestM Result
cabal' = cabalG' []

cabalG :: [String] -> String -> [String] -> TestM ()
cabalG global_args cmd args = void (cabalG' global_args cmd args)

cabalG' :: [String] -> String -> [String] -> TestM Result
cabalG' _ "sandbox" _ =
    -- NB: We don't just auto-pass this through, because it's
    -- possible that the first argument isn't the sub-sub-command.
    -- So make sure the user specifies it correctly.
    error "Use cabal_sandbox' instead"
cabalG' global_args cmd args = do
    env <- getTestEnv
    -- Freeze writes out cabal.config to source directory, this is not
    -- overwritable
    when (cmd `elem` ["freeze"]) requireHasSourceCopy
    let extra_args
          -- Sandboxes manage dist dir
          | testHaveSandbox env
          = install_args
          | cmd `elem` ["update", "outdated", "user-config", "manpage", "freeze"]
          = [ ]
          -- new-build commands are affected by testCabalProjectFile
          | "new-" `isPrefixOf` cmd
          = [ "--builddir", testDistDir env
            , "--project-file", testCabalProjectFile env
            , "-j1" ]
          | otherwise
          = [ "--builddir", testDistDir env ] ++
            install_args
        install_args
          | cmd == "install"
         || cmd == "build" = [ "-j1" ]
          | otherwise = []
        extra_global_args
          | testHaveSandbox env
          = [ "--sandbox-config-file", testSandboxConfigFile env ]
          | otherwise
          = []
        cabal_args = extra_global_args
                  ++ global_args
                  ++ [ cmd, marked_verbose ]
                  ++ extra_args
                  ++ args
    defaultRecordMode RecordMarked $ do
    recordHeader ["cabal", cmd]
    cabal_raw' cabal_args

cabal_sandbox :: String -> [String] -> TestM ()
cabal_sandbox cmd args = void $ cabal_sandbox' cmd args

cabal_sandbox' :: String -> [String] -> TestM Result
cabal_sandbox' cmd args = do
    env <- getTestEnv
    let cabal_args = [ "--sandbox-config-file", testSandboxConfigFile env
                     , "sandbox", cmd
                     , marked_verbose ]
                  ++ args
    defaultRecordMode RecordMarked $ do
    recordHeader ["cabal", "sandbox", cmd]
    cabal_raw' cabal_args

cabal_raw' :: [String] -> TestM Result
cabal_raw' cabal_args = runProgramM cabalProgram cabal_args

withSandbox :: TestM a -> TestM a
withSandbox m = do
    env0 <- getTestEnv
    -- void $ cabal_raw' ["sandbox", "init", "--sandbox", testSandboxDir env0]
    cabal_sandbox "init" ["--sandbox", testSandboxDir env0]
    withReaderT (\env -> env { testHaveSandbox = True }) m

withProjectFile :: FilePath -> TestM a -> TestM a
withProjectFile fp m =
    withReaderT (\env -> env { testCabalProjectFile = fp }) m

-- | Assuming we've successfully configured a new-build project,
-- read out the plan metadata so that we can use it to do other
-- operations.
withPlan :: TestM a -> TestM a
withPlan m = do
    env0 <- getTestEnv
    Just plan <- JSON.decode `fmap`
                    liftIO (BSL.readFile (testDistDir env0 </> "cache" </> "plan.json"))
    withReaderT (\env -> env { testPlan = Just plan }) m

-- | Run an executable from a package.  Requires 'withPlan' to have
-- been run so that we can find the dist dir.
runPlanExe :: String {- package name -} -> String {- component name -}
           -> [String] -> TestM ()
runPlanExe pkg_name cname args = void $ runPlanExe' pkg_name cname args

-- | Run an executable from a package.  Requires 'withPlan' to have
-- been run so that we can find the dist dir.  Also returns 'Result'.
runPlanExe' :: String {- package name -} -> String {- component name -}
            -> [String] -> TestM Result
runPlanExe' pkg_name cname args = do
    Just plan <- testPlan `fmap` getTestEnv
    let dist_dir = planDistDir plan (mkPackageName pkg_name)
                        (CExeName (mkUnqualComponentName cname))
    defaultRecordMode RecordAll $ do
    recordHeader [pkg_name, cname]
    runM (dist_dir </> "build" </> cname </> cname) args

------------------------------------------------------------------------
-- * Running ghc-pkg

withPackageDb :: TestM a -> TestM a
withPackageDb m = do
    env <- getTestEnv
    let db_path = testPackageDbDir env
    if testHavePackageDb env
        then m
        else withReaderT (\nenv ->
                            nenv { testPackageDBStack
                                    = testPackageDBStack env
                                   ++ [SpecificPackageDB db_path]
                                , testHavePackageDb = True
                                } )
               $ do ghcPkg "init" [db_path]
                    m

ghcPkg :: String -> [String] -> TestM ()
ghcPkg cmd args = void (ghcPkg' cmd args)

ghcPkg' :: String -> [String] -> TestM Result
ghcPkg' cmd args = do
    env <- getTestEnv
    unless (testHavePackageDb env) $
        error "Must initialize package database using withPackageDb"
    -- NB: testDBStack already has the local database
    ghcConfProg <- requireProgramM ghcProgram
    let db_stack = testPackageDBStack env
        extraArgs = ghcPkgPackageDBParams
                        (fromMaybe
                            (error "ghc-pkg: cannot detect version")
                            (programVersion ghcConfProg))
                        db_stack
    recordHeader ["ghc-pkg", cmd]
    runProgramM ghcPkgProgram (cmd : extraArgs ++ args)

ghcPkgPackageDBParams :: Version -> PackageDBStack -> [String]
ghcPkgPackageDBParams version dbs = concatMap convert dbs where
    convert :: PackageDB -> [String]
    -- Ignoring global/user is dodgy but there's no way good
    -- way to give ghc-pkg the correct flags in this case.
    convert  GlobalPackageDB         = []
    convert  UserPackageDB           = []
    convert (SpecificPackageDB path)
        | version >= mkVersion [7,6]
        = ["--package-db=" ++ path]
        | otherwise
        = ["--package-conf=" ++ path]

------------------------------------------------------------------------
-- * Running other things

-- | Run an executable that was produced by cabal.  The @exe_name@
-- is precisely the name of the executable section in the file.
runExe :: String -> [String] -> TestM ()
runExe exe_name args = void (runExe' exe_name args)

runExe' :: String -> [String] -> TestM Result
runExe' exe_name args = do
    env <- getTestEnv
    defaultRecordMode RecordAll $ do
    recordHeader [exe_name]
    runM (testDistDir env </> "build" </> exe_name </> exe_name) args

-- | Run an executable that was installed by cabal.  The @exe_name@
-- is precisely the name of the executable.
runInstalledExe :: String -> [String] -> TestM ()
runInstalledExe exe_name args = void (runInstalledExe' exe_name args)

-- | Run an executable that was installed by cabal.  Use this
-- instead of 'runInstalledExe' if you need to inspect the
-- stdout/stderr output.
runInstalledExe' :: String -> [String] -> TestM Result
runInstalledExe' exe_name args = do
    env <- getTestEnv
    defaultRecordMode RecordAll $ do
    recordHeader [exe_name]
    runM (testPrefixDir env </> "bin" </> exe_name) args

-- | Run a shell command in the current directory.
shell :: String -> [String] -> TestM Result
shell exe args = runM exe args

------------------------------------------------------------------------
-- * Repository manipulation

-- Workflows we support:
--  1. Test comes with some packages (directories in repository) which
--  should be in the repository and available for depsolving/installing
--  into global store.
--
-- Workflows we might want to support in the future
--  * Regression tests may want to test on Hackage index.  They will
--  operate deterministically as they will be pinned to a timestamp.
--  (But should we allow this? Have to download the tarballs in that
--  case. Perhaps dep solver only!)
--  * We might sdist a local package, and then upload it to the
--  repository
--  * Some of our tests involve old versions of Cabal.  This might
--  be one of the rare cases where we're willing to grab the entire
--  tarball.
--
-- Properties we want to hold:
--  1. Tests can be run offline.  No dependence on hackage.haskell.org
--  beyond what we needed to actually get the build of Cabal working
--  itself
--  2. Tests are deterministic.  Updates to Hackage should not cause
--  tests to fail.  (OTOH, it's good to run tests on most recent
--  Hackage index; some sort of canary test which is run nightly.
--  Point is it should NOT be tied to cabal source code.)
--
-- Technical notes:
--  * We depend on hackage-repo-tool binary.  It would better if it was
--  libified into hackage-security but this has not been done yet.
--

hackageRepoTool :: String -> [String] -> TestM ()
hackageRepoTool cmd args = void $ hackageRepoTool' cmd args

hackageRepoTool' :: String -> [String] -> TestM Result
hackageRepoTool' cmd args = do
    recordHeader ["hackage-repo-tool", cmd]
    runProgramM hackageRepoToolProgram (cmd : args)

tar :: [String] -> TestM ()
tar args = void $ tar' args

tar' :: [String] -> TestM Result
tar' args = do
    recordHeader ["tar"]
    runProgramM tarProgram args

-- | Creates a tarball of a directory, such that if you
-- archive the directory "/foo/bar/baz" to "mine.tgz", @tar tf@ reports
-- @baz/file1@, @baz/file2@, etc.
archiveTo :: FilePath -> FilePath -> TestM ()
src `archiveTo` dst = do
    -- TODO: Consider using the @tar@ library?
    let (src_parent, src_dir) = splitFileName src
    -- TODO: --format ustar, like createArchive?
    tar ["-czf", dst, "-C", src_parent, src_dir]

infixr 4 `archiveTo`

-- | Given a directory (relative to the 'testCurrentDir') containing
-- a series of directories representing packages, generate an
-- external repository corresponding to all of these packages
withRepo :: FilePath -> TestM a -> TestM a
withRepo repo_dir m = do
    env <- getTestEnv

    -- Check if hackage-repo-tool is available, and skip if not
    skipUnless =<< isAvailableProgram hackageRepoToolProgram

    -- 1. Generate keys
    hackageRepoTool "create-keys" ["--keys", testKeysDir env]
    -- 2. Initialize repo directory
    let package_dir = testRepoDir env </> "package"
    liftIO $ createDirectoryIfMissing True (testRepoDir env </> "index")
    liftIO $ createDirectoryIfMissing True package_dir
    -- 3. Create tarballs
    pkgs <- liftIO $ getDirectoryContents (testCurrentDir env </> repo_dir)
    forM_ pkgs $ \pkg -> do
        case pkg of
            '.':_ -> return ()
            _     -> testCurrentDir env </> repo_dir </> pkg
                        `archiveTo`
                            package_dir </> pkg <.> "tar.gz"
    -- 4. Initialize repository
    hackageRepoTool "bootstrap" ["--keys", testKeysDir env, "--repo", testRepoDir env]
    -- 5. Wire it up in .cabal/config
    -- TODO: libify this
    let package_cache = testHomeDir env </> ".cabal" </> "packages"
    liftIO $ appendFile (testUserCabalConfigFile env)
           $ unlines [ "repository test-local-repo"
                     , "  url: file:" ++ testRepoDir env
                     , "  secure: True"
                     -- TODO: Hypothetically, we could stick in the
                     -- correct key here
                     , "  root-keys: "
                     , "  key-threshold: 0"
                     , "remote-repo-cache: " ++ package_cache ]
    -- 6. Create local directories (TODO: this is a bug #4136, once you
    -- fix that this can be removed)
    liftIO $ createDirectoryIfMissing True (package_cache </> "test-local-repo")
    -- 7. Update our local index
    cabal "update" []
    -- 8. Profit
    withReaderT (\env' -> env' { testHaveRepo = True }) m
    -- TODO: Arguably should undo everything when we're done...

------------------------------------------------------------------------
-- * Subprocess run results

requireSuccess :: Result -> TestM Result
requireSuccess r@Result { resultCommand = cmd
                        , resultExitCode = exitCode
                        , resultOutput = output } = withFrozenCallStack $ do
    env <- getTestEnv
    when (exitCode /= ExitSuccess && not (testShouldFail env)) $
        assertFailure $ "Command " ++ cmd ++ " failed.\n" ++
        "Output:\n" ++ output ++ "\n"
    when (exitCode == ExitSuccess && testShouldFail env) $
        assertFailure $ "Command " ++ cmd ++ " succeeded.\n" ++
        "Output:\n" ++ output ++ "\n"
    return r

initWorkDir :: TestM ()
initWorkDir = do
    env <- getTestEnv
    liftIO $ createDirectoryIfMissing True (testWorkDir env)

-- | Record a header to help identify the output to the expect
-- log.  Unlike the 'recordLog', we don't record all arguments;
-- just enough to give you an idea of what the command might have
-- been.  (This is because the arguments may not be deterministic,
-- so we don't want to spew them to the log.)
recordHeader :: [String] -> TestM ()
recordHeader args = do
    env <- getTestEnv
    let mode = testRecordMode env
        str_header = "# " ++ intercalate " " args ++ "\n"
        header = C.pack (testRecordNormalizer env str_header)
    case mode of
        DoNotRecord -> return ()
        _ -> do
            initWorkDir
            liftIO $ putStr str_header
            liftIO $ C.appendFile (testWorkDir env </> "test.log") header
            liftIO $ C.appendFile (testActualFile env) header

recordLog :: Result -> TestM ()
recordLog res = do
    env <- getTestEnv
    let mode = testRecordMode env
    initWorkDir
    liftIO $ C.appendFile (testWorkDir env </> "test.log")
                         (C.pack $ "+ " ++ resultCommand res ++ "\n"
                            ++ resultOutput res ++ "\n\n")
    liftIO . C.appendFile (testActualFile env) . C.pack . testRecordNormalizer env $
        case mode of
            RecordAll    -> unlines (lines (resultOutput res))
            RecordMarked -> getMarkedOutput (resultOutput res)
            DoNotRecord  -> ""

getMarkedOutput :: String -> String -- trailing newline
getMarkedOutput out = unlines (go (lines out) False)
  where
    go [] _ = []
    go (x:xs) True
        | "-----END CABAL OUTPUT-----"   `isPrefixOf` x
                    =     go xs False
        | otherwise = x : go xs True
    go (x:xs) False
        -- NB: Windows has extra goo at the end
        | "-----BEGIN CABAL OUTPUT-----" `isPrefixOf` x
                    = go xs True
        | otherwise = go xs False

------------------------------------------------------------------------
-- * Test helpers

assertFailure :: WithCallStack (String -> m ())
assertFailure msg = withFrozenCallStack $ error msg

assertExitCode :: MonadIO m => WithCallStack (ExitCode -> Result -> m ())
assertExitCode code result =
  when (code /= resultExitCode result) $
    assertFailure $ "Expected exit code: "
                 ++ show code
                 ++ "\nActual: "
                 ++ show (resultExitCode result)

assertEqual :: (Eq a, Show a, MonadIO m) => WithCallStack (String -> a -> a -> m ())
assertEqual s x y =
    withFrozenCallStack $
      when (x /= y) $
        error (s ++ ":\nExpected: " ++ show x ++ "\nActual: " ++ show y)

assertNotEqual :: (Eq a, Show a, MonadIO m) => WithCallStack (String -> a -> a -> m ())
assertNotEqual s x y =
    withFrozenCallStack $
      when (x == y) $
        error (s ++ ":\nGot both: " ++ show x)

assertBool :: MonadIO m => WithCallStack (String -> Bool -> m ())
assertBool s x =
    withFrozenCallStack $
      unless x $ error s

shouldExist :: MonadIO m => WithCallStack (FilePath -> m ())
shouldExist path =
    withFrozenCallStack $
    liftIO $ doesFileExist path >>= assertBool (path ++ " should exist")

shouldNotExist :: MonadIO m => WithCallStack (FilePath -> m ())
shouldNotExist path =
    withFrozenCallStack $
    liftIO $ doesFileExist path >>= assertBool (path ++ " should exist") . not

assertRegex :: MonadIO m => String -> String -> Result -> m ()
assertRegex msg regex r =
    withFrozenCallStack $
    let out = resultOutput r
    in assertBool (msg ++ ",\nactual output:\n" ++ out)
       (out =~ regex)

fails :: TestM a -> TestM a
fails = withReaderT (\env -> env { testShouldFail = not (testShouldFail env) })

defaultRecordMode :: RecordMode -> TestM a -> TestM a
defaultRecordMode mode = withReaderT (\env -> env {
    testRecordDefaultMode = mode
    })

recordMode :: RecordMode -> TestM a -> TestM a
recordMode mode = withReaderT (\env -> env {
    testRecordUserMode = Just mode
    })

recordNormalizer :: (String -> String) -> TestM a -> TestM a
recordNormalizer f =
    withReaderT (\env -> env { testRecordNormalizer = testRecordNormalizer env . f })

assertOutputContains :: MonadIO m => WithCallStack (String -> Result -> m ())
assertOutputContains needle result =
    withFrozenCallStack $
    unless (needle `isInfixOf` (concatOutput output)) $
    assertFailure $ " expected: " ++ needle
  where output = resultOutput result

assertOutputDoesNotContain :: MonadIO m => WithCallStack (String -> Result -> m ())
assertOutputDoesNotContain needle result =
    withFrozenCallStack $
    when (needle `isInfixOf` (concatOutput output)) $
    assertFailure $ "unexpected: " ++ needle
  where output = resultOutput result

assertFindInFile :: MonadIO m => WithCallStack (String -> FilePath -> m ())
assertFindInFile needle path =
    withFrozenCallStack $
    liftIO $ withFileContents path
                 (\contents ->
                  unless (needle `isInfixOf` contents)
                         (assertFailure ("expected: " ++ needle ++ "\n" ++
                                         " in file: " ++ path)))

assertFileDoesContain :: MonadIO m => WithCallStack (FilePath -> String -> m ())
assertFileDoesContain path needle =
    withFrozenCallStack $
    liftIO $ withFileContents path
                 (\contents ->
                  unless (needle `isInfixOf` contents)
                         (assertFailure ("expected: " ++ needle ++ "\n" ++
                                         " in file: " ++ path)))

assertFileDoesNotContain :: MonadIO m => WithCallStack (FilePath -> String -> m ())
assertFileDoesNotContain path needle =
    withFrozenCallStack $
    liftIO $ withFileContents path
                 (\contents ->
                  when (needle `isInfixOf` contents)
                       (assertFailure ("expected: " ++ needle ++ "\n" ++
                                       " in file: " ++ path)))

-- | Replace line breaks with spaces, correctly handling "\r\n".
concatOutput :: String -> String
concatOutput = unwords . lines . filter ((/=) '\r')

------------------------------------------------------------------------
-- * Skipping tests

hasSharedLibraries  :: TestM Bool
hasSharedLibraries = do
    shared_libs_were_removed <- ghcVersionIs (>= mkVersion [7,8])
    return (not (buildOS == Windows && shared_libs_were_removed))

hasProfiledLibraries :: TestM Bool
hasProfiledLibraries = do
    env <- getTestEnv
    ghc_path <- programPathM ghcProgram
    let prof_test_hs = testWorkDir env </> "Prof.hs"
    liftIO $ writeFile prof_test_hs "module Prof where"
    r <- liftIO $ run (testVerbosity env) (Just (testCurrentDir env))
                      (testEnvironment env) ghc_path ["-prof", "-c", prof_test_hs]
    return (resultExitCode r == ExitSuccess)

-- | Check if the GHC that is used for compiling package tests has
-- a shared library of the cabal library under test in its database.
--
-- An example where this is needed is if you want to dynamically link
-- detailed-0.9 test suites, since those depend on the Cabal library unde rtest.
hasCabalShared :: TestM Bool
hasCabalShared = do
  env <- getTestEnv
  return (testHaveCabalShared env)

ghcVersionIs :: WithCallStack ((Version -> Bool) -> TestM Bool)
ghcVersionIs f = do
    ghc_program <- requireProgramM ghcProgram
    case programVersion ghc_program of
        Nothing -> error $ "ghcVersionIs: no ghc version for "
                        ++ show (programLocation ghc_program)
        Just v -> return (f v)

isWindows :: TestM Bool
isWindows = return (buildOS == Windows)

isOSX :: TestM Bool
isOSX = return (buildOS == OSX)

isLinux :: TestM Bool
isLinux = return (buildOS == Linux)

hasCabalForGhc :: TestM Bool
hasCabalForGhc = do
    env <- getTestEnv
    ghc_program <- requireProgramM ghcProgram
    (runner_ghc_program, _) <- liftIO $ requireProgram
        (testVerbosity env)
        ghcProgram
        (runnerProgramDb (testScriptEnv env))
    -- TODO: I guess, to be more robust what we should check for
    -- specifically is that the Cabal library we want to use
    -- will be picked up by the package db stack of ghc-program
    return (programPath ghc_program == programPath runner_ghc_program)

-- | If you want to use a Custom setup with new-build, it needs to
-- be 1.20 or later.  Ordinarily, Cabal can go off and build a
-- sufficiently recent Cabal if necessary, but in our test suite,
-- by default, we try to avoid doing so (since that involves a
-- rather lengthy build process), instead using the boot Cabal if
-- possible.  But some GHCs don't have a recent enough boot Cabal!
-- You'll want to exclude them in that case.
--
hasNewBuildCompatBootCabal :: TestM Bool
hasNewBuildCompatBootCabal = ghcVersionIs (>= mkVersion [7,9])

------------------------------------------------------------------------
-- * Broken tests

expectBroken :: Int -> TestM a -> TestM ()
expectBroken ticket m = do
    env <- getTestEnv
    liftIO . withAsync (runReaderT m env) $ \a -> do
        r <- waitCatch a
        case r of
            Left e  -> do
                putStrLn $ "This test is known broken, see #" ++ show ticket ++ ":"
                print e
                runReaderT expectedBroken env
            Right _ -> do
                runReaderT unexpectedSuccess env

expectBrokenIf :: Bool -> Int -> TestM a -> TestM ()
expectBrokenIf False _ m = void $ m
expectBrokenIf True ticket m = expectBroken ticket m

expectBrokenUnless :: Bool -> Int -> TestM a -> TestM ()
expectBrokenUnless b = expectBrokenIf (not b)

------------------------------------------------------------------------
-- * Miscellaneous

git :: String -> [String] -> TestM ()
git cmd args = void $ git' cmd args

git' :: String -> [String] -> TestM Result
git' cmd args = do
    recordHeader ["git", cmd]
    runProgramM gitProgram (cmd : args)

gcc :: [String] -> TestM ()
gcc args = void $ gcc' args

gcc' :: [String] -> TestM Result
gcc' args = do
    recordHeader ["gcc"]
    runProgramM gccProgram args

ghc :: [String] -> TestM ()
ghc args = void $ ghc' args

ghc' :: [String] -> TestM Result
ghc' args = do
    recordHeader ["ghc"]
    runProgramM ghcProgram args

-- | If a test needs to modify or write out source files, it's
-- necessary to make a hermetic copy of the source files to operate
-- on.  This function arranges for this to be done.
--
-- This requires the test repository to be a Git checkout, because
-- we use the Git metadata to figure out what files to copy into the
-- hermetic copy.
withSourceCopy :: TestM a -> TestM a
withSourceCopy m = do
    env <- getTestEnv
    let cwd  = testCurrentDir env
        dest = testSourceCopyDir env
    r <- git' "ls-files" ["--cached", "--modified"]
    forM_ (lines (resultOutput r)) $ \f -> do
        unless (isTestFile f) $ do
            liftIO $ createDirectoryIfMissing True (takeDirectory (dest </> f))
            liftIO $ copyFile (cwd </> f) (dest </> f)
    withReaderT (\nenv -> nenv { testHaveSourceCopy = True }) m

-- | Look up the 'InstalledPackageId' of a package name.
getIPID :: String -> TestM String
getIPID pn = do
    r <- ghcPkg' "field" ["--global", pn, "id"]
    -- Don't choke on warnings from ghc-pkg
    case mapMaybe (stripPrefix "id: ") (lines (resultOutput r)) of
        [x] -> return (takeWhile (not . Char.isSpace) x)
        _ -> error $ "could not determine id of " ++ pn

-- | Delay a sufficient period of time to permit file timestamp
-- to be updated.
delay :: TestM ()
delay = do
    env <- getTestEnv
    is_old_ghc <- ghcVersionIs (< mkVersion [7,7])
    -- For old versions of GHC, we only had second-level precision,
    -- so we need to sleep a full second.  Newer versions use
    -- millisecond level precision, so we only have to wait
    -- the granularity of the underlying filesystem.
    -- TODO: cite commit when GHC got better precision; this
    -- version bound was empirically generated.
    liftIO . threadDelay $
        if is_old_ghc
            then 1000000
            else fromMaybe
                    (error "Delay must be enclosed by withDelay")
                    (testMtimeChangeDelay env)

-- | Calibrate file modification time delay, if not
-- already determined.
withDelay :: TestM a -> TestM a
withDelay m = do
    env <- getTestEnv
    case testMtimeChangeDelay env of
        Nothing -> do
            -- Figure out how long we need to delay for recompilation tests
            (_, mtimeChange) <- liftIO $ calibrateMtimeChangeDelay
            withReaderT (\nenv -> nenv { testMtimeChangeDelay = Just mtimeChange }) m
        Just _ -> m

-- | Create a symlink for the duration of the provided action. If the symlink
-- already exists, it is deleted. Does not work on Windows.
withSymlink :: FilePath -> FilePath -> TestM a -> TestM a
#ifdef mingw32_HOST_OS
withSymlink _oldpath _newpath _act =
  error "PackageTests.PackageTester.withSymlink: does not work on Windows!"
#else
withSymlink oldpath newpath0 act = do
  env <- getTestEnv
  let newpath = testCurrentDir env </> newpath0
  symlinkExists <- liftIO $ doesFileExist newpath
  when symlinkExists $ liftIO $ removeFile newpath
  bracket_ (liftIO $ createSymbolicLink oldpath newpath)
           (liftIO $ removeFile newpath) act
#endif

writeSourceFile :: FilePath -> String -> TestM ()
writeSourceFile fp s = do
    requireHasSourceCopy
    cwd <- fmap testCurrentDir getTestEnv
    liftIO $ writeFile (cwd </> fp) s

copySourceFileTo :: FilePath -> FilePath -> TestM ()
copySourceFileTo src dest = do
    requireHasSourceCopy
    cwd <- fmap testCurrentDir getTestEnv
    liftIO $ copyFile (cwd </> src) (cwd </> dest)

requireHasSourceCopy :: TestM ()
requireHasSourceCopy = do
    env <- getTestEnv
    unless (testHaveSourceCopy env) $ do
        error "This operation requires a source copy; use withSourceCopy and 'git add' all test files"

-- NB: Keep this synchronized with partitionTests
isTestFile :: FilePath -> Bool
isTestFile f =
    case takeExtensions f of
        ".test.hs"      -> True
        ".multitest.hs" -> True
        _               -> False
