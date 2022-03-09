module Rules.Test (testRules) where

import System.Environment

import Base
import CommandLine
import Expression
import Flavour
import Hadrian.Haskell.Cabal.Type (packageDependencies)
import Hadrian.Oracles.Cabal (readPackageData)
import Oracles.Setting
import Oracles.TestSettings
import Oracles.Flag
import Packages
import Settings
import Settings.Builders.RunTest
import Settings.Program (programContext)
import Target
import Utilities
import Context.Type
import qualified System.Directory as IO

ghcConfigHsPath :: FilePath
ghcConfigHsPath = "testsuite/mk/ghc-config.hs"

ghcConfigProgPath :: FilePath
ghcConfigProgPath = "test/bin/ghc-config" <.> exe

checkPprProgPath, checkPprSourcePath :: FilePath
checkPprProgPath = "test/bin/check-ppr" <.> exe
checkPprSourcePath = "utils/check-ppr/Main.hs"
checkPprExtra :: [String]
checkPprExtra = []

checkExactProgPath, checkExactSourcePath :: FilePath
checkExactProgPath = "test/bin/check-exact" <.> exe
checkExactSourcePath = "utils/check-exact/Main.hs"
checkExactExtra :: [String]
checkExactExtra = ["-iutils/check-exact"]

countDepsProgPath, countDepsSourcePath :: FilePath
countDepsProgPath = "test/bin/count-deps" <.> exe
countDepsSourcePath = "utils/count-deps/Main.hs"
countDepsExtra :: [String]
countDepsExtra = ["-iutils/count-deps"]

noteLinterProgPath, noteLinterSourcePath :: FilePath
noteLinterProgPath = "test/bin/lint-notes" <.> exe
noteLinterSourcePath = "linters/lint-notes/Main.hs"
noteLinterExtra :: [String]
noteLinterExtra = ["-ilinters/lint-notes"]

whitespaceLinterProgPath, whitespaceLinterSourcePath :: FilePath
whitespaceLinterProgPath = "test/bin/lint-whitespace" <.> exe
whitespaceLinterSourcePath = "linters/lint-whitespace/Main.hs"
whitespaceLinterExtra :: [String]
whitespaceLinterExtra = ["-ilinters/lint-whitespace", "-ilinters/linters-common"]

checkPrograms :: [(String,FilePath, FilePath, [String], Package, Stage -> Stage, [Package] -> [Package])]
checkPrograms =
    [ ("test:check-ppr",checkPprProgPath, checkPprSourcePath, checkPprExtra, checkPpr, id, id)
    , ("test:check-exact",checkExactProgPath, checkExactSourcePath, checkExactExtra, checkExact, id, id)
    , ("test:count-deps",countDepsProgPath, countDepsSourcePath, countDepsExtra, countDeps, id, id)
    , ("lint:notes", noteLinterProgPath, noteLinterSourcePath, noteLinterExtra, lintNotes, const Stage0, id)
    , ("lint:whitespace", whitespaceLinterProgPath, whitespaceLinterSourcePath, whitespaceLinterExtra, lintWhitespace, const Stage0, filter (/= lintersCommon))
    ]

inTreeOutTree :: (Stage -> Action b) -> Action b -> Action b
inTreeOutTree inTree outTree = do
    args <- userSetting defaultTestArgs
    let testCompilerArg = testCompiler args
    case stageOf testCompilerArg of
      Just stg -> inTree stg
      Nothing -> outTree

testsuiteDeps :: Rules ()
testsuiteDeps = do
  root <- buildRootRules
  "test:ghc" ~> inTreeOutTree
                    (\stg -> do
                      needTestsuitePackages stg
                      need [(root -/- ghcConfigPath)])
                    (return ())

ghcConfigPath :: FilePath
ghcConfigPath = "test/ghcconfig"

-- TODO: clean up after testing
testRules :: Rules ()
testRules = do
    root <- buildRootRules

    testsuiteDeps

    -- Using program shipped with testsuite to generate ghcconfig file.
    root -/- ghcConfigProgPath %> \_ -> do
        ghc0Path <- getCompilerPath "stage0"
        -- Invoke via bash to work around #17362.
        -- Reasons why this is required are not entirely clear.
        cmd ["bash"] ["-c", ghc0Path ++ " " ++ ghcConfigHsPath ++ " -o " ++ (root -/- ghcConfigProgPath)]

    -- we need to create wrappers to test the stage1 compiler
    -- as the stage1 compiler needs the stage2 libraries
    -- to have any hope of passing tests.
    root -/- "stage1-test/bin/*" %> \path -> do
      let prog = takeBaseName path
          stage0prog = root -/- "stage0/bin" -/- prog <.> exe
      need [stage0prog]
      abs_prog_path <- liftIO (IO.canonicalizePath stage0prog)
      -- Use the stage1 package database
      pkgDb <- liftIO . IO.makeAbsolute =<< packageDbPath Stage1
      if prog `elem` ["ghc","runghc"] then do
          let flags = [ "-no-user-package-db", "-hide-package", "ghc" , "-package-env","-","-package-db",pkgDb]
          writeFile' path $ unlines ["#!/bin/sh",unwords ((abs_prog_path : flags) ++ ["${1+\"$@\"}"])]
          makeExecutable path
      else if prog == "ghc-pkg" then do
        let flags = ["--no-user-package-db", "--global-package-db", pkgDb]
        writeFile' path $ unlines ["#!/bin/sh",unwords ((abs_prog_path : flags) ++ ["${1+\"$@\"}"])]
        makeExecutable path
      else createFileLink abs_prog_path path

    -- Rules for building check-ppr, check-exact and
    -- check-ppr-annotations with the compiler we are going to test
    -- (in-tree or out-of-tree).
    forM_ checkPrograms $ \(name, progPath, sourcePath, mextra, progPkg, mod_stage, mod_pkgs) -> do
        name ~> need [root -/- progPath]
        root -/- progPath %> \path -> do
            need [ sourcePath ]
            testGhc <- testCompiler <$> userSetting defaultTestArgs

            -- when we're about to test an in-tree compiler, just build the package
            -- normally, NOT stage3, as there are no rules for stage4 yet
            case stageOf testGhc of
              Just stg -> do
                fs <- pkgFile (mod_stage stg) progPkg
                need [fs]
                prog_path <- programPath =<< programContext (mod_stage stg) progPkg
                abs_prog_path <- liftIO (IO.canonicalizePath prog_path)
                createFileLink abs_prog_path path
            -- otherwise, build it by directly invoking ghc
              Nothing -> do
                top <- topDirectory
                depsPkgs <- mod_pkgs . packageDependencies <$> readPackageData progPkg
                bindir <- getBinaryDirectory testGhc
                debugged <- ghcDebugged <$> flavour
                dynPrograms <- dynamicGhcPrograms =<< flavour
                cmd [bindir </> "ghc" <.> exe] $
                    concatMap (\p -> ["-package", pkgName p]) depsPkgs ++
                    ["-o", top -/- path, top -/- sourcePath] ++
                    mextra ++
                    -- If GHC is build with debug options, then build check-ppr
                    -- also with debug options.  This allows, e.g., to print debug
                    -- messages of various RTS subsystems while using check-ppr.
                    (if debugged then ["-debug"] else []) ++
                    -- If GHC is build dynamic, then build check-ppr also dynamic.
                    (if dynPrograms then ["-dynamic"] else [])

    root -/- ghcConfigPath %> \_ -> do
        alwaysRerun
        args <- userSetting defaultTestArgs
        let testGhc = testCompiler args
        ghcPath <- getCompilerPath testGhc
        whenJust (stageOf testGhc) $ \stg ->
          need . (:[]) =<< programPath (Context stg ghc vanilla)
        cwd <- liftIO $ IO.getCurrentDirectory
        need [makeRelative cwd ghcPath]
        need [root -/- ghcConfigProgPath]
        cmd [FileStdout $ root -/- ghcConfigPath] (root -/- ghcConfigProgPath)
            [ghcPath]

    root -/- timeoutPath %> \_ -> timeoutProgBuilder

    "test" ~> do

        args <- userSetting defaultTestArgs
        let testCompilerArg = testCompiler args
        let stg = fromMaybe Stage2 $ stageOf testCompilerArg
        let test_target tt = target (vanillaContext stg compiler) (Testsuite tt) [] []

        -- We need to ask the testsuite if it needs any extra hadrian dependencies for the
        -- tests it is going to run,
        -- for example "docs_haddock"
        -- We then need to go and build these dependencies
        extra_targets <- words <$> askWithResources [] (test_target GetExtraDeps)
        need $ filter (isOkToBuild args) extra_targets

        -- Prepare Ghc configuration file for input compiler.
        need [root -/- timeoutPath]


        ghcPath <- getCompilerPath testCompilerArg


        makePath        <- builderPath $ Make ""
        top             <- topDirectory
        ghcFlags        <- runTestGhcFlags
        let ghciFlags = ghcFlags ++ unwords
              [ "--interactive", "-v0", "-ignore-dot-ghci"
              , "-fno-ghci-history"
              ]
        ccPath          <- settingsFileSetting SettingsFileSetting_CCompilerCommand
        ccFlags         <- settingsFileSetting SettingsFileSetting_CCompilerFlags

        pythonPath      <- builderPath Python

        -- Set environment variables for test's Makefile.
        -- TODO: Ideally we would define all those env vars in 'env', so that
        --       Shake can keep track of them, but it is not as easy as it seems
        --       to get that to work.
        liftIO $ do
            -- Many of those env vars are used by Makefiles in the
            -- test infrastructure, or from tests or their
            -- Makefiles.
            setEnv "MAKE" makePath
            setEnv "PYTHON" pythonPath
            setEnv "TEST_HC" ghcPath
            setEnv "TEST_HC_OPTS" ghcFlags
            setEnv "TEST_HC_OPTS_INTERACTIVE" ghciFlags
            setEnv "TEST_CC" ccPath
            setEnv "TEST_CC_OPTS" ccFlags
            setEnv "CHECK_PPR" (top -/- root -/- checkPprProgPath)
            setEnv "CHECK_EXACT" (top -/- root -/- checkExactProgPath)
            setEnv "COUNT_DEPS" (top -/- root -/- countDepsProgPath)
            setEnv "LINT_NOTES" (top -/- root -/- noteLinterProgPath)
            setEnv "LINT_WHITESPACE" (top -/- root -/- whitespaceLinterProgPath)

            -- This lets us bypass the need to generate a config
            -- through Make, which happens in testsuite/mk/boilerplate.mk
            -- which is in turn included by all test 'Makefile's.
            setEnv "ghc_config_mk" (top -/- root -/- ghcConfigPath)


        -- Execute the test target.
        -- We override the verbosity setting to make sure the user can see
        -- the test output: https://gitlab.haskell.org/ghc/ghc/issues/15951.
        withVerbosity Diagnostic $ buildWithCmdOptions [] $ test_target RunTest

-- | Given a test compiler and a hadrian dependency (target), check if we
-- can build the target with the compiler
--
-- We can always build a target with an intree compiler But we can only build
-- targets with special support (checkPrograms) with arbitrary compilers.
--
-- We need to build the dependencies if --test-have-intree-files is set.
-- We should have built them already by this point, but
isOkToBuild :: TestArgs -> String -> Bool
isOkToBuild args target
   = stageOf (testCompiler args) `elem` [Just Stage1, Just Stage2]
  || testHasInTreeFiles args
  || target `elem` map fst7 checkPrograms
  where
    fst7 (a,_,_,_,_,_,_) = a

-- | Build the timeout program.
-- See: https://github.com/ghc/ghc/blob/master/testsuite/timeout/Makefile#L23

timeoutProgBuilder :: Action ()
timeoutProgBuilder = do
    root    <- buildRoot
    if windowsHost
        then do
            prog <- programPath =<< programContext Stage0 timeout
            copyFile prog (root -/- timeoutPath)
        else do
            python <- builderPath Python
            copyFile "testsuite/timeout/timeout.py" (root -/- timeoutPath <.> "py")
            let script = unlines
                    [ "#!/bin/sh"
                    , "exec " ++ python ++ " $0.py \"$@\"" ]
            writeFile' (root -/- timeoutPath) script
            makeExecutable (root -/- timeoutPath)

-- | Build extra programs and libraries required by testsuite
needTestsuitePackages :: Stage -> Action ()
needTestsuitePackages stg = do
  allpkgs <- packages <$> flavour
  -- We need the libraries of the successor stage
  libpkgs <- map (Stage1,) . filter isLibrary <$> allpkgs (succ stg)
  -- And the executables of the current stage
  exepkgs <- map (stg,) . filter isProgram <$> allpkgs stg
  -- Don't require lib:ghc or lib:cabal when testing the stage1 compiler
  -- This is a hack, but a major usecase for testing the stage1 compiler is
  -- so that we can use it even if ghc stage2 fails to build
  -- Unfortunately, we still need the liba
  let pkgs = filter (\(_,p) -> not $ "iserv" `isInfixOf` pkgName p || ((pkgName p `elem` ["ghc", "Cabal"]) && stg == Stage0))
                    (libpkgs ++ exepkgs ++ [ (stg,timeout) | windowsHost ])
  need =<< mapM (uncurry pkgFile) pkgs
  cross <- flag CrossCompiling
  when (not cross) $ needIservBins stg
  root <- buildRoot
  -- require the shims for testing stage1
  need =<< sequence [(\f -> root -/- "stage1-test/bin" -/- takeFileName f) <$> (pkgFile Stage0 p) | (Stage0,p) <- exepkgs]

-- stage 1 ghc lives under stage0/bin,
-- stage 2 ghc lives under stage1/bin, etc
stageOf :: String -> Maybe Stage
stageOf "stage1" = Just Stage0
stageOf "stage2" = Just Stage1
stageOf "stage3" = Just Stage2
stageOf _ = Nothing

needIservBins :: Stage -> Action ()
needIservBins stg = do
  let ws = [vanilla, profiling, dynamic]
  progs <- catMaybes <$> mapM (canBuild stg) ws
  need progs
  where
    -- Only build iserv binaries if all dependencies are built the right
    -- way already. In particular this fixes the case of no_profiled_libs
    -- not working with the testsuite, see #19624
    canBuild Stage0 _ = pure Nothing
    canBuild stg w = do
      contextDeps <- contextDependencies (Context stg iserv w)
      ws <- forM contextDeps $ \c ->
              interpretInContext c (getLibraryWays <>
                                    if Context.Type.package c == rts
                                      then getRtsWays
                                      else mempty)
      if (all (w `elem`) ws)
        then Just <$> programPath (Context stg iserv w)
        else return Nothing


pkgFile :: Stage -> Package -> Action FilePath
pkgFile stage pkg
    | isLibrary pkg = pkgConfFile (Context stage pkg profilingDynamic)
    | otherwise     = programPath =<< programContext stage pkg
