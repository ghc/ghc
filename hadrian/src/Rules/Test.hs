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
import Packages
import Settings
import Settings.Default
import Settings.Builders.RunTest
import Settings.Program (programContext)
import Target
import Utilities
import Context.Type

ghcConfigHsPath :: FilePath
ghcConfigHsPath = "testsuite/mk/ghc-config.hs"

ghcConfigProgPath :: FilePath
ghcConfigProgPath = "test/bin/ghc-config" <.> exe

checkPprProgPath, checkPprSourcePath :: FilePath
checkPprProgPath = "test/bin/check-ppr" <.> exe
checkPprSourcePath = "utils/check-ppr/Main.hs"
checkPprExtra :: Maybe String
checkPprExtra = Nothing

checkExactProgPath, checkExactSourcePath :: FilePath
checkExactProgPath = "test/bin/check-exact" <.> exe
checkExactSourcePath = "utils/check-exact/Main.hs"
checkExactExtra :: Maybe String
checkExactExtra = Just "-iutils/check-exact"


checkPrograms :: [(FilePath, FilePath, Maybe String, Package)]
checkPrograms =
    [ (checkPprProgPath, checkPprSourcePath, checkPprExtra, checkPpr)
    , (checkExactProgPath, checkExactSourcePath, checkExactExtra, checkExact)
    ]

ghcConfigPath :: FilePath
ghcConfigPath = "test/ghcconfig"

-- TODO: clean up after testing
testRules :: Rules ()
testRules = do
    root <- buildRootRules

    -- Using program shipped with testsuite to generate ghcconfig file.
    root -/- ghcConfigProgPath %> \_ -> do
        ghc0Path <- getCompilerPath "stage0"
        -- Invoke via bash to work around #17362.
        -- Reasons why this is required are not entirely clear.
        cmd ["bash"] ["-c", ghc0Path ++ " " ++ ghcConfigHsPath ++ " -o " ++ (root -/- ghcConfigProgPath)]

    -- Rules for building check-ppr, check-exact and
    -- check-ppr-annotations with the compiler we are going to test
    -- (in-tree or out-of-tree).
    forM_ checkPrograms $ \(progPath, sourcePath, mextra, progPkg) ->
        root -/- progPath %> \path -> do
            need [ sourcePath ]
            testGhc <- testCompiler <$> userSetting defaultTestArgs
            top <- topDirectory
            depsPkgs <- packageDependencies <$> readPackageData progPkg

            -- when we're about to test an in-tree compiler, just build the package
            -- normally, NOT stage3, as there are no rules for stage4 yet
            if (testGhc `elem` ["stage1", "stage2"])
              then do
                let stg = stageOf testGhc
                prog_path <- programPath =<< programContext stg progPkg
                createFileLink prog_path path
            -- otherwise, build it by directly invoking ghc
              else do
                bindir <- getBinaryDirectory testGhc
                debugged <- ghcDebugged <$> flavour
                dynPrograms <- dynamicGhcPrograms =<< flavour
                cmd [bindir </> "ghc" <.> exe] $
                    concatMap (\p -> ["-package", pkgName p]) depsPkgs ++
                    ["-o", top -/- path, top -/- sourcePath] ++
                    (maybe [] (\e -> [e]) mextra) ++
                    -- If GHC is build with debug options, then build check-ppr
                    -- also with debug options.  This allows, e.g., to print debug
                    -- messages of various RTS subsystems while using check-ppr.
                    (if debugged then ["-debug"] else []) ++
                    -- If GHC is build dynamic, then build check-ppr also dynamic.
                    (if dynPrograms then ["-dynamic"] else [])

    root -/- ghcConfigPath %> \_ -> do
        args <- userSetting defaultTestArgs
        let testGhc = testCompiler args
            stg = stageOf testGhc
        ghcPath <- getCompilerPath testGhc
        when (testGhc `elem` ["stage1", "stage2", "stage3"]) $
            need . (:[]) =<< programPath (Context stg ghc vanilla)
        need [root -/- ghcConfigProgPath]
        cmd [FileStdout $ root -/- ghcConfigPath] (root -/- ghcConfigProgPath)
            [ghcPath]

    root -/- timeoutPath %> \_ -> timeoutProgBuilder

    "test" ~> do
        needTestBuilders

        -- TODO : Should we remove the previously generated config file?
        -- Prepare Ghc configuration file for input compiler.
        need [root -/- ghcConfigPath, root -/- timeoutPath]

        args <- userSetting defaultTestArgs
        ghcPath <- getCompilerPath (testCompiler args)

        -- TODO This approach doesn't work.
        -- Set environment variables for test's Makefile.
        env <- sequence
            [ builderEnvironment "MAKE" $ Make ""
            , builderEnvironment "TEST_HC" $ Ghc CompileHs Stage2
            , AddEnv "TEST_HC_OPTS" <$> runTestGhcFlags ]

        makePath        <- builderPath $ Make ""
        top             <- topDirectory
        ghcFlags        <- runTestGhcFlags
        let ghciFlags = ghcFlags ++ unwords
              [ "--interactive", "-v0", "-ignore-dot-ghci"
              , "-fno-ghci-history"
              ]

        pythonPath      <- builderPath Python
        need [ root -/- checkPprProgPath
             , root -/- checkExactProgPath ]

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
            setEnv "CHECK_PPR" (top -/- root -/- checkPprProgPath)
            setEnv "CHECK_EXACT" (top -/- root -/- checkExactProgPath)

            -- This lets us bypass the need to generate a config
            -- through Make, which happens in testsuite/mk/boilerplate.mk
            -- which is in turn included by all test 'Makefile's.
            setEnv "ghc_config_mk" (top -/- root -/- ghcConfigPath)

        -- Execute the test target.
        -- We override the verbosity setting to make sure the user can see
        -- the test output: https://gitlab.haskell.org/ghc/ghc/issues/15951.
        withVerbosity Loud $ buildWithCmdOptions env $
            target (vanillaContext Stage2 compiler) RunTest [] []

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
                    [ "#!/usr/bin/env sh"
                    , "exec " ++ python ++ " $0.py \"$@\"" ]
            writeFile' (root -/- timeoutPath) script
            makeExecutable (root -/- timeoutPath)

needTestBuilders :: Action ()
needTestBuilders = do
    testGhc <- testCompiler <$> userSetting defaultTestArgs
    when (testGhc `elem` ["stage1", "stage2", "stage3"])
         (needTestsuitePackages testGhc)

-- | Build extra programs and libraries required by testsuite
needTestsuitePackages :: String -> Action ()
needTestsuitePackages testGhc = do
    when (testGhc `elem` ["stage1", "stage2", "stage3"]) $ do
        let stg = stageOf testGhc
        allpkgs   <- packages <$> flavour
        stgpkgs   <- allpkgs (succ stg)
        testpkgs  <- testsuitePackages
        let pkgs = filter (\p -> not $ "iserv" `isInfixOf` pkgName p)
                          (stgpkgs ++ testpkgs)
        need =<< mapM (pkgFile stg) pkgs
        needIservBins

-- stage 1 ghc lives under stage0/bin,
-- stage 2 ghc lives under stage1/bin, etc
stageOf :: String -> Stage
stageOf "stage1" = Stage0
stageOf "stage2" = Stage1
stageOf "stage3" = Stage2
stageOf _ = error "unexpected stage argument"

needIservBins :: Action ()
needIservBins = do
  testGhc <- testCompiler <$> userSetting defaultTestArgs
  let stg = stageOf testGhc
      ws = [vanilla, profiling, dynamic]
  progs <- catMaybes <$> mapM (canBuild stg) ws
  need progs
  where
    -- Only build iserv binaries if all dependencies are built the right
    -- way already. In particular this fixes the case of no_profiled_libs
    -- not working with the testsuite, see #19624
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
