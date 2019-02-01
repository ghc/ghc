module Rules.Test (testRules) where

import System.Environment

import Base
import CommandLine
import Expression
import Oracles.Setting
import Oracles.TestSettings
import Packages
import Settings
import Settings.Default
import Settings.Builders.RunTest
import Target
import Utilities

ghcConfigHsPath :: FilePath
ghcConfigHsPath = "testsuite/mk/ghc-config.hs"

ghcConfigProgPath :: FilePath
ghcConfigProgPath = "test/bin/ghc-config"

ghcConfigPath :: FilePath
ghcConfigPath = "test/ghcconfig"

-- TODO: clean up after testing
testRules :: Rules ()
testRules = do
    root <- buildRootRules

    -- Using program shipped with testsuite to generate ghcconfig file.
    root -/- ghcConfigProgPath ~> do
        ghc0Path <- getCompilerPath "stage0"
        createDirectory $ takeDirectory (root -/- ghcConfigProgPath)
        cmd ghc0Path [ghcConfigHsPath, "-o" , root -/- ghcConfigProgPath]

    -- TODO : Use input test compiler and not just stage2 compiler.
    root -/- ghcConfigPath ~> do
        args <- userSetting defaultTestArgs
        let testGhc = testCompiler args
        ghcPath <- getCompilerPath testGhc
        when (testGhc `elem` ["stage1", "stage2"]) $ need [ ghcPath ]
        need [root -/- ghcConfigProgPath]
        cmd [FileStdout $ root -/- ghcConfigPath] (root -/- ghcConfigProgPath)
            [ghcPath]

    root -/- timeoutPath ~> timeoutProgBuilder

    "test" ~> do
        -- needTestBuilders

        -- TODO : Should we remove the previosly generated config file?
        -- Prepare Ghc configuration file for input compiler.
        need [root -/- ghcConfigPath, root -/- timeoutPath]

        args            <- userSetting defaultTestArgs
        ghcPath         <- getCompilerPath (testCompiler args)

        -- TODO This approach doesn't work.
        -- Set environment variables for test's Makefile.
        env <- sequence
            [ builderEnvironment "MAKE" $ Make ""
            , pure (AddEnv "TEST_HC" ghcPath)
            , AddEnv "TEST_HC_OPTS" <$> runTestGhcFlags ]

        makePath        <- builderPath $ Make ""
        top             <- topDirectory
        ghcFlags        <- runTestGhcFlags

        -- where to get those from?
        checkPprPath    <- needFile Stage0 checkPpr
        annotationsPath <- needFile Stage0 checkApiAnnotations
        pythonPath      <- builderPath Python
        need [ checkPprPath, annotationsPath ]

        -- Set environment variables for test's Makefile.
        liftIO $ do
            setEnv "MAKE" makePath
            setEnv "PYTHON" pythonPath
            setEnv "TEST_HC" ghcPath
            setEnv "TEST_HC_OPTS" ghcFlags
            setEnv "CHECK_PPR" (top </> checkPprPath)
            setEnv "CHECK_API_ANNOTATIONS" (top </> annotationsPath)

        -- Execute the test target.
        -- We override the verbosity setting to make sure the user can see
        -- the test output: https://ghc.haskell.org/trac/ghc/ticket/15951.
        withVerbosity Loud $ buildWithCmdOptions env $
            -- is it really the right target, especially with an
            -- external ghc?
            target (vanillaContext Stage2 compiler) RunTest [] []

-- | Build the timeout program.
-- See: https://github.com/ghc/ghc/blob/master/testsuite/timeout/Makefile#L23
timeoutProgBuilder :: Action ()
timeoutProgBuilder = do
    root    <- buildRoot
    windows <- windowsHost
    if windows
        then do
            prog <- programPath =<< programContext Stage1 timeout
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
    ghcPath <- getCompilerPath testGhc
    bindir <- getBinaryDirectory testGhc
    when (testGhc `elem` ["stage1", "stage2"]) needTestsuitePackages

-- | Build extra programs and libraries required by testsuite
needTestsuitePackages :: Action ()
needTestsuitePackages = do
    testGhc <- testCompiler <$> userSetting defaultTestArgs
    when (testGhc `elem` ["stage1", "stage2"]) $ do
        let stg = stageOf testGhc
        targets   <- mapM (needFile stg) =<< testsuitePackages
        -- iserv is not supported under Windows
        windows <- windowsHost
        when (not windows) needIservBins
        need targets

    where stageOf "stage1" = Stage1
          stageOf "stage2" = Stage2
          stageOf _ = error "unexpected argument"

needIservBins :: Action ()
needIservBins = do
    testGhc <- testCompiler <$> userSetting defaultTestArgs
    let stg = if testGhc == "stage1" then Stage0 else Stage1
    rtsways <- interpretInContext (vanillaContext stg ghc) getRtsWays
    need =<< traverse programPath
               [ Context stg iserv w
               | w <- [vanilla, profiling
                    -- TODO dynamic way has been reverted as the dynamic build
                    --      is broken. See #15837.
                    -- , dynamic
                    ]
               , w `elem` rtsways
               ]

needFile :: Stage -> Package -> Action FilePath
needFile stage pkg
-- TODO (Alp): we might sometimes need more than vanilla!
-- This should therefore depend on what test ways
-- we are going to use, I suppose?
    | isLibrary pkg = pkgConfFile (Context stage pkg profilingDynamic)
    | otherwise     = programPath =<< programContext stage pkg
