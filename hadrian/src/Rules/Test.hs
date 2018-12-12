module Rules.Test (testRules) where

import System.Environment

import Base
import Expression
import Oracles.Setting
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
        ghc <- builderPath $ Ghc CompileHs Stage0
        createDirectory $ takeDirectory (root -/- ghcConfigProgPath)
        cmd ghc [ghcConfigHsPath, "-o" , root -/- ghcConfigProgPath]

    -- TODO : Use input test compiler and not just stage2 compiler.
    root -/- ghcConfigPath ~> do
        ghcPath <- needFile Stage1 ghc
        need [root -/- ghcConfigProgPath]
        cmd [FileStdout $ root -/- ghcConfigPath] (root -/- ghcConfigProgPath)
            [ghcPath]

    root -/- timeoutPath ~> timeoutProgBuilder

    "validate" ~> do
        needTestBuilders
        build $ target (vanillaContext Stage2 compiler) (Make "testsuite/tests") [] []

    "test" ~> do
        needTestBuilders

        -- TODO : Should we remove the previosly generated config file?
        -- Prepare Ghc configuration file for input compiler.
        need [root -/- ghcConfigPath, root -/- timeoutPath]

        -- TODO This approach doesn't work.
        -- Set environment variables for test's Makefile.
        env <- sequence
            [ builderEnvironment "MAKE" $ Make ""
            , builderEnvironment "TEST_HC" $ Ghc CompileHs Stage2
            , AddEnv "TEST_HC_OPTS" <$> runTestGhcFlags ]

        makePath        <- builderPath $ Make ""
        top             <- topDirectory
        ghcPath         <- (top -/-) <$> builderPath (Ghc CompileHs Stage2)
        ghcFlags        <- runTestGhcFlags
        checkPprPath    <- (top -/-) <$> needFile Stage1 checkPpr
        annotationsPath <- (top -/-) <$> needFile Stage1 checkApiAnnotations

        -- Set environment variables for test's Makefile.
        liftIO $ do
            setEnv "MAKE" makePath
            setEnv "TEST_HC" ghcPath
            setEnv "TEST_HC_OPTS" ghcFlags
            setEnv "CHECK_PPR" checkPprPath
            setEnv "CHECK_API_ANNOTATIONS" annotationsPath

        -- Execute the test target.
        -- We override the verbosity setting to make sure the user can see
        -- the test output: https://ghc.haskell.org/trac/ghc/ticket/15951.
        withVerbosity Loud $ buildWithCmdOptions env $
            target (vanillaContext Stage2 compiler) RunTest [] []

-- | Build extra programs and libraries required by testsuite
needTestsuitePackages :: Action ()
needTestsuitePackages = do
    targets   <- mapM (needFile Stage1) =<< testsuitePackages
    -- iserv is not supported under Windows
    windows <- windowsHost
    when (not windows) needIservBins
    need targets

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

needIservBins :: Action ()
needIservBins = do
    rtsways <- interpretInContext (vanillaContext Stage1 ghc) getRtsWays
    need =<< traverse programPath
               [ Context Stage1 iserv w
               | w <- [vanilla, profiling
                    -- TODO dynamic way has been reverted as the dynamic build
                    --      is broken. See #15837.
                    -- , dynamic
                    ]
               , w `elem` rtsways
               ]

needTestBuilders :: Action ()
needTestBuilders = do
    needBuilder $ Ghc CompileHs Stage2
    needBuilder $ GhcPkg Update Stage1
    needBuilder Hpc
    needBuilder $ Hsc2Hs Stage1
    needTestsuitePackages

needFile :: Stage -> Package -> Action FilePath
needFile stage pkg
-- TODO (Alp): we might sometimes need more than vanilla!
-- This should therefore depend on what test ways
-- we are going to use, I suppose?
    | isLibrary pkg = pkgConfFile (Context stage pkg profilingDynamic)
    | otherwise     = programPath =<< programContext stage pkg
