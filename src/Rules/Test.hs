module Rules.Test (testRules, runTestGhcFlags, timeoutProgPath) where

import Base
import Expression
import GHC
import Oracles.Flag
import Oracles.Setting
import Settings
import Target
import Utilities

import System.Environment

-- TODO: clean up after testing
testRules :: Rules ()
testRules = do
    root <- buildRootRules

    root -/- timeoutPyPath ~> do
        copyFile "testsuite/timeout/timeout.py" (root -/- timeoutPyPath)

    -- TODO windows is still not supported.
    --
    -- See: https://github.com/ghc/ghc/blob/master/testsuite/timeout/Makefile#L23
    root -/- timeoutProgPath ~> do
        python <- builderPath Python
        need [root -/- timeoutPyPath]
        let script = unlines
                [ "#!/usr/bin/env sh"
                , "exec " ++ python ++ " $0.py \"$@\""
                ]
        liftIO $ do
            writeFile (root -/- timeoutProgPath) script
        makeExecutable (root -/- timeoutProgPath)

    "validate" ~> do
        needTestBuilders
        build $ target (vanillaContext Stage2 compiler) (Make "testsuite/tests") [] []

    "test" ~> do
        needTestBuilders

        -- Prepare the timeout program.
        need [ root -/- timeoutProgPath ]

        -- TODO This approach doesn't work.
        -- Set environment variables for test's Makefile.
        env <- sequence
            [ builderEnvironment "MAKE" $ Make ""
            , builderEnvironment "TEST_HC" $ Ghc CompileHs Stage2
            , AddEnv "TEST_HC_OPTS" <$> runTestGhcFlags ]

        makePath       <- builderPath $ Make ""
        top            <- topDirectory
        ghcPath        <- (top -/-) <$> builderPath (Ghc CompileHs Stage2)
        ghcFlags       <- runTestGhcFlags

        -- Set environment variables for test's Makefile.
        liftIO $ do
            setEnv "MAKE" makePath
            setEnv "TEST_HC" ghcPath
            setEnv "TEST_HC_OPTS" ghcFlags

        -- Execute the test target.
        buildWithCmdOptions env $ target (vanillaContext Stage2 compiler) RunTest [] []

-- | Build extra programs required by testsuite
needTestsuiteBuilders :: Action ()
needTestsuiteBuilders = do
    targets <- mapM (needfile Stage1) =<< testsuitePackages
    need targets
  where
    needfile :: Stage -> Package -> Action FilePath
    needfile stage pkg = programPath =<< programContext stage pkg

needTestBuilders :: Action ()
needTestBuilders = do
    needBuilder $ Ghc CompileHs Stage2
    needBuilder $ GhcPkg Update Stage1
    needBuilder Hpc
    needBuilder (Hsc2Hs Stage1)
    needTestsuiteBuilders

-- | Extra flags to send to the Haskell compiler to run tests.
runTestGhcFlags :: Action String
runTestGhcFlags = do
    unregisterised <- flag GhcUnregisterised

    let ifMinGhcVer ver opt = do v <- ghcCanonVersion
                                 if ver <= v then pure opt
                                             else pure ""

    -- Read extra argument for test from command line, like `-fvectorize`.
    ghcOpts <- fromMaybe "" <$> (liftIO $ lookupEnv "EXTRA_HC_OPTS")

    -- See: https://github.com/ghc/ghc/blob/master/testsuite/mk/test.mk#L28
    let ghcExtraFlags = if unregisterised
                           then "-optc-fno-builtin"
                           else ""

    -- Take flags to send to the Haskell compiler from test.mk.
    -- See: https://github.com/ghc/ghc/blob/master/testsuite/mk/test.mk#L37
    unwords <$> sequence
        [ pure " -dcore-lint -dcmm-lint -no-user-package-db -rtsopts"
        , pure ghcOpts
        , pure ghcExtraFlags
        , ifMinGhcVer "711" "-fno-warn-missed-specialisations"
        , ifMinGhcVer "711" "-fshow-warning-groups"
        , ifMinGhcVer "801" "-fdiagnostics-color=never"
        , ifMinGhcVer "801" "-fno-diagnostics-show-caret"
        , pure "-dno-debug-output"
        ]

timeoutPyPath :: FilePath
timeoutPyPath = "test/bin/timeout.py"

timeoutProgPath :: FilePath
timeoutProgPath = "test/bin/timeout" <.> exe
