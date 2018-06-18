module Rules.Test (testRules, runTestGhcFlags, timeoutProgPath) where

import Base
import Expression
import GHC
import GHC.Packages (timeout)
import Oracles.Flag
import Oracles.Setting
import Settings
import Target
import Utilities

import System.Environment

-- TODO: clean up after testing
testRules :: Rules ()
testRules = do
    "validate" ~> do
        needTestBuilders
        build $ target (vanillaContext Stage2 compiler) (Make "testsuite/tests") [] []

    "test" ~> do
        needTestBuilders

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
    needfile stage pkg
      -- TODO (Alp): we might sometimes need more than vanilla!
      -- This should therefore depend on what test ways
      -- we are going to use, I suppose?
      | isLibrary pkg = pkgConfFile (vanillaContext stage pkg)
      | otherwise = programPath =<< programContext stage pkg

-- | Build the timeout program.
-- See: https://github.com/ghc/ghc/blob/master/testsuite/timeout/Makefile#L23
timeoutProgBuilder :: Action ()
timeoutProgBuilder = do
    root    <- buildRoot
    windows <- windowsHost
    if windows
        then do
            prog <- programPath =<< programContext Stage1 timeout
            need [ prog ]
            copyFile prog (root -/- timeoutProgPath)
        else do
            python <- builderPath Python
            copyFile "testsuite/timeout/timeout.py" (root -/- "test/bin/timeout.py")
            let script = unlines
                    [ "#!/usr/bin/env sh"
                    , "exec " ++ python ++ " $0.py \"$@\""
                    ]
            liftIO $ do
                writeFile (root -/- timeoutProgPath) script
            makeExecutable (root -/- timeoutProgPath)

needTestBuilders :: Action ()
needTestBuilders = do
    needBuilder $ Ghc CompileHs Stage2
    needBuilder $ GhcPkg Update Stage1
    needBuilder Hpc
    needBuilder (Hsc2Hs Stage1)
    needTestsuiteBuilders
    timeoutProgBuilder

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

timeoutProgPath :: FilePath
timeoutProgPath = "testsuite/timeout/install-inplace/bin/timeout" <.> exe
