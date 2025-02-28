{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Rules.Test (testRules) where

import Base
import CommandLine
import Expression
import Flavour
import Hadrian.Haskell.Cabal.Type (packageDependencies)
import Hadrian.Oracles.Cabal (readPackageData)
import Hadrian.Oracles.Path (fixAbsolutePathOnWindows)
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

import GHC.Toolchain as Toolchain
import GHC.Toolchain.Program as Toolchain

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

dumpDeclsProgPath, dumpDeclsSourcePath :: FilePath
dumpDeclsProgPath = "test/bin/dump-decls" <.> exe
dumpDeclsSourcePath = "utils/dump-decls/Main.hs"
dumpDeclsExtra :: [String]
dumpDeclsExtra = []

noteLinterProgPath, noteLinterSourcePath :: FilePath
noteLinterProgPath = "test/bin/lint-notes" <.> exe
noteLinterSourcePath = "linters/lint-notes/Main.hs"
noteLinterExtra :: [String]
noteLinterExtra = ["-ilinters/lint-notes"]

codeLinterProgPath, codeLinterSourcePath :: FilePath
codeLinterProgPath = "test/bin/lint-codes" <.> exe
codeLinterSourcePath = "linters/lint-codes/Main.hs"
codeLinterExtra :: [String]
codeLinterExtra = ["-ilinters/lint-codes"]

whitespaceLinterProgPath, whitespaceLinterSourcePath :: FilePath
whitespaceLinterProgPath = "test/bin/lint-whitespace" <.> exe
whitespaceLinterSourcePath = "linters/lint-whitespace/Main.hs"
whitespaceLinterExtra :: [String]
whitespaceLinterExtra = ["-ilinters/lint-whitespace", "-ilinters/linters-common"]

data CheckProgram =
        CheckProgram { cp_target :: String -- ^ Name for the hadrian target
                     , cp_exe_path :: FilePath -- ^ Path to resulting executable
                     , cp_src_path :: FilePath -- ^ Source to the Main.hs for the executable
                     , cp_extra_args :: [String] -- ^ Any extra arguments to use when compiling Main.hs
                     , cp_hadrian_pkg :: Package -- ^ How to build the executable when using in-tree compiler.
                     , cp_modify_stage :: Stage -> Stage -- ^ Which stage GHC to build the executable with.
                     , cp_modify_deps  :: [Package] -> [Package] -- ^ How to modify the package dependencies, only used for the linter to remove the dependency on lintersCommon.
                     }

checkPrograms :: [CheckProgram]
checkPrograms =
    [ CheckProgram "test:check-ppr" checkPprProgPath checkPprSourcePath checkPprExtra checkPpr id id
    , CheckProgram "test:check-exact" checkExactProgPath checkExactSourcePath checkExactExtra checkExact id id
    , CheckProgram "test:count-deps" countDepsProgPath countDepsSourcePath countDepsExtra countDeps id id
    , CheckProgram "test:dump-decls" dumpDeclsProgPath dumpDeclsSourcePath dumpDeclsExtra dumpDecls id id
    , CheckProgram "lint:notes" noteLinterProgPath  noteLinterSourcePath  noteLinterExtra  lintNotes  (const stage0Boot)  id
    , CheckProgram "lint:codes" codeLinterProgPath  codeLinterSourcePath  codeLinterExtra  lintCodes  id id
    , CheckProgram "lint:whitespace"  whitespaceLinterProgPath  whitespaceLinterSourcePath  whitespaceLinterExtra  lintWhitespace  (const stage0Boot)  (filter (/= lintersCommon))
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
  "test:all_deps" ~> do
    need ("test:ghc" : map cp_target checkPrograms)

  "test:ghc" ~> inTreeOutTree
                    (\stg -> do
                      needTestsuitePackages stg
                      need [(root -/- ghcConfigPath)]
                      -- This is here because it's the one place we know that GHC is
                      -- up-to-date. Later when we compute the in/out tree arguments
                      -- we can't be sure whether checking this assertion will trigger
                      -- a rebuild.
                      assertSameCompilerArgs stg)

                    (return ())

ghcConfigPath :: FilePath
ghcConfigPath = "test/ghcconfig"

-- TODO: clean up after testing
testRules :: Rules ()
testRules = do
    root <- buildRootRules

    testsuiteDeps

    -- Rules for building check-ppr, check-exact and
    -- check-ppr-annotations with the compiler we are going to test
    -- (in-tree or out-of-tree).
    forM_ checkPrograms $ \(CheckProgram name progPath sourcePath mextra progPkg mod_stage mod_pkgs) -> do
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
                test_args <- outOfTreeCompilerArgs
                let dynPrograms = hasDynamic test_args
                cmd [bindir </> "ghc" <.> exe] $
                    concatMap (\p -> ["-package", pkgName p]) depsPkgs ++
                    ["-o", top -/- path, top -/- sourcePath] ++
                    mextra ++
                    -- If GHC is build dynamic, then build check-ppr also dynamic.
                    (if dynPrograms then ["-dynamic"] else [])

    root -/- ghcConfigPath %> \_ -> do
        alwaysRerun
        args <- userSetting defaultTestArgs
        let testGhc = testCompiler args
        ghcPath <- getCompilerPath testGhc
        whenJust (stageOf testGhc) $ \stg ->
          need . (:[]) =<< programPath (Context stg ghc vanilla Final)
        ghcConfigProgPath <- programPath =<< programContext stage0InTree ghcConfig
        cwd <- liftIO $ IO.getCurrentDirectory
        need [makeRelative cwd ghcPath, ghcConfigProgPath]
        cmd [FileStdout $ root -/- ghcConfigPath] ghcConfigProgPath [ghcPath]

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
        let ok_to_build = filter (isOkToBuild args) extra_targets
        putVerbose $ " | ExtraTargets: " ++ intercalate ", " extra_targets
        putVerbose $ " | ExtraTargets (ok-to-build): " ++ intercalate ", " ok_to_build
        need $ ok_to_build ++ [root -/- timeoutPath]

        -- force stage0 program building for cross
        cross <- flag CrossCompiling
        when cross $ mapM (relativePathStage (Stage0 InTreeLibs)) [hpc, haddock, runGhc] >>= need

        -- Set environment variables for test's Makefile.
        env <- testEnv

        -- Execute the test target.
        -- We override the verbosity setting to make sure the user can see
        -- the test output: https://gitlab.haskell.org/ghc/ghc/issues/15951.
        withVerbosity Diagnostic $ buildWithCmdOptions [AddEnv k v | (k,v) <- env] $ test_target RunTest

testEnv :: Action [(String, String)]
testEnv = do
    cross           <- flag CrossCompiling
    makePath        <- builderPath $ Make ""
    prog_ghc_pkg    <- absolutePathStage Stage1 ghcPkg
    prog_hsc2hs     <- absolutePathStage Stage1 hsc2hs
    prog_hp2ps      <- absolutePathStage Stage1 hp2ps
    prog_haddock    <- absolutePathStage (Stage0 InTreeLibs) haddock
    prog_hpc        <- absolutePathStage (Stage0 InTreeLibs) hpc
    prog_runghc     <- absolutePathStage (Stage0 InTreeLibs) runGhc

    root <- buildRoot
    args <- userSetting defaultTestArgs
    let testCompilerArg = testCompiler args
    ghcPath <- getCompilerPath testCompilerArg

    top             <- topDirectory
    pythonPath      <- builderPath Python
    ccPath          <- queryTargetTarget (Toolchain.prgPath . Toolchain.ccProgram . Toolchain.tgtCCompiler)
    ccFlags         <- queryTargetTarget (unwords . Toolchain.prgFlags . Toolchain.ccProgram . Toolchain.tgtCCompiler)
    ghcFlags        <- runTestGhcFlags
    let ghciFlags = ghcFlags ++ unwords
          [ "--interactive", "-v0", "-ignore-dot-ghci"
          , "-fno-ghci-history", "-fprint-error-index-links=never"
          ]

    -- Many of those env vars are used by Makefiles in the
    -- test infrastructure, or from tests or their
    -- Makefiles.
    return $
      [ "MAKE" .= makePath
      , "PYTHON" .= pythonPath
      , "TEST_HC" .= ghcPath
      , "TEST_HC_OPTS" .= ghcFlags
      , "TEST_HC_OPTS_INTERACTIVE" .= ghciFlags
      , "TEST_CC" .= ccPath
      , "TEST_CC_OPTS" .= ccFlags
      , "CHECK_PPR" .= (top -/- root -/- checkPprProgPath)
      , "CHECK_EXACT" .= (top -/- root -/- checkExactProgPath)
      , "DUMP_DECLS" .= (top -/- root -/- dumpDeclsProgPath)
      , "COUNT_DEPS" .= (top -/- root -/- countDepsProgPath)
      , "LINT_NOTES" .= (top -/- root -/- noteLinterProgPath)
      , "LINT_CODES" .= (top -/- root -/- codeLinterProgPath)
      , "LINT_WHITESPACE" .= (top -/- root -/- whitespaceLinterProgPath)
      -- This lets us bypass the need to generate a config
      -- through Make, which happens in testsuite/mk/boilerplate.mk
      -- which is in turn included by all test 'Makefile's.
      , "ghc_config_mk" .= (top -/- root -/- ghcConfigPath)
      ] ++
      if_ cross
      [ "GHC_PKG"   .= prog_ghc_pkg
      , "HSC2HS"    .= prog_hsc2hs
      , "HP2PS_ABS" .= prog_hp2ps
      , "HPC"       .= prog_hpc
      , "HADDOCK"   .= prog_haddock
      , "RUNGHC"    .= prog_runghc
      ]
  where
    if_ :: Bool -> [a] -> [a]
    if_ True xs = xs
    if_ False _ = []

    (.=) = (,)

needProgramStage :: Stage -> Package -> Action ()
needProgramStage s p = relativePathStage s p >>= need . (:[])

-- | Get relative path for the given program in the given stage.
relativePathStage :: Stage -> Package -> Action FilePath
relativePathStage s p = programPath =<< programContext s p

absolutePathStage :: Stage -> Package -> Action FilePath
absolutePathStage s p =
    relativePathStage s p >>= make_absolute
  where
    make_absolute rel_path = do
      abs_path <- liftIO (makeAbsolute rel_path)
      fixAbsolutePathOnWindows abs_path

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
   = isInTreeCompiler (testCompiler args)
  || testHasInTreeFiles args
  || target `elem` map cp_target checkPrograms

-- | Build the timeout program.
-- See: https://github.com/ghc/ghc/blob/master/testsuite/timeout/Makefile#L23

timeoutProgBuilder :: Action ()
timeoutProgBuilder = do
    root    <- buildRoot
    if windowsHost
        then do
            prog <- programPath =<< programContext stage0InTree timeout
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
  libpkgs <- map (Stage1,) . filter isLibrary <$> allpkgs (succStage stg)
  -- And the executables of the current stage
  exepkgs <- map (stg,) . filter isProgram <$> allpkgs stg
  -- Don't require lib:ghc or lib:cabal when testing the stage1 compiler
  -- This is a hack, but a major usecase for testing the stage1 compiler is
  -- so that we can use it even if ghc stage2 fails to build
  -- Unfortunately, we still need the liba
  let pkgs = filter (\(_,p) -> not $ "iserv" `isInfixOf` pkgName p || ((pkgName p `elem` ["ghc", "Cabal"]) && isStage0 stg))
                    (libpkgs ++ exepkgs ++ [ (stg,timeout) | windowsHost ])
  need =<< mapM (uncurry pkgFile) pkgs
  cross <- flag CrossCompiling
  when (not cross) $ needIservBins stg

-- stage 1 ghc lives under stage0/bin,
-- stage 2 ghc lives under stage1/bin, etc
stageOf :: String -> Maybe Stage
stageOf "stage1" = Just stage0InTree
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
    canBuild (Stage0 {}) _ = pure Nothing
    canBuild stg w = do
      contextDeps <- contextDependencies (Context stg iserv w Final)
      ws <- forM contextDeps $ \c ->
              interpretInContext c (getLibraryWays <>
                                    if Context.Type.package c == rts
                                      then getRtsWays
                                      else mempty)
      if (all (w `elem`) ws)
        then Just <$> programPath (Context stg iserv w Final)
        else return Nothing


pkgFile :: Stage -> Package -> Action FilePath
pkgFile stage pkg
    | isLibrary pkg = pkgConfFile (Context stage pkg profilingDynamic Final)
    | otherwise     = programPath =<< programContext stage pkg
