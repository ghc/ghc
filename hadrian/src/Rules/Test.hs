{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Rules.Test (testRules) where

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
import qualified System.Directory as IO

import GHC.Toolchain as Toolchain
import GHC.Toolchain.Program as Toolchain
import Hadrian.Oracles.Path
import Hadrian.Oracles.TextFile (getHostTarget, getTargetTarget)
import GHC.Platform.ArchOS (ArchOS (..), Arch (..), OS (..))

checkPprProgPath :: ArchOS -> FilePath
checkPprProgPath archos = "test/bin/check-ppr" <.> exe archos
checkPprSourcePath :: FilePath
checkPprSourcePath = "utils/check-ppr/Main.hs"
checkPprExtra :: [String]
checkPprExtra = []

checkExactProgPath :: ArchOS -> FilePath
checkExactProgPath archos = "test/bin/check-exact" <.> exe archos
checkExactSourcePath :: FilePath
checkExactSourcePath = "utils/check-exact/Main.hs"
checkExactExtra :: [String]
checkExactExtra = ["-iutils/check-exact"]

countDepsProgPath :: ArchOS -> FilePath
countDepsProgPath archos = "test/bin/count-deps" <.> exe archos
countDepsSourcePath :: FilePath
countDepsSourcePath = "utils/count-deps/Main.hs"
countDepsExtra :: [String]
countDepsExtra = ["-iutils/count-deps"]

dumpDeclsProgPath :: ArchOS -> FilePath
dumpDeclsProgPath archos = "test/bin/dump-decls" <.> exe archos
dumpDeclsSourcePath :: FilePath
dumpDeclsSourcePath = "utils/dump-decls/Main.hs"
dumpDeclsExtra :: [String]
dumpDeclsExtra = []

noteLinterProgPath :: ArchOS -> FilePath
noteLinterProgPath archos = "test/bin/lint-notes" <.> exe archos
noteLinterSourcePath :: FilePath
noteLinterSourcePath = "linters/lint-notes/Main.hs"
noteLinterExtra :: [String]
noteLinterExtra = ["-ilinters/lint-notes"]

codeLinterProgPath :: ArchOS -> FilePath
codeLinterProgPath archos = "test/bin/lint-codes" <.> exe archos
codeLinterSourcePath :: FilePath
codeLinterSourcePath = "linters/lint-codes/Main.hs"
codeLinterExtra :: [String]
codeLinterExtra = ["-ilinters/lint-codes"]

whitespaceLinterProgPath :: ArchOS -> FilePath
whitespaceLinterProgPath archos = "test/bin/lint-whitespace" <.> exe archos
whitespaceLinterSourcePath :: FilePath
whitespaceLinterSourcePath = "linters/lint-whitespace/Main.hs"
whitespaceLinterExtra :: [String]
whitespaceLinterExtra = ["-ilinters/lint-whitespace", "-ilinters/linters-common"]

changelogDProgPath :: ArchOS -> FilePath
changelogDProgPath archos = "test/bin/changelog-d" <.> exe archos
changelogDSourcePath :: FilePath
changelogDSourcePath = "utils/changelog-d/ChangelogD.hs"
changelogDExtra :: [String]
changelogDExtra = ["-iutils/changelog-d"]

data CheckProgram =
        CheckProgram { cp_target :: String -- ^ Name for the hadrian target
                     , cp_exe_path :: ArchOS -> FilePattern
                        -- ^ Path to resulting executable.
                        -- This depends on the taret the program is built for,
                        -- not the host that hadrian is built on
                     , cp_src_path :: FilePath -- ^ Source to the Main.hs for the executable
                     , cp_extra_args :: [String] -- ^ Any extra arguments to use when compiling Main.hs
                     , cp_hadrian_pkg :: Package -- ^ How to build the executable when using in-tree compiler.
                     , cp_modify_stage :: Stage -> Stage -- ^ Which stage GHC to build the executable with.
                     , cp_modify_deps  :: [Package] -> [Package] -- ^ How to modify the package dependencies, only used for the linter to remove the dependency on lintersCommon.
                     }

checkPrograms :: [CheckProgram]
checkPrograms =
    [ CheckProgram { cp_target = "test:check-ppr", cp_exe_path = checkPprProgPath, cp_src_path = checkPprSourcePath, cp_extra_args = checkPprExtra, cp_hadrian_pkg = checkPpr, cp_modify_stage = id, cp_modify_deps = id }
    , CheckProgram { cp_target = "test:check-exact", cp_exe_path = checkExactProgPath, cp_src_path = checkExactSourcePath, cp_extra_args = checkExactExtra, cp_hadrian_pkg = checkExact, cp_modify_stage = id, cp_modify_deps = id }
    , CheckProgram { cp_target = "test:count-deps", cp_exe_path = countDepsProgPath, cp_src_path = countDepsSourcePath, cp_extra_args = countDepsExtra, cp_hadrian_pkg = countDeps, cp_modify_stage = id, cp_modify_deps = id }
    , CheckProgram { cp_target = "test:dump-decls", cp_exe_path = dumpDeclsProgPath, cp_src_path = dumpDeclsSourcePath, cp_extra_args = dumpDeclsExtra, cp_hadrian_pkg = dumpDecls, cp_modify_stage = id, cp_modify_deps = id }
    , CheckProgram { cp_target = "lint:notes", cp_exe_path = noteLinterProgPath, cp_src_path = noteLinterSourcePath, cp_extra_args = noteLinterExtra, cp_hadrian_pkg = lintNotes, cp_modify_stage = (const stage0Boot), cp_modify_deps = id }
    , CheckProgram { cp_target = "lint:codes", cp_exe_path = codeLinterProgPath, cp_src_path = codeLinterSourcePath, cp_extra_args = codeLinterExtra, cp_hadrian_pkg = lintCodes, cp_modify_stage = id, cp_modify_deps = id }
    , CheckProgram { cp_target = "lint:whitespace", cp_exe_path = whitespaceLinterProgPath, cp_src_path = whitespaceLinterSourcePath, cp_extra_args = whitespaceLinterExtra, cp_hadrian_pkg = lintWhitespace, cp_modify_stage = (const stage0Boot), cp_modify_deps = (filter (/= lintersCommon)) }
    -- N.B. The lint:changelog build is replicated by lint_changelog in
    -- .gitlab/ci.sh. Keep its package dependencies in sync with this target.
    , CheckProgram { cp_target = "lint:changelog", cp_exe_path = changelogDProgPath, cp_src_path = changelogDSourcePath, cp_extra_args = changelogDExtra, cp_hadrian_pkg = changelogD, cp_modify_stage = (const stage0Boot), cp_modify_deps = id }
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
                      -- For cross builds, the test compiler (Stage1 binary) uses
                      -- target libraries from Stage2; build those too.
                      cross <- flag CrossCompiling
                      when (cross && stg == Stage1) $
                        needTestsuiteLibs Stage2
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

    -- the test targets will all be compiled by the test comppiler which is going
    -- to produce artifacts for its target

    -- Rules for building check-ppr, check-exact and
    -- check-ppr-annotations with the compiler we are going to test
    -- (in-tree or out-of-tree).
    forM_ checkPrograms $ \(CheckProgram name progPathForArch sourcePath mextra progPkg mod_stage mod_pkgs) -> do
        name ~> do
          tt <- getTargetTarget
          need [root -/- progPathForArch (tgtArchOs tt)]

        -- HACK: we don't havea program path here and just a pattern
        -- because we can't query the target before declaring the rule.
        -- we specify target arch linux (no extension), then append a pattern
        -- for any extension
        let filePat = progPathForArch ArchOS { archOS_arch = ArchUnknown, archOS_OS = OSLinux } <> "*"
        root -/- filePat %> \path -> do
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
                ht <- getHostTarget
                tt <- getTargetTarget
                targetPlatform <- setting TargetPlatformFull
                let mkGhcProg prog
                     | targetPlatformTriple ht /= targetPlatformTriple tt = targetPlatform ++ "-" ++ prog
                     | otherwise = prog
                let dynPrograms = hasDynamic test_args
                cmd [bindir </> mkGhcProg "ghc" <.> exe (tgtArchOs ht)] $
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
        -- For cross builds, the test compiler (Stage1 binary) uses
        -- target libraries from Stage2; build those too.
        when (cross && stg == Stage1) $
          needTestsuiteLibs Stage2

        -- Set environment variables for test's Makefile.
        env <- testEnv stg

        -- Execute the test target.
        -- We override the verbosity setting to make sure the user can see
        -- the test output: https://gitlab.haskell.org/ghc/ghc/issues/15951.
        withVerbosity Diagnostic $ buildWithCmdOptions [AddEnv k v | (k,v) <- env] $ test_target RunTest

testEnv :: Stage -> Action [(String, String)]
testEnv stg = do

    testGhc <- testCompiler <$> userSetting defaultTestArgs

    cross <- getTestCross testGhc

    prog_ghc_pkg     <- getTestExePath testGhc ghcPkg
    prog_hsc2hs      <- getTestExePath testGhc hsc2hs
    prog_hp2ps       <- getTestExePath testGhc hp2ps
    prog_haddock     <- getTestExePath testGhc haddock
    prog_hpc         <- getTestExePath testGhc hpc
    prog_runghc      <- getTestExePath testGhc runGhc
    makePath        <- builderPath $ Make ""

    root <- buildRoot
    args <- userSetting defaultTestArgs
    let testCompilerArg = testCompiler args
    ghcPath <- getCompilerPath testCompilerArg

    top             <- topDirectory
    pythonPath      <- builderPath Python
    -- MP: TODO wrong, should use the ccPath and ccFlags from the bindist we are testing.
    tgt <- queryPerStageTargetSpec stg id
    let ccPath = tgt.tgtCCompiler.ccProgram.prgPath
    let ccFlags = unwords tgt.tgtCCompiler.ccProgram.prgFlags
    let archos = tgt.tgtArchOs
    let mkProgPath k = top -/- root -/- k archos

    ghcFlags        <- runTestGhcFlags stg
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
      , "CHECK_PPR" .= mkProgPath checkPprProgPath
      , "CHECK_EXACT" .= mkProgPath checkExactProgPath
      , "DUMP_DECLS" .= mkProgPath dumpDeclsProgPath
      , "COUNT_DEPS" .= mkProgPath countDepsProgPath
      , "LINT_NOTES" .= mkProgPath noteLinterProgPath
      , "LINT_CODES" .= mkProgPath codeLinterProgPath
      , "LINT_WHITESPACE" .= mkProgPath whitespaceLinterProgPath
      , "CHANGELOG_D" .= mkProgPath changelogDProgPath
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
            writeFileAtomic (root -/- timeoutPath) script
            makeExecutable (root -/- timeoutPath)

-- | Build extra programs and libraries required by testsuite
needTestsuitePackages :: Stage -> Action ()
needTestsuitePackages stg = do
  allpkgs <- packages <$> flavour
  libpkgs <- filter isLibrary <$> allpkgs stg
  -- And the executables of the current stage
  exepkgs <- filter isProgram <$> allpkgs stg
  -- Don't require lib:ghc or lib:cabal when testing the stage1 compiler
  -- This is a hack, but a major usecase for testing the stage1 compiler is
  -- so that we can use it even if ghc stage2 fails to build
  -- Unfortunately, we still need the liba
  let pkgs = filter (\p -> not $ (pkgName p `elem` ["ghc", "Cabal"]) && isStage0 stg)
                    (libpkgs ++ exepkgs ++ [ timeout | windowsHost ])
  need =<< mapM (pkgFile stg) pkgs

-- | Build only the libraries for the given stage (no executables).
-- Used for cross Stage2 target libraries, which cannot run on the host.
needTestsuiteLibs :: Stage -> Action ()
needTestsuiteLibs stg = do
  allpkgs <- packages <$> flavour
  libpkgs <- filter isLibrary <$> allpkgs stg
  need =<< mapM (pkgFile stg) libpkgs

-- stage 1 ghc lives under stage0/bin,
-- stage 2 ghc lives under stage1/bin, etc
stageOf :: String -> Maybe Stage
stageOf "stage1" = Just stage0InTree
stageOf "stage2" = Just Stage1
stageOf "stage3" = Just Stage2
stageOf _ = Nothing

pkgFile :: Stage -> Package -> Action FilePath
pkgFile stage pkg
    | isLibrary pkg = pkgConfFile (Context stage pkg profilingDynamic Final)
    | otherwise     = programPath =<< programContext stage pkg
