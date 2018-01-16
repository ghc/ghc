-- | Contains an @sdist@ like function which computes the source files
-- that we should track to determine if a rebuild is necessary.
-- Unlike @sdist@, we can operate directly on the true
-- 'PackageDescription' (not flattened).
--
-- The naming convention, roughly, is that to declare we need the
-- source for some type T, you use the function needT; some functions
-- need auxiliary information.
--
-- We can only use this code for non-Custom scripts; Custom scripts
-- may have arbitrary extra dependencies (esp. new preprocessors) which
-- we cannot "see" easily.
module Distribution.Client.SourceFiles (needElaboratedConfiguredPackage) where

import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.RebuildMonad

import Distribution.Solver.Types.OptionalStanza

import Distribution.Simple.PreProcess

import Distribution.Types.PackageDescription
import Distribution.Types.Component
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.Library
import Distribution.Types.Executable
import Distribution.Types.Benchmark
import Distribution.Types.BenchmarkInterface
import Distribution.Types.TestSuite
import Distribution.Types.TestSuiteInterface
import Distribution.Types.BuildInfo
import Distribution.Types.ForeignLib

import Distribution.ModuleName

import Prelude ()
import Distribution.Client.Compat.Prelude

import System.FilePath
import Control.Monad
import qualified Data.Set as Set

needElaboratedConfiguredPackage :: ElaboratedConfiguredPackage -> Rebuild ()
needElaboratedConfiguredPackage elab =
    case elabPkgOrComp elab of
        ElabComponent ecomp -> needElaboratedComponent elab ecomp
        ElabPackage   epkg  -> needElaboratedPackage   elab epkg

needElaboratedPackage :: ElaboratedConfiguredPackage -> ElaboratedPackage -> Rebuild ()
needElaboratedPackage elab epkg =
    mapM_ (needComponent pkg_descr) (enabledComponents pkg_descr enabled)
  where
    pkg_descr = elabPkgDescription elab
    enabled_stanzas = pkgStanzasEnabled epkg
    -- TODO: turn this into a helper function somewhere
    enabled =
        ComponentRequestedSpec {
            testsRequested      = TestStanzas  `Set.member` enabled_stanzas,
            benchmarksRequested = BenchStanzas `Set.member` enabled_stanzas
        }

needElaboratedComponent :: ElaboratedConfiguredPackage -> ElaboratedComponent -> Rebuild ()
needElaboratedComponent elab ecomp =
    case mb_comp of
        Nothing   -> needSetup
        Just comp -> needComponent pkg_descr comp
  where
    pkg_descr = elabPkgDescription elab
    mb_comp   = fmap (getComponent pkg_descr) (compComponentName ecomp)

needComponent :: PackageDescription -> Component -> Rebuild ()
needComponent pkg_descr comp =
    case comp of
        CLib lib     -> needLibrary    pkg_descr lib
        CFLib flib   -> needForeignLib pkg_descr flib
        CExe exe     -> needExecutable pkg_descr exe
        CTest test   -> needTestSuite  pkg_descr test
        CBench bench -> needBenchmark  pkg_descr bench

needSetup :: Rebuild ()
needSetup = findFirstFileMonitored id ["Setup.hs", "Setup.lhs"] >> return ()

needLibrary :: PackageDescription -> Library -> Rebuild ()
needLibrary pkg_descr (Library { exposedModules = modules
                               , signatures     = sigs
                               , libBuildInfo   = bi })
  = needBuildInfo pkg_descr bi (modules ++ sigs)

needForeignLib :: PackageDescription -> ForeignLib -> Rebuild ()
needForeignLib pkg_descr (ForeignLib { foreignLibModDefFile = fs
                                     , foreignLibBuildInfo = bi })
  = do mapM_ needIfExists fs
       needBuildInfo pkg_descr bi []

needExecutable :: PackageDescription -> Executable -> Rebuild ()
needExecutable pkg_descr (Executable { modulePath = mainPath
                                     , buildInfo  = bi })
  = do needBuildInfo pkg_descr bi []
       needMainFile  bi mainPath

needTestSuite :: PackageDescription -> TestSuite -> Rebuild ()
needTestSuite pkg_descr t
  = case testInterface t of
      TestSuiteExeV10 _ mainPath -> do
        needBuildInfo pkg_descr bi []
        needMainFile  bi mainPath
      TestSuiteLibV09 _ m ->
        needBuildInfo pkg_descr bi [m]
      TestSuiteUnsupported _ -> return () -- soft fail
 where
  bi = testBuildInfo t

needMainFile :: BuildInfo -> FilePath -> Rebuild ()
needMainFile bi mainPath = do
    -- The matter here is subtle.  It might *seem* that we
    -- should just search for mainPath, but as per
    -- b61cb051f63ed5869b8f4a6af996ff7e833e4b39 'main-is'
    -- will actually be the source file AFTER preprocessing,
    -- whereas we need to get the file *prior* to preprocessing.
    ppFile <- findFileWithExtensionMonitored
                (ppSuffixes knownSuffixHandlers)
                (hsSourceDirs bi)
                (dropExtension mainPath)
    case ppFile of
        -- But check the original path in the end, because
        -- maybe it's a non-preprocessed file with a non-traditional
        -- extension.
        Nothing -> findFileMonitored (hsSourceDirs bi) mainPath
                    >>= maybe (return ()) need
        Just pp -> need pp

needBenchmark :: PackageDescription -> Benchmark -> Rebuild ()
needBenchmark pkg_descr bm
  = case benchmarkInterface bm of
     BenchmarkExeV10 _ mainPath -> do
       needBuildInfo pkg_descr bi []
       needMainFile  bi mainPath
     BenchmarkUnsupported _ -> return () -- soft fail
 where
  bi = benchmarkBuildInfo bm

needBuildInfo :: PackageDescription -> BuildInfo -> [ModuleName] -> Rebuild ()
needBuildInfo pkg_descr bi modules = do
    -- NB: These are separate because there may be both A.hs and
    -- A.hs-boot; need to track both.
    findNeededModules ["hs", "lhs", "hsig", "lhsig"]
    findNeededModules ["hs-boot", "lhs-boot"]
    mapM_ needIfExists (cSources bi ++ jsSources bi)
    -- A MASSIVE HACK to (1) make sure we rebuild when header
    -- files change, but (2) not have to rebuild when anything
    -- in extra-src-files changes (most of these won't affect
    -- compilation).  It would be even better if we knew on a
    -- per-component basis which headers would be used but that
    -- seems to be too difficult.
    mapM_ needIfExists (filter ((==".h").takeExtension) (extraSrcFiles pkg_descr))
    forM_ (installIncludes bi) $ \f ->
        findFileMonitored ("." : includeDirs bi) f
            >>= maybe (return ()) need
  where
    findNeededModules exts =
        mapM_ (findNeededModule exts)
              (modules ++ otherModules bi)
    findNeededModule exts m =
        findFileWithExtensionMonitored
            (ppSuffixes knownSuffixHandlers ++ exts)
            (hsSourceDirs bi)
            (toFilePath m)
          >>= maybe (return ()) need
