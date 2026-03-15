module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import GHC
import GHC.Driver.DynFlags
import GHC.Driver.Env (hsc_dflags)
import GHC.Settings
import System.Directory
import System.Environment
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import Unsafe.Coerce (unsafeCoerce)

-- Verify that a LibDir setting in the settings file is respected:
--   1. fileSettings_libDir and fileSettings_globalPackageDatabase reflect the
--      configured LibDir path (not topDir)
--   2. GHC can still compile with a LibDir that differs from topDir
--   3. --print-libdir and --print-global-package-db output the correct paths
--
-- We create a symlink to the real lib dir so that the package DB remains
-- findable, but use a separate topDir so that topDir ≠ libDir, proving
-- the LibDir setting is actually used.
--
-- Tested for both relative and absolute LibDir values.
main :: IO ()
main = do
  libdir : ghcBin : _ <- getArgs

  (rawSettingOpts, rawTargetOpts, realLibDir) <- runGhc (Just libdir) $ do
    dflags <- hsc_dflags <$> getSession
    pure (rawSettings dflags, rawTarget dflags, fileSettings_libDir (fileSettings dflags))

  tmpDir <- getTemporaryDirectory
  let topDir = tmpDir </> "T19174_top"
      symlinkLib = tmpDir </> "T19174_lib"
  -- Remove stale dirs from prior runs; createDirectoryLink fails if path exists.
  removePathForcibly topDir
  removePathForcibly symlinkLib
  createDirectoryIfMissing True (topDir </> "targets")
  createDirectoryLink realLibDir symlinkLib

  let testWithLibDir libDirValue = do
        writeTopDirFiles topDir rawSettingOpts rawTargetOpts libDirValue
        runGhc (Just topDir) $ do
          assertSettings topDir symlinkLib
          compileAndRunTestExpr
        assertGhcFlags ghcBin topDir symlinkLib

  testWithLibDir (".." </> takeFileName symlinkLib)
  testWithLibDir symlinkLib

  putStrLn "OK"

writeTopDirFiles ::
  (Show a) =>
  FilePath ->
  [(String, String)] ->
  a ->
  String ->
  IO ()
writeTopDirFiles topDir rawSettingOpts rawTargetOpts libDirValue = do
  let settings = filter ((/= "LibDir") . fst) rawSettingOpts ++ [("LibDir", libDirValue)]
  writeFile (topDir </> "settings") $
    "[" ++ intercalate "\n," (map show settings) ++ "]"
  writeFile (topDir </> "targets" </> "default.target") $
    show rawTargetOpts

assertSettings :: FilePath -> FilePath -> Ghc ()
assertSettings topDir expectedLib = do
  dflags <- hsc_dflags <$> getSession
  let fs = fileSettings dflags
      actualLib = fileSettings_libDir fs
      actualPkgDb = fileSettings_globalPackageDatabase fs
  normActualLib <- liftIO $ canonicalizePath actualLib
  normExpected <- liftIO $ canonicalizePath expectedLib
  normTopDir <- liftIO $ canonicalizePath topDir
  normActualPkgDb <- liftIO $ canonicalizePath actualPkgDb
  normExpectedPkgDb <- liftIO $ canonicalizePath (expectedLib </> "package.conf.d")
  liftIO $ do
    when (normActualLib /= normExpected) $
      die
        [ "FAIL: libDir should be " ++ normExpected,
          "             got       " ++ normActualLib
        ]
    when (normActualLib == normTopDir) $
      die ["FAIL: libDir equals topDir — LibDir setting was ignored"]
    when (normActualPkgDb /= normExpectedPkgDb) $
      die
        [ "FAIL: globalPackageDB should be " ++ normExpectedPkgDb,
          "                      got       " ++ normActualPkgDb
        ]

assertGhcFlags :: FilePath -> FilePath -> FilePath -> IO ()
assertGhcFlags ghcBin topDir expectedLib = do
  normExpectedLib <- canonicalizePath expectedLib
  normExpectedPkgDb <- canonicalizePath (expectedLib </> "package.conf.d")

  printedLibDir <- trim <$> readProcess ghcBin ["-B" ++ topDir, "--print-libdir"] ""
  normPrintedLib <- canonicalizePath printedLibDir
  when (normPrintedLib /= normExpectedLib) $
    die
      [ "FAIL: --print-libdir should be " ++ normExpectedLib,
        "                     got       " ++ normPrintedLib
      ]

  printedPkgDb <- trim <$> readProcess ghcBin ["-B" ++ topDir, "--print-global-package-db"] ""
  normPrintedPkgDb <- canonicalizePath printedPkgDb
  when (normPrintedPkgDb /= normExpectedPkgDb) $
    die
      [ "FAIL: --print-global-package-db should be " ++ normExpectedPkgDb,
        "                                 got       " ++ normPrintedPkgDb
      ]

compileAndRunTestExpr :: Ghc ()
compileAndRunTestExpr = do
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags dflags
  setContext [IIDecl (simpleImportDecl (mkModuleName "Prelude"))]
  result <- compileExpr "length [1,2,3 :: Int]"
  liftIO $ print (unsafeCoerce result :: Int)

trim :: String -> String
trim = reverse . dropWhile (== '\n') . reverse

die :: [String] -> IO ()
die msgs = mapM_ (hPutStrLn stderr) msgs >> exitWith (ExitFailure 1)
