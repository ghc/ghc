{-# LANGUAGE RecordWildCards #-}

module Test.Haddock
  ( module Test.Haddock.Config
  , runAndCheck
  , runHaddock
  , checkFiles
  ) where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.Foldable (for_)
import Data.Maybe
import GHC.ResponseFile
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import Test.Haddock.Config
import Test.Haddock.Process
import Test.Haddock.Utils

data CheckResult
  = Fail
  | Pass
  | NoRef
  | Error String
  | Accepted
  deriving (Eq)

runAndCheck :: Config c -> IO ()
runAndCheck cfg = do
  crashed <- runHaddock cfg
  checkFiles cfg crashed

checkFiles :: Config c -> Bool -> IO ()
checkFiles cfg@(Config{..}) somethingCrashed = do
  putStrLn "Testing output files..."

  createDirectoryIfMissing True (cfgOutDir cfg)
  files <- ignore <$> getDirectoryTree (cfgOutDir cfg)
  failed <- liftM catMaybes . forM files $ \file -> do
    putStr $ "Checking \"" ++ file ++ "\"... "

    status <- maybeAcceptFile cfg file =<< checkFile cfg file
    case status of
      Fail -> putStrLn "FAIL" >> (return $ Just file)
      Pass -> putStrLn "PASS" >> (return Nothing)
      NoRef -> putStrLn "PASS [no .ref]" >> (return Nothing)
      Error msg -> putStrLn ("ERROR (" ++ msg ++ ")") >> return Nothing
      Accepted -> putStrLn "ACCEPTED" >> return Nothing

  if (null failed && not somethingCrashed)
    then do
      putStrLn "All tests passed!"
      exitSuccess
    else do
      unless (null failed) $ maybeDiff cfg failed
      when somethingCrashed $ putStrLn "Some tests crashed."
      exitFailure
  where
    ignore = filter (not . dcfgCheckIgnore cfgDirConfig)

maybeDiff :: Config c -> [FilePath] -> IO ()
maybeDiff (Config{cfgDiffTool = Nothing}) _ = pure ()
maybeDiff cfg@(Config{cfgDiffTool = (Just diff)}) files = do
  putStrLn "Diffing failed cases..."
  forM_ files $ diffFile cfg diff

-- | Runs Haddock on all of the test packages, and returns whether 'True' if
-- any of them caused Haddock to crash.
runHaddock :: Config c -> IO Bool
runHaddock cfg@(Config{..}) = do
  createEmptyDirectory $ cfgOutDir cfg
  createEmptyDirectory $ cfgNoCompilationOutDir cfg
  createEmptyDirectory $ cfgOneShotOutDir cfg

  putStrLn "Generating documentation..."
  successes <- forM cfgPackages $ \tpkg -> do
    let pc =
          processConfig
            { pcArgs =
                concat
                  [ cfgHaddockArgs
                  , pure $ "--odir=" ++ outDir cfgDirConfig tpkg
                  , tpkgFiles tpkg
                  ]
            , pcEnv = Just cfgEnv
            }

    let msg = "Failed to run Haddock on test package '" ++ tpkgName tpkg ++ "'"
    succeeded <- waitForSuccess msg stdout =<< runProcess' cfgHaddockPath pc
    unless succeeded $ removeDirectoryRecursive (outDir cfgDirConfig tpkg)

    let noCompilationDir = noCompilationOutDir cfgDirConfig tpkg
        hiDir = noCompilationDir </> "hi"
        hieDir = noCompilationDir </> "hie"

    createEmptyDirectory noCompilationDir
    createEmptyDirectory hiDir
    createEmptyDirectory hieDir

    -- Build .hi files
    let pc =
          processConfig
            { pcArgs =
                concat
                  [
                    [ "--make"
                    , "-haddock"
                    , "-fwrite-interface"
                    , "-fwrite-ide-info"
                    , "-no-keep-o-files"
                    , "-hidir=" ++ hiDir
                    , "-hiedir=" ++ hieDir
                    ]
                  , tpkgFiles tpkg
                  ]
            , pcEnv = Just cfgEnv
            }
    let msg = "Failed to run GHC on test package '" ++ tpkgName tpkg ++ "'"
    _ <- waitForSuccess msg stdout =<< runProcess' cfgGhcPath pc

    -- Generate documentation with no-compilation flag
    let pc =
          processConfig
            { pcArgs =
                concat
                  [ cfgHaddockArgs
                  , [ "--odir=" ++ noCompilationDir
                    , "--optghc=-hidir=" ++ hiDir
                    , "--optghc=-hiedir=" ++ hieDir
                    , "--no-compilation"
                    ]
                  , tpkgFiles tpkg
                  ]
            , pcEnv = Just cfgEnv
            }

    let msg = "Failed to run Haddock in no-compilation mode on test package '" ++ tpkgName tpkg ++ "'"
    succeededNC <- waitForSuccess msg stdout =<< runProcess' cfgHaddockPath pc

    -- Generate documentation incrementally
    if cfgSkipOneShot then pure (succeeded && succeededNC) else do
      let oneShotDir = oneShotOutDir cfgDirConfig tpkg
          responseFile = hiDir </> "response-file"
      createEmptyDirectory oneShotDir
      writeFile responseFile $ escapeArgs
        [ "--odir=" ++ oneShotDir
        , "--optghc=-hidir=" ++ hiDir
        , "--optghc=-hiedir=" ++ hieDir
        ]

      files <- filter ((== ".hi") . takeExtension) <$> listDirectory hiDir
      -- Use the output order of GHC as a simple dependency order
      filesSorted <- Map.elems . Map.fromList <$> traverse (\file -> (\mt -> ((mt,file),file)) <$> getModificationTime (hiDir </> file)) files
      let srcRef = if "--hyperlinked-source" `elem` cfgHaddockArgs then ",src,visible," else ""
          loop [] = pure True
          loop (file : files) = do
            let hiFile = hiDir </> file
                haddockFile = hiFile ++ ".haddock"
                pc =
                  processConfig
                    { pcArgs =
                        concat
                          [ cfgHaddockArgs
                          , [ "@" ++ responseFile
                            , "--incremental=" ++ takeBaseName hiFile
                            , "--dump-interface=" ++ haddockFile
                            ]
                          ]
                    , pcEnv = Just cfgEnv
                    }
            let msg = "Failed to run Haddock in one-shot mode on file '" ++ hiFile ++ "'"
            succeeded <- waitForSuccess msg stdout =<< runProcess' cfgHaddockPath pc
            if succeeded
              -- Allow subsequent files to depend on this file
              then do
                appendFile responseFile $
                  escapeArgs [ "--read-interface=" ++ srcRef ++ haddockFile ]
                loop files
              else pure False
      succeededOS <- loop filesSorted
      when (succeededNC && succeededOS) $ do
        removeDirectoryRecursive hiDir
        removeDirectoryRecursive hieDir
      pure (succeeded && succeededNC && succeededOS)

  let somethingFailed = any not successes
  pure somethingFailed

checkFile :: Config c -> FilePath -> IO CheckResult
checkFile cfg file = do
  mref <- readRef cfg file
  case mref of
    Just ref -> do
      let checkStep dcfgDir = ccfgEqual ccfg ref <$> readOut cfg dcfgDir file
      result <- checkStep dcfgOutDir
      resultNC <- if dcfgCheckIgnoreNoCompilation (cfgDirConfig cfg) file
        then pure True
        else checkStep dcfgNoCompilationOutDir
      resultOS <- if cfgSkipOneShot cfg || dcfgCheckIgnoreOneShot (cfgDirConfig cfg) file
        then pure True
        else checkStep dcfgOneShotOutDir
      pure $ if and [result, resultNC, resultOS] then Pass else Fail
    Nothing -> return NoRef
  where
    ccfg = cfgCheckConfig cfg
    dcfg = cfgDirConfig cfg

-- We use ByteString here to ensure that no lazy I/O is performed.
-- This way to ensure that the reference file isn't held open in
-- case after `diffFile` (which is problematic if we need to rewrite
-- the reference file in `maybeAcceptFile`)

-- | Read the reference artifact for a test
readRef :: Config c -> FilePath -> IO (Maybe c)
readRef cfg file =
  ccfgRead ccfg . BS.unpack
    <$> BS.readFile (refFile dcfg file)
  where
    ccfg = cfgCheckConfig cfg
    dcfg = cfgDirConfig cfg

-- | Read (and clean) the test output artifact for a test
readOut :: Config c -> (DirConfig -> FilePath) -> FilePath -> IO c
readOut cfg dcfgDir file = do
  res <- fmap (ccfgClean ccfg file) . ccfgRead ccfg . BS.unpack
    <$> BS.readFile outFile
  case res of
    Just out -> return out
    Nothing -> error $ "Failed to parse output file: " ++ outFile
  where
    ccfg = cfgCheckConfig cfg
    dcfg = cfgDirConfig cfg
    outFile = dcfgDir dcfg </> file

diffFile :: Config c -> FilePath -> FilePath -> IO ()
diffFile cfg diff file = do
  Just ref <- readRef cfg file
  out <- readOut cfg dcfgOutDir file
  noCompilationOut <- readOut cfg dcfgNoCompilationOutDir file
  oneShotOut <- readOut cfg dcfgOneShotOutDir file
  writeFile (dumpFile "ref") $ ccfgDump ccfg ref
  writeFile (dumpFile "out") $ ccfgDump ccfg out
  writeFile (dumpFile "oneShot") $ ccfgDump ccfg oneShotOut
  writeFile (dumpFile "noCompilation") $ ccfgDump ccfg oneShotOut

  for_ ["out", "oneShot", "noCompilation"] $ \nm -> do
    let outFile = dumpFile nm
        refFile = dumpFile "ref"
    putStrLn $ "Diff for file \"" ++ outFile ++ "\":"
    hFlush stdout
    handle <-
      runProcess' diff $
        processConfig
          { pcArgs = [outFile, refFile]
          , pcStdOut = Just stdout
          }
    void $ waitForProcess handle
  where
    dcfg = cfgDirConfig cfg
    ccfg = cfgCheckConfig cfg
    dumpFile nm = dcfgOutDir dcfg </> file <.> nm <.> "dump"

maybeAcceptFile :: Config c -> FilePath -> CheckResult -> IO CheckResult
maybeAcceptFile cfg file result
  | cfgAccept cfg && result `elem` [NoRef, Fail] = do
      out <- readOut cfg dcfgOutDir file
      let ref = refFile dcfg file
      createDirectoryIfMissing True (takeDirectory ref)
      writeFile ref $ ccfgDump ccfg out
      pure Accepted
  where
    dcfg = cfgDirConfig cfg
    ccfg = cfgCheckConfig cfg
maybeAcceptFile _ _ result = pure result

outDir :: DirConfig -> TestPackage -> FilePath
outDir dcfg tpkg = dcfgOutDir dcfg </> tpkgName tpkg

oneShotOutDir :: DirConfig -> TestPackage -> FilePath
oneShotOutDir dcfg tpkg = dcfgOneShotOutDir dcfg </> tpkgName tpkg

noCompilationOutDir :: DirConfig -> TestPackage -> FilePath
noCompilationOutDir dcfg tpkg = dcfgNoCompilationOutDir dcfg </> tpkgName tpkg

refFile :: DirConfig -> FilePath -> FilePath
refFile dcfg file = dcfgRefDir dcfg </> file
