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

    if cfgSkipOneShot then pure succeeded else do
      let oneShotDir = oneshotOutDir cfgDirConfig tpkg
          hiDir = oneShotDir </> "hi"
          hieDir = oneShotDir </> "hie"
          responseFile = hiDir </> "response-file"
      createEmptyDirectory oneShotDir
      createEmptyDirectory hiDir
      createEmptyDirectory hieDir
      writeFile responseFile $ escapeArgs
        [ "--odir=" ++ oneShotDir
        , "--optghc=-hidir=" ++ hiDir
        , "--optghc=-hiedir=" ++ hieDir
        ]

      -- Build .hi files
      let pc' =
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
      _ <- waitForSuccess msg stdout =<< runProcess' cfgGhcPath pc'

      files <- filter ((== ".hi") . takeExtension) <$> listDirectory hiDir
      -- Use the output order of GHC as a simple dependency order
      filesSorted <- Map.elems . Map.fromList <$> traverse (\file -> (,file) <$> getModificationTime (hiDir </> file)) files
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
      succeeded2 <- loop filesSorted
      when succeeded2 $ do
        removeDirectoryRecursive hiDir
        removeDirectoryRecursive hieDir
      pure succeeded2

  let somethingFailed = any not successes
  pure somethingFailed

checkFile :: Config c -> FilePath -> IO CheckResult
checkFile cfg file = do
  hasRef <- doesFileExist $ refFile dcfg file
  if hasRef
    then do
      mout <- readOut cfg file
      mref <- readRef cfg file
      case (mout, mref) of
        (Just out, Just ref)
          | ccfgEqual ccfg out ref ->
              if cfgSkipOneShot cfg || dcfgCheckIgnoreOneShot (cfgDirConfig cfg) file
                then return Pass
                else do
                  mOneShotOut <- readOneShotOut cfg file
                  return $ case mOneShotOut of
                    Just oneShotOut
                      | ccfgEqual ccfg oneShotOut out -> Pass
                      | otherwise -> Fail
                    Nothing -> Error "Failed to parse one-shot input file"
          | otherwise -> return Fail
        _ -> return $ Error "Failed to parse input files"
    else return NoRef
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
readOut :: Config c -> FilePath -> IO (Maybe c)
readOut cfg file =
  fmap (ccfgClean ccfg file) . ccfgRead ccfg . BS.unpack
    <$> BS.readFile (outFile dcfg file)
  where
    ccfg = cfgCheckConfig cfg
    dcfg = cfgDirConfig cfg

readOneShotOut :: Config c -> FilePath -> IO (Maybe c)
readOneShotOut cfg file =
  fmap (ccfgClean ccfg file) . ccfgRead ccfg . BS.unpack
    <$> BS.readFile (oneShotOutFile dcfg file)
  where
    ccfg = cfgCheckConfig cfg
    dcfg = cfgDirConfig cfg

diffFile :: Config c -> FilePath -> FilePath -> IO ()
diffFile cfg diff file = do
  Just out <- readOut cfg file
  Just oneShotOut <- readOneShotOut cfg file
  Just ref <- readRef cfg file
  writeFile outFile' $ ccfgDump ccfg out
  writeFile oneShotOutFile' $ ccfgDump ccfg oneShotOut
  writeFile refFile' $ ccfgDump ccfg ref

  putStrLn $ "Diff for file \"" ++ file ++ "\":"
  hFlush stdout
  handle <-
    runProcess' diff $
      processConfig
        { pcArgs = [outFile', refFile']
        , pcStdOut = Just stdout
        }
  void $ waitForProcess handle
  handle' <-
    runProcess' diff $
      processConfig
        { pcArgs = [oneShotOutFile', outFile']
        , pcStdOut = Just stdout
        }
  void $ waitForProcess handle'
  return ()
  where
    dcfg = cfgDirConfig cfg
    ccfg = cfgCheckConfig cfg
    outFile' = outFile dcfg file <.> "dump"
    oneShotOutFile' = oneShotOutFile dcfg file <.> "dump"
    refFile' = outFile dcfg file <.> "ref" <.> "dump"

maybeAcceptFile :: Config c -> FilePath -> CheckResult -> IO CheckResult
maybeAcceptFile cfg file result
  | cfgAccept cfg && result `elem` [NoRef, Fail] = do
      Just out <- readOut cfg file
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

oneshotOutDir :: DirConfig -> TestPackage -> FilePath
oneshotOutDir dcfg tpkg = dcfgOneShotOutDir dcfg </> tpkgName tpkg

outFile :: DirConfig -> FilePath -> FilePath
outFile dcfg file = dcfgOutDir dcfg </> file

oneShotOutFile :: DirConfig -> FilePath -> FilePath
oneShotOutFile dcfg file = dcfgOneShotOutDir dcfg </> file

refFile :: DirConfig -> FilePath -> FilePath
refFile dcfg file = dcfgRefDir dcfg </> file
