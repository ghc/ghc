{-# LANGUAGE RecordWildCards #-}


module Test.Haddock
    ( module Test.Haddock.Config
    , runAndCheck, runHaddock, checkFiles
    ) where


import Control.Monad

import Data.Maybe

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import Test.Haddock.Config
import Test.Haddock.Process


data CheckResult
    = Fail
    | Pass
    | NoRef
    | Error String


runAndCheck :: Config c -> IO ()
runAndCheck cfg = do
    runHaddock cfg
    checkFiles cfg


checkFiles :: Config c -> IO ()
checkFiles cfg@(Config { .. }) = do
    putStrLn "Testing output files..."

    files <- getDirectoryContents (cfgOutDir cfg)
    failed <- liftM catMaybes . forM files $ \file -> do
        putStr $ "Checking \"" ++ file ++ "\"... "

        status <- checkFile cfg file
        case status of
            Fail -> putStrLn "FAIL" >> (return $ Just file)
            Pass -> putStrLn "PASS" >> (return Nothing)
            NoRef -> putStrLn "PASS [no .ref]" >> (return Nothing)
            Error msg -> putStrLn ("ERROR (" ++ msg ++ ")") >> return Nothing

    if null failed
        then do
            putStrLn "All tests passed!"
            exitSuccess
        else do
            maybeDiff cfg failed
            exitFailure


maybeDiff :: Config c -> [FilePath] -> IO ()
maybeDiff (Config { cfgDiffTool = Nothing }) _ = pure ()
maybeDiff cfg@(Config { cfgDiffTool = (Just diff) }) files = do
    putStrLn "Diffing failed cases..."
    forM_ files $ diffFile cfg diff


runHaddock :: Config c -> IO ()
runHaddock (Config { .. }) = do
    haddockStdOut <- openFile cfgHaddockStdOut WriteMode

    putStrLn "Generating documentation..."
    forM_ cfgPackages $ \tpkg -> do
        handle <- runProcess' cfgHaddockPath $ processConfig
            { pcArgs = concat
                [ cfgHaddockArgs
                , pure $ "--odir=" ++ outDir cfgDirConfig tpkg
                , tpkgFiles tpkg
                ]
            , pcEnv = Just $ cfgEnv
            , pcStdOut = Just $ haddockStdOut
            }
        waitForSuccess "Failed to run Haddock on specified test files" handle


checkFile :: Config c -> FilePath -> IO CheckResult
checkFile cfg file = do
    hasRef <- doesFileExist $ refFile dcfg file
    if hasRef
        then do
            mout <- ccfgRead ccfg file <$> readFile (outFile dcfg file)
            mref <- ccfgRead ccfg file <$> readFile (refFile dcfg file)
            return $ case (mout, mref) of
                (Just out, Just ref)
                    | ccfgEqual ccfg out ref -> Pass
                    | otherwise -> Fail
                _ -> Error "Failed to parse input files"
        else return NoRef
  where
    ccfg = cfgCheckConfig cfg
    dcfg = cfgDirConfig cfg


diffFile :: Config c -> FilePath -> FilePath -> IO ()
diffFile cfg diff file = do
    Just out <- ccfgRead ccfg file <$> readFile (outFile dcfg file)
    Just ref <- ccfgRead ccfg file <$> readFile (refFile dcfg file)
    writeFile outFile' $ ccfgDump ccfg out
    writeFile refFile' $ ccfgDump ccfg ref

    putStrLn $ "Diff for file \"" ++ file ++ "\":"
    hFlush stdout
    handle <- runProcess' diff $ processConfig
        { pcArgs = [outFile', refFile']
        , pcStdOut = Just $ stdout
        }
    waitForProcess handle >> return ()
  where
    dcfg = cfgDirConfig cfg
    ccfg = cfgCheckConfig cfg
    outFile' = outFile dcfg file <.> "dump"
    refFile' = outFile dcfg file <.> "ref" <.> "dump"


outDir :: DirConfig -> TestPackage -> FilePath
outDir dcfg tpkg = dcfgOutDir dcfg </> tpkgName tpkg


outFile :: DirConfig -> FilePath -> FilePath
outFile dcfg file = dcfgOutDir dcfg </> file


refFile :: DirConfig -> FilePath -> FilePath
refFile dcfg file = dcfgRefDir dcfg </> file
