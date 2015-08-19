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
import Test.Haddock.Utils


data CheckResult
    = Fail
    | Pass
    | NoRef
    | Error String
    | Accepted
    deriving Eq


runAndCheck :: Config c -> IO ()
runAndCheck cfg = do
    runHaddock cfg
    checkFiles cfg


checkFiles :: Config c -> IO ()
checkFiles cfg@(Config { .. }) = do
    putStrLn "Testing output files..."

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

    if null failed
        then do
            putStrLn "All tests passed!"
            exitSuccess
        else do
            maybeDiff cfg failed
            exitFailure
  where
    ignore = filter (not . dcfgCheckIgnore cfgDirConfig)


maybeDiff :: Config c -> [FilePath] -> IO ()
maybeDiff (Config { cfgDiffTool = Nothing }) _ = pure ()
maybeDiff cfg@(Config { cfgDiffTool = (Just diff) }) files = do
    putStrLn "Diffing failed cases..."
    forM_ files $ diffFile cfg diff


runHaddock :: Config c -> IO ()
runHaddock cfg@(Config { .. }) = do
    createEmptyDirectory $ cfgOutDir cfg

    putStrLn "Generating documentation..."
    forM_ cfgPackages $ \tpkg -> do
        haddockStdOut <- openFile cfgHaddockStdOut WriteMode
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


maybeAcceptFile :: Config c -> FilePath -> CheckResult -> IO CheckResult
maybeAcceptFile cfg@(Config { cfgDirConfig = dcfg }) file result
    | cfgAccept cfg && result `elem` [NoRef, Fail] = do
        copyFile (outFile dcfg file) (refFile dcfg file)
        pure Accepted
maybeAcceptFile _ _ result = pure result


outDir :: DirConfig -> TestPackage -> FilePath
outDir dcfg tpkg = dcfgOutDir dcfg </> tpkgName tpkg


outFile :: DirConfig -> FilePath -> FilePath
outFile dcfg file = dcfgOutDir dcfg </> file


refFile :: DirConfig -> FilePath -> FilePath
refFile dcfg file = dcfgRefDir dcfg </> file
