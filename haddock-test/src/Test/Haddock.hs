{-# LANGUAGE RecordWildCards #-}


module Test.Haddock
    ( module Test.Haddock
    , module Test.Haddock.Config
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


checkFiles :: Config c -> IO ()
checkFiles cfg@(Config { .. }) = do
    putStrLn "Testing output files..."
    failed <- liftM catMaybes . forM cfgFiles $ \file -> do
        let mdl = takeBaseName file
        putStr $ "Checking " ++ mdl ++ "... "

        status <- checkModule cfg mdl
        case status of
            Fail -> putStrLn "FAIL" >> (return $ Just mdl)
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


maybeDiff :: Config c -> [String] -> IO ()
maybeDiff (Config { cfgDiffTool = Nothing }) _ = pure ()
maybeDiff cfg@(Config { cfgDiffTool = (Just diff) }) mdls = do
    putStrLn "Diffing failed cases..."
    forM_ mdls $ diffModule cfg diff


runHaddock :: Config c -> IO ()
runHaddock (Config { .. }) = do
    putStrLn "Running Haddock process..."

    haddockStdOut <- openFile cfgHaddockStdOut WriteMode
    handle <- runProcess' cfgHaddockPath $ processConfig
        { pcArgs = cfgHaddockArgs ++ cfgFiles
        , pcEnv = Just $ cfgEnv
        , pcStdOut = Just $ haddockStdOut
        }
    waitForSuccess "Failed to run Haddock on specified test files" handle


checkModule :: Config c -> String -> IO CheckResult
checkModule cfg mdl = do
    hasRef <- doesFileExist $ refFile dcfg mdl
    if hasRef
        then do
            mout <- ccfgRead ccfg mdl <$> readFile (outFile dcfg mdl)
            mref <- ccfgRead ccfg mdl <$> readFile (refFile dcfg mdl)
            return $ case (mout, mref) of
                (Just out, Just ref)
                    | ccfgEqual ccfg out ref -> Pass
                    | otherwise -> Fail
                _ -> Error "Failed to parse input files"
        else return NoRef
  where
    ccfg = cfgCheckConfig cfg
    dcfg = cfgDirConfig cfg


diffModule :: Config c -> FilePath -> String -> IO ()
diffModule cfg diff mdl = do
    Just out <- ccfgRead ccfg mdl <$> readFile (outFile dcfg mdl)
    Just ref <- ccfgRead ccfg mdl <$> readFile (refFile dcfg mdl)
    writeFile outFile' $ ccfgDump ccfg out
    writeFile refFile' $ ccfgDump ccfg ref

    putStrLn $ "Diff for module " ++ show mdl ++ ":"
    hFlush stdout
    handle <- runProcess' diff $ processConfig
        { pcArgs = [outFile', refFile']
        , pcStdOut = Just $ stdout
        }
    waitForProcess handle >> return ()
  where
    dcfg = cfgDirConfig cfg
    ccfg = cfgCheckConfig cfg
    outFile' = outFile dcfg mdl <.> "nolinks"
    refFile' = outFile dcfg mdl <.> "ref" <.> "nolinks"


outFile :: DirConfig -> String -> FilePath
outFile dcfg mdl = dcfgOutDir dcfg </> mdl <.> "html"


refFile :: DirConfig -> String -> FilePath
refFile dcfg mdl = dcfgRefDir dcfg </> mdl <.> "html"
