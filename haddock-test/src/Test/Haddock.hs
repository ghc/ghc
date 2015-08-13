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
import Test.Haddock.Xhtml

import qualified Text.XML.Light as Xml


data CheckResult
    = Fail
    | Pass
    | NoRef


checkFiles :: Config -> IO ()
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

    if null failed
        then do
            putStrLn "All tests passed!"
            exitSuccess
        else do
            maybeDiff cfg failed
            exitFailure


maybeDiff :: Config -> [String] -> IO ()
maybeDiff (Config { cfgDiffTool = Nothing }) _ = pure ()
maybeDiff cfg@(Config { cfgDiffTool = (Just diff) }) mdls = do
    putStrLn "Diffing failed cases..."
    forM_ mdls $ diffModule cfg diff


runHaddock :: Config -> IO ()
runHaddock (Config { .. }) = do
    putStrLn "Running Haddock process..."

    haddockStdOut <- openFile cfgHaddockStdOut WriteMode
    handle <- runProcess' cfgHaddockPath $ processConfig
        { pcArgs = cfgHaddockArgs ++ cfgFiles
        , pcEnv = Just $ cfgEnv
        , pcStdOut = Just $ haddockStdOut
        }
    waitForSuccess "Failed to run Haddock on specified test files" handle


checkModule :: Config -> String -> IO CheckResult
checkModule cfg mdl = do
    hasRef <- doesFileExist $ refFile dcfg mdl
    if hasRef
        then do
            Just outXml <- readXml $ outFile dcfg mdl
            Just refXml <- readXml $ refFile dcfg mdl
            return $ if strip outXml == strip refXml
                then Pass
                else Fail
        else return NoRef
  where
    dcfg = cfgDirConfig cfg


diffModule :: Config -> FilePath -> String -> IO ()
diffModule cfg diff mdl = do
    Just outXml <- readXml $ outFile dcfg mdl
    Just refXml <- readXml $ refFile dcfg mdl
    let outXml' = strip outXml
    let refXml' = strip refXml
    writeFile outFile' $ Xml.ppElement outXml'
    writeFile refFile' $ Xml.ppElement refXml'

    putStrLn $ "Diff for module " ++ show mdl ++ ":"
    hFlush stdout
    handle <- runProcess' diff $ processConfig
        { pcArgs = [outFile', refFile']
        , pcStdOut = Just $ stdout
        }
    waitForProcess handle >> return ()
  where
    dcfg = cfgDirConfig cfg
    outFile' = outFile dcfg mdl <.> "nolinks"
    refFile' = outFile dcfg mdl <.> "ref" <.> "nolinks"


outFile :: DirConfig -> String -> FilePath
outFile dcfg mdl = dcfgOutDir dcfg </> mdl <.> "html"


refFile :: DirConfig -> String -> FilePath
refFile dcfg mdl = dcfgRefDir dcfg </> mdl <.> "html"
