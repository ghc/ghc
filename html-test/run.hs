{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}


import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.List

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

import qualified Text.XML.Light as Xml

import Test.Haddock.Process
import Test.Haddock.Config
import Test.Haddock.Xhtml


baseDir, rootDir :: FilePath
baseDir = takeDirectory __FILE__
rootDir = baseDir </> ".."

srcDir, refDir, outDir :: FilePath
srcDir = baseDir </> "src"
refDir = baseDir </> "ref"
outDir = baseDir </> "out"

resDir :: FilePath
resDir = rootDir </> "resources"


data CheckResult
    = Fail
    | Pass
    | NoRef


main :: IO ()
main = do
    let dcfg = defaultDirConfig baseDir
    cfg <- uncurry (loadConfig dcfg) =<< checkOpt =<< getArgs
    runHaddock cfg
    checkFiles cfg


checkFiles :: Config -> IO ()
checkFiles (Config { .. }) = do
    putStrLn "Testing output files..."
    failed <- liftM catMaybes . forM cfgFiles $ \file -> do
        let mdl = takeBaseName file
        putStr $ "Checking " ++ mdl ++ "... "

        status <- checkModule mdl
        case status of
            Fail -> putStrLn "FAIL" >> (return $ Just mdl)
            Pass -> putStrLn "PASS" >> (return Nothing)
            NoRef -> putStrLn "PASS [no .ref]" >> (return Nothing)

    if null failed
        then do
            putStrLn "All tests passed!"
            exitSuccess
        else do
            maybeDiff cfgDiffTool failed
            exitFailure


maybeDiff :: Maybe FilePath -> [String] -> IO ()
maybeDiff Nothing _ = pure ()
maybeDiff (Just diff) mdls = do
    putStrLn "Diffing failed cases..."
    forM_ mdls $ diffModule diff


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


checkModule :: String -> IO CheckResult
checkModule mdl = do
    hasRef <- doesFileExist $ refFile mdl
    if hasRef
        then do
            Just outXml <- readXml $ outFile mdl
            Just refXml <- readXml $ refFile mdl
            return $ if strip outXml == strip refXml
                then Pass
                else Fail
        else return NoRef


diffModule :: FilePath -> String -> IO ()
diffModule diff mdl = do
    Just outXml <- readXml $ outFile mdl
    Just refXml <- readXml $ refFile mdl
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
    outFile' = outFile mdl <.> "nolinks"
    refFile' = outFile mdl <.> "ref" <.> "nolinks"


outFile :: String -> FilePath
outFile mdl = outDir </> mdl <.> "html"


refFile :: String -> FilePath
refFile mdl = refDir </> mdl <.> "html"


-- *** OLD TEST RUNNER UTILITY FUNCTIONS ***
-- These are considered bad and should be replaced as soon as possible.


-- | List of modules in which we don't 'stripLinks'
preserveLinksModules :: [String]
preserveLinksModules = ["Bug253"]
