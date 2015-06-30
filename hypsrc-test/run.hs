#!/usr/bin/env runhaskell
{-# LANGUAGE CPP #-}


import Control.Applicative
import Control.Monad

import Data.List
import Data.Maybe

import System.IO
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

import Distribution.Verbosity
import Distribution.Simple.Utils hiding (die)


baseDir, rootDir :: FilePath
baseDir = takeDirectory __FILE__ 
rootDir = baseDir </> ".."

srcDir, refDir, outDir :: FilePath
srcDir = baseDir </> "src"
refDir = baseDir </> "ref"
outDir = baseDir </> "out"

haddockPath :: FilePath
haddockPath = rootDir </> "dist" </> "build" </> "haddock" </> "haddock"


main :: IO ()
main = do
    haddockAvailable <- doesFileExist haddockPath
    unless haddockAvailable $ die "Haddock exectuable not available"

    (args, mods) <- partition ("-" `isPrefixOf`) <$> getArgs
    let args' = filter (\arg -> not $ arg == "--all" || arg == "-a") args
    mods' <- map (srcDir </>) <$> if "--all" `elem` args || "-a" `elem` args
        then getAllSrcModules
        else return mods

    putHaddockVersion
    putGhcVersion

    putStrLn "Running tests..."
    runHaddock $
        [ "--odir=" ++ outDir
        , "--no-warnings"
        , "--hyperlinked-source"
        ] ++ args' ++ mods'

    forM_ mods' $ check True


check :: Bool -> FilePath -> IO ()
check strict mdl = do
    hasReference <- doesFileExist refFile
    if hasReference
    then do
        out <- readFile outFile
        ref <- readFile refFile
        if out == ref
        then putStrLn $ "Pass: " ++ mdl
        else do
            putStrLn $ "Fail: " ++ mdl
            diff refFile outFile
            when strict $ die "Aborting further tests."
    else do
        putStrLn $ "Pass: " ++ mdl ++ " (no reference file)"
  where
    refFile = refDir </> takeBaseName mdl ++ ".html"
    outFile = outDir </> takeBaseName mdl ++ ".html"


diff :: FilePath -> FilePath -> IO ()
diff fileA fileB = do
    colorDiffPath <- findProgramLocation silent "colordiff"
    let cmd = fromMaybe "diff" colorDiffPath

    result <- system $ cmd ++ " " ++ fileA ++ " " ++ fileB
    unless (result == ExitSuccess) $ die "Failed to run `diff` command."


getAllSrcModules :: IO [FilePath]
getAllSrcModules =
    filter isValid <$> getDirectoryContents srcDir
  where
    isValid = (== ".hs") . takeExtension


putHaddockVersion :: IO ()
putHaddockVersion = do
    putStrLn "Haddock version:"
    runHaddock ["--version"]
    putStrLn ""


putGhcVersion :: IO ()
putGhcVersion = do
    putStrLn "GHC version:"
    runHaddock ["--ghc-version"]
    putStrLn ""


runHaddock :: [String] -> IO ()
runHaddock args = do
    env <- Just <$> getEnvironment
    handle <- runProcess haddockPath args Nothing env Nothing Nothing Nothing
    waitForSuccess handle $ "Failed to invoke haddock with " ++ show args


waitForSuccess :: ProcessHandle -> String -> IO ()
waitForSuccess handle msg = do
    result <- waitForProcess handle
    unless (result == ExitSuccess) $ die msg
