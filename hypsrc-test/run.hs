#!/usr/bin/env runhaskell
{-# LANGUAGE CPP #-}


import Control.Monad

import Data.List
import Data.Maybe

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

import Distribution.Verbosity
import Distribution.Simple.Utils hiding (die)

import Utils


main :: IO ()
main = do
    haddockAvailable <- doesFileExist haddockPath
    unless haddockAvailable $ die "Haddock exectuable not available"

    (args, mods) <- partition ("-" `isPrefixOf`) <$> getArgs
    let args' = filter (\arg -> not $ arg == "--all" || arg == "-a") args
    mods' <- map (srcDir </>) <$> case args of
        [] -> getAllSrcModules
        _ -> return $ map (++ ".hs") mods

    putHaddockVersion
    putGhcVersion

    putStrLn "Running tests..."
    runHaddock $
        [ "--odir=" ++ outDir
        , "--no-warnings"
        , "--hyperlinked-source"
        , "--pretty-html"
        ] ++ args' ++ mods'

    forM_ mods' $ check True


check :: Bool -> FilePath -> IO ()
check strict mdl = do
    hasReference <- doesFileExist refFile
    if hasReference
    then do
        ref <- readFile refFile
        out <- readFile outFile
        compareOutput strict mdl ref out
    else do
        putStrLn $ "Pass: " ++ mdl ++ " (no reference file)"
  where
    refFile = refDir' </> takeBaseName mdl ++ ".html"
    outFile = outDir' </> takeBaseName mdl ++ ".html"


compareOutput :: Bool -> FilePath -> String -> String -> IO ()
compareOutput strict mdl ref out = do
    if ref' == out'
    then putStrLn $ "Pass: " ++ mdl
    else do
        putStrLn $ "Fail: " ++ mdl
        diff mdl ref' out'
        when strict $ die "Aborting further tests."
  where
    ref' = stripLocalReferences ref
    out' = stripLocalReferences out


diff :: FilePath -> String -> String -> IO ()
diff mdl ref out = do
    colorDiffPath <- findProgramLocation silent "colordiff"
    let cmd = fromMaybe "diff" colorDiffPath

    writeFile refFile ref
    writeFile outFile out

    result <- system $ cmd ++ " " ++ refFile ++ " " ++ outFile
    unless (result == ExitSuccess) $ die "Failed to run `diff` command."
  where
    refFile = outDir </> takeFileName mdl </> ".ref.nolinks"
    outFile = outDir </> takeFileName mdl </> ".nolinks"



getAllSrcModules :: IO [FilePath]
getAllSrcModules =
    filter isHaskellFile <$> getDirectoryContents srcDir
  where
    isHaskellFile = (== ".hs") . takeExtension


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
    menv <- Just <$> getEnvironment
    handle <- runProcess haddockPath args Nothing menv Nothing Nothing Nothing
    waitForSuccess handle $ "Failed to invoke haddock with " ++ show args


waitForSuccess :: ProcessHandle -> String -> IO ()
waitForSuccess handle msg = do
    result <- waitForProcess handle
    unless (result == ExitSuccess) $ die msg
