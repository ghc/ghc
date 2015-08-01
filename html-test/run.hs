#!/usr/bin/env runhaskell
{-# LANGUAGE CPP #-}


import System.Environment
import System.FilePath


baseDir, rootDir :: FilePath
baseDir = takeDirectory __FILE__
rootDir = baseDir </> ".."

srcDir, refDir, outDir :: FilePath
srcDir = baseDir </> "src"
refDir = baseDir </> "ref"
outDir = baseDir </> "out"


main :: IO ()
main = do
    files <- map processArg <$> getArgs
    putStrLn $ "Files to test: " ++ show files


processArg :: String -> FilePath
processArg arg
    | takeExtension arg `elem` [".hs", ".lhs"] = arg
    | otherwise = srcDir </> arg <.> "hs"
