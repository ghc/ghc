#!/usr/bin/env runhaskell
{-# LANGUAGE CPP #-}


import System.Directory
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
    files <- processArgs =<< getArgs
    putStrLn $ "Files to test: " ++ show files


processArgs :: [String] -> IO [FilePath]
processArgs [] = filter isSourceFile <$> getDirectoryContents srcDir
processArgs args = pure $ map processArg args


processArg :: String -> FilePath
processArg arg
    | isSourceFile arg = arg
    | otherwise = srcDir </> arg <.> "hs"


isSourceFile :: FilePath -> Bool
isSourceFile path = takeExtension path `elem` [".hs", ".lhs"]
