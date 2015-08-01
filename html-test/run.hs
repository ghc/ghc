#!/usr/bin/env runhaskell
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}


import Control.Monad

import Data.Maybe

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath


baseDir, rootDir :: FilePath
baseDir = takeDirectory __FILE__
rootDir = baseDir </> ".."

srcDir, refDir, outDir :: FilePath
srcDir = baseDir </> "src"
refDir = baseDir </> "ref"
outDir = baseDir </> "out"


data Config = Config
    { cfgHaddockPath :: FilePath
    , cfgFiles :: [FilePath]
    }


main :: IO ()
main = do
    Config { .. } <- parseArgs =<< getArgs
    putStrLn $ "Files to test: " ++ show cfgFiles


parseArgs :: [String] -> IO Config
parseArgs args = do
    let (flags, files, errors) = getOpt Permute options args

    when (not $ null errors) $ do
        mapM_ putStrLn errors
        exitFailure

    when (FlagHelp `elem` flags) $ do
        putStrLn $ usageInfo "" options
        exitSuccess

    cfgFiles <- processFileArgs files
    let cfgHaddockPath = haddockPath flags

    return $ Config { .. }

processFileArgs :: [String] -> IO [FilePath]
processFileArgs [] = filter isSourceFile <$> getDirectoryContents srcDir
processFileArgs args = pure $ map processFileArg args


processFileArg :: String -> FilePath
processFileArg arg
    | isSourceFile arg = arg
    | otherwise = srcDir </> arg <.> "hs"


isSourceFile :: FilePath -> Bool
isSourceFile path = takeExtension path `elem` [".hs", ".lhs"]


data Flag
    = FlagHaddockPath FilePath
    | FlagHelp
    deriving Eq


options :: [OptDescr Flag]
options =
    [ Option [] ["haddock-path"] (ReqArg FlagHaddockPath "FILE")
        "path to Haddock executable to exectue tests with"
    , Option ['h'] ["help"] (NoArg FlagHelp)
        "display this help end exit"
    ]


haddockPath :: [Flag] -> FilePath
haddockPath flags = case mlast [ path | FlagHaddockPath path <- flags ] of
    Just path -> path
    Nothing -> rootDir </> "dist" </> "build" </> "haddock" </> "haddock"


mlast :: [a] -> Maybe a
mlast = listToMaybe . reverse
