#!/usr/bin/env runhaskell
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}


import Control.Monad

import Data.Maybe

import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Simple.GHC
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Verbosity

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process


baseDir, rootDir :: FilePath
baseDir = takeDirectory __FILE__
rootDir = baseDir </> ".."

srcDir, refDir, outDir :: FilePath
srcDir = baseDir </> "src"
refDir = baseDir </> "ref"
outDir = baseDir </> "out"

resDir :: FilePath
resDir = rootDir </> "resources"


data Config = Config
    { cfgHaddockPath :: FilePath
    , cfgGhcPath :: FilePath
    , cfgFiles :: [FilePath]
    , cfgHaddockArgs :: [String]
    } deriving Show


main :: IO ()
main = do
    cfg <- loadConfig =<< getArgs
    putStrLn $ show cfg


loadConfig :: [String] -> IO Config
loadConfig args = do
    let (flags, files, errors) = getOpt Permute options args

    when (not $ null errors) $ do
        hPutStr stderr $ concat errors
        exitFailure

    when (FlagHelp `elem` flags) $ do
        hPutStrLn stderr $ usageInfo "" options
        exitSuccess

    env <- Just . (:) ("haddock_datadir", resDir) <$> getEnvironment

    cfgHaddockPath <- pure $ flip fromMaybe (flagsHaddockPath flags) $
        rootDir </> "dist" </> "build" </> "haddock" </> "haddock"

    printVersions env cfgHaddockPath

    cfgGhcPath <- flip fromMaybe (flagsGhcPath flags) <$>
         init <$> rawSystemStdout normal cfgHaddockPath ["--print-ghc-path"]

    cfgFiles <- processFileArgs files

    cfgHaddockArgs <- liftM concat . sequence $
        [ pure ["--no-warnings"]
        , pure ["--odir=" ++ outDir]
        , pure ["--pretty-html"]
        , pure ["--optghc=--w"]
        , pure $ flagsHaddockOptions flags
        , baseDependencies cfgGhcPath
        ]

    return $ Config { .. }


printVersions :: Maybe [(String, String)] -> FilePath -> IO ()
printVersions env haddockPath = do
    handle <- runProcess' haddockPath $ processConfig
        { pcEnv = env
        , pcArgs = ["--version"]
        }
    waitForSuccess "Failed to run `haddock --version`" handle

    handle <- runProcess' haddockPath $ processConfig
        { pcEnv = env
        , pcArgs = ["--ghc-version"]
        }
    waitForSuccess "Failed to run `haddock --ghc-version`" handle


baseDependencies :: FilePath -> IO [String]
baseDependencies ghcPath = do
    (_, _, cfg) <- configure normal (Just ghcPath) Nothing
        defaultProgramConfiguration
    pkgIndex <- getInstalledPackages normal [GlobalPackageDB] cfg
    mapM (getDependency pkgIndex) ["base", "process", "ghc-prim"]
  where
    getDependency pkgIndex name = case ifaces pkgIndex name of
        [] -> do
            hPutStrLn stderr $ "Couldn't find base test dependency: " ++ name
            exitFailure
        (ifArg:_) -> pure ifArg
    ifaces pkgIndex name = do
        pkg <- join $ snd <$> lookupPackageName pkgIndex (PackageName name)
        iface <$> haddockInterfaces pkg <*> haddockHTMLs pkg
    iface file html = "--read-interface=" ++ html ++ "," ++ file


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
    | FlagGhcPath FilePath
    | FlagHaddockOptions String
    | FlagHelp
    deriving Eq


options :: [OptDescr Flag]
options =
    [ Option [] ["haddock-path"] (ReqArg FlagHaddockPath "FILE")
        "path to Haddock executable to exectue tests with"
    , Option [] ["ghc-path"] (ReqArg FlagGhcPath "FILE")
        "path to GHC executable"
    , Option [] ["haddock-options"] (ReqArg FlagHaddockOptions "OPTS")
        "additional options to run Haddock with"
    , Option ['h'] ["help"] (NoArg FlagHelp)
        "display this help end exit"
    ]


flagsHaddockPath :: [Flag] -> Maybe FilePath
flagsHaddockPath flags = mlast [ path | FlagHaddockPath path <- flags ]


flagsGhcPath :: [Flag] -> Maybe FilePath
flagsGhcPath flags = mlast [ path | FlagGhcPath path <- flags ]


flagsHaddockOptions :: [Flag] -> [String]
flagsHaddockOptions flags = concat
    [ words opts | FlagHaddockOptions opts <- flags ]


data ProcessConfig = ProcessConfig
    { pcArgs :: [String]
    , pcWorkDir :: Maybe FilePath
    , pcEnv :: Maybe [(String, String)]
    , pcStdIn :: Maybe Handle
    , pcStdOut :: Maybe Handle
    , pcStdErr :: Maybe Handle
    }


processConfig :: ProcessConfig
processConfig = ProcessConfig
    { pcArgs = []
    , pcWorkDir = Nothing
    , pcEnv = Nothing
    , pcStdIn = Nothing
    , pcStdOut = Nothing
    , pcStdErr = Nothing
    }


runProcess' :: FilePath -> ProcessConfig -> IO ProcessHandle
runProcess' path (ProcessConfig { .. }) = runProcess
    path pcArgs pcWorkDir pcEnv pcStdIn pcStdOut pcStdErr


waitForSuccess :: String -> ProcessHandle -> IO ()
waitForSuccess msg handle = do
    result <- waitForProcess handle
    unless (result == ExitSuccess) $ do
        hPutStrLn stderr $ msg
        exitFailure


mlast :: [a] -> Maybe a
mlast = listToMaybe . reverse
