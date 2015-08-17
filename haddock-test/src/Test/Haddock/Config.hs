{-# LANGUAGE RecordWildCards #-}


module Test.Haddock.Config
    ( CheckConfig(..), DirConfig(..), Config(..)
    , defaultDirConfig
    , cfgSrcDir, cfgRefDir, cfgOutDir, cfgResDir
    , parseArgs, checkOpt, loadConfig
    ) where


import Control.Applicative
import Control.Monad

import qualified Data.List as List
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
import System.Exit
import System.Environment
import System.FilePath
import System.IO

import Test.Haddock.Process
import Test.Haddock.Utils


data CheckConfig c = CheckConfig
    { ccfgRead :: String -> String -> Maybe c
    , ccfgDump :: c -> String
    , ccfgEqual :: c -> c -> Bool
    }


data DirConfig = DirConfig
    { dcfgSrcDir :: FilePath
    , dcfgRefDir :: FilePath
    , dcfgOutDir :: FilePath
    , dcfgResDir :: FilePath
    }


defaultDirConfig :: FilePath -> DirConfig
defaultDirConfig baseDir = DirConfig
    { dcfgSrcDir = baseDir </> "src"
    , dcfgRefDir = baseDir </> "ref"
    , dcfgOutDir = baseDir </> "out"
    , dcfgResDir = rootDir </> "resources"
    }
  where
    rootDir = baseDir </> ".."


data Config c = Config
    { cfgHaddockPath :: FilePath
    , cfgFiles :: [FilePath]
    , cfgHaddockArgs :: [String]
    , cfgHaddockStdOut :: FilePath
    , cfgDiffTool :: Maybe FilePath
    , cfgEnv :: Environment
    , cfgAccept :: Bool
    , cfgCheckConfig :: CheckConfig c
    , cfgDirConfig :: DirConfig
    }


cfgSrcDir, cfgRefDir, cfgOutDir, cfgResDir :: Config c -> FilePath
cfgSrcDir = dcfgSrcDir . cfgDirConfig
cfgRefDir = dcfgRefDir . cfgDirConfig
cfgOutDir = dcfgOutDir . cfgDirConfig
cfgResDir = dcfgResDir . cfgDirConfig



data Flag
    = FlagHaddockPath FilePath
    | FlagHaddockOptions String
    | FlagHaddockStdOut FilePath
    | FlagDiffTool FilePath
    | FlagNoDiff
    | FlagAccept
    | FlagHelp
    deriving Eq


flagsHaddockPath :: [Flag] -> Maybe FilePath
flagsHaddockPath flags = mlast [ path | FlagHaddockPath path <- flags ]


flagsHaddockOptions :: [Flag] -> [String]
flagsHaddockOptions flags = concat
    [ words opts | FlagHaddockOptions opts <- flags ]


flagsHaddockStdOut :: [Flag] -> Maybe FilePath
flagsHaddockStdOut flags = mlast [ path | FlagHaddockStdOut path <- flags ]


flagsDiffTool :: [Flag] -> Maybe FilePath
flagsDiffTool flags = mlast [ path | FlagDiffTool path <- flags ]


options :: [OptDescr Flag]
options =
    [ Option [] ["haddock-path"] (ReqArg FlagHaddockPath "FILE")
        "path to Haddock executable to exectue tests with"
    , Option [] ["haddock-options"] (ReqArg FlagHaddockOptions "OPTS")
        "additional options to run Haddock with"
    , Option [] ["haddock-stdout"] (ReqArg FlagHaddockStdOut "FILE")
        "where to redirect Haddock output"
    , Option [] ["diff-tool"] (ReqArg FlagDiffTool "PATH")
        "diff tool to use when printing failed cases"
    , Option ['a'] ["accept"] (NoArg FlagAccept)
        "accept generated output"
    , Option [] ["no-diff"] (NoArg FlagNoDiff)
        "do not print diff for failed cases"
    , Option ['h'] ["help"] (NoArg FlagHelp)
        "display this help end exit"
    ]


parseArgs :: CheckConfig c -> DirConfig -> [String] -> IO (Config c)
parseArgs ccfg dcfg args = uncurry (loadConfig ccfg dcfg) =<< checkOpt args


checkOpt :: [String] -> IO ([Flag], [String])
checkOpt args = do
    let (flags, files, errors) = getOpt Permute options args

    unless (null errors) $ do
        hPutStr stderr $ concat errors
        exitFailure

    when (FlagHelp `elem` flags) $ do
        hPutStrLn stderr $ usageInfo "" options
        exitSuccess

    return (flags, files)


loadConfig :: CheckConfig c -> DirConfig -> [Flag] -> [String] -> IO (Config c)
loadConfig ccfg dcfg flags files = do
    cfgEnv <- (:) ("haddock_datadir", dcfgResDir dcfg) <$> getEnvironment

    systemHaddockPath <- List.lookup "HADDOCK_PATH" <$> getEnvironment
    cfgHaddockPath <- case flagsHaddockPath flags <|> systemHaddockPath of
        Just path -> pure path
        Nothing -> do
            hPutStrLn stderr $ "Haddock executable not specified"
            exitFailure

    ghcPath <- init <$> rawSystemStdout normal cfgHaddockPath
        ["--print-ghc-path"]

    printVersions cfgEnv cfgHaddockPath

    cfgFiles <- processFileArgs dcfg files

    cfgHaddockArgs <- liftM concat . sequence $
        [ pure ["--no-warnings"]
        , pure ["--odir=" ++ dcfgOutDir dcfg]
        , pure ["--pretty-html"]
        , pure ["--html"]
        , pure ["--optghc=-w"]
        , pure $ flagsHaddockOptions flags
        , baseDependencies ghcPath
        ]

    let cfgHaddockStdOut = fromMaybe "/dev/null" (flagsHaddockStdOut flags)

    cfgDiffTool <- if FlagNoDiff `elem` flags
        then pure Nothing
        else (<|>) <$> pure (flagsDiffTool flags) <*> defaultDiffTool

    let cfgAccept = FlagAccept `elem` flags

    let cfgCheckConfig = ccfg
    let cfgDirConfig = dcfg

    return $ Config { .. }


printVersions :: Environment -> FilePath -> IO ()
printVersions env haddockPath = do
    handleHaddock <- runProcess' haddockPath $ processConfig
        { pcEnv = Just env
        , pcArgs = ["--version"]
        }
    waitForSuccess "Failed to run `haddock --version`" handleHaddock

    handleGhc <- runProcess' haddockPath $ processConfig
        { pcEnv = Just env
        , pcArgs = ["--ghc-version"]
        }
    waitForSuccess "Failed to run `haddock --ghc-version`" handleGhc


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


defaultDiffTool :: IO (Maybe FilePath)
defaultDiffTool =
    liftM listToMaybe . filterM isAvailable $ ["colordiff", "diff"]
  where
    isAvailable = liftM isJust . findProgramLocation silent


processFileArgs :: DirConfig -> [String] -> IO [FilePath]
processFileArgs dcfg [] =
    map toModulePath . filter isSourceFile <$> getDirectoryContents srcDir
  where
    srcDir = dcfgSrcDir dcfg
    toModulePath = modulePath dcfg . takeBaseName
processFileArgs dcfg args = pure $ map (processFileArg dcfg) args


processFileArg :: DirConfig -> String -> FilePath
processFileArg dcfg arg
    | isSourceFile arg = arg
    | otherwise = modulePath dcfg arg


isSourceFile :: FilePath -> Bool
isSourceFile path = takeExtension path `elem` [".hs", ".lhs"]


modulePath :: DirConfig -> String -> FilePath
modulePath dcfg mdl = dcfgSrcDir dcfg </> mdl <.> "hs"
