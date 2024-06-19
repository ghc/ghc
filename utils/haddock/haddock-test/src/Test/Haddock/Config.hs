{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Test.Haddock.Config
    ( TestPackage(..), CheckConfig(..), DirConfig(..), Config(..)
    , defaultDirConfig
    , cfgSrcDir, cfgRefDir, cfgOutDir, cfgResDir, cfgOneShotOutDir
    , parseArgs, checkOpt, loadConfig
    ) where


import Control.Applicative
import Control.Monad

import qualified Data.List as List
import Data.Maybe

import Distribution.Text
import Distribution.Types.PackageName
import Distribution.InstalledPackageInfo
import Distribution.Simple.Compiler (PackageDB(..))
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


data TestPackage = TestPackage
    { tpkgName :: String
    , tpkgFiles :: [FilePath]
    }


data CheckConfig c = CheckConfig
    { ccfgRead :: String -> Maybe c
      -- ^ @f contents@ parses file contents @contents@ to
      -- produce a thing to be compared.
    , ccfgClean :: String -> c -> c
      -- ^ @f fname x@ cleans @x@ to such that it can be compared
    , ccfgDump :: c -> String
    , ccfgEqual :: c -> c -> Bool
    }


data DirConfig = DirConfig
    { dcfgSrcDir :: FilePath
    , dcfgRefDir :: FilePath
    , dcfgOutDir :: FilePath
    , dcfgOneShotOutDir :: FilePath
    , dcfgResDir :: FilePath
    , dcfgCheckIgnore :: FilePath -> Bool
    , dcfgCheckIgnoreOneShot :: FilePath -> Bool
    }


defaultDirConfig :: FilePath -> DirConfig
defaultDirConfig baseDir = DirConfig
    { dcfgSrcDir = baseDir </> "src"
    , dcfgRefDir = baseDir </> "ref"
    , dcfgOutDir = baseDir </> "out"
    , dcfgOneShotOutDir = baseDir </> "one-shot-out"
    , dcfgResDir = rootDir </> "resources"
    , dcfgCheckIgnore = const False
    , dcfgCheckIgnoreOneShot = const False
    }
  where
    rootDir = baseDir </> ".."


data Config c = Config
    { cfgHaddockPath :: FilePath
    , cfgGhcPath :: FilePath
    , cfgPackages :: [TestPackage]
    , cfgHaddockArgs :: [String]
    , cfgDiffTool :: Maybe FilePath
    , cfgEnv :: Environment
    , cfgAccept :: Bool
    , cfgCheckConfig :: CheckConfig c
    , cfgDirConfig :: DirConfig
    , cfgSkipOneShot :: Bool
    }


cfgSrcDir, cfgRefDir, cfgOutDir, cfgResDir, cfgOneShotOutDir :: Config c -> FilePath
cfgSrcDir = dcfgSrcDir . cfgDirConfig
cfgRefDir = dcfgRefDir . cfgDirConfig
cfgOutDir = dcfgOutDir . cfgDirConfig
cfgResDir = dcfgResDir . cfgDirConfig
cfgOneShotOutDir = dcfgOneShotOutDir . cfgDirConfig



data Flag
    = FlagHaddockPath FilePath
    | FlagHaddockOptions String
    | FlagGhcPath FilePath
    | FlagDiffTool FilePath
    | FlagNoDiff
    | FlagAccept
    | FlagHelp
    deriving Eq


flagsHaddockPath :: [Flag] -> Maybe FilePath
flagsHaddockPath flags = mlast [ path | FlagHaddockPath path <- flags ]

flagsGhcPath :: [Flag] -> Maybe FilePath
flagsGhcPath flags = mlast [ path | FlagGhcPath path <- flags ]

flagsHaddockOptions :: [Flag] -> [String]
flagsHaddockOptions flags = concat
    [ words opts | FlagHaddockOptions opts <- flags ]


flagsDiffTool :: [Flag] -> Maybe FilePath
flagsDiffTool flags = mlast [ path | FlagDiffTool path <- flags ]


options :: [OptDescr Flag]
options =
    [ Option [] ["haddock-path"] (ReqArg FlagHaddockPath "FILE")
        "path to Haddock executable to exectue tests with"
    , Option [] ["haddock-options"] (ReqArg FlagHaddockOptions "OPTS")
        "additional options to run Haddock with"
    , Option [] ["ghc-path"] (ReqArg FlagGhcPath "FILE")
        "path ghc executable"
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

    -- Find Haddock executable
    systemHaddockPath <- List.lookup "HADDOCK_PATH" <$> getEnvironment
    haddockOnPath <- findExecutable "haddock"

    let haddock_path = msum [ flagsHaddockPath flags
                            , systemHaddockPath
                            , haddockOnPath
                            ]

    cfgHaddockPath <- case haddock_path of
        Just path -> pure path
        Nothing   -> do
          hPutStrLn stderr "Haddock executable not found; consider using the `--haddock-path` flag."
          exitFailure

    -- Perhaps Haddock knows where you can find GHC?
    queriedGhcPath <- do
      p <- init <$> rawSystemStdout normal cfgHaddockPath ["--print-ghc-path"]
      exists <- doesFileExist p
      pure $ if exists then Just p else Nothing


    let ghc_path = msum [ flagsGhcPath flags
                        , queriedGhcPath
                        ]

    cfgGhcPath <- case ghc_path of
        Just path -> pure path
        Nothing   -> do
          hPutStrLn stderr "GHC executable not found; consider using the `--ghc-path` flag."
          exitFailure

    printVersions cfgEnv cfgHaddockPath

    cfgPackages <- processFileArgs dcfg files

    cfgHaddockArgs <- liftM concat . sequence $
        [ pure ["--no-warnings"]
        , pure ["--bypass-interface-version-check"]
        , pure ["--odir=" ++ dcfgOutDir dcfg]
        , pure ["--optghc=-w"]
        , pure ["--optghc=-hide-all-packages"]
        , pure $ flagsHaddockOptions flags
        , baseDependencies cfgGhcPath
        ]

    cfgDiffTool <- if FlagNoDiff `elem` flags
        then pure Nothing
        else (<|>) <$> pure (flagsDiffTool flags) <*> defaultDiffTool

    let cfgAccept = FlagAccept `elem` flags

    let cfgCheckConfig = ccfg
    let cfgDirConfig   = dcfg
    let cfgSkipOneShot = False

    return $ Config { .. }


printVersions :: Environment -> FilePath -> IO ()
printVersions env haddockPath = do
    handleHaddock <- runProcess' haddockPath $ processConfig
        { pcEnv = Just env
        , pcArgs = ["--version"]
        }
    void $ waitForSuccess "Failed to run `haddock --version`" stderr handleHaddock

    handleGhc <- runProcess' haddockPath $ processConfig
        { pcEnv = Just env
        , pcArgs = ["--ghc-version"]
        }
    void $ waitForSuccess "Failed to run `haddock --ghc-version`" stderr handleGhc


baseDependencies :: FilePath -> IO [String]
baseDependencies ghcPath = do
    -- The 'getInstalledPackages' crashes if used when "GHC_PACKAGE_PATH" is
    -- set to some value. I am not sure why is that happening and what are the
    -- consequences of unsetting it - but looks like it works (for now).
    unsetEnv "GHC_PACKAGE_PATH"

    (comp, _, cfg) <- configure normal (Just ghcPath) Nothing
        defaultProgramDb
#if MIN_VERSION_Cabal(1,23,0)
    pkgIndex <- getInstalledPackages normal comp [GlobalPackageDB] cfg
#else
    pkgIndex <- getInstalledPackages normal [GlobalPackageDB] cfg
#endif
    let
      pkgs =
        [ "array"
        , "base"
        , "ghc-prim"
        , "process"
        , "template-haskell"
        ]
    concat `fmap` mapM (getDependency pkgIndex) pkgs
  where
    getDependency pkgIndex name = case ifaces pkgIndex name of
      [] -> do
        hPutStrLn stderr $ "Couldn't find base test dependency: " ++ name
        exitFailure

      (unit, ifaceOpt, htmlOpt) : alts -> do
        when (not . null $ alts) $
          hPutStr stderr $ unlines
            [ "Multiple options found for base test dependency: " ++ name
            , "Choosing the first of these, which has unit id: " ++ unit
            ]

        case (ifaceOpt, htmlOpt) of
          (Nothing, _) -> do
            hPutStr stderr $
              "No '.haddock' file found for base test dependency: " ++ name
            exitFailure

          (Just iface, Nothing) -> do
            hPutStrLn stderr $
              "No HTML directory found for base test dependency: " ++ name
            pure [ "--optghc=-package" ++ name
                 , "--read-interface=" ++ iface
                 ]

          (Just iface, Just html) ->
            pure [ "--optghc=-package" ++ name
                 , "--read-interface=" ++ html ++ "," ++ iface
                 ]

    ifaces pkgIndex name = do
        pkg <- join $ snd <$> lookupPackageName pkgIndex (mkPackageName name)

        let unitId = display (installedUnitId pkg)
            ifaceOpt = listToMaybe (haddockInterfaces pkg)
            htmlDirOpt = listToMaybe (haddockHTMLs pkg)

        pure (unitId, ifaceOpt, htmlDirOpt)


defaultDiffTool :: IO (Maybe FilePath)
defaultDiffTool =
    liftM listToMaybe . filterM isAvailable $ ["colordiff", "diff"]
  where
    isAvailable = liftM isJust . findExecutable


defaultStdOut :: FilePath
#ifdef mingw32_HOST_OS
defaultStdOut = "nul"
#else
defaultStdOut = "/dev/null"
#endif


processFileArgs :: DirConfig -> [String] -> IO [TestPackage]
processFileArgs dcfg [] =
    processFileArgs' dcfg . filter isValidEntry =<< getDirectoryContents srcDir
  where
    isValidEntry entry
        | hasExtension entry = isSourceFile entry
        | otherwise = isRealDir entry
    srcDir = dcfgSrcDir dcfg
processFileArgs dcfg args = processFileArgs' dcfg args


processFileArgs' :: DirConfig -> [String] -> IO [TestPackage]
processFileArgs' dcfg args = do
    (dirs, mdls) <- partitionM doesDirectoryExist' . map takeBaseName $ args
    rootPkg <- pure $ TestPackage
        { tpkgName = ""
        , tpkgFiles = map (srcDir </>) mdls
        }
    otherPkgs <- forM dirs $ \dir -> do
        let srcDir' = srcDir </> dir
        files <- filterM (isModule dir) =<< getDirectoryContents srcDir'
        pure $ TestPackage
            { tpkgName = dir
            , tpkgFiles = map (srcDir' </>) files
            }
    pure . filter (not . null . tpkgFiles) $ rootPkg:otherPkgs
  where
    doesDirectoryExist' path = doesDirectoryExist (srcDir </> path)
    isModule dir file = (isSourceFile file &&) <$>
        doesFileExist (srcDir </> dir </> file)
    srcDir = dcfgSrcDir dcfg


isSourceFile :: FilePath -> Bool
isSourceFile file = takeExtension file `elem` [".hs", ".lhs"]


isRealDir :: FilePath -> Bool
isRealDir dir = not $ dir `elem` [".", ".."]
