{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Main where

import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Types.LocalBuildInfo
import Distribution.Verbosity
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Simple.Setup
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
#endif

import System.IO
import System.Directory
import System.FilePath
import System.Environment
import Control.Monad
import Data.Char
import GHC.ResponseFile

main :: IO ()
main = defaultMainWithHooks ghcHooks
  where
    ghcHooks = simpleUserHooks
      { postConf = \args cfg pd lbi -> do
          let verbosity = fromFlagOrDefault minBound (configVerbosity cfg)
          ghcAutogen verbosity lbi
          postConf simpleUserHooks args cfg pd lbi
      }

ghcAutogen :: Verbosity -> LocalBuildInfo -> IO ()
ghcAutogen verbosity lbi@LocalBuildInfo{..} = do
#if MIN_VERSION_Cabal(3,14,0)
  let fromSymPath = interpretSymbolicPathLBI lbi
#else
  let fromSymPath = id
#endif

  -- Get compiler/ root directory from the cabal file
  let Just compilerRoot = (takeDirectory . fromSymPath) <$> pkgDescrFile

  let platformHostFile = "GHC/Platform/Host.hs"
      platformHostPath = fromSymPath (autogenPackageModulesDir lbi) </> platformHostFile
      ghcVersionFile = "GHC/Version.hs"
      ghcVersionPath = fromSymPath (autogenPackageModulesDir lbi) </> ghcVersionFile

  -- Get compiler settings
  settings <- lookupEnv "HADRIAN_SETTINGS" >>= \case
    Just settings -> pure $ Left $ read settings
    Nothing -> do
      (ghc,withPrograms) <- requireProgram normal ghcProgram withPrograms
      Right . read <$> getProgramOutput normal ghc ["--info"]

  -- Write GHC.Platform.Host
  createDirectoryIfMissingVerbose verbosity True (takeDirectory platformHostPath)
  rewriteFileEx verbosity platformHostPath (generatePlatformHostHs settings)

  -- Write GHC.Version
  createDirectoryIfMissingVerbose verbosity True (takeDirectory ghcVersionPath)
  rewriteFileEx verbosity ghcVersionPath (generateVersionHs settings)

-- | Takes either a list of hadrian generated settings, or a list of settings from ghc --info,
-- and keys in both lists, and looks up the value in the appropriate list
getSetting :: Either [(String,String)] [(String,String)] -> String -> String -> Either String String
getSetting settings kh kr = case settings of
  Left settings -> go settings kh
  Right settings -> go settings kr
  where
    go settings k =  case lookup k settings of
      Nothing -> Left (show k ++ " not found in settings: " ++ show settings)
      Just v -> Right v

generatePlatformHostHs :: Either [(String,String)] [(String,String)] -> String
generatePlatformHostHs settings = either error id $ do
    let getSetting' = getSetting settings
    cHostPlatformArch <- getSetting' "hostPlatformArch" "target arch"
    cHostPlatformOS   <- getSetting' "hostPlatformOS"   "target os"
    return $ unlines
        [ "module GHC.Platform.Host where"
        , ""
        , "import GHC.Platform.ArchOS"
        , ""
        , "hostPlatformArch :: Arch"
        , "hostPlatformArch = " ++ cHostPlatformArch
        , ""
        , "hostPlatformOS   :: OS"
        , "hostPlatformOS   = " ++ cHostPlatformOS
        , ""
        , "hostPlatformArchOS :: ArchOS"
        , "hostPlatformArchOS = ArchOS hostPlatformArch hostPlatformOS"
        ]

generateVersionHs :: Either [(String,String)] [(String,String)] -> String
generateVersionHs settings = either error id $ do
    let getSetting' = getSetting settings
    cProjectGitCommitId <- getSetting' "cProjectGitCommitId" "Project Git commit id"
    cProjectVersion     <- getSetting' "cProjectVersion"     "Project version"
    cProjectVersionInt  <- getSetting' "cProjectVersionInt"  "Project Version Int"

    cProjectPatchLevel  <- getSetting' "cProjectPatchLevel"  "Project Patch Level"
    cProjectPatchLevel1 <- getSetting' "cProjectPatchLevel1" "Project Patch Level1"
    cProjectPatchLevel2 <- getSetting' "cProjectPatchLevel2" "Project Patch Level2"
    return $ unlines
        [ "module GHC.Version where"
        , ""
        , "import Prelude -- See Note [Why do we import Prelude here?]"
        , ""
        , "cProjectGitCommitId   :: String"
        , "cProjectGitCommitId   = " ++ show cProjectGitCommitId
        , ""
        , "cProjectVersion       :: String"
        , "cProjectVersion       = " ++ show cProjectVersion
        , ""
        , "cProjectVersionInt    :: String"
        , "cProjectVersionInt    = " ++ show cProjectVersionInt
        , ""
        , "cProjectPatchLevel    :: String"
        , "cProjectPatchLevel    = " ++ show cProjectPatchLevel
        , ""
        , "cProjectPatchLevel1   :: String"
        , "cProjectPatchLevel1   = " ++ show cProjectPatchLevel1
        , ""
        , "cProjectPatchLevel2   :: String"
        , "cProjectPatchLevel2   = " ++ show cProjectPatchLevel2
        ]
