{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ComponentName (ComponentName(CLibName))
import Distribution.Types.LocalBuildInfo
import Distribution.Types.LibraryName (LibraryName(LMainLibName))
import Distribution.Verbosity
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Simple.Setup

import System.IO
import System.Process
import System.Directory
import System.FilePath
import Control.Monad
import Data.Char
import qualified Data.Map as Map
import GHC.ResponseFile
import System.Environment

main :: IO ()
main = defaultMainWithHooks ghcHooks
  where
    ghcHooks = simpleUserHooks
      { postConf = \args cfg pd lbi -> do
          let verbosity = fromFlagOrDefault minBound (configVerbosity cfg)
          ghcAutogen verbosity lbi
          postConf simpleUserHooks args cfg pd lbi
      }

-- Mapping from primop-*.hs-incl file to command
primopIncls :: [(String,String)]
primopIncls =
    [ ("primop-data-decl.hs-incl"         , "--data-decl")
    , ("primop-tag.hs-incl"               , "--primop-tag")
    , ("primop-list.hs-incl"              , "--primop-list")
    , ("primop-effects.hs-incl"           , "--primop-effects")
    , ("primop-out-of-line.hs-incl"       , "--out-of-line")
    , ("primop-commutable.hs-incl"        , "--commutable")
    , ("primop-code-size.hs-incl"         , "--code-size")
    , ("primop-strictness.hs-incl"        , "--strictness")
    , ("primop-is-work-free.hs-incl"      , "--is-work-free")
    , ("primop-is-cheap.hs-incl"          , "--is-cheap")
    , ("primop-fixity.hs-incl"            , "--fixity")
    , ("primop-primop-info.hs-incl"       , "--primop-primop-info")
    , ("primop-vector-uniques.hs-incl"    , "--primop-vector-uniques")
    , ("primop-vector-tys.hs-incl"        , "--primop-vector-tys")
    , ("primop-vector-tys-exports.hs-incl", "--primop-vector-tys-exports")
    , ("primop-vector-tycons.hs-incl"     , "--primop-vector-tycons")
    , ("primop-docs.hs-incl"              , "--wired-in-docs")
    ]

ghcAutogen :: Verbosity -> LocalBuildInfo -> IO ()
ghcAutogen verbosity lbi@LocalBuildInfo{..} = do
  -- Get compiler/ root directory from the cabal file
  let Just compilerRoot = takeDirectory <$> pkgDescrFile

  -- Require the necessary programs
  (gcc   ,withPrograms) <- requireProgram normal gccProgram withPrograms
  (ghc   ,withPrograms) <- requireProgram normal ghcProgram withPrograms

  settings <- read <$> getProgramOutput normal ghc ["--info"]
  -- We are reinstalling GHC
  -- Write primop-*.hs-incl
  let hsCppOpts = case lookup "Haskell CPP flags" settings of
        Just fs -> unescapeArgs fs
        Nothing -> []
      primopsTxtPP = compilerRoot </> "GHC/Builtin/primops.txt.pp"
      cppOpts = hsCppOpts ++ ["-P","-x","c"]
      cppIncludes = map ("-I"++) [compilerRoot]
  -- Preprocess primops.txt.pp
  primopsStr <- getProgramOutput normal gcc (cppOpts ++ cppIncludes ++ [primopsTxtPP])
  -- Call genprimopcode to generate *.hs-incl
  forM_ primopIncls $ \(file,command) -> do
    contents <- readProcess "genprimopcode" [command] primopsStr
#if MIN_VERSION_Cabal(3,11,0)
    rewriteFileEx verbosity (buildDir lbi </> file) contents
#else
    rewriteFileEx verbosity (buildDir </> file) contents
#endif

  -- Write GHC.Platform.Constants
  let platformConstantsPath = autogenPackageModulesDir lbi </> "GHC/Platform/Constants.hs"
      targetOS = case lookup "target os" settings of
        Nothing -> error "no target os in settings"
        Just os -> os
  createDirectoryIfMissingVerbose verbosity True (takeDirectory platformConstantsPath)
  withTempFile (takeDirectory platformConstantsPath) "Constants_tmp.hs" $ \tmp h -> do
    hClose h
    callProcess "deriveConstants" ["--gen-haskell-type","-o",tmp,"--target-os",targetOS]
    renameFile tmp platformConstantsPath

  let cProjectUnitId = case Map.lookup (CLibName LMainLibName) componentNameMap of
                         Just [LibComponentLocalBuildInfo{componentUnitId}] -> unUnitId componentUnitId
                         _ -> error "Couldn't find unique cabal library when building ghc"

  -- Write GHC.Settings.Config
      configHsPath = autogenPackageModulesDir lbi </> "GHC/Settings/Config.hs"
      configHs = generateConfigHs cProjectUnitId settings
  createDirectoryIfMissingVerbose verbosity True (takeDirectory configHsPath)
  rewriteFileEx verbosity configHsPath configHs

getSetting :: [(String,String)] -> String -> String -> Either String String
getSetting settings kh kr = go settings kr
  where
    go settings k =  case lookup k settings of
      Nothing -> Left (show k ++ " not found in settings: " ++ show settings)
      Just v -> Right v

generateConfigHs :: String -- ^ ghc's cabal-generated unit-id, which matches its package-id/key
                 -> [(String,String)] -> String
generateConfigHs cProjectUnitId settings = either error id $ do
    let getSetting' = getSetting $ (("cStage","2"):) settings
    buildPlatform  <- getSetting' "cBuildPlatformString" "Host platform"
    hostPlatform   <- getSetting' "cHostPlatformString" "Target platform"
    cProjectName   <- getSetting' "cProjectName" "Project name"
    cBooterVersion <- getSetting' "cBooterVersion" "Project version"
    cStage         <- getSetting' "cStage" "cStage"
    return $ unlines
        [ "module GHC.Settings.Config"
        , "  ( module GHC.Version"
        , "  , cBuildPlatformString"
        , "  , cHostPlatformString"
        , "  , cProjectName"
        , "  , cBooterVersion"
        , "  , cStage"
        , "  , cProjectUnitId"
        , "  ) where"
        , ""
        , "import GHC.Prelude.Basic"
        , ""
        , "import GHC.Version"
        , ""
        , "cBuildPlatformString :: String"
        , "cBuildPlatformString = " ++ show buildPlatform
        , ""
        , "cHostPlatformString :: String"
        , "cHostPlatformString = " ++ show hostPlatform
        , ""
        , "cProjectName          :: String"
        , "cProjectName          = " ++ show cProjectName
        , ""
        , "cBooterVersion        :: String"
        , "cBooterVersion        = " ++ show cBooterVersion
        , ""
        , "cStage                :: String"
        , "cStage                = show ("++ cStage ++ " :: Int)"
        , ""
        , "cProjectUnitId :: String"
        , "cProjectUnitId = " ++ show cProjectUnitId
        ]
