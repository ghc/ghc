{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Types.LocalBuildInfo
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
    , ("primop-has-side-effects.hs-incl"  , "--has-side-effects")
    , ("primop-out-of-line.hs-incl"       , "--out-of-line")
    , ("primop-commutable.hs-incl"        , "--commutable")
    , ("primop-code-size.hs-incl"         , "--code-size")
    , ("primop-can-fail.hs-incl"          , "--can-fail")
    , ("primop-strictness.hs-incl"        , "--strictness")
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

  settings <- lookupEnv "HADRIAN_SETTINGS" >>= \case
    Just settings -> pure $ Left $ read settings
    Nothing -> do
      (ghc,withPrograms) <- requireProgram normal ghcProgram withPrograms
      Right . read <$> getProgramOutput normal ghc ["--info"]
  case settings of
    Left _ -> pure () -- hadrian will call genprimopcode and deriveConstants
                      -- This way we can have nice, sound recompilation checking
    Right settings -> do
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
        rewriteFileEx verbosity (buildDir </> file) contents

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

  -- Write GHC.Settings.Config
  let configHsPath = autogenPackageModulesDir lbi </> "GHC/Settings/Config.hs"
      configHs = generateConfigHs settings
  createDirectoryIfMissingVerbose verbosity True (takeDirectory configHsPath)
  rewriteFileEx verbosity configHsPath configHs

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

generateConfigHs :: Either [(String,String)] [(String,String)] -> String
generateConfigHs settings = either error id $ do
    let getSetting' = getSetting $ fmap (("cStage","2"):) settings
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
        , "  ) where"
        , ""
        , "import GHC.Prelude"
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
        ]
