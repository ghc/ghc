module Rules.Wrappers (
  WrappedBinary(..), Wrapper, inplaceWrappers, installWrappers
  ) where

import Base
import Expression
import GHC
import Settings (getPackages, latestBuildStage)
import Settings.Install (installPackageDbDirectory)
import Settings.Path (buildPath, inplacePackageDbDirectory)
import Oracles.Path (getTopDirectory, bashPath)
import Oracles.Config.Setting (SettingList(..), settingList)

-- | Wrapper is an expression depending on the 'FilePath' to the
-- | library path and name of the wrapped binary.
data WrappedBinary = WrappedBinary {
  binaryLibPath :: FilePath,
  binaryName    :: String
}

type Wrapper = WrappedBinary -> Expr String

ghcWrapper :: WrappedBinary -> Expr String
ghcWrapper WrappedBinary{..} = do
    lift $ need [sourcePath -/- "Rules/Wrappers.hs"]
    bash <- lift bashPath
    return $ unlines
        [ "#!"++bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
          ++ " -B" ++ binaryLibPath ++ " ${1+\"$@\"}" ]

inplaceRunGhcWrapper :: WrappedBinary -> Expr String
inplaceRunGhcWrapper WrappedBinary{..} = do
    lift $ need [sourcePath -/- "Rules/Wrappers.hs"]
    bash <- lift bashPath
    return $ unlines
        [ "#!"++bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
          ++ " -f" ++ (binaryLibPath -/- "bin/ghc-stage2") -- TODO: use ProgramName
          ++ " -B" ++ binaryLibPath ++ " ${1+\"$@\"}" ]

installRunGhcWrapper :: WrappedBinary -> Expr String
installRunGhcWrapper WrappedBinary{..} = do
    lift $ need [sourcePath -/- "Rules/Wrappers.hs"]
    bash <- lift bashPath
    return $ unlines
        [ "#!"++bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
          ++ " -f" ++ (binaryLibPath -/- "bin/ghc") -- TODO: use ProgramName
          ++ " -B" ++ binaryLibPath ++ " ${1+\"$@\"}" ]

inplaceGhcPkgWrapper :: WrappedBinary -> Expr String
inplaceGhcPkgWrapper WrappedBinary{..} = do
    lift $ need [sourcePath -/- "Rules/Wrappers.hs"]
    stage <- getStage
    top <- getTopDirectory
    -- Use the package configuration for the next stage in the wrapper.
    -- The wrapper is generated in StageN, but used in StageN+1.
    let packageDb = top -/- inplacePackageDbDirectory (succ stage)
    bash <- lift bashPath
    return $ unlines
        [ "#!"++bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
          ++ " --global-package-db " ++ packageDb ++ " ${1+\"$@\"}" ]

installGhcPkgWrapper :: WrappedBinary -> Expr String
installGhcPkgWrapper WrappedBinary{..} = do
    lift $ need [sourcePath -/- "Rules/Wrappers.hs"]
    stage <- getStage
    top <- getTopDirectory
    -- Use the package configuration for the next stage in the wrapper.
    -- The wrapper is generated in StageN, but used in StageN+1.
    let packageDb = installPackageDbDirectory binaryLibPath top (succ stage)
    bash <- lift bashPath
    return $ unlines
        [ "#!"++bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
          ++ " --global-package-db " ++ packageDb ++ " ${1+\"$@\"}" ]

hp2psWrapper :: WrappedBinary -> Expr String
hp2psWrapper WrappedBinary{..} = do
    lift $ need [sourcePath -/- "Rules/Wrappers.hs"]
    bash <- lift bashPath
    return $ unlines
        [ "#!"++bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName) ++ " ${1+\"$@\"}" ]

hpcWrapper :: WrappedBinary -> Expr String
hpcWrapper WrappedBinary{..} = do
    lift $ need [sourcePath -/- "Rules/Wrappers.hs"]
    bash <- lift bashPath
    return $ unlines
        [ "#!"++bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName) ++ " ${1+\"$@\"}" ]

hsc2hsWrapper :: WrappedBinary -> Expr String
hsc2hsWrapper WrappedBinary{..} = do
    top <- getTopDirectory
    lift $ need [ sourcePath -/- "Rules/Wrappers.hs" ]
    contents <- lift $ readFile' $ top -/- "utils/hsc2hs/hsc2hs.wrapper"
    let executableName = binaryLibPath -/- "bin" -/- binaryName
    confCcArgs <- lift $ settingList (ConfCcArgs Stage1)
    confGccLinkerArgs <- lift $ settingList (ConfGccLinkerArgs Stage1)
    let hsc2hsExtra = unwords (map ("-cflags=" ++) confCcArgs) ++ " " ++
                      unwords (map ("-lflags=" ++) confGccLinkerArgs)
    bash <- lift bashPath
    return $ unlines
        [ "#!"++bash
        , "executablename=\"" ++ executableName ++ "\""
        , "HSC2HS_EXTRA=\"" ++ hsc2hsExtra ++ "\""
        , contents ]

haddockWrapper :: WrappedBinary -> Expr String
haddockWrapper WrappedBinary{..} = do
  lift $ need [sourcePath -/- "Rules/Wrappers.hs"]
  return $ unlines
    [ "#!/bin/bash"
    , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
      ++ " -B" ++ binaryLibPath ++ " -l" ++ binaryLibPath ++ " ${1+\"$@\"}" ]

iservBinWrapper :: WrappedBinary -> Expr String
iservBinWrapper WrappedBinary{..} = do
    lift $ need [sourcePath -/- "Rules/Wrappers.hs"]
    activePackages <- filter isLibrary <$> getPackages
    -- TODO: Figure our the reason of this hardcoded exclusion
    let pkgs = activePackages \\ [ cabal, process, haskeline
                                 , terminfo, ghcCompact, hpc, compiler ]
    contexts <- catMaybes <$> mapM (\p -> do
                                        m <- lift $ latestBuildStage p
                                        return $ fmap (\s -> vanillaContext s p) m
                                   ) pkgs
    let buildPaths = map buildPath contexts
    return $ unlines
        [ "#!/bin/bash"
        , "export DYLD_LIBRARY_PATH=\"" ++ intercalate ":" buildPaths ++
          "${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}\""
       , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName) ++ " ${1+\"$@\"}" ]

wrappersCommon :: [(Context, Wrapper)]
wrappersCommon = [ (vanillaContext Stage0 ghc   , ghcWrapper)
                 , (vanillaContext Stage1 ghc   , ghcWrapper)
                 , (vanillaContext Stage1 hp2ps , hp2psWrapper)
                 , (vanillaContext Stage1 hpc   , hpcWrapper)
                 , (vanillaContext Stage1 hsc2hs, hsc2hsWrapper)
                 , (vanillaContext Stage2 haddock, haddockWrapper)
                 , (vanillaContext Stage1 iservBin, iservBinWrapper) ]

-- | List of wrappers for inplace artefacts
inplaceWrappers :: [(Context, Wrapper)]
inplaceWrappers = wrappersCommon ++
                  [ (vanillaContext Stage0 ghcPkg, inplaceGhcPkgWrapper)
                  , (vanillaContext Stage1 runGhc, inplaceRunGhcWrapper) ]

-- | List of wrappers for installation
installWrappers :: [(Context, Wrapper)]
installWrappers = wrappersCommon ++
                  [ (vanillaContext Stage0 ghcPkg, installGhcPkgWrapper)
                  , (vanillaContext Stage1 runGhc, installRunGhcWrapper) ]
