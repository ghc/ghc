module Rules.Wrappers (
    WrappedBinary(..), Wrapper, inplaceWrappers, installWrappers
    ) where

import Hadrian.Oracles.Path

import Base
import Expression
import Oracles.Setting
import Settings

-- | Wrapper is an expression depending on (i) the 'FilePath' to the library and
-- (ii) the name of the wrapped binary.
data WrappedBinary = WrappedBinary
    { binaryLibPath :: FilePath
    , binaryName    :: String }

type Wrapper = WrappedBinary -> Expr String

ghcWrapper :: WrappedBinary -> Expr String
ghcWrapper WrappedBinary{..} = do
    expr $ need [sourcePath -/- "Rules/Wrappers.hs"]
    bash <- expr bashPath
    return $ unlines
        [ "#!"++bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
          ++ " -B" ++ binaryLibPath ++ " ${1+\"$@\"}" ]

inplaceRunGhcWrapper :: WrappedBinary -> Expr String
inplaceRunGhcWrapper WrappedBinary{..} = do
    expr $ need [sourcePath -/- "Rules/Wrappers.hs"]
    bash <- expr bashPath
    return $ unlines
        [ "#!"++bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
          ++ " -f" ++ (binaryLibPath -/- "bin/ghc-stage2") -- TODO: use ProgramName
          ++ " -B" ++ binaryLibPath ++ " ${1+\"$@\"}" ]

installRunGhcWrapper :: WrappedBinary -> Expr String
installRunGhcWrapper WrappedBinary{..} = do
    expr $ need [sourcePath -/- "Rules/Wrappers.hs"]
    bash <- expr bashPath
    return $ unlines
        [ "#!"++bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
          ++ " -f" ++ (binaryLibPath -/- "bin/ghc") -- TODO: use ProgramName
          ++ " -B" ++ binaryLibPath ++ " ${1+\"$@\"}" ]

inplaceGhcPkgWrapper :: WrappedBinary -> Expr String
inplaceGhcPkgWrapper WrappedBinary{..} = do
    expr $ need [sourcePath -/- "Rules/Wrappers.hs"]
    top <- expr topDirectory
    -- The wrapper is generated in StageN, but used in StageN+1. Therefore, we
    -- always use the inplace package database, located at 'inplacePackageDbPath',
    -- which is used in Stage1 and later.
    bash <- expr bashPath
    return $ unlines
        [ "#!" ++ bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName) ++
          " --global-package-db " ++ top -/- inplacePackageDbPath ++ " ${1+\"$@\"}" ]

installGhcPkgWrapper :: WrappedBinary -> Expr String
installGhcPkgWrapper WrappedBinary{..} = do
    expr $ need [sourcePath -/- "Rules/Wrappers.hs"]
    stage <- getStage
    top   <- expr topDirectory
    -- Use the package configuration for the next stage in the wrapper.
    -- The wrapper is generated in StageN, but used in StageN+1.
    packageDb <- expr $ installPackageDbPath binaryLibPath top (succ stage)
    bash <- expr bashPath
    return $ unlines
        [ "#!"++bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
          ++ " --global-package-db " ++ packageDb ++ " ${1+\"$@\"}" ]

hp2psWrapper :: WrappedBinary -> Expr String
hp2psWrapper WrappedBinary{..} = do
    expr $ need [sourcePath -/- "Rules/Wrappers.hs"]
    bash <- expr bashPath
    return $ unlines
        [ "#!"++bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName) ++ " ${1+\"$@\"}" ]

hpcWrapper :: WrappedBinary -> Expr String
hpcWrapper WrappedBinary{..} = do
    expr $ need [sourcePath -/- "Rules/Wrappers.hs"]
    bash <- expr bashPath
    return $ unlines
        [ "#!"++bash
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName) ++ " ${1+\"$@\"}" ]

hsc2hsWrapper :: WrappedBinary -> Expr String
hsc2hsWrapper WrappedBinary{..} = do
    top <- expr topDirectory
    expr $ need [ sourcePath -/- "Rules/Wrappers.hs" ]
    contents <- expr $ readFile' $ top -/- "utils/hsc2hs/hsc2hs.wrapper"
    let executableName = binaryLibPath -/- "bin" -/- binaryName
    confCcArgs <- expr $ settingList (ConfCcArgs Stage1)
    confGccLinkerArgs <- expr $ settingList (ConfGccLinkerArgs Stage1)
    let hsc2hsExtra = unwords (map ("-cflags=" ++) confCcArgs) ++ " " ++
                      unwords (map ("-lflags=" ++) confGccLinkerArgs)
    bash <- expr bashPath
    return $ unlines
        [ "#!"++bash
        , "executablename=\"" ++ executableName ++ "\""
        , "HSC2HS_EXTRA=\"" ++ hsc2hsExtra ++ "\""
        , contents ]

haddockWrapper :: WrappedBinary -> Expr String
haddockWrapper WrappedBinary{..} = do
  expr $ need [sourcePath -/- "Rules/Wrappers.hs"]
  return $ unlines
    [ "#!/bin/bash"
    , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
      ++ " -B" ++ binaryLibPath ++ " -l" ++ binaryLibPath ++ " ${1+\"$@\"}" ]

iservBinWrapper :: WrappedBinary -> Expr String
iservBinWrapper WrappedBinary{..} = do
    expr $ need [sourcePath -/- "Rules/Wrappers.hs"]
    stage <- getStage
    stageLibraries <- expr $ filter isLibrary <$> stagePackages stage
    -- TODO: Figure our the reason of this hardcoded exclusion
    let pkgs = stageLibraries \\ [ cabal, process, haskeline
                                 , terminfo, ghcCompact, hpc, compiler ]
    contexts <- expr $ concatForM pkgs $ \p -> do
        maybeStage <- installStage p
        return [ vanillaContext s p | s <- maybeToList maybeStage ]
    buildPaths <- expr $ mapM buildPath contexts
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

-- | In the final installation path specified by @DEST@, there is another
-- @package.conf.d@ different from 'inplacePackageDbPath' defined in "Base".
installPackageDbPath :: FilePath -> FilePath -> Stage -> Action FilePath
installPackageDbPath _ top Stage0 = do
    path <- buildRoot
    return $ top -/- path -/- "stage0/bootstrapping.conf"
installPackageDbPath libdir _ _ = return $ libdir -/- "package.conf.d"
