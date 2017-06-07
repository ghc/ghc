module Rules.Wrappers (
  WrappedBinary(..), Wrapper, ghcWrapper, runGhcWrapper,
  inplaceGhcPkgWrapper, installGhcPkgWrapper, hp2psWrapper,
  hpcWrapper, hsc2hsWrapper
  ) where

import Base
import Expression
import Settings.Install (installPackageDbDirectory)
import Settings.Path (inplacePackageDbDirectory)
import Oracles.Path (getTopDirectory)
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
    return $ unlines
        [ "#!/bin/bash"
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
          ++ " -B" ++ binaryLibPath ++ " ${1+\"$@\"}" ]


runGhcWrapper :: WrappedBinary -> Expr String
runGhcWrapper WrappedBinary{..} = do
    lift $ need [sourcePath -/- "Rules/Wrappers.hs"]
    return $ unlines
        [ "#!/bin/bash"
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
          ++ " -f" ++ (binaryLibPath -/- "bin/ghc-stage2") -- HACK
          ++ " -B" ++ binaryLibPath ++ " ${1+\"$@\"}" ]

inplaceGhcPkgWrapper :: WrappedBinary -> Expr String
inplaceGhcPkgWrapper WrappedBinary{..} = do
    lift $ need [sourcePath -/- "Rules/Wrappers.hs"]
    stage <- getStage
    top <- getTopDirectory
    -- Use the package configuration for the next stage in the wrapper.
    -- The wrapper is generated in StageN, but used in StageN+1.
    let packageDb = top -/- inplacePackageDbDirectory (succ stage)
    return $ unlines
        [ "#!/bin/bash"
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
    return $ unlines
        [ "#!/bin/bash"
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName)
          ++ " --global-package-db " ++ packageDb ++ " ${1+\"$@\"}" ]

hp2psWrapper :: WrappedBinary -> Expr String
hp2psWrapper WrappedBinary{..} = do
    lift $ need [sourcePath -/- "Rules/Wrappers.hs"]
    return $ unlines
        [ "#!/bin/bash"
        , "exec " ++ (binaryLibPath -/- "bin" -/- binaryName) ++ " ${1+\"$@\"}" ]

hpcWrapper :: WrappedBinary -> Expr String
hpcWrapper WrappedBinary{..} = do
    lift $ need [sourcePath -/- "Rules/Wrappers.hs"]
    return $ unlines
        [ "#!/bin/bash"
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
    return $ unlines
        [ "#!/bin/bash"
        , "executablename=\"" ++ executableName ++ "\""
        , "HSC2HS_EXTRA=\"" ++ hsc2hsExtra ++ "\""
        , contents ]
