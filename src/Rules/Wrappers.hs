module Rules.Wrappers (
  WrappedBinary(..), Wrapper, ghcWrapper, runGhcWrapper,
  inplaceGhcPkgWrapper, installGhcPkgWrapper
  ) where

import Base
import Expression (Expr, getStage)
import Settings.Install (installPackageDbDirectory)
import Settings.Path (inplacePackageDbDirectory)
import Oracles.Path (getTopDirectory)

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
