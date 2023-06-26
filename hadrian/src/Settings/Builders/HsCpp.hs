module Settings.Builders.HsCpp (hsCppBuilderArgs) where

import Packages
import Settings.Builders.Common
import qualified GHC.Toolchain as T
import GHC.Toolchain.Program

hsCppBuilderArgs :: Args
hsCppBuilderArgs = builder HsCpp ? do
    stage   <- getStage
    ghcPath <- expr $ buildPath (vanillaContext stage compiler)
    mconcat [ prgFlags . T.hsCppProgram . T.tgtHsCPreprocessor <$> getStagedTarget
            , arg "-P"
            , arg "-Irts/include"
            , arg $ "-I" ++ ghcPath
            , arg "-x", arg "c"
            , arg =<< getInput ]
