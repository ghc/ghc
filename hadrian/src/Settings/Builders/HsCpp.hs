module Settings.Builders.HsCpp (hsCppBuilderArgs) where

import Packages
import Settings.Builders.Common

hsCppBuilderArgs :: Args
hsCppBuilderArgs = builder HsCpp ? do
    stage   <- getStage
    ghcPath <- expr $ buildPath (vanillaContext stage compiler)
    libPath <- expr $ stageLibPath stage
    mconcat [ getSettingList HsCppArgs
            , arg "-P"
            , arg "-Iincludes"
            , arg $ "-I" ++ libPath
            , arg $ "-I" ++ ghcPath
            , arg "-x", arg "c"
            , arg =<< getInput ]
