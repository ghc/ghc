module Settings.Builders.HsCpp (hsCppBuilderArgs) where

import Packages
import Settings.Builders.Common

hsCppBuilderArgs :: Args
hsCppBuilderArgs = builder HsCpp ? do
    stage   <- getStage
    ghcPath <- expr $ buildPath (vanillaContext stage compiler)
    mconcat [ getSettingList HsCppArgs
            , arg "-P"
            , arg "-Irts/include"
            , arg $ "-I" ++ ghcPath
            , arg "-x", arg "c"
            , arg =<< getInput ]
