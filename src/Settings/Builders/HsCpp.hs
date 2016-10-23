module Settings.Builders.HsCpp (hsCppBuilderArgs) where

import GHC
import Oracles.Config.Setting
import Predicate
import Settings.Builders.Common
import Settings.Paths

hsCppBuilderArgs :: Args
hsCppBuilderArgs = builder HsCpp ? do
    stage <- getStage
    mconcat [ argSettingList HsCppArgs
            , arg "-P"
            , arg "-Iincludes"
            , arg $ "-I" ++ generatedPath
            , arg $ "-I" ++ buildPath (vanillaContext stage compiler)
            , arg "-x", arg "c"
            , arg =<< getInput ]
