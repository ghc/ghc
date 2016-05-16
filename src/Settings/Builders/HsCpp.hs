module Settings.Builders.HsCpp (hsCppBuilderArgs) where

import Expression
import GHC
import Oracles.Config.Setting
import Predicates
import Settings.Builders.GhcCabal
import Settings.Paths

hsCppBuilderArgs :: Args
hsCppBuilderArgs = builder HsCpp ? do
    stage <- getStage
    mconcat [ append =<< getSettingList HsCppArgs
            , arg "-P"
            , cppArgs
            , arg $ "-I" ++ buildPath (vanillaContext stage compiler)
            , arg "-x"
            , arg "c"
            , arg =<< getInput ]
