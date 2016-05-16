module Settings.Builders.HsCpp (hsCppBuilderArgs) where

import GHC
import Oracles.Config.Setting
import Predicate
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
