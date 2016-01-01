module Settings.Builders.HsCpp (hsCppBuilderArgs) where

import Expression
import Oracles
import Predicates (builder)
import Settings.Builders.GhcCabal

hsCppBuilderArgs :: Args
hsCppBuilderArgs = builder HsCpp ? do
    stage <- getStage
    mconcat [ append =<< getSettingList HsCppArgs
            , arg "-P"
            , cppArgs
            , arg $ "-Icompiler/" ++ stageString stage
            , arg "-x"
            , arg "c"
            , arg =<< getInput ]
