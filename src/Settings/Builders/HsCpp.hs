module Settings.Builders.HsCpp (hsCppBuilderArgs) where

import Expression
import GHC
import Oracles
import Predicates (builder)
import Settings.Builders.GhcCabal
import Settings.Paths

hsCppBuilderArgs :: Args
hsCppBuilderArgs = builder HsCpp ? do
    stage <- getStage
    mconcat [ append =<< getSettingList HsCppArgs
            , arg "-P"
            , cppArgs
            , arg $ "-I" ++ targetPath stage compiler
            , arg "-x"
            , arg "c"
            , arg =<< getInput ]
