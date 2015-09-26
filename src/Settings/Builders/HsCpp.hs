module Settings.Builders.HsCpp (hsCppArgs) where

import Expression
import Oracles
import Predicates (builder)
import Settings.Builders.GhcCabal

hsCppArgs :: Args
hsCppArgs = builder HsCpp ? do
    stage <- getStage
    mconcat [ append =<< getSettingList HsCppArgs
            , arg "-P"
            , cppArgs
            , arg $ "-Icompiler/stage" ++ show (succ stage)
            , arg "-x"
            , arg "c"
            , arg =<< getInput ]
