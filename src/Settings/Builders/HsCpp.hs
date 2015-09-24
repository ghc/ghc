module Settings.Builders.HsCpp (hsCppArgs) where

import Expression
import Oracles
import Predicates (builder)
import Settings.Builders.GhcCabal

-- TODO: why process the result with grep -v '^#pragma GCC'? No such lines!
hsCppArgs :: Args
hsCppArgs = builder HsCpp ? do
    stage <- getStage
    src   <- getSource
    args  <- getSettingList HsCppArgs
    mconcat [ append args
            , arg "-P"
            , cppArgs
            , arg $ "-Icompiler/stage" ++ show (succ stage)
            , arg "-x"
            , arg "c"
            , arg src ]
