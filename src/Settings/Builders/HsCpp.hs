module Settings.Builders.HsCpp (hsCppArgs) where

import Expression
import Predicates (builder)
import Settings.Builders.GhcCabal

-- TODO: why process the result with grep -v '^#pragma GCC'? No such lines!
hsCppArgs :: Args
hsCppArgs = builder HsCpp ? do
    stage <- getStage
    src   <- getSource
    mconcat [ arg "-P"
            , cppArgs
            , arg $ "-Icompiler/stage" ++ show stage
            , arg "-x"
            , arg "c"
            , arg src ]
