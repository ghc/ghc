module Settings.Builders.Ld (ldArgs) where

import Expression
import Oracles
import Predicates (builder)

ldArgs :: Args
ldArgs = builder Ld ? do
    file <- getFile
    objs <- getSources
    args <- getSettingList . ConfLdLinkerArgs =<< getStage
    mconcat [ append args
            , arg "-r"
            , arg "-o", arg file
            , append objs ]
