module Settings.Builders.Ld (ldArgs) where

import Expression
import Oracles
import Predicates (builder)

ldArgs :: Args
ldArgs = builder Ld ? do
    args <- getSettingList . ConfLdLinkerArgs =<< getStage
    mconcat [ append args
            , arg "-r"
            , arg "-o", arg =<< getOutput
            , append =<< getInputs ]
