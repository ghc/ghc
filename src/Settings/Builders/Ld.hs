module Settings.Builders.Ld (ldBuilderArgs) where

import Expression
import Oracles
import Predicates (builder)

ldBuilderArgs :: Args
ldBuilderArgs = builder Ld ? do
    args <- getSettingList . ConfLdLinkerArgs =<< getStage
    mconcat [ append args
            , arg "-r"
            , arg "-o", arg =<< getOutput
            , append =<< getInputs ]
