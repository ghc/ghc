module Settings.Builders.Ld (ldBuilderArgs) where

import Oracles.Config.Setting
import Predicates

ldBuilderArgs :: Args
ldBuilderArgs = builder Ld ? do
    args <- getSettingList . ConfLdLinkerArgs =<< getStage
    mconcat [ append args
            , arg "-r"
            , arg "-o", arg =<< getOutput
            , append =<< getInputs ]
