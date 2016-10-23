module Settings.Builders.Ld (ldBuilderArgs) where

import Oracles.Config.Setting
import Predicate
import Settings.Builders.Common

ldBuilderArgs :: Args
ldBuilderArgs = builder Ld ? mconcat [ argStagedSettingList ConfLdLinkerArgs
                                     , arg "-r"
                                     , arg "-o", arg =<< getOutput
                                     , append =<< getInputs ]
