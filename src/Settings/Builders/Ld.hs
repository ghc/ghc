module Settings.Builders.Ld (ldBuilderArgs) where

import Settings.Builders.Common

ldBuilderArgs :: Args
ldBuilderArgs = builder Ld ? mconcat [ argStagedSettingList ConfLdLinkerArgs
                                     , arg "-r"
                                     , arg "-o", arg =<< getOutput
                                     , append =<< getInputs ]
