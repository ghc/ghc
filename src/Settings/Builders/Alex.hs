module Settings.Builders.Alex (alexBuilderArgs) where

import Settings.Builders.Common

alexBuilderArgs :: Args
alexBuilderArgs = builder Alex ? mconcat [ arg "-g"
                                         , arg =<< getInput
                                         , arg "-o", arg =<< getOutput ]
