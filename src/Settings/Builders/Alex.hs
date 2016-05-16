module Settings.Builders.Alex (alexBuilderArgs) where

import Predicate

alexBuilderArgs :: Args
alexBuilderArgs = builder Alex ? mconcat [ arg "-g"
                                         , arg =<< getInput
                                         , arg "-o", arg =<< getOutput ]
