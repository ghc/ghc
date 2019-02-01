module Settings.Builders.Happy (happyBuilderArgs) where

import Settings.Builders.Common

happyBuilderArgs :: Args
happyBuilderArgs = builder Happy ? mconcat [ arg "-ag" -- TODO (int-index): restore the -c option when happy/pull/134 is merged.
                                           , arg "--strict"
                                           , arg =<< getInput
                                           , arg "-o", arg =<< getOutput ]
