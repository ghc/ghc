module Settings.Builders.Happy (happyBuilderArgs) where

import Expression
import Predicates (builder)

happyBuilderArgs :: Args
happyBuilderArgs = builder Happy ? mconcat [ arg "-agc"
                                           , arg "--strict"
                                           , arg =<< getInput
                                           , arg "-o", arg =<< getOutput ]
