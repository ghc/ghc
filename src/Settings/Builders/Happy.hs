module Settings.Builders.Happy (happyArgs) where

import Expression
import Predicates (builder)

happyArgs :: Args
happyArgs = builder Happy ? mconcat [ arg "-agc"
                                    , arg "--strict"
                                    , arg =<< getInput
                                    , arg "-o", arg =<< getOutput ]
