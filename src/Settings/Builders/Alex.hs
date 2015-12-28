module Settings.Builders.Alex (alexArgs) where

import Expression
import Predicates (builder)

alexArgs :: Args
alexArgs = builder Alex ? mconcat [ arg "-g"
                                  , arg =<< getInput
                                  , arg "-o", arg =<< getOutput ]
