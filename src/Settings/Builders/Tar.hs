module Settings.Builders.Tar (tarBuilderArgs) where

import Expression
import Predicates (builder)

tarBuilderArgs :: Args
tarBuilderArgs = builder Tar ? do
    mconcat [ arg "-xzf"
            , arg =<< getInput
            , arg "-C", arg =<< getOutput ]
