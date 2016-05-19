module Settings.Builders.Tar (tarBuilderArgs) where

import Predicate

tarBuilderArgs :: Args
tarBuilderArgs = builder Tar ? do
    mconcat [ arg "-xf"
            , input "*.gz"  ? arg "--gzip"
            , input "*.bz2" ? arg "--bzip2"
            , arg =<< getInput
            , arg "-C", arg =<< getOutput ]
