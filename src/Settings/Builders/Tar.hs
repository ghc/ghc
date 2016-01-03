module Settings.Builders.Tar (tarBuilderArgs) where

import Base
import Expression
import Predicates (builder)

tarBuilderArgs :: Args
tarBuilderArgs = builder Tar ? do
    input <- getInput
    mconcat [ arg "-xf"
            , ("*.gz"  ?== input) ? arg "--gzip"
            , ("*.bz2" ?== input) ? arg "--bzip2"
            , arg input
            , arg "-C", arg =<< getOutput ]
