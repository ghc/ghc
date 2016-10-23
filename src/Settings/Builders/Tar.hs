module Settings.Builders.Tar (tarBuilderArgs) where

import Settings.Builders.Common

tarBuilderArgs :: Args
tarBuilderArgs = builder Tar ? mconcat [ arg "-xf"
                                       , input "*.gz"  ? arg "--gzip"
                                       , input "*.bz2" ? arg "--bzip2"
                                       , arg =<< getInput
                                       , arg "-C", arg =<< getOutput ]
