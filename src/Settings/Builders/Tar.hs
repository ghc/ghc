module Settings.Builders.Tar (tarBuilderArgs) where

import Settings.Builders.Common

tarBuilderArgs :: Args
tarBuilderArgs =
    mconcat [ builder (Tar Create) ? mconcat
                [ arg "-c"
                , output "//*.gz" ? arg "--gzip"
                , output "//*.bz2" ? arg "--bzip2"
                , output "//*.xz" ? arg "--xz"
                , arg "-f", arg =<< getOutput
                , getInputs ]
            , builder (Tar Extract) ? mconcat
                [ arg "-x"
                , input "*.gz"  ? arg "--gzip"
                , input "*.bz2" ? arg "--bzip2"
                , input "*.xz" ? arg "--xz"
                , arg "-f", arg =<< getInput
                , arg "-C", arg =<< getOutput ] ]
