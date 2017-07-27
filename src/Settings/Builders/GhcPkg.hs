module Settings.Builders.GhcPkg (ghcPkgBuilderArgs) where

import Settings.Builders.Common

ghcPkgBuilderArgs :: Args
ghcPkgBuilderArgs = mconcat
    [ builder (GhcPkg Init) ? mconcat [ arg "init", arg =<< getOutput ]

    , builder (GhcPkg Update) ? do
        verbosity <- expr getVerbosity
        mconcat [ arg "update"
                , arg "--force"
                , verbosity < Chatty ? arg "-v0"
                , bootPackageDatabaseArgs
                , arg . pkgInplaceConfig =<< getContext ] ]
