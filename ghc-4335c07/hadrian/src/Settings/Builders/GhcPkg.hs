module Settings.Builders.GhcPkg (ghcPkgBuilderArgs) where

import Settings.Builders.Common

ghcPkgBuilderArgs :: Args
ghcPkgBuilderArgs = mconcat
    [ builder (GhcPkg Init) ? mconcat [ arg "init", arg =<< getOutput ]

    , builder (GhcPkg Update) ? do
        verbosity <- expr getVerbosity
        context   <- getContext
        config    <- expr $ pkgInplaceConfig context
        mconcat [ arg "update"
                , arg "--force"
                , verbosity < Chatty ? arg "-v0"
                , bootPackageDatabaseArgs
                , arg config ] ]
