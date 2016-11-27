module Settings.Builders.GhcPkg (ghcPkgBuilderArgs) where

import Settings.Builders.Common

ghcPkgBuilderArgs :: Args
ghcPkgBuilderArgs = builder GhcPkg ? (initArgs <> updateArgs)

initPredicate :: Predicate
initPredicate = orM $ map (output . packageDbDirectory) [Stage0 ..]

initArgs :: Args
initArgs = initPredicate ? mconcat [ arg "init", arg =<< getOutput ]

updateArgs :: Args
updateArgs = notM initPredicate ? do
    verbosity <- lift $ getVerbosity
    mconcat [ arg "update"
            , arg "--force"
            , verbosity < Chatty ? arg "-v0"
            , bootPackageDatabaseArgs
            , arg . pkgInplaceConfig =<< getContext ]
