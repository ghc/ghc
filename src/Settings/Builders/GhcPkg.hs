module Settings.Builders.GhcPkg (ghcPkgBuilderArgs) where

import Settings.Builders.Common

ghcPkgBuilderArgs :: Args
ghcPkgBuilderArgs = builder GhcPkg ? (initArgs <> updateArgs)

initPredicate :: Predicate
initPredicate = orM $ map (output . packageDbDirectory) [Stage0 ..]

initArgs :: Args
initArgs = initPredicate ? mconcat [ arg "init", arg =<< getOutput ]

-- TODO: Move inplace-pkg-config to buildRootPath, see #113.
updateArgs :: Args
updateArgs = notM initPredicate ? do
    pkg <- getPackage
    dir <- getContextDirectory
    verbosity <- lift $ getVerbosity
    mconcat [ arg "update"
            , arg "--force"
            , verbosity < Chatty ? arg "-v0"
            , bootPackageDatabaseArgs
            , arg $ pkgPath pkg -/- dir -/- "inplace-pkg-config" ]
