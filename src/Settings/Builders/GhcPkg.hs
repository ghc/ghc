module Settings.Builders.GhcPkg (ghcPkgBuilderArgs) where

import Base
import Predicate
import Settings
import Settings.Builders.GhcCabal
import Settings.Paths

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
    mconcat [ arg "update"
            , arg "--force"
            , bootPackageDatabaseArgs
            , arg $ pkgPath pkg -/- dir -/- "inplace-pkg-config" ]
