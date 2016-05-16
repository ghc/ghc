module Settings.Builders.GhcPkg (ghcPkgBuilderArgs) where

import Base
import Builder
import Predicates
import Settings
import Settings.Builders.GhcCabal

ghcPkgBuilderArgs :: Args
ghcPkgBuilderArgs = builder GhcPkg ? (initArgs <> updateArgs)

initPredicate :: Predicate
initPredicate = orM $ map (output . packageDbDirectory) [Stage0 ..]

initArgs :: Args
initArgs = initPredicate ? do
    mconcat [ arg "init"
            , arg =<< getOutput ]

-- TODO: move inplace-pkg-config to buildRootPath, see #113.
updateArgs :: Args
updateArgs = notM initPredicate ? do
    pkg <- getPackage
    dir <- getContextDirectory
    mconcat [ arg "update"
            , arg "--force"
            , bootPackageDbArgs
            , arg $ pkgPath pkg -/- dir -/- "inplace-pkg-config" ]
