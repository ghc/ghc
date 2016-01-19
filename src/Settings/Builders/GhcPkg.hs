module Settings.Builders.GhcPkg (ghcPkgBuilderArgs) where

import Base
import Builder
import Expression
import Predicates
import Settings
import Settings.Builders.GhcCabal

ghcPkgBuilderArgs :: Args
ghcPkgBuilderArgs = stagedBuilder GhcPkg ? (initArgs <> updateArgs)

initPredicate :: Predicate
initPredicate = orM $ map (file . packageDbDirectory) [Stage0 ..]

initArgs :: Args
initArgs = initPredicate ? do
    mconcat [ arg "init"
            , arg =<< getOutput ]

-- TODO: move inplace-pkg-config to buildRootPath, see #113.
updateArgs :: Args
updateArgs = notM initPredicate ? do
    pkg       <- getPackage
    targetDir <- getTargetDirectory
    mconcat [ arg "update"
            , arg "--force"
            , bootPackageDbArgs
            , arg $ pkgPath pkg -/- targetDir -/- "inplace-pkg-config" ]
