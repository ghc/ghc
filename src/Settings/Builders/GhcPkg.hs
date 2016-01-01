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
initPredicate = orM $ map (file . packageConfiguration) [Stage0 ..]

initArgs :: Args
initArgs = initPredicate ? do
    mconcat [ arg "init"
            , arg =<< getOutput ]

updateArgs :: Args
updateArgs = notM initPredicate ? do
    path <- getTargetPath
    mconcat [ arg "update"
            , arg "--force"
            , bootPackageDbArgs
            , arg $ path -/- "inplace-pkg-config" ]
