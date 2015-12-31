module Settings.Builders.GhcPkg (ghcPkgArgs) where

import Base
import Builder
import Expression
import Predicates
import Settings
import Settings.Builders.GhcCabal

ghcPkgArgs :: Args
ghcPkgArgs = stagedBuilder GhcPkg ? (initArgs <> updateArgs)

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
