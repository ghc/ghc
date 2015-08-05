module Settings.GhcM (ghcMArgs) where

import Way
import Util
import Builder
import Expression
import Oracles.PackageData
import Settings.Ghc
import Settings.Util
import Settings.Ways

ghcMArgs :: Args
ghcMArgs = stagedBuilder GhcM ? do
    ways    <- getWays
    hsSrcs  <- getHsSources
    hsArgs  <- getPkgDataList HsArgs
    cppArgs <- getPkgDataList CppArgs
    path    <- getTargetPath
    let buildPath = path -/- "build"
    mconcat
        [ arg "-M"
        , packageGhcArgs
        , includeGhcArgs
        , append hsArgs
        , append . map ("-optP" ++) $ cppArgs
        , arg "-odir"        , arg buildPath
        , arg "-stubdir"     , arg buildPath
        , arg "-hidir"       , arg buildPath
        , arg "-dep-makefile", arg $ buildPath -/- "haskell.deps"
        , append . concatMap (\way -> ["-dep-suffix", wayPrefix way]) $ ways
        , arg "-no-user-package-db" -- TODO: is this needed?
        , arg "-rtsopts"            -- TODO: is this needed?
        , append hsSrcs ]
