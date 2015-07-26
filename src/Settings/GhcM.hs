module Settings.GhcM (ghcMArgs) where

import Way
import Util
import Stage
import Builder
import Switches
import Expression
import Oracles.Flag
import Oracles.PackageData
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
        , append . map ("-optP" ++) $ cppArgs
        , arg "-odir"        , arg buildPath
        , arg "-stubdir"     , arg buildPath
        , arg "-hidir"       , arg buildPath
        , arg "-dep-makefile", arg $ buildPath -/- "haskell.deps"
        , append . concatMap (\way -> ["-dep-suffix", wayPrefix way]) $ ways
        , append hsArgs
        , arg "-no-user-package-db" -- TODO: is this needed?
        , arg "-rtsopts"            -- TODO: is this needed?
        , append hsSrcs ]


packageGhcArgs :: Args
packageGhcArgs = do
    stage              <- getStage
    supportsPackageKey <- getFlag SupportsPackageKey
    pkgKey             <- getPkgData PackageKey
    pkgDepKeys         <- getPkgDataList DepKeys
    pkgDeps            <- getPkgDataList Deps
    mconcat
        [ arg "-hide-all-packages"
        , arg "-no-user-package-db"
        , arg "-include-pkg-deps"
        , stage0 ? arg "-package-db libraries/bootstrapping.conf"
        , if supportsPackageKey || stage /= Stage0
          then mconcat [ arg $ "-this-package-key " ++ pkgKey
                       , append . map ("-package-key " ++) $ pkgDepKeys ]
          else mconcat [ arg $ "-package-name" ++ pkgKey
                       , append . map ("-package " ++) $ pkgDeps ]]

includeGhcArgs :: Args
includeGhcArgs = do
    path    <- getTargetPath
    pkgPath <- getPackagePath
    srcDirs <- getPkgDataList SrcDirs
    incDirs <- getPkgDataList IncludeDirs
    cppArgs <- getPkgDataList CppArgs
    let buildPath   = path -/- "build"
        autogenPath = buildPath -/- "autogen"
    mconcat
        [ arg "-i"
        , append . map (\dir -> "-i" ++ pkgPath -/- dir) $ srcDirs
        , arg $ "-i" ++ buildPath
        , arg $ "-i" ++ autogenPath
        , arg $ "-I" ++ buildPath
        , arg $ "-I" ++ autogenPath
        , append . map (\dir -> "-I" ++ pkgPath -/- dir) $ incDirs
        , arg "-optP-include", arg $ "-optP" ++ autogenPath -/- "cabal_macros.h"
        , append . map ("-optP" ++) $ cppArgs ]
