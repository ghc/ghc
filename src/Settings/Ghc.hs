module Settings.Ghc (ghcArgs, packageGhcArgs, includeGhcArgs) where

import Way
import Util
import Stage
import Builder
import Switches
import Expression
import Oracles.Flag
import Oracles.PackageData
import Settings.Util

ghcArgs :: Args
ghcArgs = stagedBuilder Ghc ? do
    way     <- getWay
    hsArgs  <- getPkgDataList HsArgs
    cppArgs <- getPkgDataList CppArgs
    srcs    <- getDependencies
    file    <- getFile
    path    <- getTargetPath
    let buildPath = path -/- "build"
    mconcat
        [ arg "-hisuf", arg $ hisuf way
        , arg "-osuf" , arg $  osuf way
        , arg "-hcsuf", arg $ hcsuf way
        , wayHcArgs
        , packageGhcArgs
        , includeGhcArgs
        , append hsArgs
        , append . map ("-optP" ++) $ cppArgs
        , arg "-odir"   , arg buildPath
        , arg "-stubdir", arg buildPath
        , arg "-hidir"  , arg buildPath
        , splitObjects  ? arg "-split-objs"
        , arg "-no-user-package-db" -- TODO: is this needed?
        , arg "-rtsopts"            -- TODO: is this needed?
        , arg "-c", append srcs
        , arg "-o", arg file ]

-- TODO: do '-ticky' in all debug ways?
wayHcArgs :: Args
wayHcArgs = do
    way <- getWay
    mconcat [ if (Dynamic `wayUnit` way)
              then append ["-fPIC", "-dynamic"]
              else arg "-static"
            , (Threaded  `wayUnit` way) ? arg "-optc-DTHREADED_RTS"
            , (Debug     `wayUnit` way) ? arg "-optc-DDEBUG"
            , (Profiling `wayUnit` way) ? arg "-prof"
            , (Logging   `wayUnit` way) ? arg "-eventlog"
            , (Parallel  `wayUnit` way) ? arg "-parallel"
            , (GranSim   `wayUnit` way) ? arg "-gransim"
            , (way == debug || way == debugDynamic) ?
              append ["-ticky", "-DTICKY_TICKY"] ]

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
