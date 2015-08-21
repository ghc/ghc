module Settings.Builders.Ghc (ghcArgs, ghcMArgs, commonGhcArgs) where

import Way
import Util
import Stage
import Builder
import Switches (stagedBuilder, splitObjects, stage0)
import Expression
import Oracles.Flag
import Oracles.PackageData
import Settings.Util
import Settings.Ways

-- TODO: add support for -dyno
-- $1/$2/build/%.$$($3_o-bootsuf) : $1/$4/%.hs-boot
--     $$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@
--     $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),-dyno
--     $$(addsuffix .$$(dyn_osuf)-boot,$$(basename $$@)))
ghcArgs :: Args
ghcArgs = stagedBuilder Ghc ? do
    file <- getFile
    srcs <- getSources
    mconcat [ commonGhcArgs
            , arg "-c", append srcs
            , arg "-o", arg file ]

ghcMArgs :: Args
ghcMArgs = stagedBuilder GhcM ? do
    ways <- getWays
    file <- getFile
    srcs <- getSources
    mconcat [ arg "-M"
            , commonGhcArgs
            , arg "-dep-makefile", arg file
            , append $ concat [ ["-dep-suffix", wayPrefix w] | w <- ways ]
            , append srcs ]

-- This is included into ghcArgs, ghcMArgs and haddockArgs.
commonGhcArgs :: Args
commonGhcArgs = do
    way     <- getWay
    hsArgs  <- getPkgDataList HsArgs
    cppArgs <- getPkgDataList CppArgs
    path    <- getTargetPath
    let buildPath = path -/- "build"
    mconcat [ arg "-hisuf", arg $ hisuf way
            , arg "-osuf" , arg $  osuf way
            , arg "-hcsuf", arg $ hcsuf way
            , wayGhcArgs
            , packageGhcArgs
            , includeGhcArgs
            , append hsArgs
            , append $ map ("-optP" ++) cppArgs
            , arg "-odir"    , arg buildPath
            , arg "-stubdir" , arg buildPath
            , arg "-hidir"   , arg buildPath
            , splitObjects   ? arg "-split-objs"
            , arg "-rtsopts" ]          -- TODO: is this needed?

-- TODO: do '-ticky' in all debug ways?
wayGhcArgs :: Args
wayGhcArgs = do
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
    pkgDepIds          <- getPkgDataList DepIds
    mconcat
        [ arg "-hide-all-packages"
        , arg "-no-user-package-db"
        , arg "-include-pkg-deps"
        , stage0 ? arg "-package-db libraries/bootstrapping.conf"
        , if supportsPackageKey || stage /= Stage0
          then arg $ "-this-package-key " ++ pkgKey
          else arg $ "-package-name "     ++ pkgKey
        , append $ map ("-package-id " ++) pkgDepIds ]

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
        , arg $ "-i" ++ buildPath
        , arg $ "-i" ++ autogenPath
        , arg $ "-I" ++ buildPath
        , arg $ "-I" ++ autogenPath
        , append [ "-i" ++ pkgPath -/- dir | dir <- srcDirs ]
        , append [ "-I" ++ pkgPath -/- dir | dir <- incDirs ]
        , arg "-optP-include", arg $ "-optP" ++ autogenPath -/- "cabal_macros.h"
        , append $ map ("-optP" ++) cppArgs ]

-- TODO: see ghc.mk
-- # And then we strip it out again before building the package:
-- define libraries/ghc-prim_PACKAGE_MAGIC
-- libraries/ghc-prim_dist-install_MODULES := $$(filter-out GHC.Prim,$$(libraries/ghc-prim_dist-install_MODULES))
-- endef
