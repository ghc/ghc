{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

module Settings (
    -- IntegerLibrary (..), integerLibrary, integerLibraryName,
    buildHaddock
    ) where

import Base hiding (arg, args, Args)
import Targets
import Ways
import Oracles.Builder
import Expression.Base

whenPackageKey :: BuildPredicate
whenPackageKey = supportsPackageKey && notStage Stage0

packageSettings :: Settings
packageSettings = mconcat
    [ args ["-hide-all-packages", "-no-user-package-db", "-include-pkg-deps"]
    , stage Stage0 ? args ["-package-db libraries/bootstrapping.conf"]
    , whenPackageKey ??
      ( argPackageKey "-this-package-key" <> argPackageDepKeys "-package-key"
      , argPackageKey "-package-name"     <> argPackageDeps    "-package"    )]

librarySettings :: Ways -> Settings
librarySettings ways = mconcat
    [ whenExists vanilla ways     ?? ( arg  "--enable-library-vanilla"
                                     , arg "--disable-library-vanilla" )
    , (ghcWithInterpreter
      && not dynamicGhcPrograms
      && whenExists vanilla ways) ?? ( arg  "--enable-library-for-ghci"
                                     , arg "--disable-library-for-ghci" )
    , whenExists profiling ways   ?? ( arg  "--enable-library-profiling"
                                     , arg "--disable-library-profiling" )
    , whenExists dynamic ways     ?? ( arg  "--enable-shared"
                                     , arg "--disable-shared" )]

validating :: BuildPredicate
validating = false

ccSettings :: Settings
ccSettings = mconcat
    [ package integerLibrary ? arg "-Ilibraries/integer-gmp2/gmp"
    , builder GhcCabal ? argConfigStaged "conf-cc-args"
    , validating ? mconcat
        [ not (builder GhcCabal) ? arg "-Werror"
        , arg "-Wall"
        , gccIsClang ??
          ( arg "-Wno-unknown-pragmas" <>
            not gccLt46 && windowsHost ? arg "-Werror=unused-but-set-variable"
          , not gccLt46 ? arg "-Wno-error=inline" )
        ]
    ]

ldSettings :: Settings
ldSettings = builder GhcCabal ? argConfigStaged "conf-gcc-linker-args"

cppSettings :: Settings
cppSettings = builder GhcCabal ? argConfigStaged "conf-cpp-args"

configureSettings :: Settings
configureSettings =
    let conf key = argComplex $ "--configure-option=" ++ key ++ "="
    in
    mconcat [ conf "CFLAGS"   ccSettings
            , conf "LDFLAGS"  ldSettings
            , conf "CPPFLAGS" cppSettings
            , argComplex "--gcc-options="   (ccSettings <> ldSettings)
            , conf "--with-iconv-includes"  (argConfig "iconv-include-dirs")
            , conf "--with-iconv-libraries" (argConfig "iconv-lib-dirs")
            , conf "--with-gmp-includes"    (argConfig "gmp-include-dirs")
            , conf "--with-gmp-libraries"   (argConfig "gmp-lib-dirs")
            -- TODO: why TargetPlatformFull and not host?
            , crossCompiling ?
              conf "--host"    (argConfig "target-platform-full")
            , conf "--with-cc" (argStagedBuilderPath Gcc) ]

-- this is a positional argument, hence:
-- * if it is empty, we need to emit one empty string argument
-- * otherwise, we must collapse it into one space-separated string
dllSettings :: Settings
dllSettings = arg ""

-- customConfArgs
customConfigureSettings :: Settings
customConfigureSettings = mconcat
    [ package base    ? arg ("--flags=" ++ integerLibraryName)
    , package ghcPrim ? arg "--flag=include-ghc-prim"
    , package integerLibrary && windowsHost ?
        arg "--configure-option=--with-intree-gmp"
    ]

-- bootPackageDb
bootPackageDbSettings :: Settings
bootPackageDbSettings =
    stage Stage0 ?
        argPath "--package-db="
        (argConfig "ghc-source-path" <> arg "libraries/bootstrapping.conf")

cabalSettings :: Settings
cabalSettings =
    argsOrdered ["configure", argBuildPath, argBuildDist, dllSettings]
    `fence`
    mconcat
    [ argStagedBuilderPath Ghc -- TODO: used to be limited to max stage1 GHC
    , argStagedBuilderPath GhcPkg
    , customConfigureSettings
    , bootPackageDbSettings
    , librarySettings targetWays
    , configNonEmpty "hscolour" ? argBuilderPath HsColour -- TODO: more reuse
    , configureSettings
    , stage Stage0 ? argBootPkgConstraints
    , argStagedBuilderPath Gcc
    , notStage Stage0 ? argBuilderPath Ld
    , argBuilderPath Ar
    , argBuilderPath Alex
    , argBuilderPath Happy ] -- TODO: reorder with's

ghcPkgSettings :: Settings
ghcPkgSettings =
    arg "update"
    `fence` mconcat
    [ arg "--force"
    , argPath "" $
      mconcat [argBuildPath, argBuildDist, arg "inplace-pkg-config"]
    , bootPackageDbSettings ]
