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

validating :: BuildPredicate
validating = false

packageSettings :: Settings
packageSettings = msum
    [ args ["-hide-all-packages", "-no-user-package-db", "-include-pkg-deps"]
    , stage Stage0 ?
      (arg "-package-db" |> argPath "libraries/bootstrapping.conf")
    , supportsPackageKey && notStage Stage0 ??
      ( argPairs "-this-package-key" argPackageKey <|>
        argPairs "-package-key"      argPackageDepKeys
      , argPairs "-package-name"     argPackageKey <|>
        argPairs "-package"          argPackageDeps )]

librarySettings :: Ways -> Settings
librarySettings ways = msum
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

ccSettings :: Settings
ccSettings = msum
    [ package integerLibrary ? argPath "-Ilibraries/integer-gmp2/gmp"
    , builder GhcCabal ? argStagedConfig "conf-cc-args"
    , validating ? msum
        [ not (builder GhcCabal) ? arg "-Werror"
        , arg "-Wall"
        , gccIsClang ??
          ( arg "-Wno-unknown-pragmas" <|>
            not gccLt46 && windowsHost ? arg "-Werror=unused-but-set-variable"
          , not gccLt46 ? arg "-Wno-error=inline" )]]

ldSettings :: Settings
ldSettings = builder GhcCabal ? argStagedConfig "conf-gcc-linker-args"

cppSettings :: Settings
cppSettings = builder GhcCabal ? argStagedConfig "conf-cpp-args"

configureSettings :: Settings
configureSettings =
    let conf key = argPrefix ("--configure-option=" ++ key ++ "=")
                 . argConcatSpace
    in
    msum [ conf "CFLAGS"   ccSettings
         , conf "LDFLAGS"  ldSettings
         , conf "CPPFLAGS" cppSettings
         , argPrefix "--gcc-options=" $
           argConcatSpace (ccSettings <|> ldSettings)
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
customConfigureSettings = msum
    [ package base    ? arg ("--flags=" ++ integerLibraryName)
    , package ghcPrim ? arg "--flag=include-ghc-prim"
    , package integerLibrary && windowsHost ?
        arg "--configure-option=--with-intree-gmp"
    ]

bootPackageDbSettings :: Settings
bootPackageDbSettings =
    stage Stage0 ?
        argPrefix "--package-db="
        (argConcatPath $ argConfig "ghc-source-path" |>
                         argPath "libraries/bootstrapping.conf")

cabalSettings :: Settings
cabalSettings =
    mproduct
    [ argBuilderPath GhcCabal
    , arg "configure"
    , argBuildPath
    , argBuildDir
    , dllSettings
    , msum
      [ argWithStagedBuilder Ghc -- TODO: used to be limited to max stage1 GHC
      , argWithStagedBuilder GhcPkg
      , customConfigureSettings
      , stage Stage0 ? bootPackageDbSettings
      , librarySettings targetWays
      , configNonEmpty "hscolour" ? argWithBuilder HsColour -- TODO: more reuse
      , configureSettings
      , stage Stage0 ? argBootPkgConstraints
      , argWithStagedBuilder Gcc
      , notStage Stage0 ? argWithBuilder Ld
      , argWithBuilder Ar
      , argWithBuilder Alex
      , argWithBuilder Happy ]] -- TODO: reorder with's

ghcPkgSettings :: Settings
ghcPkgSettings =
    arg "update" |> msum
        [ arg "--force"
        , argConcatPath $
          msum [argBuildPath, argBuildDir, arg "inplace-pkg-config"]
        , bootPackageDbSettings ]
