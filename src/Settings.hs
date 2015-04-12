{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

module Settings (
    IntegerLibrary (..), integerLibrary, integerLibraryName,
    buildHaddock
    ) where

import Base hiding (arg, args, Args)
import Ways
import Oracles.Builder
import Expression.Base

data IntegerLibrary = IntegerGmp | IntegerGmp2 | IntegerSimple

integerLibrary :: IntegerLibrary
integerLibrary = IntegerGmp2

integerLibraryName :: String
integerLibraryName = case integerLibrary of
    IntegerGmp    -> "integer-gmp"
    IntegerGmp2   -> "integer-gmp2"
    IntegerSimple -> "integer-simple"

buildHaddock :: Bool
buildHaddock = True

-- TODO: move to Targets.hs
targetWays :: Ways
targetWays = mconcat [ singleton vanilla
                     , notStage Stage0 ? singleton profiling
                     , platformSupportsSharedLibs ? singleton dynamic ]

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
librarySettings ways =
    mconcat [ whenExists vanilla ways ??
                ( arg  "--enable-library-vanilla"
                , arg "--disable-library-vanilla" )
            , (ghcWithInterpreter
              && not dynamicGhcPrograms
              && whenExists vanilla ways) ??
                ( arg  "--enable-library-for-ghci"
                , arg "--disable-library-for-ghci" )
            , whenExists profiling ways ??
                ( arg  "--enable-library-profiling"
                , arg "--disable-library-profiling" )
            , whenExists dynamic ways ??
                ( arg  "--enable-shared"
                , arg "--disable-shared" )]

ccSettings :: Settings
ccSettings = mempty

ldSettings :: Settings
ldSettings = mempty

cppSettings :: Settings
cppSettings = mempty

configureSettings :: Settings
configureSettings =
    let conf key    = argComplex $ "--configure-option=" ++ key ++ "="
        ccSettings' = remove ["-Werror"] ccSettings
    in
    mconcat [ conf "CFLAGS"   ccSettings'
            , conf "LDFLAGS"  ldSettings
            , conf "CPPFLAGS" cppSettings
            , argComplex "--gcc-options="   $ ccSettings' <> ldSettings
            , conf "--with-iconv-includes"  $ argConfig "iconv-include-dirs"
            , conf "--with-iconv-libraries" $ argConfig "iconv-lib-dirs"
            , conf "--with-gmp-includes"    $ argConfig "gmp-include-dirs"
            , conf "--with-gmp-libraries"   $ argConfig "gmp-lib-dirs"
            -- TODO: why TargetPlatformFull and not host?
            , crossCompiling ?
              conf "--host"    (argConfig "target-platform-full")
            , conf "--with-cc" $ argStagedBuilderPath Gcc ]

-- this is a positional argument, hence:
-- * if it is empty, we need to emit one empty string argument
-- * otherwise, we must collapse it into one space-separated string
dllSettings :: Settings
dllSettings = arg ""

-- customConfArgs
customConfigureSettings :: Settings
customConfigureSettings = mempty

-- bootPackageDb
bootPackageDbSettings :: Settings
bootPackageDbSettings =
    stage Stage0 ?
        argPath "--package-db="
        (argConfig "ghc-source-path" <> arg "libraries/bootstrapping.conf")

cabalSettings :: Settings
cabalSettings =
    arg "configure"
    `fence` argBuildPath
    `fence` argBuildDist
    `fence` dllSettings
    `fence` mconcat
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
