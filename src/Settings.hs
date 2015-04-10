{-# LANGUAGE FlexibleInstances #-}

module Settings (
    IntegerLibrary (..), integerLibrary, integerLibraryName,
    buildHaddock
    ) where

import Base
import Ways
import Oracles.Builder
import Expression.Base
import Expression.Predicate
import Expression.PGPredicate

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

whenPackageKey :: Guard
whenPackageKey = supportsPackageKey `And` notStage Stage0

packageSettings :: Settings
packageSettings =
    opts ["-hide-all-packages", "-no-user-package-db", "-include-pkg-deps"]
    <>
    stage Stage0 ? opts ["-package-db libraries/bootstrapping.conf"]
    <>
    ite whenPackageKey
        (packageKey "-this-package-key" <> packageDepKeys "-package-key")
        (packageKey "-package-name"     <> packageDeps    "-package"    )

librarySettings :: Ways -> Settings
librarySettings ways =
    ite (whenExists vanilla ways)
        (opt  "--enable-library-vanilla")
        (opt "--disable-library-vanilla")
    <>
    ite (ghcWithInterpreter
        `And` (Not dynamicGhcPrograms)
        `And` whenExists vanilla ways)
        (opt  "--enable-library-for-ghci")
        (opt "--disable-library-for-ghci")
    <>
    ite (whenExists profiling ways)
        (opt  "--enable-library-profiling")
        (opt "--disable-library-profiling")
    <>
    ite (whenExists dynamic ways)
        (opt  "--enable-shared")
        (opt "--disable-shared")


ccSettings :: Settings
ccSettings = mempty

ldSettings :: Settings
ldSettings = mempty

cppSettings :: Settings
cppSettings = mempty

configureSettings :: Settings
configureSettings =
    let conf key    = subSettings $ "--configure-option=" ++ key ++ "="
        ccSettings' = remove ["-Werror"] ccSettings
    in
    mconcat [ conf "CFLAGS"   ccSettings'
            , conf "LDFLAGS"  ldSettings
            , conf "CPPFLAGS" cppSettings
            , subSettings "--gcc-options="  $ ccSettings' <> ldSettings
            , conf "--with-iconv-includes"  $ optKeyValue "iconv-include-dirs"
            , conf "--with-iconv-libraries" $ optKeyValue "iconv-lib-dirs"
            , conf "--with-gmp-includes"    $ optKeyValue "gmp-include-dirs"
            , conf "--with-gmp-libraries"   $ optKeyValue "gmp-lib-dirs"
            -- TODO: why TargetPlatformFull and not host?
            , crossCompiling ?
              conf "--host"    (optKeyValue "target-platform-full")
            , conf "--with-cc" $ optStagedBuilder Gcc ]

-- this is a positional argument, hence:
-- * if it is empty, we need to emit one empty string argument
-- * otherwise, we must collapse it into one space-separated string
dllSettings :: Settings
dllSettings = opt ""

-- customConfArgs
customConfigureSettings :: Settings
customConfigureSettings = mempty

-- bootPackageDb
bootPackageDbSettings :: Settings
bootPackageDbSettings =
    stage Stage0 ?
        optPath "--package-db="
        (optKeyValue "ghc-source-path" <> opt "libraries/bootstrapping.conf")

cabalSettings :: Settings
cabalSettings =
    opt "configure"
    `fence` optBuildPath
    `fence` optBuildDist
    `fence` dllSettings
    `fence` mconcat
    [ optStagedBuilder Ghc -- TODO: used to be limited to max stage1 GHC
    , optStagedBuilder GhcPkg
    , customConfigureSettings
    , bootPackageDbSettings
    , librarySettings targetWays
    , keyNonEmpty "hscolour" ? optBuilder HsColour -- TODO: more reuse
    , configureSettings
    , stage Stage0 ? optBootPkgConstraints
    , optStagedBuilder Gcc
    , notStage Stage0 ? optBuilder Ld
    , optBuilder Ar
    , optBuilder Alex
    , optBuilder Happy ] -- TODO: reorder with's

ghcPkgSettings :: Settings
ghcPkgSettings =
    opt "update"
    `fence` mconcat
    [ opt "--force"
    , optPath "" $
      mconcat [optBuildPath, optBuildDist, opt "inplace-pkg-config"]
    , bootPackageDbSettings ]
