module Settings.Builders.GhcCabal (
    cabalArgs, ghcCabalHsColourArgs,
    bootPackageDbArgs, customPackageArgs
    ) where

import Way
import Base
import Util
import Stage
import Builder
import Package
import Expression
import Predicates
import Oracles.Flag
import Oracles.Setting
import Settings.User
import Settings.Ways
import Settings.Util
import Settings.Packages

cabalArgs :: Args
cabalArgs = builder GhcCabal ? do
    path <- getPackagePath
    dir  <- getTargetDirectory
    mconcat [ arg "configure"
            , arg path
            , arg dir
            , dllArgs
            , withStaged Ghc
            , withStaged GhcPkg
            , stage0 ? bootPackageDbArgs
            , libraryArgs
            , with HsColour
            , configureArgs
            , packageConstraints
            , withStaged Gcc
            , notStage0 ? with Ld
            , with Ar
            , with Alex
            , with Happy ]

ghcCabalHsColourArgs :: Args
ghcCabalHsColourArgs = builder GhcCabalHsColour ? do
    path <- getPackagePath
    dir  <- getTargetDirectory
    mconcat [ arg "hscolour"
            , arg path
            , arg dir ]

-- TODO: Isn't vanilla always built? If yes, some conditions are redundant.
-- TODO: Need compiler_stage1_CONFIGURE_OPTS += --disable-library-for-ghci?
libraryArgs :: Args
libraryArgs = do
    ways   <- getWays
    ghcInt <- lift $ ghcWithInterpreter
    append [ if vanilla `elem` ways
             then  "--enable-library-vanilla"
             else "--disable-library-vanilla"
           , if vanilla `elem` ways && ghcInt && not dynamicGhcPrograms
             then  "--enable-library-for-ghci"
             else "--disable-library-for-ghci"
           , if profiling `elem` ways
             then  "--enable-library-profiling"
             else "--disable-library-profiling"
           , if dynamic `elem` ways
             then  "--enable-shared"
             else "--disable-shared" ]

configureArgs :: Args
configureArgs = do
    let conf key = appendSubD $ "--configure-option=" ++ key
        cFlags   = mconcat [ ccArgs
                           , remove ["-Werror"]
                           , argStagedSettingList ConfCcArgs ]
        ldFlags  = ldArgs  <> (argStagedSettingList ConfGccLinkerArgs)
        cppFlags = cppArgs <> (argStagedSettingList ConfCppArgs)
    mconcat
        [ conf "CFLAGS"   cFlags
        , conf "LDFLAGS"  ldFlags
        , conf "CPPFLAGS" cppFlags
        , appendSubD "--gcc-options" $ cFlags <> ldFlags
        , conf "--with-iconv-includes"  $ argSettingList IconvIncludeDirs
        , conf "--with-iconv-libraries" $ argSettingList IconvLibDirs
        , conf "--with-gmp-includes"    $ argSettingList GmpIncludeDirs
        , conf "--with-gmp-libraries"   $ argSettingList GmpLibDirs
        -- TODO: why TargetPlatformFull and not host?
        , crossCompiling ? (conf "--host" $ argSetting TargetPlatformFull)
        , conf "--with-cc" $ argStagedBuilderPath Gcc ]

bootPackageDbArgs :: Args
bootPackageDbArgs = do
    path <- getSetting GhcSourcePath
    arg $ "--package-db=" ++ path -/- "libraries/bootstrapping.conf"

-- This is a positional argument, hence:
-- * if it is empty, we need to emit one empty string argument;
-- * otherwise, we must collapse it into one space-separated string.
-- TODO: should be non-empty for compiler
dllArgs :: Args
dllArgs = arg ""

packageConstraints :: Args
packageConstraints = stage0 ? do
    constraints <- lift . readFileLines $ bootPackageConstraints
    append $ concat [ ["--constraint", c] | c <- constraints ]

-- TODO: should be in a different file
-- TODO: put all validating options together in one file
ccArgs :: Args
ccArgs = validating ? do
    let gccGe46 = notP gccLt46
    mconcat [ arg "-Werror"
            , arg "-Wall"
            , gccIsClang ??
              ( arg "-Wno-unknown-pragmas" <>
                gccGe46 ? windowsHost ? arg "-Werror=unused-but-set-variable"
              , gccGe46 ? arg "-Wno-error=inline" )]

ldArgs :: Args
ldArgs = mempty

ghcIncludeDirs :: [FilePath]
ghcIncludeDirs = [ "includes", "includes/dist"
                 , "includes/dist-derivedconstants/header"
                 , "includes/dist-ghcconstants/header" ]

cppArgs :: Args
cppArgs = append $ map ("-I" ++) ghcIncludeDirs

-- TODO: Is this needed?
-- ifeq "$(GMP_PREFER_FRAMEWORK)" "YES"
-- libraries/integer-gmp_CONFIGURE_OPTS += --with-gmp-framework-preferred
-- endif

-- TODO: move this somewhere
customPackageArgs :: Args
customPackageArgs = do
    nextStage <- fmap succ getStage
    rtsWays   <- getRtsWays
    mconcat
        [ package integerGmp ?
          mconcat [ windowsHost ? builder GhcCabal ?
                    arg "--configure-option=--with-intree-gmp"
                  , appendCcArgs ["-I" ++ pkgPath integerGmp -/- "gmp"] ]

        , package base ?
          builder GhcCabal ?
          arg ("--flags=" ++ takeFileName (pkgPath integerLibrary))

        , package ghcPrim ?
          builder GhcCabal ? arg "--flag=include-ghc-prim"

        , package compiler ?
          builder GhcCabal ?
          mconcat [ arg $ "--ghc-option=-DSTAGE=" ++ show nextStage
                  , arg $ "--flags=stage" ++ show nextStage
                  , arg "--disable-library-for-ghci"
                  , targetOs "openbsd" ? arg "--ld-options=-E"
                  , flag GhcUnregisterised ? arg "--ghc-option=-DNO_REGS"
                  , notP ghcWithSMP ? arg "--ghc-option=-DNOSMP"
                  , notP ghcWithSMP ? arg "--ghc-option=-optc-DNOSMP"
                  , (threaded `elem` rtsWays) ?
                    notStage0 ? arg "--ghc-option=-optc-DTHREADED_RTS"
                  , ghcWithNativeCodeGen ? arg "--flags=ncg"
                  , ghcWithInterpreter ?
                    notStage0 ? arg "--flags=ghci"
                  , ghcWithInterpreter ?
                    ghcEnableTablesNextToCode ?
                    notP (flag GhcUnregisterised) ?
                    notStage0 ? arg "--ghc-option=-DGHCI_TABLES_NEXT_TO_CODE"
                  , ghcWithInterpreter ?
                    ghciWithDebugger ?
                    notStage0 ? arg "--ghc-option=-DDEBUGGER"
                  , ghcProfiled ?
                    notStage0 ? arg "--ghc-pkg-option=--force"
                  ]
        ]

withBuilderKey :: Builder -> String
withBuilderKey b = case b of
    Ar       -> "--with-ar="
    Ld       -> "--with-ld="
    Gcc _    -> "--with-gcc="
    Ghc _    -> "--with-ghc="
    Alex     -> "--with-alex="
    Happy    -> "--with-happy="
    GhcPkg _ -> "--with-ghc-pkg="
    HsColour -> "--with-hscolour="
    _        -> error "withBuilderKey: not supported builder"

-- Expression 'with Gcc' appends "--with-gcc=/path/to/gcc" and needs Gcc.
with :: Builder -> Args
with b = specified b ? do
    path <- lift $ builderPath b
    lift $ needBuilder laxDependencies b
    append [withBuilderKey b ++ path]

withStaged :: (Stage -> Builder) -> Args
withStaged sb = (with . sb) =<< getStage
