module Settings.GhcCabal (
    cabalArgs, bootPackageDbArgs, customPackageArgs
    ) where

import Way
import Util
import Stage
import Builder
import Package
import Switches
import Expression
import Oracles.Base
import Oracles.Flag
import Oracles.Setting
import Settings.User
import Settings.Ways
import Settings.Util
import Settings.Packages
import Data.Version
import qualified Distribution.Package                  as D
import qualified Distribution.PackageDescription       as D
import qualified Distribution.Verbosity                as D
import qualified Distribution.PackageDescription.Parse as D

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

-- TODO: Isn't vanilla always built? If yes, some conditions are redundant.
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
    stage <- getStage
    let conf key = appendSubD $ "--configure-option=" ++ key
        cFlags   = mconcat [ ccArgs
                           , remove ["-Werror"]
                           , argSettingList $ ConfCcArgs stage ]
        ldFlags  = ldArgs <> (argSettingList $ ConfGccLinkerArgs stage)
        cppFlags = cppArgs <> (argSettingList $ ConfCppArgs stage)
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
        , conf "--with-cc" . argM . builderPath $ Gcc stage ]

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

-- TODO: speed up by caching the result in Shake database?
packageConstraints :: Args
packageConstraints = stage0 ? do
    pkgs <- getPackages
    constraints <- lift $ forM pkgs $ \pkg -> do
        let cabal = pkgPath pkg -/- pkgCabal pkg
        need [cabal]
        description <- liftIO $ D.readPackageDescription D.silent cabal
        let identifier         = D.package . D.packageDescription $ description
            version            = showVersion . D.pkgVersion $ identifier
            D.PackageName name = D.pkgName $ identifier
        return $ name ++ " == " ++ version
    append . concatMap (\c -> ["--constraint", c]) $ constraints

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
cppArgs = append . map ("-I" ++ ) $ ghcIncludeDirs

customPackageArgs :: Args
customPackageArgs = do
    stage   <- getStage
    rtsWays <- getRtsWays
    mconcat
        [ package integerGmp2 ?
          mconcat [ windowsHost ? builder GhcCabal ?
                    arg "--configure-option=--with-intree-gmp"
                  , appendCcArgs ["-Ilibraries/integer-gmp2/gmp"] ]

        , package base ?
          builder GhcCabal ? arg ("--flags=" ++ pkgName integerLibrary)

        , package ghcPrim ?
          builder GhcCabal ? arg "--flag=include-ghc-prim"

        , package compiler ?
          builder GhcCabal ?
          mconcat [ arg $ "--ghc-option=-DSTAGE=" ++ show (succ stage)
                  , arg $ "--flags=stage" ++ show (succ stage)
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
withBuilderKey builder = case builder of
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
with builder = specified builder ? do
    path <- lift $ builderPath builder
    lift $ needBuilder builder
    append [withBuilderKey builder ++ path]

withStaged :: (Stage -> Builder) -> Args
withStaged sb = do
    stage <- getStage
    with $ sb stage
