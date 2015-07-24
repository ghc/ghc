module Settings.GhcCabal (
    cabalArgs, bootPackageDbArgs, customPackageArgs
    ) where

import Way
import Builder
import Package
import Util
import Switches
import Expression
import Oracles.Base
import Oracles.Flag
import Oracles.Setting
import Settings.User
import Settings.Ways
import Settings.Util
import Settings.Packages
import Settings.TargetDirectory
import Data.List
import Control.Applicative

cabalArgs :: Args
cabalArgs = builder GhcCabal ? do
    stage <- asks getStage
    pkg   <- asks getPackage
    mconcat [ arg "configure"
            , arg $ pkgPath pkg
            , arg $ targetDirectory stage pkg
            , dllArgs
            , with $ Ghc stage
            , with $ GhcPkg stage
            , stage0 ? bootPackageDbArgs
            , libraryArgs
            , with HsColour
            , configureArgs
            , stage0 ? packageConstraints
            , with $ Gcc stage
            , notStage0 ? with Ld
            , with Ar
            , with Alex
            , with Happy ]

-- TODO: Isn't vanilla always built? If yes, some conditions are redundant.
libraryArgs :: Args
libraryArgs = do
    ways           <- fromDiffExpr Settings.Ways.ways
    ghcInterpreter <- lift $ ghcWithInterpreter
    append [ if vanilla `elem` ways
             then  "--enable-library-vanilla"
             else "--disable-library-vanilla"
           , if vanilla `elem` ways && ghcInterpreter && not dynamicGhcPrograms
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
    stage <- asks getStage
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
    sourcePath <- lift . setting $ GhcSourcePath
    arg $ "--package-db=" ++ sourcePath -/- "libraries/bootstrapping.conf"

-- This is a positional argument, hence:
-- * if it is empty, we need to emit one empty string argument;
-- * otherwise, we must collapse it into one space-separated string.
dllArgs :: Args
dllArgs = arg ""

packageConstraints :: Args
packageConstraints = do
    pkgs <- fromDiffExpr packages
    constraints <- lift $ forM pkgs $ \pkg -> do
        let cabal  = pkgPath pkg -/- pkgCabal pkg
            prefix = dropExtension (pkgCabal pkg) ++ " == "
        need [cabal]
        content <- lines <$> liftIO (readFile cabal)
        let vs = filter (("ersion:" `isPrefixOf`) . drop 1) content
        case vs of
            [v] -> return $ prefix ++ dropWhile (not . isDigit) v
            _   -> redError $ "Cannot determine package version in '"
                            ++ cabal ++ "'."
    append $ concatMap (\c -> ["--constraint", c]) $ constraints

-- TODO: should be in a different file
-- TODO: put all validating options together in one file
ccArgs :: Args
ccArgs = validating ? do
    let gccGe46 = liftM not gccLt46
    mconcat [ arg "-Werror"
            , arg "-Wall"
            , gccIsClang ??
              ( arg "-Wno-unknown-pragmas" <>
                gccGe46 ? windowsHost ? arg "-Werror=unused-but-set-variable"
              , gccGe46 ? arg "-Wno-error=inline" )]

ldArgs :: Args
ldArgs = mempty

cppArgs :: Args
cppArgs = mempty

customPackageArgs :: Args
customPackageArgs = mconcat
    [ package integerGmp2 ?
      mconcat [ windowsHost ? builder GhcCabal ?
                arg "--configure-option=--with-intree-gmp"
              , appendCcArgs ["-Ilibraries/integer-gmp2/gmp"] ]

    , package base ?
      builder GhcCabal ? arg ("--flags=" ++ pkgName integerLibrary)

    , package ghcPrim ?
      builder GhcCabal ? arg "--flag=include-ghc-prim" ]

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
