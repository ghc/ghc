module Settings.GhcCabal (
    cabalSettings, bootPackageDbSettings
    ) where

import Base hiding (arg, args)
import Oracles.Base
import Oracles.Builder
import Ways
import Util
import Package
import Targets
import Switches
import Expression hiding (when, liftIO)
import Settings.Ways
import Settings.Util
import Settings.Packages
import UserSettings

cabalSettings :: Settings
cabalSettings = builder GhcCabal ? do
    stage <- asks getStage
    pkg   <- asks getPackage
    mconcat [ arg "configure"
            , arg $ pkgPath pkg
            , arg $ targetDirectory stage pkg
            , dllSettings
            , with' $ Ghc stage
            , with' $ GhcPkg stage
            , stage0 ? bootPackageDbSettings
            , librarySettings
            , configKeyNonEmpty "hscolour" ? with' HsColour -- TODO: generalise?
            , configureSettings
            , stage0 ? packageConstraints
            , with' $ Gcc stage
            , notStage Stage0 ? with' Ld
            , with' Ar
            , with' Alex
            , with' Happy ] -- TODO: reorder with's

-- TODO: Isn't vanilla always built? If yes, some conditions are redundant.
librarySettings :: Settings
librarySettings = do
    ways            <- fromDiff Settings.Ways.ways
    ghcInterpreter  <- ghcWithInterpreter
    dynamicPrograms <- dynamicGhcPrograms
    append [ if vanilla `elem` ways
             then  "--enable-library-vanilla"
             else "--disable-library-vanilla"
           , if vanilla `elem` ways && ghcInterpreter && not dynamicPrograms
             then  "--enable-library-for-ghci"
             else "--disable-library-for-ghci"
           , if profiling `elem` ways
             then  "--enable-library-profiling"
             else "--disable-library-profiling"
           , if dynamic `elem` ways
             then  "--enable-shared"
             else "--disable-shared" ]

configureSettings :: Settings
configureSettings = do
    stage <- asks getStage
    let conf key = appendSubD $ "--configure-option=" ++ key
        cFlags   = mconcat [ ccSettings
                           , remove ["-Werror"]
                           , argStagedConfig "conf-cc-args" ]
        ldFlags  = ldSettings <> argStagedConfig "conf-gcc-linker-args"
        cppFlags = cppSettings <> argStagedConfig "conf-cpp-args"
    mconcat
        [ conf "CFLAGS"   cFlags
        , conf "LDFLAGS"  ldFlags
        , conf "CPPFLAGS" cppFlags
        , appendSubD "--gcc-options" $ cFlags <> ldFlags
        , conf "--with-iconv-includes"  $ argConfig "iconv-include-dirs"
        , conf "--with-iconv-libraries" $ argConfig "iconv-lib-dirs"
        , conf "--with-gmp-includes"    $ argConfig "gmp-include-dirs"
        , conf "--with-gmp-libraries"   $ argConfig "gmp-lib-dirs"
        -- TODO: why TargetPlatformFull and not host?
        , crossCompiling ? (conf "--host" $ argConfig "target-platform-full")
        , conf "--with-cc" . argM . showArg $ Gcc stage ]

bootPackageDbSettings :: Settings
bootPackageDbSettings = do
    sourcePath <- lift $ askConfig "ghc-source-path"
    arg $ "--package-db=" ++ sourcePath </> "libraries/bootstrapping.conf"

dllSettings :: Settings
dllSettings = arg ""

-- TODO: remove
with' :: Builder -> Settings
with' builder = appendM $ with builder

packageConstraints :: Settings
packageConstraints = do
    pkgs <- fromDiff packages
    constraints <- lift $ forM pkgs $ \pkg -> do
        let cabal  = pkgPath pkg </> pkgCabal pkg
            prefix = dropExtension (pkgCabal pkg) ++ " == "
        need [cabal]
        content <- lines <$> liftIO (readFile cabal)
        let vs = filter (("ersion:" `isPrefixOf`) . drop 1) content
        case vs of
            [v] -> return $ prefix ++ dropWhile (not . isDigit) v
            _   -> redError $ "Cannot determine package version in '"
                            ++ cabal ++ "'."
    args $ concatMap (\c -> ["--constraint", c]) $ constraints

-- TODO: should be in a different file
ccSettings :: Settings
ccSettings = validating ? do
    let gccGe46 = liftM not gccLt46
    mconcat [ arg "-Werror"
            , arg "-Wall"
            , gccIsClang ??
              ( arg "-Wno-unknown-pragmas" <>
                gccGe46 ? windowsHost ? arg "-Werror=unused-but-set-variable"
              , gccGe46 ? arg "-Wno-error=inline" )]

ldSettings :: Settings
ldSettings = mempty

cppSettings :: Settings
cppSettings = mempty
