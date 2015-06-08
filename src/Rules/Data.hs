module Rules.Data (
    cabalSettings, ghcPkgSettings, buildPackageData
    ) where

import qualified Ways
import Base hiding (arg, args, Args)
import Package
import Expression hiding (when, liftIO)
import Oracles.Base
import Oracles.Flag (when)
import Oracles.Builder
import Targets
import Switches
import Expression.Settings
import Util

-- TODO: Isn't vanilla always built? If yes, some conditions are redundant.
librarySettings :: Ways -> Settings
librarySettings waysExpression = do
    ways        <- waysExpression
    ghcInterp   <- ghcWithInterpreter
    dynPrograms <- dynamicGhcPrograms
    return $ [ if Ways.vanilla `elem` ways
               then  "--enable-library-vanilla"
               else "--disable-library-vanilla"
             , if Ways.vanilla `elem` ways && ghcInterp && not dynPrograms
               then  "--enable-library-for-ghci"
               else "--disable-library-for-ghci"
             , if Ways.profiling `elem` ways
               then  "--enable-library-profiling"
               else "--disable-library-profiling"
             , if Ways.dynamic `elem` ways
               then  "--enable-shared"
               else "--disable-shared" ]

configureSettings :: Settings
configureSettings = do
    let conf key expr = do
            value <- liftM unwords expr
            return $ if value == ""
                     then []
                     else ["--configure-option=" ++ key ++ "=" ++ value]

    stage <- asks getStage
    gccPath <- lift $ showArg (Gcc stage)
    gccSettings <- liftM unwords (ccSettings <> ldSettings)

    mconcat [ conf "CFLAGS"   ccSettings
            , conf "LDFLAGS"  ldSettings
            , conf "CPPFLAGS" cppSettings
            , arg $ "--gcc-options=" ++ gccSettings
            , conf "--with-iconv-includes"  (argConfig "iconv-include-dirs")
            , conf "--with-iconv-libraries" (argConfig "iconv-lib-dirs")
            , conf "--with-gmp-includes"    (argConfig "gmp-include-dirs")
            , conf "--with-gmp-libraries"   (argConfig "gmp-lib-dirs")
            -- TODO: why TargetPlatformFull and not host?
            , crossCompiling ?
              conf "--host"    (argConfig "target-platform-full")
            , conf "--with-cc" (arg gccPath) ]

bootPackageDbSettings :: Settings
bootPackageDbSettings = do
    sourcePath <- lift $ askConfig "ghc-source-path"
    return $ ["--package-db=" ++ sourcePath </> "libraries/bootstrapping.conf"]

dllSettings :: Settings
dllSettings = arg ""

with' :: Builder -> Settings
with' builder = lift $ with builder

packageConstraints :: Settings
packageConstraints = do
    pkgs <- filterM packagePredicate targetPackages
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
    return $ concatMap (\c -> ["--constraint", c]) $ constraints

cabalSettings :: Settings
cabalSettings = do
    stage <- asks getStage
    pkg   <- asks getPackage
    mconcat [ arg "configure"
            , arg $ pkgPath pkg
            , arg $ targetDirectory stage pkg
            , dllSettings
            , with' $ Ghc stage
            , with' $ GhcPkg stage
            , customConfigureSettings
            , Expression.stage Stage0 ? bootPackageDbSettings
            , librarySettings targetWays
            , configKeyNonEmpty "hscolour" ? with' HsColour -- TODO: generalise?
            , configureSettings
            , Expression.stage Stage0 ? packageConstraints
            , with' $ Gcc stage
            , notStage Stage0 ? with' Ld
            , with' Ar
            , with' Alex
            , with' Happy ] -- TODO: reorder with's

ghcPkgSettings :: Settings
ghcPkgSettings = do
    stage <- asks getStage
    pkg   <- asks getPackage
    let dir = pkgPath pkg </> targetDirectory stage pkg
    mconcat [ arg "update"
            , arg "--force"
            , Expression.stage Stage0 ? bootPackageDbSettings
            , arg $ dir </> "inplace-pkg-config" ]

-- Prepare a given 'packaga-data.mk' file for parsing by readConfigFile:
-- 1) Drop lines containing '$'
-- For example, get rid of
-- libraries/Win32_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir ...
-- Reason: we don't need them and we can't parse them.
-- 2) Replace '/' and '\' with '_' before '='
-- For example libraries/deepseq/dist-install_VERSION = 1.4.0.0
-- is replaced by libraries_deepseq_dist-install_VERSION = 1.4.0.0
-- Reason: Shake's built-in makefile parser doesn't recognise slashes

postProcessPackageData :: FilePath -> Action ()
postProcessPackageData file = do
    pkgData <- (filter ('$' `notElem`) . lines) <$> liftIO (readFile file)
    length pkgData `seq` writeFileLines file $ map processLine pkgData
      where
        processLine line = replaceSeparators '_' prefix ++ suffix
          where
            (prefix, suffix) = break (== '=') line

-- this is a positional argument, hence:
-- * if it is empty, we need to emit one empty string argument
-- * otherwise, we must collapse it into one space-separated string

-- Build package-data.mk by using GhcCabal to process pkgCabal file
buildPackageData :: Stage -> Package -> FilePath -> Ways -> Settings -> Rules ()
buildPackageData stage pkg dir ways settings =
    (dir </>) <$>
    [ "package-data.mk"
    , "haddock-prologue.txt"
    , "inplace-pkg-config"
    , "setup-config"
    , "build" </> "autogen" </> "cabal_macros.h"
    -- TODO: Is this needed? Also check out Paths_cpsa.hs.
    -- , "build" </> "autogen" </> ("Paths_" ++ name) <.> "hs"
    ] &%> \_ -> do
        let configure = pkgPath pkg </> "configure"
            env = defaultEnvironment { getStage = stage, getPackage = pkg }
        need [pkgPath pkg </> pkgCabal pkg]
        -- GhcCabal will run the configure script, so we depend on it
        -- We still don't know who build the configure script from configure.ac
        when (doesFileExist $ configure <.> "ac") $ need [configure]
        run' env GhcCabal settings
        -- TODO: when (registerPackage settings) $
        run' env (GhcPkg stage) settings
        postProcessPackageData $ dir </> "package-data.mk"

run' :: Environment -> Builder -> Settings -> Action ()
run' env builder settings = do
    args <- interpret (env {getBuilder = builder}) settings
    putColoured Green (show args)
    run builder args

--buildRule :: Package -> TodoItem -> Rules ()
--buildRule pkg @ (Package name path cabal _) todo @ (stage, dist, settings) =
--    let pathDist  = path </> dist
--        cabalPath = path </> cabal
--        configure = path </> "configure"
--    in
--    -- All these files are produced by a single run of GhcCabal
--    (pathDist </>) <$>
--    [ "package-data.mk"
--    , "haddock-prologue.txt"
--    , "inplace-pkg-config"
--    , "setup-config"
--    , "build" </> "autogen" </> "cabal_macros.h"
--    -- TODO: Is this needed? Also check out Paths_cpsa.hs.
--    -- , "build" </> "autogen" </> ("Paths_" ++ name) <.> "hs"
--    ] &%> \_ -> do
--        need [cabalPath]
--        when (doesFileExist $ configure <.> "ac") $ need [configure]
--        -- GhcCabal will run the configure script, so we depend on it
--        -- We still don't know who build the configure script from configure.ac
--        run GhcCabal $ cabalArgs pkg todo
--        when (registerPackage settings) $
--            run (GhcPkg stage) $ ghcPkgArgs pkg todo
--        postProcessPackageData $ pathDist </> "package-data.mk"

-- buildSettings = + builder Gcc ? ccSettings

-- builder Gcc ? "-tricky-flag"

ccSettings :: Settings
ccSettings = do
    let gccGe46 = liftM not gccLt46
    mconcat
        [ package integerLibrary ? arg "-Ilibraries/integer-gmp2/gmp"
        , builder GhcCabal ? argStagedConfig "conf-cc-args"
        , validating ? mconcat
            [ notBuilder GhcCabal ? arg "-Werror"
            , arg "-Wall"
            , gccIsClang ??
              ( arg "-Wno-unknown-pragmas" <>
                gccGe46 ? windowsHost ? arg "-Werror=unused-but-set-variable"
              , gccGe46 ? arg "-Wno-error=inline" )]
        ]

ldSettings :: Settings
ldSettings = builder GhcCabal ? argStagedConfig "conf-gcc-linker-args"

cppSettings :: Settings
cppSettings = builder GhcCabal ? argStagedConfig "conf-cpp-args"
