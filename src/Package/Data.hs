{-# LANGUAGE NoImplicitPrelude #-}
module Package.Data (buildPackageData) where

import Package.Base
import Targets

argListDir :: FilePath
argListDir = "shake/arg/buildPackageData"

libraryArgs :: [Way] -> Args
libraryArgs ways = do
    let enable x = ((if x then "--enable-" else "--disable-") ++)
    libraryForGhci <- ghcWithInterpreter
                      && not DynamicGhcPrograms
                      && vanilla `elem` ways
    return $
        [ enable (vanilla   `elem` ways) "library-vanilla"
        , enable libraryForGhci          "library-for-ghci"
        , enable (profiling `elem` ways) "library-profiling"
        , enable (dynamic   `elem` ways) "shared" ]

configureArgs :: Stage -> Settings -> Args
configureArgs stage settings =
    let conf key as = do
            s <- unwords <$> args as
            unless (null s) $ arg $ "--configure-option=" ++ key ++ "=" ++ s
        cflags   = [ commonCcArgs `filterOut` ["-Werror"]
                   , args $ ConfCcArgs stage
                   -- , customCcArgs settings -- TODO: bring this back
                   , commonCcWarninigArgs ] -- TODO: check why cflags are glued
        ldflags  = [ commonLdArgs
                   , args $ ConfGccLinkerArgs stage
                   , customLdArgs settings ]
        cppflags = [ commonCppArgs
                   , args $ ConfCppArgs stage
                   , customCppArgs settings ]
    in args [ conf "CFLAGS"   cflags
            , conf "LDFLAGS"  ldflags
            , conf "CPPFLAGS" cppflags
            , arg $ concat <$>
              arg "--gcc-options=" <> args cflags <> arg " " <> args ldflags
            , conf "--with-iconv-includes"  IconvIncludeDirs
            , conf "--with-iconv-libraries" IconvLibDirs
            , conf "--with-gmp-includes"    GmpIncludeDirs
            , conf "--with-gmp-libraries"   GmpLibDirs
            -- TODO: why TargetPlatformFull and not host?
            , when CrossCompiling $ conf "--host" $ arg TargetPlatformFull
            , conf "--with-cc" $ arg $ Gcc stage ]

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

bootPkgConstraints :: Args
bootPkgConstraints = args $ do
    forM (targetPackagesInStage Stage0) $ \pkg @ (Package _ _ cabal _) -> do
        let depName = takeBaseName cabal
        need [cabal]
        content <- lines <$> liftIO (readFile cabal)
        let versionLines = filter (("ersion:" `isPrefixOf`) . drop 1) content
        case versionLines of
            [versionLine] -> return $ "--constraint " ++ depName ++ " == "
                                    ++ dropWhile (not . isDigit) versionLine
            _             -> redError $ "Cannot determine package version in '"
                                      ++ unifyPath cabal ++ "'."

bootPackageDb :: Args
bootPackageDb = do
    top <- showArg GhcSourcePath
    arg $ unifyPath $ "--package-db=" ++ top </> "libraries/bootstrapping.conf"

cabalArgs :: Package -> TodoItem -> Args
cabalArgs pkg @ (Package _ path _ _) todo @ (stage, dist, settings) = args
    [ args ["configure", path, dist]
    -- this is a positional argument, hence:
    -- * if it is empty, we need to emit one empty string argument
    -- * otherwise, we must collapse it into one space-separated string
    , arg (unwords <$> customDllArgs settings)
    , with $ Ghc stage -- TODO: used to be limited to max stage1 GHC
    , with $ GhcPkg stage
    , customConfArgs settings
    , when (stage == Stage0) bootPackageDb
    , libraryArgs =<< ways settings
    , when (specified HsColour) $ with HsColour
    , configureArgs stage settings
    , when (stage == Stage0) bootPkgConstraints
    , with $ Gcc stage
    , when (stage /= Stage0) $ with Ld
    , with Ar
    , with Alex
    , with Happy ] -- TODO: reorder with's

ghcPkgArgs :: Package -> TodoItem -> Args
ghcPkgArgs (Package _ path _ _) (stage, dist, _) = args $
    [ arg "update"
    , arg "--force"
    , arg $ unifyPath $ path </> dist </> "inplace-pkg-config"
    , when (stage == Stage0) bootPackageDb ]

buildRule :: Package -> TodoItem -> Rules ()
buildRule pkg @ (Package name path cabal _) todo @ (stage, dist, settings) =
    let pathDist  = path </> dist
        configure = path </> "configure"
    in
    -- All these files are produced by a single run of GhcCabal
    (pathDist </>) <$>
    [ "package-data.mk"
    , "haddock-prologue.txt"
    , "inplace-pkg-config"
    , "setup-config"
    , "build" </> "autogen" </> "cabal_macros.h"
    -- TODO: Is this needed? Also check out Paths_cpsa.hs.
    -- , "build" </> "autogen" </> ("Paths_" ++ name) <.> "hs"
    ] &%> \_ -> do
        need [cabal]
        when (doesFileExist $ configure <.> "ac") $ need [configure]
        -- GhcCabal will run the configure script, so we depend on it
        -- We still don't know who build the configure script from configure.ac
        run GhcCabal $ cabalArgs pkg todo
        when (registerPackage settings) $
            run (GhcPkg stage) $ ghcPkgArgs pkg todo
        postProcessPackageData $ pathDist </> "package-data.mk"
        -- Finally, record the argument list
        need [argListPath argListDir pkg stage]

argListRule :: Package -> TodoItem -> Rules ()
argListRule pkg todo @ (stage, _, _) =
    (argListPath argListDir pkg stage) %> \out -> do
        -- TODO: depend on ALL source files
        need $ ["shake/src/Package/Data.hs"] ++ sourceDependecies
        cabalList  <- argList GhcCabal       $ cabalArgs pkg todo
        ghcPkgList <- argList (GhcPkg stage) $ ghcPkgArgs pkg todo
        writeFileChanged out $ cabalList ++ "\n" ++ ghcPkgList

-- How to build package-data.mk using GhcCabal to process package.cabal
buildPackageData :: Package -> TodoItem -> Rules ()
buildPackageData = argListRule <> buildRule
