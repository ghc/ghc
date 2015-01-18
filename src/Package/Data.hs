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
        cflags   = [ commonCcArgs `filterOut` "-Werror"
                   , args $ ConfCcArgs stage
                   , customCcArgs settings
                   , commonCcWarninigArgs ]
        ldflags  = [ commonLdArgs
                   , args $ ConfGccLinkerArgs stage
                   , customLdArgs settings ]
        cppflags = [ commonCppArgs
                   , args $ ConfCppArgs stage
                   , customCppArgs settings ]
    in args [ conf "CFLAGS"   cflags
            , conf "LDFLAGS"  ldflags
            , conf "CPPFLAGS" cppflags
            , arg $ concat <$> "--gcc-options=" <+> cflags <+> " " <+> ldflags
            , conf "--with-iconv-includes"  IconvIncludeDirs
            , conf "--with-iconv-libraries" IconvLibDirs
            , conf "--with-gmp-includes"    GmpIncludeDirs
            , conf "--with-gmp-libraries"   GmpLibDirs
            -- TODO: why TargetPlatformFull and not host?
            , when CrossCompiling $ conf "--host" $ arg TargetPlatformFull
            , conf "--with-cc" $ arg $ Gcc stage ]

-- Prepare a given 'packaga-data.mk' file for parsing by readConfigFile:
-- 1) Drop lines containing '$'
-- 2) Replace '/' and '\' with '_' before '='
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
    forM (libraryPackagesInStage Stage0) $ \name -> do
        let path     = pkgPath $ libraryPackage name [Stage0] defaultSettings
            baseName = takeBaseName name
            cabal    = path </> baseName <.> "cabal"
        need [cabal]
        content <- lines <$> liftIO (readFile cabal)
        let versionLines = filter (("ersion:" `isPrefixOf`) . drop 1) content
        case versionLines of
            [versionLine] -> args ["--constraint", baseName ++ " == "
                                    ++ dropWhile (not . isDigit) versionLine ]
            _             -> redError $ "Cannot determine package version in '"
                                      ++ toStandard cabal ++ "'."

bootPackageDb :: Args
bootPackageDb = do
    top <- showArg GhcSourcePath
    arg $ toStandard
        $ "--package-db=" ++ top </> "libraries/bootstrapping.conf"

cabalArgs :: Package -> TodoItem -> Args
cabalArgs pkg @ (Package _ path _) todo @ (stage, dist, settings) = args
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
ghcPkgArgs (Package _ path _) (stage, dist, _) = args $
    [ arg "update"
    , arg "--force"
    , arg $ toStandard $ path </> dist </> "inplace-pkg-config"
    , when (stage == Stage0) bootPackageDb ]

buildRule :: Package -> TodoItem -> Rules ()
buildRule pkg @ (Package name path _) todo @ (stage, dist, settings) =
    let pathDist  = path </> dist
        configure = path </> "configure"
        cabal     = path </> takeBaseName name <.> "cabal"
    in
    (pathDist </>) <$>
    [ "package-data.mk"
    , "haddock-prologue.txt"
    , "inplace-pkg-config"
    , "setup-config"
    , "build" </> "autogen" </> "cabal_macros.h"
    -- TODO: Is this needed? Also check out Paths_cpsa.hs.
    -- , "build" </> "autogen" </> ("Paths_" ++ name) <.> "hs"
    ] &%> \_ -> do
        need [argListPath argListDir pkg stage, cabal]
        when (doesFileExist $ configure <.> "ac") $ need [configure]
        terseRun GhcCabal $ cabalArgs pkg todo
        when (registerPackage settings) $
            terseRun (GhcPkg stage) $ ghcPkgArgs pkg todo
        postProcessPackageData $ pathDist </> "package-data.mk"

argListRule :: Package -> TodoItem -> Rules ()
argListRule pkg todo @ (stage, _, _) =
    (argListPath argListDir pkg stage) %> \out -> do
        need $ ["shake/src/Package/Data.hs"] ++ sourceDependecies
        cabalList  <- argList GhcCabal       $ cabalArgs pkg todo
        ghcPkgList <- argList (GhcPkg stage) $ ghcPkgArgs pkg todo
        writeFileChanged out $ cabalList ++ "\n" ++ ghcPkgList

buildPackageData :: Package -> TodoItem -> Rules ()
buildPackageData = argListRule <> buildRule
