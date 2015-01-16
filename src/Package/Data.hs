{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}
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
            s <- unwords <$> arg as
            unless (null s) $ arg $ "--configure-option=" ++ key ++ "=" ++ s
        cflags   = [ commonCcArgs `filterOut` "-Werror"
                   , arg $ ConfCcArgs stage
                   , customCcArgs settings
                   , commonCcWarninigArgs ]
        ldflags  = [ commonLdArgs
                   , arg $ ConfGccLinkerArgs stage
                   , customLdArgs settings ]
        cppflags = [ commonCppArgs
                   , arg $ ConfCppArgs stage
                   , customCppArgs settings ]
    in arg [ conf "CFLAGS"   cflags
           , conf "LDFLAGS"  ldflags
           , conf "CPPFLAGS" cppflags
           , arg $ concat <$> "--gcc-options=" <+> cflags <+> " " <+> ldflags
           , conf "--with-iconv-includes"  IconvIncludeDirs
           , conf "--with-iconv-libraries" IconvLibDirs
           , conf "--with-gmp-includes"    GmpIncludeDirs
           , conf "--with-gmp-libraries"   GmpLibDirs
           -- TODO: why TargetPlatformFull and not host?
           , when CrossCompiling $ conf "--host" $ showArg TargetPlatformFull
           , conf "--with-cc" $ showArg Gcc ]

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


bootPkgConstraints :: Package -> TodoItem -> Args
bootPkgConstraints (Package _ path _) (_, dist, _) = do
    let pkgData  = path </> dist </> "package-data.mk"
    deps <- arg $ DepNames pkgData
    let depsStage0 = filter ((`elem` deps) . takeBaseName)
                     $ libraryPackageNames Stage0
    putNormal $ "depsStage0 = " ++ show depsStage0
    forM depsStage0 $ \dep -> do
        let depPkg             = libraryPackage dep [Stage0] defaultSettings
            (_, depPkgDist, _) = head $ pkgTodo depPkg
            depPkgData         = pkgPath depPkg </> depPkgDist
                                 </> "package-data.mk"
        [version] <- arg $ Version depPkgData
        return $ "--constraint " ++ dep ++ " == " ++ version

cabalArgs :: Package -> TodoItem -> Args
cabalArgs pkg @ (Package _ path _) todo @ (stage, dist, settings) = arg
    [ arg ["configure", path, dist]
    -- this is a positional argument, hence:
    -- * if it is empty, we need to emit one empty string argument
    -- * otherwise, we must collapse it into one space-separated string
    , arg (unwords <$> customDllArgs settings)
    , with (Ghc stage) -- TODO: used limited to max stage1 GHC
    , with (GhcPkg stage)
    , customConfArgs settings
    , libraryArgs =<< ways settings
    , when (specified HsColour) $ with HsColour
    , configureArgs stage settings
    , when (stage == Stage0) $ bootPkgConstraints pkg todo
    , with Gcc
    , when (stage /= Stage0) $ with Ld
    , with Ar
    , with Alex
    , with Happy ] -- TODO: reorder with's

ghcPkgArgs :: Package -> TodoItem -> Args
ghcPkgArgs (Package _ path _) (stage, dist, _) = return $
    [ "update"
    , "--force"
    , toStandard $ path </> dist </> "inplace-pkg-config" ]
    ++
    [ "--package-db=libraries/bootstrapping.conf" | stage == Stage0 ]

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
