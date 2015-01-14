{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}
module Package.Data (buildPackageData) where

import Package.Base

libraryArgs :: [Way] -> Args
libraryArgs ways =
       argEnable False "library-for-ghci" -- TODO: why always disable?
    <> argEnable (vanilla `elem` ways) "library-vanilla"
    <> (ghcWithInterpreter && not DynamicGhcPrograms && vanilla `elem` ways)
       <?> argEnable True "library-for-ghci"
    <> argEnable (profiling `elem` ways) "library-profiling"
    <> argEnable (dynamic   `elem` ways) "shared"
  where
    argEnable x key = arg $ (if x then "--enable-" else "--disable-") ++ key

configureArgs :: Stage -> Settings -> Args
configureArgs stage settings =
    let argConf key as = do
            s <- unwords <$> arg as
            unless (null s) $ arg $ "--configure-option=" ++ key ++ "=" ++ s

        cflags   =  commonCcArgs `filterOut` "-Werror"
                <+> ConfCcArgs stage
                <+> customCcArgs settings
                <+> commonCcWarninigArgs
        ldflags  =  commonLdArgs
                <+> ConfGccLinkerArgs stage
                <+> customLdArgs settings
        cppflags =  commonCppArgs
                <+> ConfCppArgs stage
                <+> customCppArgs settings

    in argConf "CFLAGS"   cflags
    <> argConf "LDFLAGS"  ldflags
    <> argConf "CPPFLAGS" cppflags
    <> arg (concat <$> "--gcc-options=" <+> cflags <+> " " <+> ldflags)
    <> argConf "--with-iconv-includes"  IconvIncludeDirs
    <> argConf "--with-iconv-libraries" IconvLibDirs
    <> argConf "--with-gmp-includes"    GmpIncludeDirs
    <> argConf "--with-gmp-libraries"   GmpLibDirs
    -- TODO: why TargetPlatformFull and not host?
    <> when CrossCompiling (argConf "--host" TargetPlatformFull)
    <> argConf "--with-cc" Gcc

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

buildPackageData :: Package -> TodoItem -> Rules ()
buildPackageData (Package name path _) (stage, dist, settings) =
    let pathDist  = path </> dist
        configure = path </> "configure"
        cabal     = path </> takeBaseName name <.> "cabal"
        cabalArgs = arg ["configure", path, dist]
            -- this is a positional argument, hence:
            -- * if it is empty, we need to emit one empty string argument
            -- * otherwise, we must collapse it into one space-separated string
            <> arg (unwords <$> customDllArgs settings)
            <> with (Ghc stage) -- TODO: used limited to max stage1 GHC
            <> with (GhcPkg stage)
            <> customConfArgs settings
            <> (libraryArgs =<< ways settings)
            <> when (specified HsColour) (with HsColour)
            <> configureArgs stage settings
            <> when (stage == Stage0) bootPkgConstraints
            <> with Gcc
            <> when (stage /= Stage0) (with Ld)
            <> with Ar
            <> with Alex
            <> with Happy -- TODO: reorder with's
        ghcPkgArgs = arg ["update", "--force"]
            <> (stage == Stage0) <?>
               arg "--package-db=libraries/bootstrapping.conf"
            <> arg (toStandard $ pathDist </> "inplace-pkg-config")
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
        need ["shake/src/Package/Data.hs"]
        need [cabal]
        when (doesFileExist $ configure <.> "ac") $ need [configure]
        terseRun GhcCabal cabalArgs
        when (registerPackage settings) $ terseRun (GhcPkg stage) ghcPkgArgs
        postProcessPackageData $ pathDist </> "package-data.mk"
