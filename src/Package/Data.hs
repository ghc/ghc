{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}
module Package.Data (buildPackageData) where

import Package.Base

libraryArgs :: [Way] -> Args
libraryArgs ways =
       argEnable False "library-for-ghci" -- TODO: why always disable?
    <> argEnable (vanilla `elem` ways) "library-vanilla"
    <> when (ghcWithInterpreter && not DynamicGhcPrograms && vanilla `elem` ways) (argEnable True "library-for-ghci")
    <> argEnable (profiling `elem` ways) "library-profiling"
    <> argEnable (dynamic   `elem` ways) "shared"
  where
    argEnable x suffix = arg $ (if x then "--enable-" else "--disable-") ++ suffix

configureArgs :: Stage -> Settings -> Args
configureArgs stage settings = 
    let argConf key as = do
            s <- unwords <$> arg as
            unless (null s) $ arg $ "--configure-option=" ++ key ++ "=" ++ s

        cflags   =  commonCcArgs `filterOut` "-Werror"
                <+> ConfCcArgs stage
                <+> customCcArgs settings
                <+> commonCcWarninigArgs
        ldflags  = commonLdArgs  <+> ConfGccLinkerArgs stage <+> customLdArgs  settings
        cppflags = commonCppArgs <+> ConfCppArgs       stage <+> customCppArgs settings

    in argConf "CFLAGS"   cflags
    <> argConf "LDFLAGS"  ldflags
    <> argConf "CPPFLAGS" cppflags
    <> arg (concat <$> "--gcc-options=" <+> cflags <+> " " <+> ldflags)
    <> argConf "--with-iconv-includes"  IconvIncludeDirs
    <> argConf "--with-iconv-libraries" IconvLibDirs
    <> argConf "--with-gmp-includes"    GmpIncludeDirs
    <> argConf "--with-gmp-libraries"   GmpLibDirs
    <> when CrossCompiling (argConf "--host" TargetPlatformFull) -- TODO: why not host?
    <> argConf "--with-cc" Gcc

buildPackageData :: Package -> TodoItem -> Rules ()
buildPackageData pkg @ (Package name path _) (stage, dist, settings) =
    let buildDir = path </> dist
        cabalArgs = arg ["configure", path, dist]
            -- this is a positional argument, hence:
            -- * if it is empty, we need to emit one empty string argument
            -- * if there are many, we must collapse them into one space-separated string
            <> arg (unwords <$> customDllArgs settings)
            <> with (Ghc stage) -- TODO: used to be stage01 (using max stage1 GHC)
            <> with (GhcPkg stage)
            <> customConfArgs settings
            <> (libraryArgs =<< ways settings)
            <> when hsColourSrcs (with HsColour)
            <> configureArgs stage settings
            <> when (stage == Stage0) bootPkgConstraints
            <> with Gcc
            <> when (stage /= Stage0) (with Ld)            
            <> with Ar
            <> with Alex
            <> with Happy -- TODO: reorder with's
        ghcPkgArgs = arg ["update", "--force"]
            <> when (stage == Stage0) (arg "--package-db=libraries/bootstrapping.conf")
            <> arg (buildDir </> "inplace-pkg-config")
    in
    (buildDir </>) <$>
    [ "package-data.mk"
    , "haddock-prologue.txt"
    , "inplace-pkg-config"
    , "setup-config"
    , "build" </> "autogen" </> "cabal_macros.h"
    , "build" </> "autogen" </> ("Paths_" ++ name) <.> "hs" -- TODO: Is this needed? Also check out Paths_cpsa.hs.
    ] &%> \_ -> do
        need ["shake/src/Package/Data.hs"] -- Track changes in this file
        need [path </> name <.> "cabal"]
        when (doesFileExist $ path </> "configure.ac") $ need [path </> "configure"]
        run GhcCabal cabalArgs
        when (registerPackage settings) $ run (GhcPkg stage) ghcPkgArgs
        postProcessPackageData $ buildDir </> "package-data.mk"
