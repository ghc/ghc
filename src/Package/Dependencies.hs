{-# LANGUAGE NoImplicitPrelude #-}
module Package.Dependencies (buildPackageDependencies) where

import Package.Base

packageKeyArgs :: Stage -> FilePath -> Args
packageKeyArgs stage pkgData =
    arg "-hide-all-packages" <>
    (pkgArgs =<< SupportsPackageKey || stage /= Stage0)
  where
    pkgArgs True = "-this-package-key"
                <+> PackageKey pkgData
                <+> prepend "-package-key " (DepKeys pkgData)
    pkgArgs _    = "-package-name"
                <+> PackageKey pkgData
                <+> prepend "-package "     (Deps    pkgData)
    prepend pref = (map (pref ++) <$>) . arg

includeArgs :: ShowArgs a => String -> FilePath -> a -> Args
includeArgs prefix path as = map includePath <$> arg as
  where
    includePath dir | isRelative dir = prefix ++ path </> dir
                    | isAbsolute dir = prefix         </> dir

srcArgs :: FilePath -> FilePath -> Args
srcArgs path pkgData = do
    mods <- map (replaceEq '.' pathSeparator) <$> arg (Modules pkgData)
    dirs <- arg (SrcDirs pkgData)
    srcs <- getDirectoryFiles "" $
        [path </> dir </> mPath <.> ext | dir <- dirs, mPath <- mods, ext <- ["hs", "lhs"]]
    arg (map normalise srcs)

buildPackageDependencies :: Package -> TodoItem -> Rules ()
buildPackageDependencies pkg @ (Package name path _) (stage, dist, settings) =
    let buildDir = path </> dist
        pkgData  = buildDir </> "package-data.mk"
    in
    (buildDir </> "build" </> name <.> "m") %> \out -> do
        need ["shake/src/Package/Dependencies.hs"] -- Track changes in this file
        run (Ghc stage) $ arg "-M"
            <> when (stage == Stage0) (arg "-package-db libraries/bootstrapping.conf")
            <> packageKeyArgs stage pkgData
            <> arg "-i"
            <> includeArgs "-i" path     (SrcDirs pkgData)
            <> includeArgs "-i" buildDir ["build", "build/autogen"]
            <> includeArgs "-I" buildDir ["build", "build/autogen"]
            <> includeArgs "-I" path     (IncludeDirs pkgData)
            <> arg ["-optP-include", "-optP" ++ buildDir </> "build/autogen/cabal_macros.h"]
            <> arg "-no-user-package-db"
            <> arg ["-odir"   , buildDir </> "build"]
            <> arg ["-stubdir", buildDir </> "build"]
            <> arg ("-dep-makefile " ++ out)
            <> (concatMap (\w -> ["-dep-suffix", suffix w]) <$> ways settings)
            <> arg "-include-pkg-deps"
            <> srcArgs path pkgData
            -- <> arg SrcHcOpts -- TODO: Check that skipping all _HC_OPTS is safe.
            -- <> wayHcOpts vanilla -- TODO: i) is this needed? ii) shall we run GHC -M multiple times?
