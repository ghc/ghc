{-# LANGUAGE NoImplicitPrelude #-}
module Package.Dependencies (buildPackageDependencies) where

import Package.Base

packageArgs :: Stage -> FilePath -> Args
packageArgs stage pkgData = do
    usePackageKey <- SupportsPackageKey || stage /= Stage0
    arg ["-hide-all-packages", "-no-user-package-db", "-include-pkg-deps"]
        <> when (stage == Stage0) (arg "-package-db libraries/bootstrapping.conf")
        <> keyArgs usePackageKey
  where
    keyArgs True  = prefixArgs "-this-package-key" (PackageKey pkgData) <>
                    prefixArgs "-package-key"      (DepKeys    pkgData)
    keyArgs False = prefixArgs "-package-name"     (PackageKey pkgData) <>
                    prefixArgs "-package"          (Deps       pkgData)

includeArgs :: ShowArgs a => String -> FilePath -> a -> Args
includeArgs prefix path as = map includePath <$> arg as
  where
    includePath dir | isRelative dir = prefix ++ path </> dir
                    | isAbsolute dir = prefix         </> dir

srcArgs :: FilePath -> FilePath -> Args
srcArgs path pkgData = do
    mods <- map (replaceEq '.' pathSeparator) <$> arg (Modules pkgData)
    dirs <- arg (SrcDirs pkgData)
    srcs <- getDirectoryFiles ""
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
            <> packageArgs stage pkgData
            <> arg "-i"
            <> includeArgs "-i" path     (SrcDirs pkgData)
            <> includeArgs "-i" buildDir ["build", "build/autogen"]
            <> includeArgs "-I" buildDir ["build", "build/autogen"]
            <> includeArgs "-I" path     (IncludeDirs pkgData)
            <> arg "-optP-include" -- TODO: Shall we also add -cpp?
            <> arg ("-optP" ++ buildDir </> "build/autogen/cabal_macros.h")
            <> arg ["-odir"        , buildDir </> "build"]
            <> arg ["-stubdir"     , buildDir </> "build"]
            <> arg ["-dep-makefile", out                 ]
            <> prefixArgs "-dep-suffix" (map suffix <$> ways settings)
            <> srcArgs path pkgData
            -- <> arg SrcHcOpts -- TODO: Check that skipping all _HC_OPTS is safe.
            -- <> wayHcOpts vanilla -- TODO: i) is this needed? ii) shall we run GHC -M multiple times?
