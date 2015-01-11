{-# LANGUAGE NoImplicitPrelude #-}
module Package.Dependencies (buildPackageDependencies) where

import Package.Base

buildPackageDependencies :: Package -> TodoItem -> Rules ()
buildPackageDependencies (Package name path _) (stage, dist, settings) =
    let buildDir = path </> dist
        pkgData  = buildDir </> "package-data.mk"
    in
    (buildDir </> "build" </> name <.> "m") %> \out -> do
        need ["shake/src/Package/Dependencies.hs"] -- Track changes in this file
        run (Ghc stage) $ arg "-M"
            <> packageArgs stage pkgData
            <> includeArgs path dist
            <> outputArgs ["-odir", "-stubdir"] (buildDir </> "build")
            <> arg ["-dep-makefile", out]
            <> prefixArgs "-dep-suffix" (map suffix <$> ways settings)
            <> srcArgs path pkgData
            -- <> arg SrcHcOpts -- TODO: Check that skipping all _HC_OPTS is safe.
            -- <> wayHcOpts vanilla -- TODO: i) is this needed? ii) shall we run GHC -M multiple times?
