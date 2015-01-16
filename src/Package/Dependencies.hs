{-# LANGUAGE NoImplicitPrelude #-}
module Package.Dependencies (buildPackageDependencies) where

import Package.Base

argListDir :: FilePath
argListDir = "shake/arg/buildPackageDependencies"

ghcArgs :: Package -> TodoItem -> Args
ghcArgs (Package name path _) (stage, dist, settings) =
    let pathDist = path </> dist
        buildDir = toStandard $ pathDist </> "build"
        depFile  = buildDir </> takeBaseName name <.> "m"
    in arg [ arg "-M"
           , packageArgs stage pathDist
           , includeArgs path dist
           , concatArgs ["-optP"] $ CppOpts pathDist
           , productArgs ["-odir", "-stubdir", "-hidir"] buildDir
           , arg ["-dep-makefile", depFile <.> "new"]
           , productArgs "-dep-suffix" $ map wayPrefix <$> ways settings
           , arg $ HsOpts pathDist
           , arg $ pkgHsSources path dist ]

buildRule :: Package -> TodoItem -> Rules ()
buildRule pkg @ (Package name path _) todo @ (stage, dist, settings) =
    let buildDir = toStandard $ path </> dist </> "build"
    in
    (buildDir </> takeBaseName name <.> "m") %> \out -> do
        need [argListPath argListDir pkg stage]
        terseRun (Ghc stage) $ ghcArgs pkg todo
        -- Avoid rebuilding dependecies of out if it hasn't changed:
        -- Note: cannot use copyFileChanged as it depends on the source file
        deps <- liftIO $ readFile $ out <.> "new"
        writeFileChanged out deps
        removeFilesAfter "." [out <.> "new"]

argListRule :: Package -> TodoItem -> Rules ()
argListRule pkg todo @ (stage, _, _) =
    (argListPath argListDir pkg stage) %> \out -> do
        need $ ["shake/src/Package/Dependencies.hs"] ++ sourceDependecies
        ghcList <- argList (Ghc stage) $ ghcArgs pkg todo
        writeFileChanged out ghcList

buildPackageDependencies :: Package -> TodoItem -> Rules ()
buildPackageDependencies = argListRule <> buildRule
