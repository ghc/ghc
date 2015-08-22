module Rules.Dependencies (buildPackageDependencies) where

import Base
import Util
import Builder
import Package
import Expression
import Target (PartialTarget (..), fullTarget)
import Oracles.PackageData
import Settings
import Rules.Actions
import Rules.Resources

buildPackageDependencies :: Resources -> PartialTarget -> Rules ()
buildPackageDependencies _ target @ (PartialTarget stage pkg) =
    let path      = targetPath stage pkg
        buildPath = path -/- "build"
        dropBuild = (pkgPath pkg ++) . drop (length buildPath)
        hDepFile  = buildPath -/- ".hs-dependencies"
    in do
        (buildPath <//> "*.c.deps") %> \out -> do
            let srcFile = dropBuild . dropExtension $ out
            need [srcFile]
            build $ fullTarget target (GccM stage) [srcFile] [out]

        hDepFile %> \file -> do
            srcs <- interpretPartial target getPackageSources
            need srcs
            build $ fullTarget target (GhcM stage) srcs [file]
            removeFileIfExists $ file <.> "bak"

        (buildPath -/- ".dependencies") %> \file -> do
            cSrcs <- pkgDataList $ CSrcs path
            let cDepFiles = [ buildPath -/- src <.> "deps" | src <- cSrcs ]
            need $ hDepFile : cDepFiles -- need all for more parallelism
            cDeps <- fmap concat $ mapM readFile' cDepFiles
            hDeps <- readFile' hDepFile
            writeFileChanged file $ cDeps ++ hDeps

