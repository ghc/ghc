module Rules.Dependencies (buildPackageDependencies) where

import Base
import Util
import Builder
import Package
import Expression
import qualified Target
import Oracles.PackageData
import Settings.Util
import Settings.TargetDirectory
import Rules.Actions
import Rules.Resources
import qualified System.Directory as IO

buildPackageDependencies :: Resources -> StagePackageTarget -> Rules ()
buildPackageDependencies _ target =
    let stage     = Target.stage target
        pkg       = Target.package target
        path      = targetPath stage pkg
        buildPath = path -/- "build"
        dropBuild = (pkgPath pkg ++) . drop (length buildPath)
        hDepFile  = buildPath -/- ".hs-dependencies"
    in do
        (buildPath <//> "*.c.deps") %> \file -> do
            let srcFile = dropBuild . dropExtension $ file
            need [srcFile]
            build $ fullTarget target (GccM stage) [srcFile] [file]

        hDepFile %> \file -> do
            srcs <- interpret target getPackageSources
            need srcs
            build $ fullTarget target (GhcM stage) srcs [file]
            liftIO . IO.removeFile $ file <.> "bak"

        (buildPath -/- ".dependencies") %> \file -> do
            cSrcs <- pkgDataList $ CSrcs path
            let cDepFiles = [ buildPath -/- src <.> "deps" | src <- cSrcs ]
            need $ hDepFile : cDepFiles -- need all for more parallelism
            cDeps <- fmap concat $ mapM readFile' cDepFiles
            hDeps <- readFile' hDepFile
            writeFileChanged file $ cDeps ++ hDeps

