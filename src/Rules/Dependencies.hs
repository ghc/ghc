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

buildPackageDependencies :: Resources -> StagePackageTarget -> Rules ()
buildPackageDependencies _ target =
    let stage     = Target.stage target
        pkg       = Target.package target
        path      = targetPath stage pkg
        buildPath = path -/- "build"
        dropBuild = (pkgPath pkg ++) . drop (length buildPath)
    in do
        (buildPath <//> "*.c.deps") %> \depFile -> do
            let srcFile = dropBuild . dropExtension $ depFile
            build $ fullTarget target [srcFile] (GccM stage) [depFile]

        (buildPath -/- "c.deps") %> \file -> do
            srcs <- pkgDataList $ CSrcs path
            let depFiles = [ buildPath -/- src <.> "deps" | src <- srcs ]
            need depFiles -- increase parallelism by needing all at once
            deps <- mapM readFile' depFiles
            writeFileChanged file (concat deps)

        (buildPath -/- "haskell.deps") %> \file -> do
            srcs <- interpret target getHsSources
            build $ fullTarget target srcs (GhcM stage) [file]
