module Rules.Dependencies (buildPackageDependencies) where

import Util
import Builder
import Package
import Expression
import qualified Target
import Oracles.PackageData
import Settings.Util
import Settings.TargetDirectory
import Rules.Actions
import Development.Shake

buildPackageDependencies :: StagePackageTarget -> Rules ()
buildPackageDependencies target =
    let stage     = Target.stage target
        pkg       = Target.package target
        path      = targetPath stage pkg
        buildPath = path -/- "build"
    in do
        (buildPath -/- "haskell.deps") %> \file -> do
            srcs <- interpretExpr target getHsSources
            build $ fullTarget target srcs (GhcM stage) [file]

        (buildPath -/- "c.deps") %> \file -> do
            srcs <- pkgDataList $ CSrcs path
            deps <- forM srcs $ \src -> do
                let srcFile = pkgPath pkg -/- src
                    depFile = buildPath -/- takeFileName src <.> "deps"
                build $ fullTarget target [srcFile] (GccM stage) [depFile]
                liftIO . readFile $ depFile
            writeFileChanged file (concat deps)
            liftIO $ removeFiles buildPath ["*.c.deps"]
