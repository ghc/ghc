module Rules.Dependencies (buildPackageDependencies) where

import Util
import Builder
import Package
import Expression
import qualified Target
import Oracles.PackageData
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
        (buildPath -/- "haskell.deps") %> \file ->
            build $ fullTarget target [file] (GhcM stage)

        (buildPath -/- "c.deps") %> \file -> do
            srcs <- pkgDataList $ CSrcs path
            deps <- fmap concat $ forM srcs $ \src -> do
                build $ fullTarget target [pkgPath pkg -/- src] (GccM stage)
                liftIO $ readFile (buildPath -/- takeFileName src <.> "deps")
            writeFileChanged file deps
            liftIO $ removeFiles path ["*.c.deps"]
