module Rules (
    generateTargets, packageRules, oracleRules,
    module Rules.Package,
    ) where

import Stage
import Expression
import Rules.Package
import Rules.Oracles
import Settings.Packages
import Settings.TargetDirectory
import Development.Shake
import Development.Shake.FilePath

-- generateTargets needs package-data.mk files of all target packages
-- TODO: make interpretDiff total
generateTargets :: Rules ()
generateTargets = action $
    forM_ [Stage0 ..] $ \stage -> do
        pkgs <- interpret (stageTarget stage) packages
        forM_ pkgs $ \pkg -> do
            need [targetPath stage pkg </> "package-data.mk"]

-- TODO: add Stage2 (compiler only?)
packageRules :: Rules ()
packageRules =
    forM_ [Stage0, Stage1] $ \stage -> do
        forM_ knownPackages $ \pkg -> do
            buildPackage (stagePackageTarget stage pkg)
