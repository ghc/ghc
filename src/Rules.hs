module Rules (
    generateTargets, packageRules, oracleRules,
    module Rules.Config,
    module Rules.Package,
    ) where

import Base
import Util
import Stage
import Expression
import Rules.Config
import Rules.Package
import Rules.Oracles
import Settings.Packages
import Settings.TargetDirectory

-- generateTargets needs package-data.mk files of all target packages
-- TODO: make interpretDiff total
generateTargets :: Rules ()
generateTargets = action $ do
    targets <- fmap concat . forM [Stage0 ..] $ \stage -> do
        pkgs <- interpret (stageTarget stage) packages
        fmap concat . forM pkgs $ \pkg -> return
            [ targetPath stage pkg -/- "build/haskell.deps"
            , targetPath stage pkg -/- "build/c.deps" ]
    need targets

-- TODO: add Stage2 (compiler only?)
packageRules :: Rules ()
packageRules =
    forM_ [Stage0, Stage1] $ \stage -> do
        forM_ knownPackages $ \pkg -> do
            buildPackage (stagePackageTarget stage pkg)
