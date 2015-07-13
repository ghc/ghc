{-# LANGUAGE NoImplicitPrelude #-}
module Rules (
    generateTargets, packageRules,
    module Rules.Package,
    ) where

import Base hiding (arg, args, Args)
import Control.Monad
import Expression
import Rules.Package
import Settings.Packages
import Settings.TargetDirectory

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
