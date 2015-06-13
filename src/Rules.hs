{-# LANGUAGE NoImplicitPrelude #-}
module Rules (
    generateTargets, packageRules,
    module Rules.Package,
    ) where

import Base hiding (arg, args, Args)
import Control.Monad
import Targets
import Settings
import Package
import Expression
import Rules.Package

-- generateTargets needs package-data.mk files of all target packages
-- TODO: make interpret total
generateTargets :: Rules ()
generateTargets = action $
    forM_ [Stage0 ..] $ \stage ->
        forM_ targetPackages $ \pkg -> do
            let env = defaultEnvironment { getStage = stage, getPackage = pkg }
                dir = targetDirectory stage pkg
            required <- interpret env (packagePredicate pkg)
            when required $ need [pkgPath pkg </> dir </> "package-data.mk"]

-- TODO: simplify
-- TODO: make interpret total
-- TODO: add Stage2 (compiler only?)
packageRules :: Rules ()
packageRules =
    forM_ [Stage0, Stage1] $ \stage -> do
        forM_ targetPackages $ \pkg -> do
            let env = defaultEnvironment { getStage = stage, getPackage = pkg }
            buildPackage env targetWays buildSettings
