module Rules (
    oracleRules, cabalRules, configRules, packageRules, generateTargets
    ) where

import Way
import Base
import Util
import Stage
import Expression
import Oracles.PackageData
import Rules.Cabal
import Rules.Config
import Rules.Package
import Rules.Oracles
import Rules.Resources
import Settings.Ways
import Settings.User
import Settings.Util
import Settings.Packages
import Settings.TargetDirectory

-- generateTargets needs package-data.mk files of all target packages
generateTargets :: Rules ()
generateTargets = action $ do
    targets <- fmap concat . forM [Stage0 ..] $ \stage -> do
        pkgs <- interpret (stageTarget stage) getPackages
        fmap concat . forM pkgs $ \pkg -> do
            let target    = stagePackageTarget stage pkg
                buildPath = targetPath stage pkg -/- "build"
            buildGhciLib <- interpret target $ getPkgData BuildGhciLib
            pkgKey       <- interpret target $ getPkgData PackageKey
            buildHaddock <- interpret target $ Settings.User.buildHaddock
            let ghciLib = [ buildPath -/- "HS" ++ pkgKey <.> "o"
                          | buildGhciLib == "YES" && stage /= Stage0 ]
                haddock = [ pkgHaddockPath pkg | buildHaddock ]

            ways <- interpret target getWays
            libs <- forM ways $ \way -> do
                extension <- libsuf way
                return $ buildPath -/- "libHS" ++ pkgKey <.> extension

            return $ ghciLib ++ libs ++ haddock

    need $ reverse targets

-- TODO: add Stage2 (compiler only?)
packageRules :: Rules ()
packageRules = do
    resources <- resourceRules
    forM_ [Stage0, Stage1] $ \stage -> do
        forM_ knownPackages $ \pkg -> do
            buildPackage resources (stagePackageTarget stage pkg)
