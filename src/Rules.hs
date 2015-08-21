module Rules (
    oracleRules, cabalRules, configRules, packageRules, generateTargets
    ) where

import Way
import Base
import Util
import Stage
import Target (PartialTarget (..))
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
        pkgs <- interpretWithStage stage getPackages
        fmap concat . forM pkgs $ \pkg -> do
            let target    = PartialTarget stage pkg
                buildPath = targetPath stage pkg -/- "build"
            libName     <- interpretPartial target $ getPkgData LibName
            needGhciLib <- interpretPartial target $ getPkgData BuildGhciLib
            needHaddock <- interpretPartial target buildHaddock
            let ghciLib = [ buildPath -/- "HS" ++ libName <.> "o"
                          | needGhciLib == "YES" && stage /= Stage0 ]
                haddock = [ pkgHaddockFile pkg | needHaddock ]

            ways <- interpretPartial target getWays
            libs <- forM ways $ \way -> do
                extension <- libsuf way
                return $ buildPath -/- "libHS" ++ libName <.> extension

            return $ ghciLib ++ libs ++ haddock

    need $ reverse targets

-- TODO: add Stage2 (compiler only?)
packageRules :: Rules ()
packageRules = do
    resources <- resourceRules
    forM_ [Stage0, Stage1] $ \stage ->
        forM_ knownPackages $ \pkg ->
            buildPackage resources $ PartialTarget stage pkg
