{-# LANGUAGE NoImplicitPrelude #-}
module Rules (
    generateTargets, packageRules,
    module Rules.Package,
    ) where

import Base hiding (arg, args, Args)
import Util
import Control.Monad
import Targets
-- import Settings
import Package
import Expression
import Rules.Package

generateTargets :: Rules ()
generateTargets = action $ do
    forM_ [Stage0 ..] $ \stage -> do
        pkgs <- evaluate $ project stage targetPackages
        case linearise pkgs of
            Nothing      -> redError "Cannot determine target packages."
            Just pkgList -> do
                forM_ pkgList $ \pkg -> do
                    dirs <- evaluate $ project (stage, pkg) targetDirectories
                    case linearise dirs of
                        Just [TargetDir dir] -> do
                            need [pkgPath pkg </> dir </> "package-data.mk"]
                        _ -> redError "Cannot determine target directory."

packageRules :: Rules ()
packageRules =
    forM_ [Stage0 ..] $ \stage -> do
        forM_ (support $ simplify $ project stage targetPackages) $ \pkg -> do
            let dirs = project (stage, pkg) targetDirectories
            case linearise dirs of
                Just [dir] -> do
                    let ways     = project (stage, pkg) targetWays
                        stgs     = project (stage, pkg, dir) buildSettings
                        buildDir = pkgPath pkg </> fromTargetDir dir
                    buildPackage stage pkg buildDir ways stgs
                _ -> action $ redError "Cannot determine target directory."
