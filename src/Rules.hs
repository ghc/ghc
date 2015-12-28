module Rules (generateTargets, packageRules) where

import Base
import Expression
import Rules.Install
import Rules.Package
import Rules.Resources
import Settings

-- TODO: not all program targets should be needed explicitly
-- | generateTargets needs top-level build targets
generateTargets :: Rules ()
generateTargets = action $ do
    targets <- fmap concat . forM [Stage0 ..] $ \stage -> do
        pkgs <- interpretWithStage stage getPackages
        let libPkgs = filter isLibrary pkgs
        libTargets <- fmap concat . forM libPkgs $ \pkg -> do
            let target = PartialTarget stage pkg
            needHaddock <- interpretPartial target buildHaddock
            return [ pkgHaddockFile pkg | needHaddock && stage == Stage1 ]
        let programTargets = [ prog | Just prog <- programPath stage <$> pkgs ]
        return $ libTargets ++ programTargets

    need $ targets ++ installTargets

packageRules :: Rules ()
packageRules = do
    resources <- resourceRules
    forM_ [Stage0 ..] $ \stage ->
        forM_ knownPackages $ \pkg ->
            buildPackage resources $ PartialTarget stage pkg
