module Rules (generateTargets, packageRules) where

import Expression
import Rules.Package
import Rules.Resources
import Settings

-- generateTargets needs top-level build targets
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
    need $ reverse targets

-- TODO: use stage 2 compiler for building stage 2 packages (instead of stage 1)
packageRules :: Rules ()
packageRules = do
    resources <- resourceRules
    forM_ [Stage0, Stage1] $ \stage ->
        forM_ knownPackages $ \pkg ->
            buildPackage resources $ PartialTarget stage pkg
