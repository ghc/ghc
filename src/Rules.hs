module Rules (generateTargets, packageRules) where

import Base
import Expression
import GHC
import Rules.Copy
import Rules.Package
import Rules.Resources
import Settings

-- TODO: not all program targets should be needed explicitly
-- | generateTargets needs top-level build targets
generateTargets :: Rules ()
generateTargets = action $ do
    targets <- fmap concat . forM [Stage0 ..] $ \stage -> do
        pkgs <- interpretWithStage stage getPackages
        let libPkgs = filter isLibrary pkgs \\ [rts, libffi]
        libTargets <- fmap concat . forM libPkgs $ \pkg -> do
            let target = PartialTarget stage pkg
            needHaddock <- interpretPartial target buildHaddock
            return [ pkgHaddockFile pkg | needHaddock && stage == Stage1 ]
        let programTargets = [ prog | Just prog <- programPath stage <$> pkgs ]
        return $ libTargets ++ programTargets

    rtsLib <- pkgLibraryFile Stage1 rts "rts" vanilla
    need $ targets ++ installTargets ++ [ rtsLib ]

packageRules :: Rules ()
packageRules = do
    resources <- resourceRules
    forM_ [Stage0 ..] $ \stage ->
        forM_ knownPackages $ \pkg ->
            buildPackage resources $ PartialTarget stage pkg
