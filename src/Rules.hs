module Rules (generateTargets, packageRules) where

import Base
import Data.Foldable
import Expression
import GHC
import Rules.Generate
import Rules.Package
import Rules.Resources
import Settings

allStages :: [Stage]
allStages = [minBound ..]

-- TODO: not all program targets should be needed explicitly
-- | 'need' all top-level build targets
generateTargets :: Rules ()
generateTargets = action $ do
    targets <- fmap concat (traverse targetsForStage allStages)
    rtsLib <- pkgLibraryFile Stage1 rts "rts" vanilla
    need $ targets ++ installTargets ++ [ rtsLib ]

targetsForStage :: Stage -> Action [String]
targetsForStage stage = do
    pkgs <- interpretWithStage stage getPackages
    let libPkgs = filter isLibrary pkgs \\ [rts, libffi]
    libTargets <- fmap concat . forM libPkgs $ \pkg -> do
        let target = PartialTarget stage pkg
        needHaddock <- interpretPartial target buildHaddock
        return [ pkgHaddockFile pkg | needHaddock && stage == Stage1 ]
    let programTargets = [ prog | Just prog <- programPath stage <$> pkgs ]
    return (libTargets ++ programTargets)

packageRules :: Rules ()
packageRules = do
    resources <- resourceRules
    for_ allStages $ \stage ->
        for_ knownPackages $ \pkg ->
            buildPackage resources $ PartialTarget stage pkg
