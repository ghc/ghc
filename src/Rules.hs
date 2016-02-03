module Rules (topLevelTargets, packageRules) where

import Base
import Data.Foldable
import Expression
import GHC
import qualified Rules.Generate
import Rules.Package
import Rules.Resources
import Settings

allStages :: [Stage]
allStages = [minBound ..]

-- | 'need' all top-level build targets
topLevelTargets :: Rules ()
topLevelTargets = do

    want $ Rules.Generate.installTargets

    -- TODO: do we want libffiLibrary to be a top-level target?

    action $ do -- TODO: Add support for all rtsWays
        rtsLib    <- pkgLibraryFile Stage1 rts vanilla
        rtsThrLib <- pkgLibraryFile Stage1 rts threaded
        need [ rtsLib, rtsThrLib ]

    for_ allStages $ \stage ->
        for_ (knownPackages \\ [rts, libffi]) $ \pkg -> action $ do
            let target = PartialTarget stage pkg
            activePackages <- interpretPartial target getPackages
            when (pkg `elem` activePackages) $
                if isLibrary pkg
                then do -- build a library
                    ways    <- interpretPartial target getLibraryWays
                    libs    <- traverse (pkgLibraryFile stage pkg) ways
                    haddock <- interpretPartial target buildHaddock
                    need $ libs ++ [ pkgHaddockFile pkg | haddock && stage == Stage1 ]
                else do -- otherwise build a program
                    need [ fromJust $ programPath stage pkg ] -- TODO: drop fromJust

packageRules :: Rules ()
packageRules = do
    resources <- resourceRules
    for_ allStages $ \stage ->
        for_ knownPackages $ \pkg ->
            buildPackage resources $ PartialTarget stage pkg
