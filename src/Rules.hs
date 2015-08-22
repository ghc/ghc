module Rules (generateTargets, packageRules) where

import Expression
import Oracles
import Rules.Package
import Rules.Resources
import Settings

-- generateTargets needs top-level build targets
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
            ways        <- interpretPartial target getWays
            let ghciLib = buildPath -/- "HS" ++ libName <.> "o"
                haddock = pkgHaddockFile pkg
            libs <- forM ways $ \way -> do
                extension <- libsuf way
                return $ buildPath -/- "libHS" ++ libName <.> extension

            return $  [ ghciLib | needGhciLib == "YES" && stage == Stage1 ]
                   ++ [ haddock | needHaddock          && stage == Stage1 ]
                   ++ libs

    need $ reverse targets

-- TODO: add Stage2 (compiler only?)
packageRules :: Rules ()
packageRules = do
    resources <- resourceRules
    forM_ [Stage0, Stage1] $ \stage ->
        forM_ knownPackages $ \pkg ->
            buildPackage resources $ PartialTarget stage pkg
