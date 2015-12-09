module Rules (generateTargets, packageRules) where

import Expression
import Oracles
import Rules.Package
import Rules.Resources
import Settings
import Settings.Builders.GhcCabal

-- generateTargets needs top-level build targets
generateTargets :: Rules ()
generateTargets = action $ do
    targets <- fmap concat . forM [Stage0 ..] $ \stage -> do
        pkgs <- interpretWithStage stage getPackages
        let (libPkgs, programPkgs) = partition isLibrary pkgs
        libTargets <- fmap concat . forM libPkgs $ \pkg -> do
            let target    = PartialTarget stage pkg
                buildPath = targetPath stage pkg -/- "build"
            libName     <- interpretPartial target $ getPkgData LibName
            needGhciLib <- interpretPartial target $ getPkgData BuildGhciLib
            needHaddock <- interpretPartial target buildHaddock
            ways        <- interpretPartial target getWays
            let ghciLib = buildPath -/- "HS" ++ libName <.> "o"
                haddock = pkgHaddockFile pkg
            libs <- fmap concat . forM ways $ \way -> do
                extension <- libsuf way
                let name = buildPath -/- "libHS" ++ libName
                dll0 <- needDll0 stage pkg
                return $ [ name <.> extension ]
                      ++ [ name ++ "-0" <.> extension | dll0 ]

            return $  [ ghciLib | needGhciLib == "YES" && stage == Stage1 ]
                   ++ [ haddock | needHaddock          && stage == Stage1 ]
                   ++ libs

        let programTargets = map (fromJust . programPath stage) programPkgs

        return $ libTargets ++ programTargets

    need $ reverse targets

-- TODO: add Stage2 (compiler only?)
packageRules :: Rules ()
packageRules = do
    resources <- resourceRules
    forM_ [Stage0, Stage1] $ \stage ->
        forM_ knownPackages $ \pkg ->
            buildPackage resources $ PartialTarget stage pkg
