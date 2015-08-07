module Rules.Library (buildPackageLibrary) where

import Way
import Base
import Util
import Builder
import Package
import Switches
import Expression
import qualified Target
import Oracles.PackageData
import Settings.Util
import Settings.TargetDirectory
import Rules.Actions
import Rules.Resources
import Data.List
import Data.Maybe

buildPackageLibrary :: Resources -> StagePackageTarget -> Rules ()
buildPackageLibrary _ target = do
    let stage     = Target.stage target
        pkg       = Target.package target
        path      = targetPath stage pkg
        buildPath = path -/- "build"

    -- TODO: handle dynamic libraries
    matchBuildResult buildPath "a" ?> \a -> do
        liftIO $ removeFiles "." [a]
        cSrcs   <- interpret target $ getPkgDataList CSrcs
        modules <- interpret target $ getPkgDataList Modules

        let way   = fromJust . detectWay $ a -- fromJust is safe
            hSrcs = map (replaceEq '.' '/') modules
            cObjs = [ buildPath -/- src -<.> osuf way | src <- cSrcs ]
            hObjs = [ buildPath -/- src  <.> osuf way | src <- hSrcs ]

        need $ cObjs ++ hObjs -- this will create split objects if required

        split <- interpret target splitObjects
        splitObjs <- if split
            then fmap concat $ forM hSrcs $ \src -> do
                let files = buildPath -/- src ++ "_" ++ osuf way ++ "_split/*"
                fmap (map unifyPath) $ getDirectoryFiles "" [files]
            else return []

        build $ fullTarget target (cObjs ++ hObjs ++ splitObjs) Ar [a]

        synopsis <- interpret target $ getPkgData Synopsis
        putSuccess $ "/--------\n| Successfully built package library '"
            ++ pkgName pkg
            ++ "' (stage " ++ show stage ++ ", way "++ show way ++ ")."
        putSuccess $ "| Package synopsis: "
            ++ dropWhileEnd isPunctuation synopsis ++ "." ++ "\n\\--------"

    -- TODO: this looks fragile as haskell objects can match this rule if their
    -- names start with "HS" and they are on top of the module hierarchy.
    priority 2 $ (buildPath -/- "HS*.o") %> \o -> do
        cSrcs   <- interpret target $ getPkgDataList CSrcs
        modules <- interpret target $ getPkgDataList Modules
        let hSrcs = map (replaceEq '.' '/') modules
            cObjs = [ buildPath -/- src -<.> "o" | src <- cSrcs ]
            hObjs = [ buildPath -/- src  <.> "o" | src <- hSrcs ]
        need $ cObjs ++ hObjs
        build $ fullTarget target (cObjs ++ hObjs) Ld [o]
