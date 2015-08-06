module Rules.Library (buildPackageLibrary) where

import Way
import Base
import Util
import Builder
import Switches
import Expression
import qualified Target
import Oracles.PackageData
import Settings.Util
import Settings.TargetDirectory
import Rules.Actions
import Rules.Resources
import Data.Maybe

buildPackageLibrary :: Resources -> StagePackageTarget -> Rules ()
buildPackageLibrary _ target = do
    let stage     = Target.stage target
        pkg       = Target.package target
        path      = targetPath stage pkg
        buildPath = path -/- "build"

    matchBuildResult buildPath "a" ?> \a -> do
        liftIO $ removeFiles "." [a]
        cSrcs   <- interpret target $ getPkgDataList CSrcs
        modules <- interpret target $ getPkgDataList Modules

        let way    = fromJust . detectWay $ a -- fromJust is safe
            hsSrcs = map (replaceEq '.' '/') modules
            cObjs  = [ buildPath -/- src -<.> osuf way | src <-  cSrcs ]
            hsObjs = [ buildPath -/- src  <.> osuf way | src <- hsSrcs ]

        need $ cObjs ++ hsObjs -- this will create split objects if required

        splitObjs <- fmap concat $ forM hsSrcs $ \src -> do
            let files = buildPath -/- src ++ "_" ++ osuf way ++ "_split/*"
            getDirectoryFiles "" [files]

        split <- interpret target splitObjects
        let allObjs = if split
                      then cObjs ++ hsObjs ++ splitObjs
                      else cObjs ++ hsObjs

        build $ fullTarget target allObjs Ar [a]

-- ldRule :: Resources -> StagePackageTarget -> Rules ()
-- ldRule pkg @ (Package name path _ _) todo @ (stage, dist, _) =
--     let pathDist = path </> dist
--         buildDir = pathDist </> "build"
--     in
--     priority 2 $ (buildDir </> "*.o") %> \out -> do
--         cObjs <- pkgCObjects path dist vanilla
--         hObjs <- pkgDepHsObjects path dist vanilla
--         need $ cObjs ++ hObjs
--         run Ld $ ldArgs stage (cObjs ++ hObjs) $ unifyPath out
--         synopsis <- dropWhileEnd isPunctuation <$> showArg (Synopsis pathDist)
--         putColoured Green $ "/--------\n| Successfully built package '"
--             ++ name ++ "' (stage " ++ show stage ++ ")."
--         putColoured Green $ "| Package synopsis: " ++ synopsis ++ "."
--             ++ "\n\\--------"
--         -- Finally, record the argument list
--         need [argListPath argListDir pkg stage]
