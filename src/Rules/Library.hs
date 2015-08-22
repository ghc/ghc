module Rules.Library (buildPackageLibrary) where

import Base hiding (splitPath, getDirectoryContents)
import Expression
import Oracles.PackageData
import Predicates (splitObjects)
import Rules.Actions
import Rules.Resources
import Settings
import System.Directory (getDirectoryContents)
import Target (PartialTarget (..), fullTarget)

buildPackageLibrary :: Resources -> PartialTarget -> Rules ()
buildPackageLibrary _ target @ (PartialTarget stage pkg) = do
    let path      = targetPath stage pkg
        buildPath = path -/- "build"

    -- TODO: handle dynamic libraries
    matchBuildResult buildPath "a" ?> \a -> do
        removeFileIfExists a
        cSrcs <- cSources target
        hSrcs <- hSources target

        let way   = detectWay a
            cObjs = [ buildPath -/- src -<.> osuf way | src <- cSrcs ]
            hObjs = [ buildPath -/- src  <.> osuf way | src <- hSrcs ]

        -- This will create split objects if required (we don't track them
        -- explicitly as this would needlessly bloat the Shake database).
        need $ cObjs ++ hObjs

        split <- interpretPartial target splitObjects
        splitObjs <- if not split then return [] else
            fmap concat $ forM hSrcs $ \src -> do
                let splitPath = buildPath -/- src ++ "_" ++ osuf way ++ "_split"
                contents <- liftIO $ getDirectoryContents splitPath
                return . map (splitPath -/-)
                       . filter (not . all (== '.')) $ contents

        build $ fullTarget target Ar (cObjs ++ hObjs ++ splitObjs) [a]

        synopsis <- interpretPartial target $ getPkgData Synopsis
        putSuccess $ "/--------\n| Successfully built package library '"
            ++ pkgName pkg
            ++ "' (stage " ++ show stage ++ ", way "++ show way ++ ")."
        putSuccess $ "| Package synopsis: "
            ++ dropWhileEnd isPunctuation synopsis ++ "." ++ "\n\\--------"

    -- TODO: this looks fragile as haskell objects can match this rule if their
    -- names start with "HS" and they are on top of the module hierarchy.
    priority 2 $ (buildPath -/- "HS*.o") %> \obj -> do
        cSrcs <- cSources target
        hSrcs <- hSources target
        let cObjs = [ buildPath -/- src -<.> "o" | src <- cSrcs ]
            hObjs = [ buildPath -/- src  <.> "o" | src <- hSrcs ]
        need $ cObjs ++ hObjs
        build $ fullTarget target Ld (cObjs ++ hObjs) [obj]

cSources :: PartialTarget -> Action [FilePath]
cSources target = interpretPartial target $ getPkgDataList CSrcs

hSources :: PartialTarget -> Action [FilePath]
hSources target = do
    modules <- interpretPartial target $ getPkgDataList Modules
    -- GHC.Prim is special: we do not build it
    return . map (replaceEq '.' '/') . filter (/= "GHC.Prim") $ modules
