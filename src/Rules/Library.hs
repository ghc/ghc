module Rules.Library (buildPackageLibrary, cSources, hSources) where

import Data.Char

import Base hiding (splitPath)
import Expression
import GHC
import Oracles
import Rules.Actions
import Rules.Gmp
import Rules.Resources
import Settings
import qualified System.Directory as IO

buildPackageLibrary :: Resources -> PartialTarget -> Rules ()
buildPackageLibrary _ target @ (PartialTarget stage pkg) = do
    let buildPath = targetPath stage pkg -/- "build"

    -- TODO: handle dynamic libraries
    matchBuildResult buildPath "a" ?> \a -> do

        removeFileIfExists a
        cSrcs <- cSources target
        hSrcs <- hSources target

        -- TODO: simplify handling of AutoApply.cmm
        let way   = detectWay a -- TODO: eliminate differences below
            cObjs = [ buildPath -/- src -<.> osuf way | src <- cSrcs
                    , not ("//AutoApply.cmm" ?== src) ]
                 ++ [ src -<.> osuf way | src <- cSrcs, "//AutoApply.cmm" ?== src ]
            hObjs = [ buildPath -/- src  <.> osuf way | src <- hSrcs ]

        -- This will create split objects if required (we don't track them
        -- explicitly as this would needlessly bloat the Shake database).
        need $ cObjs ++ hObjs

        split <- interpretPartial target splitObjects
        splitObjs <- if not split then return hObjs else -- TODO: make clearer!
            fmap concat $ forM hSrcs $ \src -> do
                let splitPath = buildPath -/- src ++ "_" ++ osuf way ++ "_split"
                contents <- liftIO $ IO.getDirectoryContents splitPath
                return . map (splitPath -/-)
                       . filter (not . all (== '.')) $ contents

        eObjs <- extraObjects target
        let objs = cObjs ++ splitObjs ++ eObjs

        asuf <- libsuf way
        let isLib0 = ("//*-0" <.> asuf) ?== a
        if isLib0
        then build $ fullTarget target Ar [] [a] -- TODO: scan for dlls
        else build $ fullTarget target Ar objs [a]

        synopsis <- interpretPartial target $ getPkgData Synopsis
        unless isLib0 . putSuccess $ renderLibrary
            ("'" ++ pkgNameString pkg ++ "' (" ++ show stage ++ ", way "++ show way ++ ").")
            a
            (dropWhileEnd isPunctuation synopsis)

    -- TODO: simplify handling of AutoApply.cmm
    -- TODO: this looks fragile as haskell objects can match this rule if their
    -- names start with "HS" and they are on top of the module hierarchy.
    -- This happens with hsc2hs, which has top-level file HSCParser.hs.
    when (pkg /= hsc2hs) $ priority 2 $ (buildPath -/- "HS*.o") %> \obj -> do
        cSrcs <- cSources target
        hSrcs <- hSources target
        let cObjs = [ buildPath -/- src -<.> "o" | src <- cSrcs
                    , not ("//AutoApply.cmm" ?== src) ]
                 ++ [ src -<.> "o" | src <- cSrcs, "//AutoApply.cmm" ?== src ]
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

extraObjects :: PartialTarget -> Action [FilePath]
extraObjects (PartialTarget _ pkg)
    | pkg == integerGmp = do
        orderOnly [gmpLibraryH] -- TODO: move this dependency elsewhere, #113?
        -- FIXME: simplify after Shake's getDirectoryFiles bug is fixed, #168
        exists <- doesDirectoryExist gmpObjects
        if exists
        then getDirectoryFiles "" [gmpObjects -/- "*.o"]
        else return []
    | otherwise         = return []
