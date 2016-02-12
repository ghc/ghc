{-# LANGUAGE RecordWildCards #-}
module Rules.Library (buildPackageLibrary, cSources, hSources) where

import Data.Char
import qualified System.Directory as IO

import Base hiding (split, splitPath)
import Context
import Expression
import GHC
import Oracles.PackageData
import Rules.Actions
import Rules.Gmp
import Rules.Resources
import Settings
import Target

-- TODO: Use way from Context, #207
buildPackageLibrary :: Resources -> Context -> Rules ()
buildPackageLibrary _ context @ (Context {..}) = do
    let buildPath = targetPath stage package -/- "build"

    -- TODO: handle dynamic libraries
    matchBuildResult buildPath "a" ?> \a -> do

        removeFileIfExists a
        cSrcs <- cSources context
        hSrcs <- hSources context

        -- TODO: simplify handling of AutoApply.cmm
        let w     = detectWay a -- TODO: eliminate differences below
            cObjs = [ buildPath -/- src -<.> osuf w | src <- cSrcs
                    , not ("//AutoApply.cmm" ?== src) ]
                 ++ [ src -<.> osuf w | src <- cSrcs, "//AutoApply.cmm" ?== src ]
            hObjs = [ buildPath -/- src  <.> osuf w | src <- hSrcs ]

        -- This will create split objects if required (we don't track them
        -- explicitly as this would needlessly bloat the Shake database).
        need $ cObjs ++ hObjs

        split <- interpretInContext context splitObjects
        splitObjs <- if not split then return hObjs else -- TODO: make clearer!
            fmap concat $ forM hSrcs $ \src -> do
                let splitPath = buildPath -/- src ++ "_" ++ osuf way ++ "_split"
                contents <- liftIO $ IO.getDirectoryContents splitPath
                return . map (splitPath -/-)
                       . filter (not . all (== '.')) $ contents

        eObjs <- extraObjects context
        let objs = cObjs ++ splitObjs ++ eObjs

        asuf <- libsuf way
        let isLib0 = ("//*-0" ++ asuf) ?== a
        if isLib0
        then build $ Target context Ar [] [a] -- TODO: scan for dlls
        else build $ Target context Ar objs [a]

        synopsis <- interpretInContext context $ getPkgData Synopsis
        unless isLib0 . putSuccess $ renderLibrary
            ("'" ++ pkgNameString package ++ "' (" ++ show stage ++ ", way "++ show way ++ ").")
            a
            (dropWhileEnd isPunctuation synopsis)

    -- TODO: simplify handling of AutoApply.cmm
    -- TODO: this looks fragile as haskell objects can match this rule if their
    -- names start with "HS" and they are on top of the module hierarchy.
    -- This happens with hsc2hs, which has top-level file HSCParser.hs.
    when (package /= hsc2hs) $ priority 2 $ (buildPath -/- "HS*.o") %> \obj -> do
        cSrcs <- cSources context
        hSrcs <- hSources context
        let cObjs = [ buildPath -/- src -<.> "o" | src <- cSrcs
                    , not ("//AutoApply.cmm" ?== src) ]
                 ++ [ src -<.> "o" | src <- cSrcs, "//AutoApply.cmm" ?== src ]
            hObjs = [ buildPath -/- src  <.> "o" | src <- hSrcs ]
        need $ cObjs ++ hObjs
        build $ Target context Ld (cObjs ++ hObjs) [obj]

cSources :: Context -> Action [FilePath]
cSources context = interpretInContext context $ getPkgDataList CSrcs

hSources :: Context -> Action [FilePath]
hSources context = do
    modules <- interpretInContext context $ getPkgDataList Modules
    -- GHC.Prim is special: we do not build it
    return . map (replaceEq '.' '/') . filter (/= "GHC.Prim") $ modules

extraObjects :: Context -> Action [FilePath]
extraObjects (Context _ package _)
    | package == integerGmp = do
        orderOnly [gmpLibraryH] -- TODO: move this dependency elsewhere, #113?
        -- FIXME: simplify after Shake's getDirectoryFiles bug is fixed, #168
        exists <- doesDirectoryExist gmpObjects
        if exists
        then map unifyPath <$> getDirectoryFiles "" [gmpObjects -/- "*.o"]
        else return []
    | otherwise         = return []
