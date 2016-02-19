module Rules.Library (
    buildPackageLibrary, buildPackageGhciLibrary, cSources, hSources
    ) where

import Data.Char
import qualified System.Directory as IO

import Base hiding (split, splitPath)
import Context
import Expression
import GHC
import Oracles.PackageData
import Rules.Actions
import Rules.Gmp
import Settings
import Target

buildPackageLibrary :: Context -> Rules ()
buildPackageLibrary context @ (Context {..}) = do
    let buildPath = contextPath context -/- "build"
        libPrefix = buildPath -/- "libHS" ++ pkgNameString package

    -- TODO: handle dynamic libraries
    matchVersionedFilePath libPrefix (waySuffix way <.> "a") ?> \a -> do
        removeFileIfExists a
        cSrcs <- cSources context
        hSrcs <- hSources context

        -- TODO: simplify handling of AutoApply.cmm, eliminate differences below
        let cObjs = [ buildPath -/- src -<.> osuf way | src <- cSrcs
                    , not ("//AutoApply.cmm" ?== src) ]
                 ++ [ src -<.> osuf way | src <- cSrcs, "//AutoApply.cmm" ?== src ]
            hObjs = [ buildPath -/- src  <.> osuf way | src <- hSrcs ]

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

buildPackageGhciLibrary :: Context -> Rules ()
buildPackageGhciLibrary context @ (Context {..}) = priority 2 $ do
    let buildPath = contextPath context -/- "build"
        libPrefix = buildPath -/- "HS" ++ pkgNameString package

    -- TODO: simplify handling of AutoApply.cmm
    matchVersionedFilePath libPrefix (waySuffix way <.> "o") ?> \obj -> do
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
