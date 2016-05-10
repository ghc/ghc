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
import Settings
import Target

buildPackageLibrary :: Context -> Rules ()
buildPackageLibrary context@Context {..} = do
    let path = buildPath context
        libPrefix = path -/- "libHS" ++ pkgNameString package

    -- TODO: handle dynamic libraries
    matchVersionedFilePath libPrefix (waySuffix way <.> "a") ?> \a -> do
        removeFile a
        cSrcs <- cSources context
        hSrcs <- hSources context

        let cObjs = [ objFile context src | src <- cSrcs
                    , takeFileName src `notElem` ["Evac_thr.c", "Scav_thr.c"]
                      || way == threaded ]
            hObjs = [ path -/- src <.> osuf way | src <- hSrcs ]

        -- This will create split objects if required (we don't track them
        -- explicitly as this would needlessly bloat the Shake database).
        need $ cObjs ++ hObjs

        split <- interpretInContext context splitObjects
        splitObjs <- if not split then return hObjs else -- TODO: make clearer!
            fmap concat $ forM hSrcs $ \src -> do
                let splitPath = path -/- src ++ "_" ++ osuf way ++ "_split"
                contents <- liftIO $ IO.getDirectoryContents splitPath
                return . map (splitPath -/-)
                       . filter (not . all (== '.')) $ contents

        eObjs <- extraObjects context
        let objs = cObjs ++ splitObjs ++ eObjs

        asuf <- libsuf way
        let isLib0 = ("//*-0" ++ asuf) ?== a
        if isLib0
        then build $ Target context Ar []   [a] -- TODO: scan for dlls
        else build $ Target context Ar objs [a]

        synopsis <- interpretInContext context $ getPkgData Synopsis
        unless isLib0 . putSuccess $ renderLibrary
            ("'" ++ pkgNameString package ++ "' (" ++ show stage ++ ", way "++ show way ++ ").")
            a
            (dropWhileEnd isPunctuation synopsis)

buildPackageGhciLibrary :: Context -> Rules ()
buildPackageGhciLibrary context@Context {..} = priority 2 $ do
    let path = buildPath context
        libPrefix = path -/- "HS" ++ pkgNameString package

    -- TODO: simplify handling of AutoApply.cmm
    matchVersionedFilePath libPrefix (waySuffix way <.> "o") ?> \obj -> do
            cSrcs <- cSources context
            hSrcs <- hSources context

            let cObjs = map (objFile context) cSrcs
                hObjs = [ path -/- src <.> osuf way | src <- hSrcs ]
                objs  = cObjs ++ hObjs
            need objs
            build $ Target context Ld objs [obj]

-- TODO: Get rid of code duplication and simplify. See also src2dep.
-- Given a 'Context' and a 'FilePath' to a source file, compute the 'FilePath'
-- to its object file. For example, in Context Stage1 rts threaded:
-- * "Task.c"                          -> "_build/stage1/rts/Task.thr_o"
-- * "_build/stage1/rts/sm/Evac_thr.c" -> "_build/stage1/rts/sm/Evac_thr.thr_o"
objFile :: Context -> FilePath -> FilePath
objFile context@Context {..} src
    | buildRootPath `isPrefixOf` src = src -<.> osuf way
    | otherwise                      = buildPath context -/- src -<.> osuf way

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
        map unifyPath <$> getDirectoryFiles "" [gmpObjects -/- "*.o"]
    | otherwise         = return []
