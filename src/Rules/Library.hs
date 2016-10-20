module Rules.Library (
    buildPackageLibrary, buildPackageGhciLibrary, cSources, hsSources
    ) where

import Data.Char
import qualified System.Directory as IO

import Base
import Context
import Expression
import Flavour
import GHC
import Oracles.PackageData
import Rules.Actions
import Rules.Generate
import Settings
import Settings.Paths
import Target
import UserSettings

buildPackageLibrary :: Context -> Rules ()
buildPackageLibrary context@Context {..} = do
    let path       = buildPath context
        libPrefix  = path -/- "libHS" ++ pkgNameString package

    -- TODO: handle dynamic libraries
    matchVersionedFilePath libPrefix (waySuffix way <.> "a") ?> \a -> do
        removeFile a
        asmSrcs <- asmSources context
        cSrcs   <- cSources   context
        cmmSrcs <- cmmSources context
        hsSrcs  <- hsSources  context

        let asmObjs = [ objFile    context src    | src <- asmSrcs ]
            cObjs   = [ objFile    context src    | src <- cSrcs   ]
            cmmObjs = [ objFile    context src    | src <- cmmSrcs ]
            hsObjs  = [ path -/- src <.> osuf way | src <- hsSrcs   ]

        -- This will create split objects if required (we don't track them
        -- explicitly as this would needlessly bloat the Shake database).
        need $ asmObjs ++ cObjs ++ cmmObjs ++ hsObjs

        split <- interpretInContext context $ splitObjects flavour
        splitObjs <- if not split then return hsObjs else -- TODO: make clearer!
            concatForM hsSrcs $ \src -> do
                let splitPath = path -/- src ++ "_" ++ osuf way ++ "_split"
                contents <- liftIO $ IO.getDirectoryContents splitPath
                return . map (splitPath -/-)
                       . filter (not . all (== '.')) $ contents

        eObjs <- extraObjects context
        let objs = asmObjs ++ cObjs ++ cmmObjs ++ splitObjs ++ eObjs

        asuf <- libsuf way
        let isLib0 = ("//*-0" ++ asuf) ?== a
        if isLib0
        then build $ Target context Ar []   [a] -- TODO: scan for dlls
        else build $ Target context Ar objs [a]

        synopsis <- interpretInContext context $ getPkgData Synopsis
        unless isLib0 . putSuccess $ renderLibrary
            (quote (pkgNameString package) ++ " (" ++ show stage ++ ", way " ++ show way ++ ").")
            a
            (dropWhileEnd isPunctuation synopsis)

buildPackageGhciLibrary :: Context -> Rules ()
buildPackageGhciLibrary context@Context {..} = priority 2 $ do
    let path = buildPath context
        libPrefix = path -/- "HS" ++ pkgNameString package

    matchVersionedFilePath libPrefix (waySuffix way <.> "o") ?> \obj -> do
        cSrcs  <- cSources context
        hsSrcs <- hsSources context
        eObjs  <- extraObjects context
        let cObjs  = map (objFile context) cSrcs
            hsObjs = [ path -/- src <.> osuf way | src <- hsSrcs ]
            objs   = cObjs ++ hsObjs ++ eObjs
        need objs
        build $ Target context Ld objs [obj]

-- | Given a 'Context' and a 'FilePath' to a source file, compute the 'FilePath'
-- to its object file. For example:
-- * "Task.c"                              -> "_build/stage1/rts/Task.thr_o"
-- * "_build/stage1/rts/cmm/AutoApply.cmm" -> "_build/stage1/rts/cmm/AutoApply.o"
objFile :: Context -> FilePath -> FilePath
objFile context@Context {..} src
    | isGenerated src = src -<.> osuf way
    | otherwise       = buildPath context -/- extension -/- src -<.> osuf way
  where
    extension = drop 1 $ takeExtension src
    isGenerated
        | extension == "c"   = isGeneratedCFile
        | extension == "cmm" = isGeneratedCmmFile
        | otherwise          = const False

asmSources :: Context -> Action [FilePath]
asmSources context = interpretInContext context $ getPkgDataList AsmSrcs

-- TODO: simplify
cSources :: Context -> Action [FilePath]
cSources context = do
    srcs <- interpretInContext context $ getPkgDataList CSrcs
    if way context == threaded
    then return srcs
    else return [ src | src <- srcs
                      , takeFileName src `notElem` ["Evac_thr.c", "Scav_thr.c"] ]

cmmSources :: Context -> Action [FilePath]
cmmSources context = interpretInContext context $ getPkgDataList CmmSrcs

hsSources :: Context -> Action [FilePath]
hsSources context = do
    modules <- interpretInContext context $ getPkgDataList Modules
    -- GHC.Prim is special: we do not build it.
    return . map (replaceEq '.' '/') . filter (/= "GHC.Prim") $ modules

extraObjects :: Context -> Action [FilePath]
extraObjects context
    | package context == integerGmp = do
        need [gmpLibraryH]
        map unifyPath <$> getDirectoryFiles "" [gmpObjects -/- "*.o"]
    | otherwise         = return []
