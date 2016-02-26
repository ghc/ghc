module Rules.Program (buildProgram) where

import Data.Char

import Base
import Context
import Expression
import GHC hiding (ghci)
import Oracles.Config.Setting
import Oracles.PackageData
import Rules.Actions
import Rules.Library
import Rules.Wrappers.Ghc
import Rules.Wrappers.GhcPkg
import Settings
import Settings.Builders.GhcCabal
import Target

-- TODO: move to buildRootPath, see #113
-- Directory for wrapped binaries
programInplaceLibPath :: FilePath
programInplaceLibPath = "inplace/lib/bin"

-- Wrapper is parameterised by the path to the wrapped binary
type Wrapper = FilePath -> Expr String

-- List of wrappers we build
wrappers :: [(Context, Wrapper)]
wrappers = [ (vanillaContext Stage0 ghc   , ghcWrapper   )
           , (vanillaContext Stage1 ghc   , ghcWrapper   )
           , (vanillaContext Stage0 ghcPkg, ghcPkgWrapper)]

buildProgram :: [(Resource, Int)] -> Context -> Rules ()
buildProgram rs context@Context {..} = do
    let match file = case programPath context of
            Nothing      -> False
            Just program -> program == file
        matchWrapped file = case programPath context of
            Nothing      -> False
            Just program -> case computeWrappedPath program of
                Nothing             -> False
                Just wrappedProgram -> wrappedProgram == file

    match ?> \bin -> do
        windows <- windowsHost
        if windows
        then buildBinary rs context bin -- We don't build wrappers on Windows
        else case find ((== context) . fst) wrappers of
            Nothing -> buildBinary rs context bin -- No wrapper found
            Just (_, wrapper) -> do
                let Just wrappedBin = computeWrappedPath bin
                need [wrappedBin]
                buildWrapper context wrapper bin wrappedBin

    matchWrapped ?> \bin -> buildBinary rs context bin

-- Replace programInplacePath with programInplaceLibPath in a given path
computeWrappedPath :: FilePath -> Maybe FilePath
computeWrappedPath =
    fmap (programInplaceLibPath ++) . stripPrefix programInplacePath

buildWrapper :: Context -> Wrapper -> FilePath -> FilePath -> Action ()
buildWrapper context@Context {..} wrapper wrapperPath binPath = do
    contents <- interpretInContext context $ wrapper binPath
    writeFileChanged wrapperPath contents
    makeExecutable wrapperPath
    putSuccess $ "| Successfully created wrapper for '" ++ pkgNameString package
               ++ "' (" ++ show stage ++ ")."

-- TODO: Get rid of the Paths_hsc2hs.o hack.
-- TODO: Do we need to consider other ways when building programs?
buildBinary :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildBinary rs context@(Context stage package _) bin = do
    let path = buildPath context
    cSrcs <- cSources context -- TODO: remove code duplication (Library.hs)
    hSrcs <- hSources context
    let cObjs = [ path -/- src -<.> osuf vanilla | src <- cSrcs   ]
        hObjs = [ path -/- src  <.> osuf vanilla | src <- hSrcs   ]
             ++ [ path -/- "Paths_hsc2hs.o"      | package == hsc2hs  ]
             ++ [ path -/- "Paths_haddock.o"     | package == haddock ]
        objs  = cObjs ++ hObjs
    ways     <- interpretInContext context getLibraryWays
    depNames <- interpretInContext context $ getPkgDataList TransitiveDepNames
    let libStage   = min stage Stage1 -- libraries are built only in Stage0/1
        libContext = vanillaContext libStage package
    pkgs <- interpretInContext libContext getPackages
    let deps = matchPackageNames (sort pkgs) (map PackageName $ sort depNames)
    libs <- fmap concat . forM deps $ \dep -> do
        let depContext = vanillaContext libStage dep
        ghciFlag <- interpretInContext depContext $ getPkgData BuildGhciLib
        libFiles <- fmap concat . forM ways $ \way -> do
            libFile  <- pkgLibraryFile  $ Context libStage dep way
            lib0File <- pkgLibraryFile0 $ Context libStage dep way
            dll0     <- needDll0 libStage dep
            return $ libFile : [ lib0File | dll0 ]
        ghciLib <- pkgGhciLibraryFile $ vanillaContext libStage dep
        return $ libFiles ++ [ ghciLib | ghciFlag == "YES" && stage == Stage1 ]
    let binDeps = if package == ghcCabal && stage == Stage0
                  then [ pkgPath package -/- src <.> "hs" | src <- hSrcs ]
                  else objs
    need $ binDeps ++ libs
    buildWithResources rs $ Target context (Ghc stage) binDeps [bin]
    synopsis <- interpretInContext context $ getPkgData Synopsis
    putSuccess $ renderProgram
        ("'" ++ pkgNameString package ++ "' (" ++ show stage ++ ").")
        bin
        (dropWhileEnd isPunctuation synopsis)
