module Rules.Program (buildProgram) where

import Data.Char

import Base
import Expression
import GHC hiding (ghci)
import Oracles
import Rules.Actions
import Rules.Library
import Rules.Resources
import Rules.Wrappers.Ghc
import Rules.Wrappers.GhcPkg
import Settings
import Settings.Builders.GhcCabal

-- TODO: move to buildRootPath, see #113
-- Directory for wrapped binaries
programInplaceLibPath :: FilePath
programInplaceLibPath = "inplace/lib/bin"

-- Wrapper is parameterised by the path to the wrapped binary
type Wrapper = FilePath -> Expr String

-- List of wrappers we build
wrappers :: [(PartialTarget, Wrapper)]
wrappers = [ (PartialTarget Stage0 ghc, ghcWrapper)
           , (PartialTarget Stage1 ghc, ghcWrapper)
           , (PartialTarget Stage0 ghcPkg, ghcPkgWrapper)]

buildProgram :: Resources -> PartialTarget -> Rules ()
buildProgram _ target @ (PartialTarget stage pkg) = do
    let match file = case programPath stage pkg of
            Nothing      -> False
            Just program -> program == file
        matchWrapped file = case programPath stage pkg of
            Nothing      -> False
            Just program -> case computeWrappedPath program of
                Nothing             -> False
                Just wrappedProgram -> wrappedProgram == file

    match ?> \bin -> do
        windows <- windowsHost
        if windows
        then buildBinary target bin -- We don't build wrappers on Windows
        else case find ((== target) . fst) wrappers of
            Nothing -> buildBinary target bin -- No wrapper found
            Just (_, wrapper) -> do
                let Just wrappedBin = computeWrappedPath bin
                need [wrappedBin]
                buildWrapper target wrapper bin wrappedBin

    matchWrapped ?> \bin -> buildBinary target bin

-- Replace programInplacePath with programInplaceLibPath in a given path
computeWrappedPath :: FilePath -> Maybe FilePath
computeWrappedPath =
    fmap (programInplaceLibPath ++) . stripPrefix programInplacePath

buildWrapper :: PartialTarget -> Wrapper -> FilePath -> FilePath -> Action ()
buildWrapper target @ (PartialTarget stage pkg) wrapper wrapperPath binPath = do
    contents <- interpretPartial target $ wrapper binPath
    writeFileChanged wrapperPath contents
    makeExecutable wrapperPath
    putSuccess $ "| Successfully created wrapper for '" ++ pkgNameString pkg
               ++ "' (" ++ show stage ++ ")."

-- TODO: Get rid of the Paths_hsc2hs.o hack.
-- TODO: Do we need to consider other ways when building programs?
buildBinary :: PartialTarget -> FilePath -> Action ()
buildBinary target @ (PartialTarget stage pkg) bin = do
    let buildPath = targetPath stage pkg -/- "build"
    cSrcs <- cSources target -- TODO: remove code duplication (Library.hs)
    hSrcs <- hSources target
    let cObjs = [ buildPath -/- src -<.> osuf vanilla | src <- cSrcs   ]
        hObjs = [ buildPath -/- src  <.> osuf vanilla | src <- hSrcs   ]
             ++ [ buildPath -/- "Paths_hsc2hs.o"      | pkg == hsc2hs  ]
             ++ [ buildPath -/- "Paths_haddock.o"     | pkg == haddock ]
        objs  = cObjs ++ hObjs
    ways     <- interpretPartial target getWays
    depNames <- interpretPartial target $ getPkgDataList TransitiveDepNames
    let libStage  = min stage Stage1 -- libraries are built only in Stage0/1
        libTarget = PartialTarget libStage pkg
    pkgs     <- interpretPartial libTarget getPackages
    ghciFlag <- interpretPartial libTarget $ getPkgData BuildGhciLib
    let deps = matchPackageNames (sort pkgs) (map PackageName $ sort depNames)
        ghci = ghciFlag == "YES" && stage == Stage1
    libs <- fmap concat . forM deps $ \dep -> do
        let depTarget = PartialTarget libStage dep
        compId <- interpretPartial depTarget $ getPkgData ComponentId
        libFiles <- fmap concat . forM ways $ \way -> do
            libFile  <- pkgLibraryFile libStage dep compId           way
            lib0File <- pkgLibraryFile libStage dep (compId ++ "-0") way
            dll0     <- needDll0 libStage dep
            return $ [ libFile ] ++ [ lib0File | dll0 ]
        return $ libFiles ++ [ pkgGhciLibraryFile libStage dep compId | ghci ]
    let binDeps = if pkg == ghcCabal && stage == Stage0
                  then [ pkgPath pkg -/- src <.> "hs" | src <- hSrcs ]
                  else objs
    need $ binDeps ++ libs
    build $ fullTargetWithWay target (Ghc stage) vanilla binDeps [bin]
    synopsis <- interpretPartial target $ getPkgData Synopsis
    putSuccess $ renderBox
        [ "Successfully built program '"
          ++ pkgNameString pkg ++ "' (" ++ show stage ++ ")."
        , "Executable: " ++ bin
        , "Program synopsis: " ++ dropWhileEnd isPunctuation synopsis ++ "." ]
