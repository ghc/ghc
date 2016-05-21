module Rules.Program (buildProgram) where

import Data.Char

import Base
import Context
import Expression
import GHC
import Oracles.Config.Setting
import Oracles.Dependencies
import Oracles.PackageData
import Rules.Actions
import Rules.Library
import Rules.Wrappers.Ghc
import Rules.Wrappers.GhcPkg
import Settings
import Target

-- TODO: Move to buildRootPath, see #113.
-- | Directory for wrapped binaries.
programInplaceLibPath :: FilePath
programInplaceLibPath = "inplace/lib/bin"

-- | Wrapper is parameterised by the path to the wrapped binary.
type Wrapper = FilePath -> Expr String

-- | List of wrappers we build.
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

-- | Replace 'programInplacePath' with 'programInplaceLibPath' in a given path.
computeWrappedPath :: FilePath -> Maybe FilePath
computeWrappedPath =
    fmap (programInplaceLibPath ++) . stripPrefix programInplacePath

buildWrapper :: Context -> Wrapper -> FilePath -> FilePath -> Action ()
buildWrapper context@Context {..} wrapper wrapperPath binPath = do
    contents <- interpretInContext context $ wrapper binPath
    writeFileChanged wrapperPath contents
    makeExecutable wrapperPath
    putSuccess $ "| Successfully created wrapper for " ++
        quote (pkgNameString package) ++ " (" ++ show stage ++ ")."

-- TODO: Get rid of the Paths_hsc2hs.o hack.
-- TODO: Do we need to consider other ways when building programs?
buildBinary :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildBinary rs context@Context {..} bin = do
    hSrcs   <- hSources context
    binDeps <- if stage == Stage0 && package == ghcCabal
        then return [ pkgPath package -/- src <.> "hs" | src <- hSrcs ]
        else do
            ways <- interpretInContext context getLibraryWays
            deps <- contextDependencies context
            needContext [ dep { way = w } | dep <- deps, w <- ways ]
            cSrcs <- cSources context -- TODO: Drop code duplication (Library.hs).
            let path = buildPath context
            return $ [ path -/- src -<.> osuf vanilla | src <- cSrcs       ]
                  ++ [ path -/- src  <.> osuf vanilla | src <- hSrcs       ]
                  ++ [ path -/- "Paths_hsc2hs.o"      | package == hsc2hs  ]
                  ++ [ path -/- "Paths_haddock.o"     | package == haddock ]
    need binDeps
    buildWithResources rs $ Target context (Ghc Link stage) binDeps [bin]
    synopsis <- interpretInContext context $ getPkgData Synopsis
    putSuccess $ renderProgram
        (quote (pkgNameString package) ++ " (" ++ show stage ++ ").")
        bin
        (dropWhileEnd isPunctuation synopsis)
