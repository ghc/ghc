module Rules.Program (buildProgram) where

import Data.Char

import Base
import Context
import Expression
import GHC
import Oracles.Config.Setting
import Oracles.Dependencies
import Oracles.ModuleFiles
import Oracles.PackageData
import Rules.Actions
import Rules.Wrappers.Ghc
import Rules.Wrappers.GhcPkg
import Settings
import Settings.Paths
import Target
import UserSettings

-- TODO: Move to buildRootPath, see #113.
-- | Directory for wrapped binaries.
programInplaceLibPath :: FilePath
programInplaceLibPath = "inplace/lib/bin"

-- | Wrapper is an expression depending on the 'FilePath' to the wrapped binary.
type Wrapper = FilePath -> Expr String

-- | List of wrappers we build.
wrappers :: [(Context, Wrapper)]
wrappers = [ (vanillaContext Stage0 ghc   , ghcWrapper   )
           , (vanillaContext Stage1 ghc   , ghcWrapper   )
           , (vanillaContext Stage0 ghcPkg, ghcPkgWrapper) ]

buildProgram :: [(Resource, Int)] -> Context -> Rules ()
buildProgram rs context@Context {..} = do
    let match file        = any (== file) (programPath context)
        matchWrapped file = any (== file) (programPath context >>= wrappedPath)
    match ?> \bin -> do
        windows <- windowsHost
        if windows
        then buildBinary rs context bin -- We don't build wrappers on Windows
        else case lookup context wrappers of
            Nothing      -> buildBinary rs context bin -- No wrapper found
            Just wrapper -> do
                let Just wrappedBin = wrappedPath bin
                need [wrappedBin]
                buildWrapper context wrapper bin wrappedBin

    matchWrapped ?> buildBinary rs context

-- | Replace 'programInplacePath' with 'programInplaceLibPath' in a given path.
wrappedPath :: FilePath -> Maybe FilePath
wrappedPath = fmap (programInplaceLibPath ++) . stripPrefix programInplacePath

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
    binDeps <- if stage == Stage0 && package == ghcCabal
        then hsSources context
        else do
            ways <- interpretInContext context getLibraryWays
            deps <- contextDependencies context
            needContext [ dep { way = w } | dep <- deps, w <- ways ]
            let path = buildPath context
            cObjs  <- map (objectPath context) <$> pkgDataList (CSrcs path)
            hsObjs <- hsObjects context
            return $ cObjs ++ hsObjs
                  ++ [ path -/- "Paths_hsc2hs.o"  | package == hsc2hs  ]
                  ++ [ path -/- "Paths_haddock.o" | package == haddock ]
    need binDeps
    buildWithResources rs $ Target context (Ghc LinkHs stage) binDeps [bin]
    synopsis <- interpretInContext context $ getPkgData Synopsis
    putSuccess $ renderProgram
        (quote (pkgNameString package) ++ " (" ++ show stage ++ ").")
        bin
        (dropWhileEnd isPunctuation synopsis)
