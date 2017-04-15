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
import Rules.Wrappers.Ghc
import Rules.Wrappers.GhcPkg
import Rules.Wrappers.Runhaskell
import Settings
import Settings.Path
import Target
import UserSettings
import Util

-- | Wrapper is an expression depending on the 'FilePath' to the wrapped binary.
type Wrapper = FilePath -> Expr String

-- | List of wrappers we build.
wrappers :: [(Context, Wrapper)]
wrappers = [ (vanillaContext Stage0 ghc   , ghcWrapper   )
           , (vanillaContext Stage1 ghc   , ghcWrapper   )
           , (vanillaContext Stage1 runGhc, runhaskellWrapper)
           , (vanillaContext Stage0 ghcPkg, ghcPkgWrapper) ]

buildProgram :: [(Resource, Int)] -> Context -> Rules ()
buildProgram rs context@Context {..} = when (isProgram package) $ do
    let installStage = do
            latest <- latestBuildStage package -- fromJust below is safe
            return $ if package == ghc then stage else fromJust latest

    buildPath context -/- programName context <.> exe %>
        buildBinaryAndWrapper rs context

    -- Rules for programs built in install directories
    when (stage == Stage0 || package == ghc) $ do
        -- Some binaries in programInplacePath are wrapped
        programInplacePath -/- programName context <.> exe %> \bin -> do
            binStage <- installStage
            buildBinaryAndWrapper rs (context { stage = binStage }) bin
        -- We build only unwrapped binaries in programInplaceLibPath
        programInplaceLibPath -/- programName context <.> exe %> \bin -> do
            binStage <- installStage
            buildBinary rs (context { stage = binStage }) bin

buildBinaryAndWrapper :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildBinaryAndWrapper rs context bin = do
    windows <- windowsHost
    if windows
    then buildBinary rs context bin -- We don't build wrappers on Windows
    else case lookup context wrappers of
        Nothing      -> buildBinary rs context bin -- No wrapper found
        Just wrapper -> do
            let wrappedBin = programInplaceLibPath -/- takeFileName bin
            need [wrappedBin]
            buildWrapper context wrapper bin wrappedBin

buildWrapper :: Context -> Wrapper -> FilePath -> FilePath -> Action ()
buildWrapper context@Context {..} wrapper wrapperPath binPath = do
    contents <- interpretInContext context $ wrapper binPath
    writeFileChanged wrapperPath contents
    makeExecutable wrapperPath
    putSuccess $ "| Successfully created wrapper for " ++
        quote (pkgNameString package) ++ " (" ++ show stage ++ ")."

-- TODO: Get rid of the Paths_hsc2hs.o hack.
buildBinary :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildBinary rs context@Context {..} bin = do
    binDeps <- if stage == Stage0 && package == ghcCabal
        then hsSources context
        else do
            needContext =<< contextDependencies context
            when (stage > Stage0) $ do
                ways <- interpretInContext context (getLibraryWays <> getRtsWays)
                needContext [ rtsContext { way = w } | w <- ways ]
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
