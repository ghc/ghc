module Rules.Program (buildProgram) where

import Data.Char

import Base
import Context
import Expression hiding (stage, way)
import GHC
import Oracles.ModuleFiles
import Oracles.PackageData
import Oracles.Setting
import Rules.Wrappers
import Settings
import Settings.Path
import Target
import Utilities

-- TODO: Drop way in build rule generation?
buildProgram :: [(Resource, Int)] -> Context -> Rules ()
buildProgram rs context@Context {..} = when (isProgram package) $ do
    let installStage = do
            latest <- latestBuildStage package -- fromJust below is safe
            return $ if package == ghc then stage else fromJust latest

    buildPath context -/- programName context <.> exe %> \bin -> do
        context' <- programContext stage package
        buildBinaryAndWrapper rs context' bin

    when (package == ghc) $ want inplaceLibCopyTargets

    -- Rules for programs built in install directories
    when (stage == Stage0 || package == ghc) $ do
        -- Some binaries in inplace/bin are wrapped
        inplaceBinPath -/- programName context <.> exe %> \bin -> do
            context' <- programContext stage package
            binStage <- installStage
            buildBinaryAndWrapper rs (context' { stage = binStage }) bin

        inplaceLibBinPath -/- programName context <.> exe %> \bin -> do
            binStage <- installStage
            context' <- programContext stage package
            if package /= iservBin then
                -- We *normally* build only unwrapped binaries in inplace/lib/bin,
                buildBinary rs (context' { stage = binStage }) bin
            else
                -- build both binary and wrapper in inplace/lib/bin
                -- for ghc-iserv on *nix platform now
                buildBinaryAndWrapperLib rs (context' { stage = binStage }) bin

        inplaceLibBinPath -/- programName context <.> "bin" %> \bin -> do
            binStage <- installStage
            context' <- programContext stage package
            buildBinary rs (context' { stage = binStage }) bin

buildBinaryAndWrapperLib :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildBinaryAndWrapperLib rs context bin = do
    windows <- windowsHost
    if windows
    then buildBinary rs context bin -- We don't build wrappers on Windows
    else case lookup context inplaceWrappers of
        Nothing      -> buildBinary rs context bin -- No wrapper found
        Just wrapper -> do
            top <- topDirectory
            let libdir = top -/- inplaceLibPath
            let wrappedBin = inplaceLibBinPath -/- programName context <.> "bin"
            need [wrappedBin]
            buildWrapper context wrapper bin (WrappedBinary libdir (takeFileName wrappedBin))

buildBinaryAndWrapper :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildBinaryAndWrapper rs context bin = do
    windows <- windowsHost
    if windows
    then buildBinary rs context bin -- We don't build wrappers on Windows
    else case lookup context inplaceWrappers of
        Nothing      -> buildBinary rs context bin -- No wrapper found
        Just wrapper -> do
            top <- topDirectory
            let libdir = top -/- inplaceLibPath
            let wrappedBin = inplaceLibBinPath -/- takeFileName bin
            need [wrappedBin]
            buildWrapper context wrapper bin (WrappedBinary libdir (takeFileName bin))

buildWrapper :: Context -> Wrapper -> FilePath -> WrappedBinary -> Action ()
buildWrapper context@Context {..} wrapper wrapperPath wrapped = do
    contents <- interpretInContext context $ wrapper wrapped
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
            needLibrary =<< contextDependencies context
            when (stage > Stage0) $ do
                ways <- interpretInContext context (getLibraryWays <> getRtsWays)
                needLibrary [ rtsContext { way = w } | w <- ways ]
            let path = buildPath context
            cObjs  <- map (objectPath context) <$> pkgDataList (CSrcs path)
            hsObjs <- hsObjects context
            return $ cObjs ++ hsObjs
                  ++ [ path -/- "Paths_hsc2hs.o"  | package == hsc2hs  ]
                  ++ [ path -/- "Paths_haddock.o" | package == haddock ]
    need binDeps
    buildWithResources rs $ target context (Ghc LinkHs stage) binDeps [bin]
    synopsis <- interpretInContext context $ getPkgData Synopsis
    putSuccess $ renderProgram
        (quote (pkgNameString package) ++ " (" ++ show stage ++ ").")
        bin
        (dropWhileEnd isPunctuation synopsis)
