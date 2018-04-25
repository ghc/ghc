module Rules.Program (buildProgram) where

import Hadrian.Haskell.Cabal

import Base
import Context
import Expression hiding (stage, way)
import Oracles.ModuleFiles
import Oracles.PackageData
import Oracles.Setting
import Rules.Wrappers
import Settings
import Settings.Packages.Rts
import Target
import Utilities

-- | TODO: Drop code duplication
buildProgram :: [(Resource, Int)] -> Package -> Rules ()
buildProgram rs package = do
    forM_ [Stage0 ..] $ \stage -> do
        let context = vanillaContext stage package

        -- Rules for programs built in 'buildRoot'
        "//" ++ contextDir context -/- programName context <.> exe %> \bin ->
            buildBinaryAndWrapper rs bin =<< programContext stage package

        -- Rules for the GHC package, which is built 'inplace'
        when (package == ghc) $ do
            inplaceBinPath -/- programName context <.> exe %> \bin ->
                buildBinaryAndWrapper rs bin =<< programContext stage package

            inplaceLibBinPath -/- programName context <.> exe %> \bin ->
                buildBinary rs bin =<< programContext stage package

            inplaceLibBinPath -/- programName context <.> "bin" %> \bin ->
                buildBinary rs bin =<< programContext stage package

    -- Rules for other programs built in inplace directories
    when (package /= ghc) $ do
        let context0 = vanillaContext Stage0 package -- TODO: get rid of context0
        inplaceBinPath -/- programName context0 <.> exe %> \bin -> do
            stage <- installStage package -- TODO: get rid of fromJust
            buildBinaryAndWrapper rs bin =<< programContext (fromJust stage) package

        inplaceLibBinPath -/- programName context0 <.> exe %> \bin -> do
            stage   <- installStage package -- TODO: get rid of fromJust
            context <- programContext (fromJust stage) package
            if package /= iservBin then
                -- We *normally* build only unwrapped binaries in inplace/lib/bin
                buildBinary rs bin context
            else
                -- Build both binary and wrapper in inplace/lib/bin for iservBin
                buildBinaryAndWrapperLib rs bin context

        inplaceLibBinPath -/- programName context0 <.> "bin" %> \bin -> do
            stage <- installStage package -- TODO: get rid of fromJust
            buildBinary rs bin =<< programContext (fromJust stage) package

buildBinaryAndWrapperLib :: [(Resource, Int)] -> FilePath -> Context -> Action ()
buildBinaryAndWrapperLib rs bin context = do
    windows <- windowsHost
    if windows
    then buildBinary rs bin context -- We don't build wrappers on Windows
    else case lookup context inplaceWrappers of
        Nothing      -> buildBinary rs bin context -- No wrapper found
        Just wrapper -> do
            top <- topDirectory
            let libdir = top -/- inplaceLibPath
            let wrappedBin = inplaceLibBinPath -/- programName context <.> "bin"
            need [wrappedBin]
            buildWrapper context wrapper bin (WrappedBinary libdir (takeFileName wrappedBin))

buildBinaryAndWrapper :: [(Resource, Int)] -> FilePath -> Context -> Action ()
buildBinaryAndWrapper rs bin context = do
    windows <- windowsHost
    if windows
    then buildBinary rs bin context -- We don't build wrappers on Windows
    else case lookup context inplaceWrappers of
        Nothing      -> buildBinary rs bin context -- No wrapper found
        Just wrapper -> do
            top <- topDirectory
            let libPath    = top -/- inplaceLibPath
                wrappedBin = inplaceLibBinPath -/- takeFileName bin
            need [wrappedBin]
            buildWrapper context wrapper bin (WrappedBinary libPath (takeFileName bin))

buildWrapper :: Context -> Wrapper -> FilePath -> WrappedBinary -> Action ()
buildWrapper context@Context {..} wrapper wrapperPath wrapped = do
    contents <- interpretInContext context $ wrapper wrapped
    writeFileChanged wrapperPath contents
    makeExecutable wrapperPath
    putSuccess $ "| Successfully created wrapper for " ++
        quote (pkgName package) ++ " (" ++ show stage ++ ")."

buildBinary :: [(Resource, Int)] -> FilePath -> Context -> Action ()
buildBinary rs bin context@Context {..} = do
    binDeps <- if stage == Stage0 && package == ghcCabal
        then hsSources context
        else do
            needLibrary =<< contextDependencies context
            when (stage > Stage0) $ do
                ways <- interpretInContext context (getLibraryWays <> getRtsWays)
                needLibrary [ rtsContext { way = w } | w <- ways ]
            path   <- buildPath context
            cSrcs  <- pkgDataList (CSrcs path)
            cObjs  <- mapM (objectPath context) cSrcs
            hsObjs <- hsObjects context
            return $ cObjs ++ hsObjs
    need binDeps
    buildWithResources rs $ target context (Ghc LinkHs stage) binDeps [bin]
    synopsis <- traverse pkgSynopsis (pkgCabalFile package)
    putSuccess $ renderProgram
        (quote (pkgName package) ++ " (" ++ show stage ++ ").") bin synopsis
