{-# LANGUAGE NoImplicitPrelude #-}
module Package.Compile (buildPackageCompile) where

import Package.Base

argListDir :: FilePath
argListDir = "shake/arg/buildPackageCompile"

suffixArgs :: Way -> Args
suffixArgs way =
    return ["-hisuf", hisuf way, "-osuf", osuf way, "-hcsuf", hcsuf way]

ghcArgs :: Package -> TodoItem -> Way -> [FilePath] -> FilePath -> Args
ghcArgs (Package _ path _) (stage, dist, _) way srcs result =
    let pathDist = path </> dist
        buildDir = toStandard $ pathDist </> "build"
    in args [ suffixArgs way
            , wayHcArgs way
            , args SrcHcArgs
            , packageArgs stage pathDist
            , includeHcArgs path dist
            , concatArgs ["-optP"] $ CppArgs pathDist
            , args $ HsArgs pathDist
            -- TODO: now we have both -O and -O2
            -- <> arg ["-O2"]
            , productArgs ["-odir", "-hidir", "-stubdir"] buildDir
            , when (splitObjects stage) $ arg "-split-objs"
            , args ("-c":srcs)
            , args ["-o", result] ]

gccArgs :: Package -> TodoItem -> [FilePath] -> FilePath -> Args
gccArgs (Package _ path _) (_, dist, _) srcs result =
    let pathDist = path </> dist
    in args [ args $ CcArgs pathDist
            , commonCcArgs
            , commonCcWarninigArgs
            , pathArgs "-I" path $ IncludeDirs pathDist
            , args ("-c":srcs)
            , args ["-o", result] ]

buildRule :: Package -> TodoItem -> Rules ()
buildRule pkg @ (Package name path _) todo @ (stage, dist, _) =
    let buildDir = toStandard $ path </> dist </> "build"
        hDepFile = buildDir </> "haskell.deps"
        cDepFile = buildDir </> "c.deps"
    in
    forM_ allWays $ \way -> do -- TODO: optimise (too many ways in allWays)
        let oPattern  = "*." ++ osuf way
        let hiPattern = "*." ++ hisuf way
        [buildDir <//> oPattern, buildDir <//> hiPattern] |%> \out -> do
            need [argListPath argListDir pkg stage, hDepFile, cDepFile]
            let obj = toStandard $ out -<.> osuf way
                vanillaObj = toStandard $ out -<.> "o"
            -- TODO: keep only vanilla dependencies in hDepFile
            hDeps <- args $ DependencyList hDepFile obj
            cDeps <- args $ DependencyList cDepFile $ takeFileName vanillaObj
            let hSrcs = filter ("//*hs" ?==) hDeps
                cSrcs = filter ("//*.c" ?==) cDeps
            -- Report impossible cases
            when (null $ hSrcs ++ cSrcs)
                $ redError_ $ "No source files found for "
                ++ toStandard out ++ "."
            when (not (null hSrcs) && not (null cSrcs))
                $ redError_ $ "Both c and Haskell sources found for "
                ++ toStandard out ++ "."
            -- Build using appropriate compiler
            need $ hDeps ++ cDeps
            when (not $ null hSrcs)
                $ terseRun (Ghc stage) $ ghcArgs pkg todo way hSrcs obj
            when (not $ null cSrcs)
                $ terseRun Gcc $ gccArgs pkg todo cSrcs obj

argListRule :: Package -> TodoItem -> Rules ()
argListRule pkg todo @ (stage, _, settings) =
    (argListPath argListDir pkg stage) %> \out -> do
        need $ ["shake/src/Package/Compile.hs"] ++ sourceDependecies
        ways' <- ways settings
        ghcList <- forM ways' $ \way ->
            argListWithComment
                ("way '" ++ tag way ++ "'")
                (Ghc stage)
                (ghcArgs pkg todo way ["input.hs"] $ "output" <.> osuf way)
        writeFileChanged out $ unlines ghcList

buildPackageCompile :: Package -> TodoItem -> Rules ()
buildPackageCompile = argListRule <> buildRule
