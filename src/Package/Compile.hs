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
        buildDir = unifyPath $ pathDist </> "build"
    in args [ suffixArgs way
            , wayHcArgs way
            , args SrcHcArgs
            , packageArgs stage pathDist
            , includeGhcArgs path dist
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

compileC :: Package -> TodoItem -> [FilePath] -> FilePath -> Action ()
compileC pkg todo @ (stage, _, _) deps obj = do
    need deps
    let srcs = filter ("//*.c" ?==) deps
    run (Gcc stage) $ gccArgs pkg todo srcs obj

compileHaskell :: Package -> TodoItem -> FilePath -> Way -> Action ()
compileHaskell pkg @ (Package _ path _) todo @ (stage, dist, _) obj way = do
    let buildDir = unifyPath $ path </> dist </> "build"
    -- TODO: keep only vanilla dependencies in 'haskell.deps'
    deps <- args $ DependencyList (buildDir </> "haskell.deps") obj
    let (srcs, his) = partition ("//*hs" ?==) deps
        objs = map (-<.> osuf way) his
    -- Need *.o files instead of *.hi files to avoid recursive rules
    need deps
    run (Ghc stage) $ ghcArgs pkg todo way srcs obj

buildRule :: Package -> TodoItem -> Rules ()
buildRule pkg @ (Package name path _) todo @ (stage, dist, _) =
    let buildDir = unifyPath $ path </> dist </> "build"
        cDepFile = buildDir </> "c.deps"
    in
    forM_ allWays $ \way -> do -- TODO: optimise (too many ways in allWays)
        let oPattern  = "*." ++ osuf way
        let hiPattern = "*." ++ hisuf way

        (buildDir <//> hiPattern) %> \hi -> do
            let obj = hi -<.> osuf way
            -- TODO: Understand why 'need [obj]' doesn't work, leading to
            -- recursive rules error. Below is a workaround.
            -- putColoured Yellow $ "Hi " ++ hi
            compileHaskell pkg todo obj way

        (buildDir <//> oPattern) %> \obj -> do
            let vanillaObjName = takeFileName obj -<.> "o"
            cDeps <- args $ DependencyList cDepFile vanillaObjName
            if null cDeps
            then compileHaskell pkg todo obj way
            else compileC pkg todo cDeps obj
            -- Finally, record the argument list
            need [argListPath argListDir pkg stage]

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
        gccList <- forM ways' $ \way ->
            argListWithComment
                ("way '" ++ tag way ++ "'")
                (Gcc stage)
                (gccArgs pkg todo ["input.c"] $ "output" <.> osuf way)

        writeFileChanged out $ unlines ghcList ++ "\n" ++ unlines gccList

buildPackageCompile :: Package -> TodoItem -> Rules ()
buildPackageCompile = argListRule <> buildRule
