{-# LANGUAGE NoImplicitPrelude #-}
module Package.Compile (buildPackageCompile) where

import Package.Base
import Development.Shake.Util

argListDir :: FilePath
argListDir = "shake/arg/buildPackageCompile"

suffixArgs :: Way -> Args
suffixArgs way =
    return ["-hisuf", hisuf way, "-osuf", osuf way, "-hcsuf", hcsuf way]

ghcArgs :: Package -> TodoItem -> Way -> [FilePath] -> FilePath -> Args
ghcArgs (Package _ path _) (stage, dist, _) way srcs result =
    let pathDist = path </> dist
        buildDir = toStandard $ pathDist </> "build"
    in arg [ suffixArgs way
           , wayHcArgs way
           , arg SrcHcArgs
           , packageArgs stage pathDist
           , includeArgs path dist
           , concatArgs ["-optP"] $ CppOpts pathDist
           , arg $ HsOpts pathDist
           -- TODO: now we have both -O and -O2
           -- <> arg ["-O2"]
           , productArgs ["-odir", "-hidir", "-stubdir"] buildDir
           , when (splitObjects stage) $ arg "-split-objs"
           , arg ("-c":srcs)
           , arg ["-o", result] ]

buildRule :: Package -> TodoItem -> Rules ()
buildRule pkg @ (Package name path _) todo @ (stage, dist, _) =
    let buildDir = toStandard $ path </> dist </> "build"
        depFile  = buildDir </> takeBaseName name <.> "m"
    in
    [buildDir <//> "*o", buildDir <//> "*hi"] &%> \[out, _] -> do
        let way  = detectWay $ tail $ takeExtension out
        need [argListPath argListDir pkg stage, depFile]
        depContents <- parseMakefile <$> (liftIO $ readFile depFile)
        let deps = concat $ snd $ unzip $ filter ((== out) . fst) depContents
            srcs = filter ("//*hs" ?==) deps -- TODO: handle *.c sources
        need deps
        terseRun (Ghc stage) $ ghcArgs pkg todo way srcs (toStandard out)

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
