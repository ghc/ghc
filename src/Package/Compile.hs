{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}
module Package.Compile (buildPackageCompile) where

import Package.Base
import Development.Shake.Util

-- "inplace/bin/ghc-stage1.exe" -hisuf hi -osuf  o -hcsuf hc -static  -H32m -O    -this-package-key deeps_FT5iVCELxOr62eHY0nbvnU -hide-all-packages -i -ilibraries/deepseq/. -ilibraries/deepseq/dist-install/build -ilibraries/deepseq/dist-install/build/autogen -Ilibraries/deepseq/dist-install/build -Ilibraries/deepseq/dist-install/build/autogen -Ilibraries/deepseq/.    -optP-include -optPlibraries/deepseq/dist-install/build/autogen/cabal_macros.h -package-key array_3w0nMK0JfaFJPpLFn2yWAJ -package-key base_469rOtLAqwTGFEOGWxSUiQ -package-key ghcpr_FgrV6cgh2JHBlbcx1OSlwt -Wall -XHaskell2010 -O2  -no-user-package-db -rtsopts      -odir libraries/deepseq/dist-install/build -hidir libraries/deepseq/dist-install/build -stubdir libraries/deepseq/dist-install/build -split-objs  -c libraries/deepseq/./Control/DeepSeq.hs -o libraries/deepseq/dist-install/build/Control/DeepSeq.o

suffixArgs :: Way -> Args
suffixArgs way = arg ["-hisuf", hisuf way, "-osuf", osuf way, "-hcsuf", hcsuf way]

buildPackageCompile :: Package -> TodoItem -> Rules ()
buildPackageCompile (Package name path _) (stage, dist, settings) =
    let buildDir = path </> dist
        pkgData  = buildDir </> "package-data.mk"
        depFile  = buildDir </> "build" </> name <.> "m"
    in
    (buildDir </> "build//*o") %> \out -> do
        let way  = detectWay $ tail $ takeExtension out
        need ["shake/src/Package/Compile.hs"] -- Track changes in this file
        need [depFile]
        depContents <- parseMakefile <$> (liftIO $ readFile depFile)
        let deps = concat $ snd $ unzip $ filter ((== out) . fst) depContents
            srcs = filter ("//*hs" ?==) deps
        need deps
        run (Ghc stage) $ suffixArgs way
            <> wayHcArgs way
            <> arg SrcHcOpts
            <> packageArgs stage pkgData
            <> arg "-i"
            <> includeArgs "-i" path     (SrcDirs pkgData)
            <> includeArgs "-i" buildDir ["build", "build/autogen"]
            <> includeArgs "-I" buildDir ["build", "build/autogen"]
            <> includeArgs "-I" path     (IncludeDirs pkgData)
            <> arg "-optP-include" -- TODO: Shall we also add -cpp?
            <> arg ("-optP" ++ buildDir </> "build/autogen/cabal_macros.h")
            <> arg ["-Wall", "-XHaskell2010", "-O2"] -- TODO: now we have both -O and -O2
            <> arg ["-odir"        , buildDir </> "build"]
            <> arg ["-hidir"       , buildDir </> "build"]
            <> arg ["-stubdir"     , buildDir </> "build"]
            <> arg "-split-objs"
            <> arg ("-c":srcs)
            <> arg ["-o", out]
