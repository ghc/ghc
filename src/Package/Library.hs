{-# LANGUAGE NoImplicitPrelude #-}
module Package.Library (buildPackageLibrary) where

import Package.Base

arRule :: Package -> TodoItem -> Rules ()
arRule (Package _ path _) (stage, dist, _) =
    let buildDir = path </> dist </> "build"
        pkgData  = path </> dist </> "package-data.mk"
    in
    (buildDir <//> "*a") %> \out -> do
        let way = detectWay $ tail $ takeExtension out
        need ["shake/src/Package/Library.hs"]
        depObjs <- pkgDepObjects path dist way
        need depObjs
        libObjs <- pkgLibObjects path dist stage way
        liftIO $ removeFiles "." [out]
        terseRun Ar $ "q" <+> out <+> libObjs

{- "C:/msys/home/chEEtah/ghc/inplace/mingw/bin/ld.exe"  -r -o 
libraries/deepseq/dist-install/build/HSdeeps_FT5iVCELxOr62eHY0nbvnU.o  
libraries/deepseq/dist-install/build/Control/DeepSeq.o
-}

ldRule :: Package -> TodoItem -> Rules ()
ldRule (Package name path _) (stage, dist, _) =
    let buildDir = path </> dist </> "build"
        pkgData  = path </> dist </> "package-data.mk"
    in
    priority 2 $ (buildDir </> "*.o") %> \out -> do
        need ["shake/src/Package/Library.hs"]
        depObjs <- pkgDepObjects path dist vanilla
        need depObjs
        terseRun Ld $ arg (ConfLdLinkerArgs stage)
            <> arg ["-r", "-o", out]
            <> arg depObjs
        synopsis <- unwords <$> arg (Synopsis pkgData)
        putNormal $ "Successfully built package " ++ name ++ "." 
        putNormal $ "Package synopsis: " ++ synopsis ++ "."

buildPackageLibrary :: Package -> TodoItem -> Rules ()
buildPackageLibrary = arRule <> ldRule