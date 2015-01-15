{-# LANGUAGE NoImplicitPrelude #-}
module Package.Library (buildPackageLibrary) where

import Package.Base

arRule :: Package -> TodoItem -> Rules ()
arRule (Package _ path _) (stage, dist, _) =
    let buildDir = path </> dist </> "build"
    in
    (buildDir <//> "*a") %> \out -> do
        let way = detectWay $ tail $ takeExtension out
        need ["shake/src/Package/Library.hs"]
        depObjs <- pkgDepObjects path dist way
        need depObjs
        libObjs <- pkgLibObjects path dist stage way
        liftIO $ removeFiles "." [out]
        -- Splitting argument list into chunks as otherwise Ar chokes up
        maxChunk <- argSizeLimit
        forM_ (chunksOfSize maxChunk libObjs) $ \os -> do
            terseRun Ar [ arg "q"
                        , arg $ toStandard out
                        , arg os ]

ldRule :: Package -> TodoItem -> Rules ()
ldRule (Package name path _) (stage, dist, _) =
    let buildDir = path </> dist </> "build"
        pkgData  = path </> dist </> "package-data.mk"
    in
    priority 2 $ (buildDir </> "*.o") %> \out -> do
        need ["shake/src/Package/Library.hs"]
        depObjs <- pkgDepObjects path dist vanilla
        need depObjs
        terseRun Ld [ arg (ConfLdLinkerArgs stage)
                    , arg "-r"
                    , arg "-o"
                    , arg $ toStandard out
                    , arg depObjs ]
        synopsis <- unwords <$> arg (Synopsis pkgData)
        putNormal $ "/--------\nSuccessfully built package " ++ name ++ "."
        putNormal $ "Package synopsis: " ++ synopsis ++ ".\n\\--------"

buildPackageLibrary :: Package -> TodoItem -> Rules ()
buildPackageLibrary = arRule <> ldRule
