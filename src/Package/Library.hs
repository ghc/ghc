{-# LANGUAGE NoImplicitPrelude #-}
module Package.Library (buildPackageLibrary) where

import Package.Base

argListDir :: FilePath
argListDir = "shake/arg/buildPackageLibrary"

arArgs :: [FilePath] -> FilePath -> Args
arArgs objs result = args [ arg "q"
                          , arg result
                          , args objs ]

ldArgs :: Stage -> [FilePath] -> FilePath -> Args
ldArgs stage objs result = args [ args $ ConfLdLinkerArgs stage
                                , arg "-r"
                                , arg "-o"
                                , arg result
                                , args objs ]

arRule :: Package -> TodoItem -> Rules ()
arRule pkg @ (Package _ path _ _) todo @ (stage, dist, _) =
    let buildDir = path </> dist </> "build"
    in
    (buildDir <//> "*a") %> \out -> do
        let way = detectWay $ tail $ takeExtension out
        cObjs <- pkgCObjects path dist way
        hsObjs <- pkgDepHsObjects path dist way
        need $ cObjs ++ hsObjs
        libHsObjs <- pkgLibHsObjects path dist stage way
        liftIO $ removeFiles "." [out]
        -- Splitting argument list into chunks as otherwise Ar chokes up
        maxChunk <- argSizeLimit
        forM_ (chunksOfSize maxChunk $ cObjs ++ libHsObjs) $ \objs -> do
            run Ar $ arArgs objs $ unifyPath out
        -- Finally, record the argument list
        need [argListPath argListDir pkg stage]

ldRule :: Package -> TodoItem -> Rules ()
ldRule pkg @ (Package name path _ _) todo @ (stage, dist, _) =
    let pathDist = path </> dist
        buildDir = pathDist </> "build"
    in
    priority 2 $ (buildDir </> "*.o") %> \out -> do
        cObjs <- pkgCObjects path dist vanilla
        hObjs <- pkgDepHsObjects path dist vanilla
        need $ cObjs ++ hObjs
        run Ld $ ldArgs stage (cObjs ++ hObjs) $ unifyPath out
        synopsis <- dropWhileEnd isPunctuation <$> showArg (Synopsis pathDist)
        putColoured Green $ "/--------\n| Successfully built package '"
            ++ name ++ "' (stage " ++ show stage ++ ")."
        putColoured Green $ "| Package synopsis: " ++ synopsis ++ "."
            ++ "\n\\--------"
        -- Finally, record the argument list
        need [argListPath argListDir pkg stage]

argListRule :: Package -> TodoItem -> Rules ()
argListRule pkg @ (Package _ path _ _) todo @ (stage, dist, settings) =
    (argListPath argListDir pkg stage) %> \out -> do
        need $ ["shake/src/Package/Library.hs"] ++ sourceDependecies
        cObjsV  <- pkgCObjects path dist vanilla
        hsObjsV <- pkgDepHsObjects path dist vanilla
        ldList  <- argList Ld $ ldArgs stage (cObjsV ++ hsObjsV) "output.o"
        ways'   <- ways settings
        arList  <- forM ways' $ \way -> do
            cObjs  <- pkgCObjects path dist way
            hsObjs <- pkgLibHsObjects path dist stage way
            suffix <- libsuf way
            argListWithComment
                ("way '" ++ tag way ++ "'")
                Ar
                (arArgs (cObjs ++ hsObjs) $ "output" <.> suffix)
        writeFileChanged out $ unlines $ [ldList] ++ arList

buildPackageLibrary :: Package -> TodoItem -> Rules ()
buildPackageLibrary = argListRule <> arRule <> ldRule
